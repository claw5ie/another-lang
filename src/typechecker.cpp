struct Typechecker
{
  using LineInfo = Lexer::LineInfo;

  static Ast::Expr bool_type;

  static constexpr u16 MAX_BITS_IN_INTEGER = 64;

  static void typecheck(Ast &ast)
  {
    auto typechecker = Typechecker{
      .ast = &ast,
      .had_error = false,
    };
    typechecker.typecheck();
  }

  bool is_same_type(Ast::Expr *lhs, Ast::Expr *lhs_type, Ast::Expr *rhs, Ast::Expr *rhs_type)
  {
    assert((!lhs || lhs->tag != Ast::Expr::_Type)
           && lhs_type->tag == Ast::Expr::_Type
           && (!rhs || rhs->tag != Ast::Expr::_Type)
           && rhs_type->tag == Ast::Expr::_Type);

    switch (lhs_type->as.Type.tag)
    {
    case Ast::Type::_Bool:
      return lhs_type->as.Type.tag == rhs_type->as.Type.tag;
    case Ast::Type::_Int:
      return maybe_safe_cast_int_types(lhs, lhs_type, rhs, rhs_type);
    }

    UNREACHABLE();
  }

  // This functions shouldn't exist. Every time we check if two types are the same, we should perform type conversions, if necessary, but sometimes those can't happen (like when checking boolean expressions).
  bool is_same_type(Ast::Expr *lhs_type, Ast::Expr *rhs_type)
  {
    return is_same_type(nullptr, lhs_type, nullptr, rhs_type);
  }

  Ast::Expr *maybe_safe_cast_int_types(Ast::Expr *lhs, Ast::Expr *lhs_type, Ast::Expr *rhs, Ast::Expr *rhs_type)
  {
    assert(lhs_type->tag == Ast::Expr::_Type
           && rhs_type->tag == Ast::Expr::_Type);

    Ast::Expr *type = nullptr;
    auto should_cast_lhs = false;
    auto should_cast_rhs = false;
    auto &Lhs_Type = lhs_type->as.Type;
    auto &Rhs_Type = rhs_type->as.Type;

    if (Lhs_Type.tag != Ast::Type::_Int || Rhs_Type.tag != Ast::Type::_Int)
    {
      return nullptr;
    }
    else if (Lhs_Type.as.Int.is_signed == Rhs_Type.as.Int.is_signed)
    {
      if (Lhs_Type.as.Int.bits < Rhs_Type.as.Int.bits)
      {
        should_cast_lhs = true;
        type = rhs_type;
      }
      else if (Lhs_Type.as.Int.bits > Rhs_Type.as.Int.bits)
      {
        should_cast_rhs = true;
        type = lhs_type;
      }
      else
      {
        type = lhs_type;
      }
    }
    else if (Lhs_Type.as.Int.is_signed)
    {
      if (Rhs_Type.as.Int.bits < Lhs_Type.as.Int.bits)
      {
        should_cast_rhs = true;
        type = lhs_type;
      }
    }
    else
    {
      if (Lhs_Type.as.Int.bits < Rhs_Type.as.Int.bits)
      {
        should_cast_lhs = true;
        type = rhs_type;
      }
    }

    should_cast_lhs = should_cast_lhs && type && lhs;
    should_cast_rhs = should_cast_rhs && type && rhs;

    if (should_cast_lhs)
    {
      auto expr = ast->alloc<Ast::Expr>();
      *expr = *lhs;
      *lhs = Ast::Expr{
        .line_info = lhs->line_info,
        .tag = Ast::Expr::_Cast,
        .as = { .Cast = {
            .type = type,
            .expr = expr,
          } },
        .flags = 0,
      };
    }
    else if (should_cast_rhs)
    {
      auto expr = ast->alloc<Ast::Expr>();
      *expr = *rhs;
      *rhs = Ast::Expr{
        .line_info = rhs->line_info,
        .tag = Ast::Expr::_Cast,
        .as = { .Cast = {
            .type = type,
            .expr = expr,
          } },
        .flags = 0,
      };
    }

    return type;
  }

  Ast::Expr *maybe_safe_cast_negate_int(Ast::Expr *expr, Ast::Expr *type)
  {
    assert(type->tag == Ast::Expr::_Type);

    auto &Type = type->as.Type;

    if (Type.tag != Ast::Type::_Int)
      return nullptr;

    if (Type.as.Int.is_signed)
    {
      return type;
    }
    else if (Type.as.Int.bits < MAX_BITS_IN_INTEGER)
    {
      auto new_type = ast->alloc<Ast::Expr>();
      *new_type = *type;
      new_type->as.Type.as.Int = Ast::Type::Int{
        .bits = (u16)(Type.as.Int.bits + 1),
        .is_signed = true,
      };
      auto subexpr = ast->alloc<Ast::Expr>();
      *subexpr = *expr;
      *expr = Ast::Expr{
        .line_info = expr->line_info,
        .tag = Ast::Expr::_Cast,
        .as = { .Cast = {
            .type = new_type,
            .expr = subexpr,
          } },
        .flags = 0,
      };

      return new_type;
    }

    return nullptr;
  }

  void typecheck_type(Ast::Expr *type)
  {
    switch (type->tag)
    {
    case Ast::Expr::_Type:
    {
      auto &Type = type->as.Type;

      switch (Type.tag)
      {
      case Ast::Type::_Bool:
        break;
      case Ast::Type::_Int:
      {
        auto &Int = Type.as.Int;

        if (Int.bits == 0)
          type->flags |= Ast::Expr::TYPE_HAS_ZERO_BITS;
      } break;
      }
    } break;
    case Ast::Expr::_Binary_Op:
    case Ast::Expr::_Unary_Op:
    case Ast::Expr::_Cast:
    case Ast::Expr::_Boolean:
    case Ast::Expr::_Integer:
    case Ast::Expr::_Symbol:
      report_error(type->line_info, "expected type, not expression");
      COMPILER_EXIT_ERROR();
      break;
    case Ast::Expr::_Identifier:
      report_error(type->line_info, "can't alias types for now");
      COMPILER_EXIT_ERROR();
      break;
    }
  }

  Ast::Expr *typecheck_expr(Ast::Expr *expr)
  {
    switch (expr->tag)
    {
    case Ast::Expr::_Binary_Op:
    {
      auto &Binary_Op = expr->as.Binary_Op;

      auto lhs_type = typecheck_expr(Binary_Op.lhs);
      auto rhs_type = typecheck_expr(Binary_Op.rhs);

      switch (Binary_Op.tag)
      {
      case Ast::Expr::BinaryOp::Or:
      case Ast::Expr::BinaryOp::And:
      {
        if (!is_same_type(lhs_type, &bool_type) || !is_same_type(rhs_type, &bool_type))
        {
          report_error(Binary_Op.line_info, std::format("expected 'bool'/'bool', but got '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Eq:
      case Ast::Expr::BinaryOp::Neq:
      {
        if (!is_same_type(Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type))
        {
          report_error(Binary_Op.line_info, std::format("can't compare values of types '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Lt:
      case Ast::Expr::BinaryOp::Leq:
      case Ast::Expr::BinaryOp::Gt:
      case Ast::Expr::BinaryOp::Geq:
      {
        if (!maybe_safe_cast_int_types(Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type))
        {
          report_error(Binary_Op.line_info, std::format("can't compare values of types '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Add:
      case Ast::Expr::BinaryOp::Sub:
      case Ast::Expr::BinaryOp::Mul:
      case Ast::Expr::BinaryOp::Div:
      case Ast::Expr::BinaryOp::Mod:
      {
        auto result = maybe_safe_cast_int_types(Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);

        if (!result)
        {
          report_error(Binary_Op.line_info, std::format("can't perform arithmetic operation on values of types '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return result;
      };
      }

      UNREACHABLE();
    }
    case Ast::Expr::_Unary_Op:
    {
      auto &Unary_Op = expr->as.Unary_Op;

      auto subexpr_type = typecheck_expr(Unary_Op.subexpr);

      switch (Unary_Op.tag)
      {
      case Ast::Expr::UnaryOp::Not:
      {
        if (!is_same_type(subexpr_type, &bool_type))
        {
          report_error(expr->line_info, std::format("expected 'bool', but got '{}'", subexpr_type->type_to_string()));
          COMPILER_EXIT_ERROR();
        }

        return &bool_type;
      }
      case Ast::Expr::UnaryOp::Plus:
      {
        if (subexpr_type->as.Type.tag != Ast::Type::_Int)
        {
          report_error(expr->line_info, std::format("expected integer, but got '{}'", subexpr_type->type_to_string()));
          COMPILER_EXIT_ERROR();
        }

        return subexpr_type;
      }
      case Ast::Expr::UnaryOp::Minus:
      {
        auto result = maybe_safe_cast_negate_int(Unary_Op.subexpr, subexpr_type);

        if (!result)
        {
          report_error(expr->line_info, std::format("expected integer, but got '{}'", subexpr_type->type_to_string()));
          COMPILER_EXIT_ERROR();
        }

        return result;
      }
      }

      UNREACHABLE();
    }
    case Ast::Expr::_Cast:
    case Ast::Expr::_Type:
      UNREACHABLE();
    case Ast::Expr::_Boolean:
      return &bool_type;
    case Ast::Expr::_Integer:
    {
      auto Integer = expr->as.Integer;

      auto bits = Utils::log2(Integer);
      bits += (Integer != 0);
      auto type = ast->alloc<Ast::Expr>();
      *type = Ast::Expr{
        .line_info = expr->line_info,
        .tag = Ast::Expr::_Type,
        .as = { .Type = {
            .tag = Ast::Type::_Int,
            .as = { .Int = {
                .bits = (u16)bits,
                .is_signed = false,
              } },
          } },
        .flags = 0,
      };

      return type;
    }
    case Ast::Expr::_Identifier:
    {
      auto &Identifier = expr->as.Identifier;

      auto it = ast->symbol_table.map.find(Ast::SymbolKey{
          .name = Identifier,
        });

      if (it == ast->symbol_table.map.end()
          || it->second->line_info.offset > expr->line_info.offset)
      {
        report_error(expr->line_info, std::format("symbol '{}' is not defined", Identifier));
        COMPILER_EXIT_ERROR();
      }

      auto symbol = it->second;
      Ast::Expr *result = nullptr;

      switch (symbol->tag)
      {
      case Ast::Symbol::_Identifier:
        assert(symbol->as.Identifier.type);
        result = symbol->as.Identifier.type;
        break;
      case Ast::Symbol::_Variable:
        assert(symbol->as.Variable.type);
        result = symbol->as.Variable.type;
        break;
      }

      expr->tag = Ast::Expr::_Symbol;
      expr->as.Symbol = symbol;
      if (!(result->flags & Ast::Expr::TYPE_HAS_ZERO_BITS))
        expr->flags |= Ast::Expr::IS_LVALUE;

      return result;
    }
    case Ast::Expr::_Symbol:
      UNREACHABLE();
    }

    UNREACHABLE();
  }

  void typecheck_pattern_match(Ast::Expr *pattern, Ast::Expr *type)
  {
    assert(type->tag == Ast::Expr::_Type);

    switch (pattern->tag)
    {
    case Ast::Expr::_Binary_Op:
      report_error(pattern->as.Binary_Op.line_info, "can't pattern match binary operators");
      break;
    case Ast::Expr::_Unary_Op:
      report_error(pattern->line_info, "can't pattern match unary operators");
      break;
    case Ast::Expr::_Cast:
      report_error(pattern->line_info, "can't pattern match cast expression");
      break;
    case Ast::Expr::_Type:
      report_error(pattern->line_info, "unexpected type in variable definition");
      break;
    case Ast::Expr::_Boolean:
    {
      if (!is_same_type(type, &bool_type))
      {
        report_error(type->line_info, std::format("expected 'bool' type, but got '{}'", type->type_to_string()));
      }
    } break;
    case Ast::Expr::_Integer:
    {
      if (type->as.Type.tag != Ast::Type::_Int)
      {
        report_error(type->line_info, std::format("expected integer type, but got '{}'", type->type_to_string()));
      }
    } break;
    case Ast::Expr::_Symbol:
    {
      auto Symbol = pattern->as.Symbol;
      // TODO: should probably call 'typecheck_symbol' here.
      assert(Symbol->tag == Ast::Symbol::_Identifier);

      auto &Identifier = Symbol->as.Identifier;
      Identifier.type = type;
    } break;
    case Ast::Expr::_Identifier:
      UNREACHABLE();
    }
  }

  void typecheck_symbol(Ast::Symbol *symbol)
  {
    switch (symbol->tag)
    {
    case Ast::Symbol::_Variable:
    {
      auto &Variable = symbol->as.Variable;

      typecheck_type(Variable.type);
      typecheck_pattern_match(Variable.pattern, Variable.type);

      if (Variable.value)
      {
        auto value_type = typecheck_expr(Variable.value);

        if (!is_same_type(nullptr, Variable.type, Variable.value, value_type))
        {
          report_error(Variable.value->line_info, std::format("expected '{}', but got '{}'", Variable.type->type_to_string(), value_type->type_to_string()));
        }
      }
    } break;
    case Ast::Symbol::_Identifier:
      UNREACHABLE();
    }
  }

  void typecheck_stmt(Ast::Stmt *stmt)
  {
    switch (stmt->tag)
    {
    case Ast::Stmt::_Assign:
    {
      auto &Assign = stmt->as.Assign;

      auto lhs_type = typecheck_expr(Assign.lhs);

      if (!(Assign.lhs->flags & Ast::Expr::IS_LVALUE))
      {
        report_error(Assign.lhs->line_info, "expression is not a lvalue");
      }

      auto rhs_type = typecheck_expr(Assign.rhs);

      if (!is_same_type(nullptr, lhs_type, Assign.rhs, rhs_type))
      {
        report_error(Assign.rhs->line_info, std::format("expected '{}', but got '{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
      }
    } break;
    case Ast::Stmt::_Symbol:
      typecheck_symbol(stmt->as.Symbol);
      break;
    case Ast::Stmt::_Expr:
      typecheck_expr(stmt->as.Expr);
      break;
    }
  }

  void typecheck()
  {
    for (auto node = ast->stmt_list.first; node; node = node->next)
      typecheck_stmt(node->data);

    if (had_error)
      exit(EXIT_FAILURE);
  }

  void report_error(LineInfo line_info, std::string_view text)
  {
    had_error = true;
    REPORT_ERROR_HELPER(ast->filepath, line_info, "error", text);
  }

  Ast *ast;
  bool had_error;
};

Ast::Expr Typechecker::bool_type = {
  .line_info = { },
  .tag = Ast::Expr::_Type,
  .as = { .Type = {
      .tag = Ast::Type::_Bool,
      .as = { },
    } },
  .flags = 0,
};
