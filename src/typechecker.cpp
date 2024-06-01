struct Typechecker
{
  using LineInfo = Lexer::LineInfo;

  static Ast::Expr bool_type;
  static Ast::Expr int_type;

  static void typecheck(Ast &ast)
  {
    auto typechecker = Typechecker{
      .ast = &ast,
      .had_error = false,
    };
    typechecker.typecheck();
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
        if (!lhs_type->is_same_type(rhs_type) || !lhs_type->is_same_type(&bool_type))
        {
          report_error(expr->line_info, std::format("expected 'bool'/'bool', but got '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Eq:
      case Ast::Expr::BinaryOp::Neq:
      {
        if (!lhs_type->is_same_type(rhs_type))
        {
          report_error(expr->line_info, std::format("can't compare values of types '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Lt:
      case Ast::Expr::BinaryOp::Leq:
      case Ast::Expr::BinaryOp::Gt:
      case Ast::Expr::BinaryOp::Geq:
      {
        if (!lhs_type->is_same_type(rhs_type) || !lhs_type->is_same_type(&int_type))
        {
          report_error(expr->line_info, std::format("expected 'int'/'int', but got '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &bool_type;
      }
      case Ast::Expr::BinaryOp::Add:
      case Ast::Expr::BinaryOp::Sub:
      case Ast::Expr::BinaryOp::Mul:
      case Ast::Expr::BinaryOp::Div:
      case Ast::Expr::BinaryOp::Mod:
      {
        if (!lhs_type->is_same_type(rhs_type) || !lhs_type->is_same_type(&int_type))
        {
          report_error(expr->line_info, std::format("expected 'int'/'int', but got '{}'/'{}'", lhs_type->type_to_string(), rhs_type->type_to_string()));
        }

        return &int_type;
      };
      }

      UNREACHABLE();
    };
    case Ast::Expr::_Unary_Op:
    {
      auto &Unary_Op = expr->as.Unary_Op;

      auto subexpr_type = typecheck_expr(Unary_Op.subexpr);

      switch (Unary_Op.tag)
      {
      case Ast::Expr::UnaryOp::Not:
      {
        if (!subexpr_type->is_same_type(&bool_type))
        {
          report_error(expr->line_info, std::format("expected 'bool', but got '{}'", subexpr_type->type_to_string()));
          COMPILER_EXIT_ERROR();
        }

        return &bool_type;
      }
      case Ast::Expr::UnaryOp::Plus:
      case Ast::Expr::UnaryOp::Minus:
      {
        if (!subexpr_type->is_same_type(&int_type))
        {
          report_error(expr->line_info, std::format("expected integer, but got '{}'", subexpr_type->type_to_string()));
          COMPILER_EXIT_ERROR();
        }

        return &int_type;
      }
      }

      UNREACHABLE();
    };
    case Ast::Expr::_Type:
      UNREACHABLE();
    case Ast::Expr::_Boolean:
      return &bool_type;
    case Ast::Expr::_Integer:
      return &int_type;
    case Ast::Expr::_Identifier:
      UNREACHABLE();
    }

    UNREACHABLE();
  }

  void typecheck()
  {
    for (auto node = ast->exprs.first; node; node = node->next)
      typecheck_expr(node->data);

    if (had_error)
      COMPILER_EXIT_ERROR();
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
};
Ast::Expr Typechecker::int_type = {
  .line_info = { },
  .tag = Ast::Expr::_Type,
  .as = { .Type = {
      .tag = Ast::Type::_Int,
      .as = { .Int = {
          .bits = 64,
          .is_signed = true,
        } },
    } },
};
