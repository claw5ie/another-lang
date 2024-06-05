struct Transpiler
{
  static void transpile(Ast &ast)
  {
    auto transpiler = Transpiler{
      .ast = &ast,
    };
    transpiler.transpile();
  }

  static constexpr u16 round_to_next_power_of_two(u16 bits)
  {
    if (bits <= 8)
      return 8;

    bits--;
    bits |= bits >> 1;
    bits |= bits >> 2;
    bits |= bits >> 4;
    bits |= bits >> 8;
    bits++;

    return bits;
  }

  void transpile_type(Ast::Expr *type)
  {
    assert(type->tag == Ast::Expr::_Type);

    auto &Type = type->as.Type;

    switch (Type.tag)
    {
    case Ast::Type::_Pointer:
    {
      transpile_type(type->as.Type.as.Pointer);
      std::cout << "*";
    } break;
    case Ast::Type::_Void:
      std::cout << "void";
      break;
    case Ast::Type::_Bool:
      std::cout << "bool";
      break;
    case Ast::Type::_Int:
    {
      auto &Int = Type.as.Int;

      std::cout << (Int.is_signed ? 'i' : 'u') << round_to_next_power_of_two(Int.bits);
    } break;
    }
  }

  void transpile_expr(Ast::Expr *expr)
  {
    switch (expr->tag)
    {
    case Ast::Expr::_Binary_Op:
    {
      auto &Binary_Op = expr->as.Binary_Op;

      std::cout << "(";
      transpile_expr(Binary_Op.lhs);
      switch (Binary_Op.tag)
      {
      case Ast::Expr::BinaryOp::Or:  std::cout << " || "; break;
      case Ast::Expr::BinaryOp::And: std::cout << " && "; break;
      case Ast::Expr::BinaryOp::Eq:  std::cout << " == "; break;
      case Ast::Expr::BinaryOp::Neq: std::cout << " != "; break;
      case Ast::Expr::BinaryOp::Lt:  std::cout << " < "; break;
      case Ast::Expr::BinaryOp::Leq: std::cout << " <= "; break;
      case Ast::Expr::BinaryOp::Gt:  std::cout << " > "; break;
      case Ast::Expr::BinaryOp::Geq: std::cout << " >= "; break;
      case Ast::Expr::BinaryOp::Add: std::cout << " + "; break;
      case Ast::Expr::BinaryOp::Sub: std::cout << " - "; break;
      case Ast::Expr::BinaryOp::Mul: std::cout << " * "; break;
      case Ast::Expr::BinaryOp::Div: std::cout << " / "; break;
      case Ast::Expr::BinaryOp::Mod: std::cout << " % "; break;
      }
      transpile_expr(Binary_Op.rhs);
      std::cout << ")";
    } break;
    case Ast::Expr::_Unary_Op:
    {
      auto &Unary_Op = expr->as.Unary_Op;

      std::cout << "(";
      switch (Unary_Op.tag)
      {
      case Ast::Expr::UnaryOp::Not:   std::cout << "!"; break;
      case Ast::Expr::UnaryOp::Plus:  std::cout << "+"; break;
      case Ast::Expr::UnaryOp::Minus: std::cout << "-"; break;
      }
      transpile_expr(Unary_Op.subexpr);
      std::cout << ")";
    } break;
    case Ast::Expr::_Ref:
    {
      std::cout << "(&";
      transpile_expr(expr->as.Ref);
      std::cout << ")";
    } break;
    case Ast::Expr::_Deref:
    {
      std::cout << "(*";
      transpile_expr(expr->as.Deref);
      std::cout << ")";
    } break;
    case Ast::Expr::_Cast:
    {
      auto &Cast = expr->as.Cast;

      std::cout << "(";
      transpile_type(Cast.type);
      std::cout << ")";
      transpile_expr(Cast.expr);
    } break;
    case Ast::Expr::_Type:
    {
      transpile_type(expr);
    } break;
    case Ast::Expr::_Boolean:
    {
      std::cout << (expr->as.Boolean ? "true" : "false");
    } break;
    case Ast::Expr::_Integer:
    {
      std::cout << expr->as.Integer;
    } break;
    case Ast::Expr::_Null:
    {
      std::cout << "NULL";
    } break;
    case Ast::Expr::_Symbol:
    {
      auto Symbol = expr->as.Symbol;

      std::cout << Symbol->name;
    } break;
    case Ast::Expr::_Identifier:
      UNREACHABLE();
    }
  }

  void transpile_pattern_match(Ast::Expr *pattern, Ast::Expr *value)
  {
    switch (pattern->tag)
    {
    case Ast::Expr::_Boolean:
    {
      std::cout << "assert(" << pattern->as.Boolean << " == ";
      transpile_expr(value);
      std::cout << ");\n";
    } break;
    case Ast::Expr::_Integer:
    {
      std::cout << "assert(" << pattern->as.Integer << " == ";
      transpile_expr(value);
      std::cout << ");\n";
    } break;
    case Ast::Expr::_Null:
    {
      std::cout << "assert(NULL == ";
      transpile_expr(value);
      std::cout << ");\n";
    } break;
    case Ast::Expr::_Symbol:
    {
      auto Symbol = pattern->as.Symbol;

      assert(Symbol->tag == Ast::Symbol::_Identifier);
      auto &Identifier = Symbol->as.Identifier;
      Identifier.value = value;
      transpile_symbol(Symbol);
    } break;
    case Ast::Expr::_Binary_Op:
    case Ast::Expr::_Unary_Op:
    case Ast::Expr::_Ref:
    case Ast::Expr::_Deref:
    case Ast::Expr::_Cast:
    case Ast::Expr::_Type:
    case Ast::Expr::_Identifier:
      UNREACHABLE();
    }
  }

  void transpile_symbol(Ast::Symbol *symbol)
  {
    switch (symbol->tag)
    {
    case Ast::Symbol::_Identifier:
    {
      auto &Identifier = symbol->as.Identifier;

      transpile_type(Identifier.type);
      std::cout << " " << symbol->name;
      if (Identifier.value)
      {
        std::cout << " = ";
        transpile_expr(Identifier.value);
      }
      std::cout << ";";
    } break;
    case Ast::Symbol::_Variable:
    {
      auto &Variable = symbol->as.Variable;

      transpile_pattern_match(Variable.pattern, Variable.value);
    } break;
    }
  }

  void transpile_stmt(Ast::Stmt *stmt)
  {
    switch (stmt->tag)
    {
    case Ast::Stmt::_Assign:
    {
      auto &Assign = stmt->as.Assign;

      transpile_expr(Assign.lhs);
      std::cout << " = ";
      transpile_expr(Assign.rhs);
      std::cout << ";";
    } break;
    case Ast::Stmt::_Symbol:
      transpile_symbol(stmt->as.Symbol);
      break;
    case Ast::Stmt::_Expr:
      transpile_expr(stmt->as.Expr);
      std::cout << ";";
      break;
    }
  }

  void transpile()
  {
    std::cout <<
      "#include <stdio.h>\n"
      "#include <stdlib.h>\n"
      "#include <ctype.h>\n"
      "#include <string.h>\n"
      "#include <stdbool.h>\n"
      "#include <stdint.h>\n"
      "#include <limits.h>\n"
      "#include <stddef.h>\n"
      "#include <stdarg.h>\n"
      "#include <math.h>\n"
      "#include <assert.h>\n"
      "\n"
      "#include <errno.h>\n"
      "#include <fcntl.h>\n"
      "#include <unistd.h>\n"
      "#include <sys/stat.h>\n"
      "\n"
      "typedef uint8_t  u8;\n"
      "typedef uint16_t u16;\n"
      "typedef uint32_t u32;\n"
      "typedef uint64_t u64;\n"
      "\n"
      "typedef int8_t  i8;\n"
      "typedef int16_t i16;\n"
      "typedef int32_t i32;\n"
      "typedef int64_t i64;\n"
      "\n"
      "int main(void)\n{\n";

    for (auto node = ast->stmt_list.first; node; node = node->next)
    {
      std::cout << "  ";
      transpile_stmt(node->data);
      std::cout << "\n";
    }

    std::cout << "}\n";
  }

  Ast *ast;
};
