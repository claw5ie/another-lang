struct Transpiler
{
  static void transpile(Ast &ast)
  {
    auto transpiler = Transpiler{
      .ast = &ast,
    };
    transpiler.transpile();
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
    case Ast::Expr::_Type:
    {
      auto &Type = expr->as.Type;

      switch (Type.tag)
      {
      case Ast::Type::_Bool:
      {
        std::cout << "bool";
      } break;
      case Ast::Type::_Int:
      {
        auto &Int = Type.as.Int;

        std::cout << (Int.is_signed ? "i" : "u") << Int.bits;
      } break;
      }
    } break;
    case Ast::Expr::_Boolean:
    {
      std::cout << (expr->as.Boolean ? "true" : "false");
    } break;
    case Ast::Expr::_Integer:
    {
      std::cout << expr->as.Integer;
    } break;
    case Ast::Expr::_Identifier:
    {
      std::cout << expr->as.Identifier;
    } break;
    }
  }

  void transpile()
  {
    std::cout << "int main(void)\n{\n";

    for (auto node = ast->exprs.first; node; node = node->next)
    {
      std::cout << "  ";
      transpile_expr(node->data);
      std::cout << ";\n";
    }

    std::cout << "}\n";
  }

  Ast *ast;
};
