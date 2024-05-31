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
      case Ast::Expr::BinaryOp::Add: std::cout << " + "; break;
      case Ast::Expr::BinaryOp::Sub: std::cout << " - "; break;
      case Ast::Expr::BinaryOp::Mul: std::cout << " * "; break;
      case Ast::Expr::BinaryOp::Div: std::cout << " / "; break;
      }
      transpile_expr(Binary_Op.rhs);
      std::cout << ")";
    } break;
    case Ast::Expr::_Integer:
    {
      std::cout << expr->as.Integer;
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
