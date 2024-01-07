#define PUTS(string) fputs(string, stdout)

void
transpile_to_c_expr(AstExpr *expr)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        PUTS("(");
        transpile_to_c_expr(Binary_Op->lhs);
        switch (Binary_Op->tag)
          {
          case Ast_Expr_Binary_Op_Or:  PUTS(" || "); break;
          case Ast_Expr_Binary_Op_And: PUTS(" && "); break;
          case Ast_Expr_Binary_Op_Eq:  PUTS(" == "); break;
          case Ast_Expr_Binary_Op_Neq: PUTS(" != "); break;
          case Ast_Expr_Binary_Op_Leq: PUTS(" <= "); break;
          case Ast_Expr_Binary_Op_Geq: PUTS(" >= "); break;
          case Ast_Expr_Binary_Op_Lt:  PUTS(" < "); break;
          case Ast_Expr_Binary_Op_Gt:  PUTS(" > "); break;
          case Ast_Expr_Binary_Op_Add: PUTS(" + "); break;
          case Ast_Expr_Binary_Op_Sub: PUTS(" - "); break;
          case Ast_Expr_Binary_Op_Mul: PUTS(" * "); break;
          case Ast_Expr_Binary_Op_Div: PUTS(" / "); break;
          case Ast_Expr_Binary_Op_Mod: PUTS(" % "); break;
          }
        transpile_to_c_expr(Binary_Op->rhs);
        PUTS(")");
      }

      break;
    case Ast_Expr_Unary_Op:
      {
        AstExprUnaryOp *Unary_Op = &expr->as.Unary_Op;

        switch (Unary_Op->tag)
          {
          case Ast_Expr_Unary_Op_Neg: PUTS("-"); break;
          case Ast_Expr_Unary_Op_Not: PUTS("!"); break;
          }
        PUTS("(");
        transpile_to_c_expr(Unary_Op->subexpr);
        PUTS(")");
      }

      break;
    case Ast_Expr_Int64:
      {
        u64 Int64 = expr->as.Int64;

        printf("%lu", Int64);
      }

      break;
    case Ast_Expr_Bool:
      {
        u64 Bool = expr->as.Bool;

        printf("%s", Bool ? "true" : "false");
      }

      break;
    case Ast_Expr_Identifier:
      {
        StringView Identifier = expr->as.Identifier;

        printf("%.*s", FORMAT_STRING_VIEW(Identifier));
      }

      break;
    }
}

void
transpile_to_c(Ast *ast)
{
  for (LinkedListNode *node = ast->exprs.first; node != NULL; node = node->next)
    {
      AstExpr *expr = node->data;
      transpile_to_c_expr(expr);
      PUTS(";\n");
    }
}
