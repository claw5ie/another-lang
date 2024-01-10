#define TAB_SPACE 2
#define PUTS(string) fputs(string, stdout)

void
put_spaces(size_t count)
{
  while (count-- > 0)
    PUTS(" ");
}

void transpile_to_c_type(AstType *);

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
          case Ast_Expr_Unary_Op_Neg:
            {
              PUTS("-(");
              transpile_to_c_expr(Unary_Op->subexpr);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Not:
            {
              PUTS("!(");
              transpile_to_c_expr(Unary_Op->subexpr);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Ref:
            {
              PUTS("&(");
              transpile_to_c_expr(Unary_Op->subexpr);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Deref:
            {
              PUTS("(");
              transpile_to_c_expr(Unary_Op->subexpr);
              PUTS(")*");
            }

            break;
          }
      }

      break;
    case Ast_Expr_Type_Void:
      PUTS("void");
      break;
    case Ast_Expr_Type_Bool:
      PUTS("bool");
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Type_Int = &expr->as.Type_Int;

        printf("%c%i", Type_Int->is_signed ? 'i' : 'u', Type_Int->bits);
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
transpile_to_c_type(AstType *type)
{
  transpile_to_c_expr(type);
}

void
transpile_to_c_symbol(AstSymbol *symbol, size_t ident)
{
  switch (symbol->tag)
    {
    case Ast_Symbol_Variable:
      {
        AstSymbolVariable *Variable = &symbol->as.Variable;

        put_spaces(ident);

        switch (((Variable->expr != NULL) << 1) | (Variable->type != NULL))
          {
          case 1: // 0b01
            {
              transpile_to_c_type(Variable->type);
              PUTS(" ");
              printf("%.*s;", FORMAT_STRING_VIEW(symbol->name));
            }

            break;
          case 2: // 0b10
            {
              printf("auto %.*s = ", FORMAT_STRING_VIEW(symbol->name));
              transpile_to_c_expr(Variable->expr);
              PUTS(";");
            }

            break;
          case 3: // 0b11
            {
              transpile_to_c_type(Variable->type);
              PUTS(" ");
              printf("%.*s = ", FORMAT_STRING_VIEW(symbol->name));
              transpile_to_c_expr(Variable->expr);
              PUTS(";");
            }

            break;
          default: UNREACHABLE();
          }
      }

      break;
    }
}

void transpile_to_c_stmt_block(AstStmtBlock, size_t);

void
transpile_to_c_stmt(AstStmt *stmt, size_t ident)
{
  switch (stmt->tag)
    {
    case Ast_Stmt_Block:
      transpile_to_c_stmt_block(stmt->as.Block, ident);
      break;
    case Ast_Stmt_If:
      {
        AstStmtIf *If = &stmt->as.If;

        put_spaces(ident);
        PUTS("if (");
        transpile_to_c_expr(If->cond);
        PUTS(")\n");
        transpile_to_c_stmt_block(If->if_true, ident + TAB_SPACE);
        put_spaces(ident);
        PUTS("else\n");
        transpile_to_c_stmt_block(If->if_false, ident + TAB_SPACE);
      }

      break;
    case Ast_Stmt_While:
      {
        AstStmtWhile *While = &stmt->as.While;

        if (While->is_do_while)
          {
            put_spaces(ident);
            PUTS("do\n");
            transpile_to_c_stmt_block(While->block, ident + TAB_SPACE);
            put_spaces(ident);
            PUTS("while (");
            transpile_to_c_expr(While->cond);
            PUTS(");\n");
          }
        else
          {
            put_spaces(ident);
            PUTS("while (");
            transpile_to_c_expr(While->cond);
            PUTS(")\n");
            transpile_to_c_stmt_block(While->block, ident + TAB_SPACE);
          }
      }

      break;
    case Ast_Stmt_Break:
      put_spaces(ident);
      PUTS("break;");
      break;
    case Ast_Stmt_Continue:
      put_spaces(ident);
      PUTS("continue;");
      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        put_spaces(ident);
        transpile_to_c_expr(Assign->lhs);
        PUTS(" = ");
        transpile_to_c_expr(Assign->rhs);
        PUTS(";");
      }

      break;
    case Ast_Stmt_Symbol:
      transpile_to_c_symbol(stmt->as.Symbol, ident);
      break;
    case Ast_Stmt_Expr:
      put_spaces(ident);
      transpile_to_c_expr(stmt->as.Expr);
      PUTS(";");
      break;
    }
}

void
transpile_to_c_stmt_block(AstStmtBlock block, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  for (LinkedListNode *node = block.first; node != NULL; node = node->next)
    {
      AstStmt *stmt = LINKED_LIST_NODE_DATA_PTR(AstStmt, node);
      transpile_to_c_stmt(stmt, ident + TAB_SPACE);
      PUTS("\n");
    }
  put_spaces(ident);
  PUTS("}\n");
}

void
transpile_to_c(Ast *ast)
{
  for (LinkedListNode *node = ast->stmts.first; node != NULL; node = node->next)
    {
      AstStmt *stmt = LINKED_LIST_NODE_DATA_PTR(AstStmt, node);
      transpile_to_c_stmt(stmt, 0);
      PUTS("\n");
    }
}