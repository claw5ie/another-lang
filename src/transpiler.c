#define TAB_SPACE 2
#define PUTS(string) fputs(string, stdout)

void
put_spaces(size_t count)
{
  while (count-- > 0)
    PUTS(" ");
}

void transpile_to_c_expr(AstExpr *, size_t);
void transpile_to_c_type(AstType *, size_t);

void
transpile_to_c_comma_separated_exprs(LinkedList *list)
{
  for (LinkedListNode *node = list->first; node != NULL; node = node->next)
    {
      AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
      transpile_to_c_expr(expr, 0);
      PUTS(", ");
    }
}

void
transpile_to_c_procedure_header(LinkedList *params, AstType *return_type)
{
  PUTS("( ");
  for (LinkedListNode *node = params->first; node != NULL; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolParameter *Parameter = &symbol->as.Parameter;

      transpile_to_c_type(Parameter->type, 0);
      if (Parameter->has_name)
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
      PUTS(", ");
    }
  PUTS(") -> ");
  transpile_to_c_type(return_type, 0);
}

void
transpile_to_c_struct_fields(LinkedList *fields, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  for (LinkedListNode *node = fields->first; node != NULL; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolStructField *Struct_Field = &symbol->as.Struct_Field;

      put_spaces(ident + TAB_SPACE);
      transpile_to_c_type(Struct_Field->type, 0);
      PUTS(" ");
      printf("%.*s;\n", FORMAT_STRING_VIEW(symbol->name));
    }
  put_spaces(ident);
  PUTS("}");
}

void
transpile_to_c_enum_values(LinkedList *values, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  for (LinkedListNode *node = values->first; node != NULL; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      // AstSymbolEnumValue *Enum_Value = &symbol->as.Enum_Value;

      put_spaces(ident + TAB_SPACE);
      printf("%.*s,\n", FORMAT_STRING_VIEW(symbol->name));
    }
  put_spaces(ident);
  PUTS("}");
}

void
transpile_to_c_expr(AstExpr *expr, size_t ident)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        PUTS("(");
        transpile_to_c_expr(Binary_Op->lhs, 0);
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
        transpile_to_c_expr(Binary_Op->rhs, 0);
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
              transpile_to_c_expr(Unary_Op->subexpr, 0);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Not:
            {
              PUTS("!(");
              transpile_to_c_expr(Unary_Op->subexpr, 0);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Ref:
            {
              PUTS("&(");
              transpile_to_c_expr(Unary_Op->subexpr, 0);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Deref:
            {
              PUTS("(");
              transpile_to_c_expr(Unary_Op->subexpr, 0);
              PUTS(")*");
            }

            break;
          }
      }

      break;
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        transpile_to_c_expr(Array_Access->lhs, 0);
        PUTS("[");
        transpile_to_c_expr(Array_Access->index, 0);
        PUTS("]");
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
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Type_Proc = &expr->as.Type_Proc;

        PUTS("proc");
        transpile_to_c_procedure_header(&Type_Proc->params, Type_Proc->return_type);
      }

      break;
    case Ast_Expr_Type_Struct:
      {
        AstSymbolStruct *Type_Struct = &expr->as.Type_Struct;

        PUTS("struct\n");
        transpile_to_c_struct_fields(&Type_Struct->fields, ident + TAB_SPACE);
      }

      break;
    case Ast_Expr_Type_Union:
      {
        AstSymbolStruct *Type_Union = &expr->as.Type_Union;

        PUTS("union\n");
        transpile_to_c_struct_fields(&Type_Union->fields, ident + TAB_SPACE);
      }

      break;
    case Ast_Expr_Type_Enum:
      {
        AstSymbolEnum *Type_Enum = &expr->as.Type_Enum;

        PUTS("enum\n");
        transpile_to_c_enum_values(&Type_Enum->values, ident + TAB_SPACE);
      }

      break;
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call;

        transpile_to_c_expr(Call->lhs, 0);
        PUTS("( ");
        transpile_to_c_comma_separated_exprs(&Call->args);
        PUTS(")");
      }

      break;
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        transpile_to_c_expr(Field_Access->lhs, 0);
        PUTS(".");
        printf("%.*s", FORMAT_STRING_VIEW(Field_Access->name));
      }

      break;
    case Ast_Expr_Cast1:
      {
        PUTS("(auto)");
        transpile_to_c_expr(expr->as.Cast1, 0);
      }

      break;
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        PUTS("(");
        transpile_to_c_expr(Cast2->type, 0);
        PUTS(")");
        transpile_to_c_expr(Cast2->expr, 0);
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
    case Ast_Expr_Designator:
      {
        AstExprDesignator *Designator = &expr->as.Designator;

        printf("%.*s = ", FORMAT_STRING_VIEW(Designator->name));
        transpile_to_c_expr(Designator->expr, 0);
      }

      break;
    case Ast_Expr_Null:
      PUTS("NULL");
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
transpile_to_c_type(AstType *type, size_t ident)
{
  transpile_to_c_expr(type, ident);
}

void transpile_to_c_stmt_block(AstStmtBlock, size_t);
void transpile_to_c_stmt_list(AstStmtBlock, size_t);

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
              transpile_to_c_type(Variable->type, 0);
              PUTS(" ");
              printf("%.*s;", FORMAT_STRING_VIEW(symbol->name));
            }

            break;
          case 2: // 0b10
            {
              printf("auto %.*s = ", FORMAT_STRING_VIEW(symbol->name));
              transpile_to_c_expr(Variable->expr, 0);
              PUTS(";");
            }

            break;
          case 3: // 0b11
            {
              transpile_to_c_type(Variable->type, 0);
              PUTS(" ");
              printf("%.*s = ", FORMAT_STRING_VIEW(symbol->name));
              transpile_to_c_expr(Variable->expr, 0);
              PUTS(";");
            }

            break;
          default: UNREACHABLE();
          }
      }

      break;
    case Ast_Symbol_Parameter:
      {
        AstSymbolParameter *Parameter = &symbol->as.Parameter;

        printf("%.*s", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_type(Parameter->type, 0);
      }

      break;
    case Ast_Symbol_Procedure:
      {
        AstSymbolProcedure *Procedure = &symbol->as.Procedure;

        put_spaces(ident);
        printf("proc %.*s", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_procedure_header(&Procedure->params, Procedure->return_type);
        PUTS("\n");
        transpile_to_c_stmt_block(Procedure->block, ident);
        PUTS("\n");
      }

      break;
    case Ast_Symbol_Struct:
      {
        AstSymbolStruct *Struct = &symbol->as.Struct;

        put_spaces(ident);
        printf("struct %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_struct_fields(&Struct->fields, ident);
        PUTS("\n");
      }

      break;
    case Ast_Symbol_Union:
      {
        AstSymbolStruct *Union = &symbol->as.Union;

        put_spaces(ident);
        printf("union %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_struct_fields(&Union->fields, ident);
        PUTS("\n");
      }

      break;
    case Ast_Symbol_Enum:
      {
        AstSymbolEnum *Enum = &symbol->as.Enum;

        put_spaces(ident);
        printf("enum %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_enum_values(&Enum->values, ident);
        PUTS("\n");
      }

      break;
    case Ast_Symbol_Alias:
      {
        AstSymbolAlias *Alias = &symbol->as.Alias;

        put_spaces(ident);
        PUTS("typedef ");
        transpile_to_c_type(Alias->type, 0);
        printf(" %.*s;", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Symbol_Struct_Field:
    case Ast_Symbol_Enum_Value:
      UNREACHABLE();
    }

}

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
        transpile_to_c_expr(If->cond, 0);
        PUTS(")\n");
        transpile_to_c_stmt(If->if_true, ident + TAB_SPACE);
        PUTS("\n");
        put_spaces(ident);
        PUTS("else\n");
        if (If->if_false != NULL)
          transpile_to_c_stmt(If->if_false, ident + TAB_SPACE);
        else
          {
            put_spaces(ident + TAB_SPACE);
            PUTS("{ /* empty */ }");
          }
        PUTS("\n");
      }

      break;
    case Ast_Stmt_While:
      {
        AstStmtWhile *While = &stmt->as.While;

        if (While->is_do_while)
          {
            put_spaces(ident);
            PUTS("do\n");
            transpile_to_c_stmt(While->block, ident + TAB_SPACE);
            PUTS("\n");
            put_spaces(ident);
            PUTS("while (");
            transpile_to_c_expr(While->cond, 0);
            PUTS(");\n");
          }
        else
          {
            put_spaces(ident);
            PUTS("while (");
            transpile_to_c_expr(While->cond, 0);
            PUTS(")\n");
            transpile_to_c_stmt(While->block, ident + TAB_SPACE);
            PUTS("\n");
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
    case Ast_Stmt_Return_Nothing:
      put_spaces(ident);
      PUTS("return;");
      break;
    case Ast_Stmt_Return_Expr:
      put_spaces(ident);
      PUTS("return ");
      transpile_to_c_expr(stmt->as.Return_Expr, 0);
      PUTS(";");
      break;
    case Ast_Stmt_Switch:
      {
        AstStmtSwitch *Switch = &stmt->as.Switch;

        put_spaces(ident);
        PUTS("switch (");
        transpile_to_c_expr(Switch->cond, 0);
        PUTS(")\n");
        put_spaces(ident);
        PUTS("{\n");
        transpile_to_c_stmt_list(Switch->cases, ident + TAB_SPACE);
        if (Switch->default_case != NULL)
          {
            put_spaces(ident);
            PUTS("default:\n");
            transpile_to_c_stmt(Switch->default_case, ident + TAB_SPACE);
            PUTS("\n");
          }
        put_spaces(ident);
        PUTS("}\n");
      }

      break;
    case Ast_Stmt_Case:
      {
        put_spaces(ident - (ident < TAB_SPACE ? 0 : TAB_SPACE));
        PUTS("case ");
        transpile_to_c_expr(stmt->as.Case, 0);
        PUTS(":");
      }

      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        put_spaces(ident);
        transpile_to_c_expr(Assign->lhs, 0);
        PUTS(" = ");
        transpile_to_c_expr(Assign->rhs, 0);
        PUTS(";");
      }

      break;
    case Ast_Stmt_Symbol:
      transpile_to_c_symbol(stmt->as.Symbol, ident);
      break;
    case Ast_Stmt_Expr:
      put_spaces(ident);
      transpile_to_c_expr(stmt->as.Expr, 0);
      PUTS(";");
      break;
    }
}

void
transpile_to_c_stmt_list(AstStmtBlock block, size_t ident)
{
  for (LinkedListNode *node = block.first; node != NULL; node = node->next)
    {
      AstStmt *stmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);
      transpile_to_c_stmt(stmt, ident);
      PUTS("\n");
    }
}

void
transpile_to_c_stmt_block(AstStmtBlock block, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  transpile_to_c_stmt_list(block, ident + TAB_SPACE);
  put_spaces(ident);
  PUTS("}");
}

void
transpile_to_c(Ast *ast)
{
  for (LinkedListNode *node = ast->globals.first; node != NULL; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      transpile_to_c_symbol(symbol, 0);
      PUTS("\n");
    }
}
