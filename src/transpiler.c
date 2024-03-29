#define TAB_SPACE 2
#define PUTS(string) fputs(string, stdout)

void transpile_to_c_expr(AstExpr *, size_t);
void transpile_to_c_type(AstExpr *, size_t);
void transpile_to_c_stmt_block(AstStmtBlock *, size_t);

// TODO: remove this.
int ast_flags = 0;

void
put_spaces(size_t count)
{
  while (count-- > 0)
    PUTS(" ");
}

void
transpile_to_c_expr_list(LinkedList *list, size_t ident)
{
  for (LinkedListNode *node = list->first; node; node = node->next)
    {
      AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
      transpile_to_c_expr(expr, ident);

      if (node->next)
        PUTS(", ");
    }
}

void
transpile_to_c_type_proc_params(AstExprTypeProc *proc, size_t ident)
{
  PUTS("(");
  for (LinkedListNode *node = proc->params.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolParameter *Parameter = &symbol->as.Parameter;

      transpile_to_c_expr(Parameter->type, ident);
      if (Parameter->has_name)
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));

      if (node->next)
        PUTS(", ");
    }
  PUTS(")");
}

void
transpile_to_c_struct_fields(LinkedList *fields, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  for (LinkedListNode *node = fields->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolStructField *Struct_Field = &symbol->as.Struct_Field;

      put_spaces(ident + TAB_SPACE);
      transpile_to_c_expr(Struct_Field->type, ident);
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
  for (LinkedListNode *node = values->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolEnumValue *Enum_Value = &symbol->as.Enum_Value;

      put_spaces(ident + TAB_SPACE);
      printf("%.*s", FORMAT_STRING_VIEW(symbol->name));
      if (Enum_Value->expr)
        {
          PUTS(" = ");
          transpile_to_c_expr(Enum_Value->expr, ident);
        }
      PUTS(",\n");
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
        transpile_to_c_expr(Binary_Op->lhs, ident);
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
        transpile_to_c_expr(Binary_Op->rhs, ident);
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
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Not:
            {
              PUTS("!(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Ref:
            {
              PUTS("&(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              PUTS(")");
            }

            break;
          case Ast_Expr_Unary_Op_Deref:
            {
              PUTS("(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              PUTS(")*");
            }

            break;
          }
      }

      break;
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        transpile_to_c_expr(Array_Access->lhs, ident);
        PUTS("[");
        transpile_to_c_expr(Array_Access->index, ident);
        PUTS("]");
      }

      break;
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call;

        transpile_to_c_expr(Call->lhs, ident);
        PUTS("(");
        transpile_to_c_expr_list(&Call->args, ident);
        PUTS(")");
      }

      break;
    case Ast_Expr_Type_Cons:
      {
        AstExprCall *Type_Cons = &expr->as.Type_Cons;

        assert(Type_Cons->lhs);

        if (!(ast_flags & AST_FLAG_DONT_CAST_EXPR_LIST))
          {
            PUTS("(");
            transpile_to_c_type(Type_Cons->lhs, ident);
            PUTS(")");
          }
        PUTS("{");
        transpile_to_c_expr_list(&Type_Cons->args, ident);
        PUTS("}");
      }

      break;
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        transpile_to_c_expr(Field_Access->lhs, ident);
        PUTS(".");
        printf("%.*s", FORMAT_STRING_VIEW(Field_Access->name));
      }

      break;
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        PUTS("(");
        transpile_to_c_type(Cast2->type, ident);
        PUTS(")");
        transpile_to_c_expr(Cast2->expr, ident);
      }

      break;
    case Ast_Expr_Type:
      transpile_to_c_type(expr, ident);
      break;
    case Ast_Expr_Int64:
      {
        u64 Int64 = expr->as.Int64;

        printf("%lu", Int64);
      }

      break;
    case Ast_Expr_Bool:
      {
        bool Bool = expr->as.Bool;

        printf("%s", Bool ? "true" : "false");
      }

      break;
    case Ast_Expr_Designator:
      {
        AstExprDesignator *Designator = &expr->as.Designator;

        printf(".%.*s = ", FORMAT_STRING_VIEW(Designator->name));
        transpile_to_c_expr(Designator->expr, ident);
      }

      break;
    case Ast_Expr_Null:
      PUTS("NULL");
      break;
    case Ast_Expr_Symbol:
      {
        AstSymbol *Symbol = expr->as.Symbol;

        printf("%.*s", FORMAT_STRING_VIEW(Symbol->name));
      }

      break;
    case Ast_Expr_Enum_Identifier:
      {
        AstExprIdentifier *Enum_Identifier = &expr->as.Enum_Identifier;

        printf(".%.*s", FORMAT_STRING_VIEW(Enum_Identifier->name));
      }

      break;
    case Ast_Expr_Cast1:
    case Ast_Expr_Identifier:
      UNREACHABLE();
    }
}

void
transpile_to_c_type_array(AstExpr *type, AstSymbol *symbol, size_t ident)
{
  switch (type->as.Type.tag)
    {
    case Ast_Expr_Type_Array:
      {
        AstExprArrayAccess *Array = &type->as.Type.as.Array;

        transpile_to_c_type_array(Array->lhs, symbol, ident);
        PUTS("[");
        transpile_to_c_expr(Array->index, ident);
        PUTS("]");
      }

      break;
    default:
      transpile_to_c_type(type, ident);
      if (symbol)
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
    }
}

void
transpile_to_c_type(AstExpr *type, size_t ident)
{
  AstExprType *Type = &type->as.Type;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      PUTS("void");
      break;
    case Ast_Expr_Type_Bool:
      PUTS("bool");
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        printf("%c%i", Int->is_signed ? 'i' : 'u', Int->bits);
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      PUTS("i64");
      break;
    case Ast_Expr_Type_Pointer:
      transpile_to_c_type(Type->as.Pointer, ident);
      PUTS("*");
      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        transpile_to_c_type(Proc->return_type, ident);
        PUTS(" (*)");
        transpile_to_c_type_proc_params(Proc, ident);
      }

      break;
    case Ast_Expr_Type_Array:
      transpile_to_c_type_array(type, NULL, ident);
      break;
    case Ast_Expr_Type_Struct:
      assert(Type->symbol);
      printf("struct %.*s", FORMAT_STRING_VIEW(Type->symbol->name));
      break;
    case Ast_Expr_Type_Union:
      assert(Type->symbol);
      printf("union %.*s", FORMAT_STRING_VIEW(Type->symbol->name));
      break;
    case Ast_Expr_Type_Enum:
      assert(Type->symbol);
      printf("enum %.*s", FORMAT_STRING_VIEW(Type->symbol->name));
      break;
    }
}

void
transpile_to_c_symbol_type(AstExpr *type, AstSymbol *symbol, size_t ident)
{
  assert(type->tag == Ast_Expr_Type);
  AstExprType *Type = &type->as.Type;

  put_spaces(ident);

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      printf("typedef void %.*s;", FORMAT_STRING_VIEW(symbol->name));
      break;
    case Ast_Expr_Type_Bool:
      printf("typedef bool %.*s;", FORMAT_STRING_VIEW(symbol->name));
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        printf("typedef %c%i %.*s;", Int->is_signed ? 'i' : 'u', Int->bits, FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      printf("typedef i64 %.*s;", FORMAT_STRING_VIEW(symbol->name));
      break;
    case Ast_Expr_Type_Pointer:
      {
        PUTS("typedef ");
        transpile_to_c_type(type, ident);
        printf(" %.*s;", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        PUTS("typedef ");
        transpile_to_c_type(Proc->return_type, ident);
        printf(" (*%.*s)", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_type_proc_params(Proc, ident);
        PUTS(";");
      }

      break;
    case Ast_Expr_Type_Array:
      PUTS("typedef ");
      transpile_to_c_type_array(type, symbol, ident);
      PUTS(";");
      break;
    case Ast_Expr_Type_Struct:
      {
        AstSymbolStruct *Struct = &Type->as.Struct;

        printf("struct %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_struct_fields(&Struct->fields, ident);
        PUTS(";\n");
      }

      break;
    case Ast_Expr_Type_Union:
      {
        AstSymbolStruct *Union = &Type->as.Union;

        printf("union %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_struct_fields(&Union->fields, ident);
        PUTS(";\n");
      }

      break;
    case Ast_Expr_Type_Enum:
      {
        AstSymbolEnum *Enum = &Type->as.Enum;

        printf("enum %.*s\n", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_enum_values(&Enum->values, ident + TAB_SPACE);
        PUTS(";\n");
      }

      break;
    }
}

void
transpile_to_c_symbol_variable_type(AstExpr *type, AstSymbol *symbol, size_t ident)
{
  assert(type->tag == Ast_Expr_Type);
  AstExprType *Type = &type->as.Type;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      UNREACHABLE();
    case Ast_Expr_Type_Bool:
      printf("bool %.*s", FORMAT_STRING_VIEW(symbol->name));
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        printf("%c%i %.*s", Int->is_signed ? 'i' : 'u', Int->bits, FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      printf("i64 %.*s", FORMAT_STRING_VIEW(symbol->name));
      break;
    case Ast_Expr_Type_Pointer:
      {
        transpile_to_c_type(type, ident);
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        transpile_to_c_type(Proc->return_type, ident);
        printf(" (*%.*s)", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_type_proc_params(Proc, ident);
      }

      break;
    case Ast_Expr_Type_Array:
      transpile_to_c_type_array(type, symbol, ident);
      break;
    case Ast_Expr_Type_Struct:
      {
        AstSymbolStruct *Struct = &Type->as.Struct;

        PUTS("struct\n");
        transpile_to_c_struct_fields(&Struct->fields, ident);
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Union:
      {
        AstSymbolStruct *Union = &Type->as.Union;

        PUTS("union\n");
        transpile_to_c_struct_fields(&Union->fields, ident);
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Type_Enum:
      {
        AstSymbolEnum *Enum = &Type->as.Enum;

        PUTS("enum\n");
        transpile_to_c_enum_values(&Enum->values, ident + TAB_SPACE);
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    }
}

void
transpile_to_c_symbol(AstSymbol *symbol, size_t ident)
{
  switch (symbol->tag)
    {
    case Ast_Symbol_Variable:
      {
        AstSymbolVariable *Variable = &symbol->as.Variable;

        AstFlagsType old_flags = ast_flags;
        ast_flags |= AST_FLAG_DONT_CAST_EXPR_LIST;

        put_spaces(ident);

        transpile_to_c_symbol_variable_type(Variable->type, symbol, ident);
        if (Variable->expr)
          {
            PUTS(" = ");
            transpile_to_c_expr(Variable->expr, ident);
          }
        PUTS(";");

        ast_flags = old_flags;
      }

      break;
    case Ast_Symbol_Parameter:
      {
        AstSymbolParameter *Parameter = &symbol->as.Parameter;

        transpile_to_c_symbol_variable_type(Parameter->type, symbol, ident);
      }

      break;
    case Ast_Symbol_Procedure:
      {
        AstSymbolProcedure *Procedure = &symbol->as.Procedure;
        AstExprTypeProc *Proc = &Procedure->type->as.Type.as.Proc;
        assert(Procedure->type->tag == Ast_Expr_Type && Procedure->type->as.Type.tag == Ast_Expr_Type_Proc);

        put_spaces(ident);
        transpile_to_c_type(Proc->return_type, ident);
        printf(" %.*s", FORMAT_STRING_VIEW(symbol->name));
        transpile_to_c_type_proc_params(Proc, ident);
        PUTS("\n");
        transpile_to_c_stmt_block(&Procedure->block, ident);
        PUTS("\n");
      }

      break;
    case Ast_Symbol_Type:
      {
        AstExpr *Type = symbol->as.Type;
        assert(Type->tag == Ast_Expr_Type);

        if (!(symbol->flags & AST_SYMBOL_FLAG_IS_UNPACKED))
          transpile_to_c_symbol_type(Type, symbol, ident);
      }

      break;
    case Ast_Symbol_Struct_Field:
    case Ast_Symbol_Enum_Value:
    case Ast_Symbol_Alias:
      UNREACHABLE();
    }
}

void
transpile_to_c_stmt(AstStmt *stmt, size_t ident)
{
  switch (stmt->tag)
    {
    case Ast_Stmt_Block:
      transpile_to_c_stmt_block(&stmt->as.Block, ident);
      break;
    case Ast_Stmt_If:
      {
        AstStmtIf *If = &stmt->as.If;

        put_spaces(ident);
        PUTS("if (");
        transpile_to_c_expr(If->cond, ident);
        PUTS(")\n");
        transpile_to_c_stmt(If->if_true, ident + TAB_SPACE);
        PUTS("\n");
        put_spaces(ident);
        PUTS("else\n");
        if (If->if_false)
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
            transpile_to_c_expr(While->cond, ident);
            PUTS(");\n");
          }
        else
          {
            put_spaces(ident);
            PUTS("while (");
            transpile_to_c_expr(While->cond, ident);
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
      transpile_to_c_expr(stmt->as.Return_Expr, ident);
      PUTS(";");
      break;
    case Ast_Stmt_Switch:
      {
        AstStmtSwitch *Switch = &stmt->as.Switch;

        put_spaces(ident);
        PUTS("switch (");
        transpile_to_c_expr(Switch->cond, ident);
        PUTS(")\n");
        transpile_to_c_stmt_block(&Switch->cases, ident + TAB_SPACE);
        // Default case is always null for now.
      }

      break;
    case Ast_Stmt_Case:
      {
        AstStmtCase *Case = &stmt->as.Case;

        put_spaces(ident - (ident < TAB_SPACE ? 0 : TAB_SPACE));
        PUTS("case ");
        transpile_to_c_expr(Case->expr, ident);
        PUTS(":\n");
        transpile_to_c_stmt(Case->substmt, ident);
      }

      break;
    case Ast_Stmt_Default:
      {
        put_spaces(ident - (ident < TAB_SPACE ? 0 : TAB_SPACE));
        PUTS("default:\n");
        transpile_to_c_stmt(stmt->as.Default, ident);
      }

      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        put_spaces(ident);
        transpile_to_c_expr(Assign->lhs, ident);
        PUTS(" = ");
        transpile_to_c_expr(Assign->rhs, ident);
        PUTS(";");
      }

      break;
    case Ast_Stmt_Symbol:
      transpile_to_c_symbol(stmt->as.Symbol, ident);
      break;
    case Ast_Stmt_Expr:
      put_spaces(ident);
      transpile_to_c_expr(stmt->as.Expr, ident);
      PUTS(";");
      break;
    }
}

void
transpile_to_c_stmt_block(AstStmtBlock *block, size_t ident)
{
  put_spaces(ident);
  PUTS("{\n");
  ident += TAB_SPACE;
  for (LinkedListNode *node = block->first; node; node = node->next)
    {
      AstStmt *stmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);
      transpile_to_c_stmt(stmt, ident);
      PUTS("\n");
    }
  ident -= TAB_SPACE;
  put_spaces(ident);
  PUTS("}");
}

void
transpile_to_c(Ast *ast)
{
  for (LinkedListNode *node = ast->globals.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      transpile_to_c_symbol(symbol, 0);
      PUTS("\n");
    }
}
