// TODO: round int types to closest power of two.
//       add bit fields.
//       add strings.
//       typedef/shuffle recursive types.
//       forward declare functions.
//       precompute enum values (or #define them???)
//       add typedef for each function type ('cause can't have inline fields with function type, for example).

#define TAB_SIZE 2

#define put_string(string) fputs(string, stdout)

void transpile_to_c_expr(AstExpr *, size_t);
void transpile_to_c_type(AstExpr *, size_t);
void transpile_to_c_type_with_symbol(AstExpr *, AstSymbol *, size_t);
void transpile_to_c_stmt_block_no_curly(AstStmtBlock *, size_t);
void transpile_to_c_stmt_block(AstStmtBlock *, size_t);

// TODO: remove this global variable.
int ast_flags = 0;

void
put_spaces(size_t count)
{
  while (count-- > 0)
    put_string(" ");
}

void
transpile_to_c_expr_list(LinkedList *list, size_t ident)
{
  for (LinkedListNode *node = list->first; node; node = node->next)
    {
      AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
      transpile_to_c_expr(expr, ident);

      if (node->next)
        put_string(", ");
    }
}

void
transpile_to_c_type_procedure_params(AstExprTypeProcedure *procedure, size_t ident)
{
  put_string("(");
  for (LinkedListNode *node = procedure->params.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolParameter *Parameter = &symbol->as.Parameter;

      transpile_to_c_type(Parameter->type, ident);
      if (Parameter->has_name)
        printf(" " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));

      if (node->next)
        put_string(", ");
    }
  put_string(")");
}

void
transpile_to_c_struct_fields(LinkedList *fields, size_t ident)
{
  put_spaces(ident);
  put_string("{\n");
  for (LinkedListNode *node = fields->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolField *Field = &symbol->as.Struct_Or_Union_Field;

      put_spaces(ident + TAB_SIZE);
      transpile_to_c_type(Field->type, ident);
      printf(" " SYMBOL_NAME_SPECIFIER ";\n", FORMAT_SYMBOL_NAME(symbol));
    }
  put_spaces(ident);
  put_string("}");
}

void
transpile_to_c_enum_values(LinkedList *values, size_t ident)
{
  put_spaces(ident);
  put_string("{\n");
  for (LinkedListNode *node = values->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolEnumValue *Enum_Value = &symbol->as.Enum_Value;

      put_spaces(ident + TAB_SIZE);
      printf(SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));
      if (Enum_Value->expr)
        {
          put_string(" = ");
          transpile_to_c_expr(Enum_Value->expr, ident);
        }
      put_string(",\n");
    }
  put_spaces(ident);
  put_string("}");
}

void
transpile_to_c_expr(AstExpr *expr, size_t ident)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        put_string("(");
        transpile_to_c_expr(Binary_Op->lhs, ident);
        switch (Binary_Op->tag)
          {
          case Ast_Expr_Binary_Op_Or:  put_string(" || "); break;
          case Ast_Expr_Binary_Op_And: put_string(" && "); break;
          case Ast_Expr_Binary_Op_Eq:  put_string(" == "); break;
          case Ast_Expr_Binary_Op_Neq: put_string(" != "); break;
          case Ast_Expr_Binary_Op_Leq: put_string(" <= "); break;
          case Ast_Expr_Binary_Op_Geq: put_string(" >= "); break;
          case Ast_Expr_Binary_Op_Lt:  put_string(" < "); break;
          case Ast_Expr_Binary_Op_Gt:  put_string(" > "); break;
          case Ast_Expr_Binary_Op_Add: put_string(" + "); break;
          case Ast_Expr_Binary_Op_Sub: put_string(" - "); break;
          case Ast_Expr_Binary_Op_Mul: put_string(" * "); break;
          case Ast_Expr_Binary_Op_Div: put_string(" / "); break;
          case Ast_Expr_Binary_Op_Mod: put_string(" % "); break;
          }
        transpile_to_c_expr(Binary_Op->rhs, ident);
        put_string(")");
      }

      break;
    case Ast_Expr_Unary_Op:
      {
        AstExprUnaryOp *Unary_Op = &expr->as.Unary_Op;

        switch (Unary_Op->tag)
          {
          case Ast_Expr_Unary_Op_Neg:
            {
              put_string("-(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              put_string(")");
            }

            break;
          case Ast_Expr_Unary_Op_Not:
            {
              put_string("!(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              put_string(")");
            }

            break;
          case Ast_Expr_Unary_Op_Ref:
            {
              put_string("&(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              put_string(")");
            }

            break;
          case Ast_Expr_Unary_Op_Deref:
            {
              put_string("(");
              transpile_to_c_expr(Unary_Op->subexpr, ident);
              put_string(")*");
            }

            break;
          }
      }

      break;
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        transpile_to_c_expr(Array_Access->lhs, ident);
        put_string("[");
        transpile_to_c_expr(Array_Access->index, ident);
        put_string("]");
      }

      break;
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call_Or_Type_Cons;

        transpile_to_c_expr(Call->lhs, ident);
        put_string("(");
        transpile_to_c_expr_list(&Call->args, ident);
        put_string(")");
      }

      break;
    case Ast_Expr_Type_Cons:
      {
        AstExprCall *Type_Cons = &expr->as.Call_Or_Type_Cons;

        assert(Type_Cons->lhs);

        if (!(ast_flags & AST_FLAG_DONT_CAST_EXPR_LIST))
          {
            put_string("(");
            transpile_to_c_type(Type_Cons->lhs, ident);
            put_string(")");
          }
        put_string("{");
        transpile_to_c_expr_list(&Type_Cons->args, ident);
        put_string("}");
      }

      break;
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        transpile_to_c_expr(Field_Access->lhs, ident);
        put_string(".");
        printf(SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Field_Access->symbol));
      }

      break;
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        put_string("(");
        transpile_to_c_type(Cast2->type, ident);
        put_string(")");
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

        printf("." SYMBOL_NAME_SPECIFIER " = ", FORMAT_SYMBOL_NAME(Designator->symbol));
        transpile_to_c_expr(Designator->expr, ident);
      }

      break;
    case Ast_Expr_Null:
      put_string("NULL");
      break;
    case Ast_Expr_Symbol:
      {
        AstSymbol *Symbol = expr->as.Symbol;

        printf(SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Symbol));
      }

      break;
    case Ast_Expr_Cast1:
    case Ast_Expr_Unresolved_Field:
    case Ast_Expr_Unresolved_Designator:
    case Ast_Expr_Unresolved_Enum_Value:
    case Ast_Expr_Unresolved_Identifier:
      UNREACHABLE();
    }
}

void
transpile_to_c_type_procedure(AstExprTypeProcedure *type, AstSymbol *symbol, size_t ident)
{
  transpile_to_c_type_with_symbol(type->return_type, NULL, ident);
  put_string(" (*");
  if (symbol)
    printf(SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));
  put_string(")");
  transpile_to_c_type_procedure_params(type, ident);
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
        put_string("[");
        transpile_to_c_expr(Array->index, ident);
        put_string("]");
      }

      break;
    default:
      transpile_to_c_type(type, ident);
      if (symbol)
        printf(" " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));
    }
}

void
transpile_to_c_type_with_symbol(AstExpr *type, AstSymbol *symbol, size_t ident)
{
  AstExprType *Type = &type->as.Type;

  bool should_print_symbol_name = true;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      put_string("void");
      break;
    case Ast_Expr_Type_Bool:
      put_string("bool");
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        printf("%c%i", Int->is_signed ? 'i' : 'u', Int->bits);
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      put_string("i64");
      break;
    case Ast_Expr_Type_Pointer:
      transpile_to_c_type_with_symbol(Type->as.Pointer, symbol, ident);
      put_string("*");
      break;
    case Ast_Expr_Type_Procedure:
      {
        if (Type->symbol)
          printf(SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Type->symbol));
        else
          {
            AstExprTypeProcedure *Procedure = &Type->as.Procedure;

            should_print_symbol_name = false;
            transpile_to_c_type_procedure(Procedure, symbol, ident);
          }
      }

      break;
    case Ast_Expr_Type_Array:
      should_print_symbol_name = false;
      transpile_to_c_type_array(type, symbol, ident);
      break;
    case Ast_Expr_Type_Struct:
      if (Type->symbol)
        printf("struct " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Type->symbol));
      else
        {
          AstExprTypeStruct *Struct = &Type->as.Struct_Or_Union;

          printf("struct\n");
          transpile_to_c_struct_fields(&Struct->fields, ident);
        }

      break;
    case Ast_Expr_Type_Union:
      if (Type->symbol)
        printf("union " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Type->symbol));
      else
        {
          AstExprTypeStruct *Union = &Type->as.Struct_Or_Union;

          printf("union\n");
          transpile_to_c_struct_fields(&Union->fields, ident);
        }

      break;
    case Ast_Expr_Type_Enum:
      if (Type->symbol)
        printf("enum " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(Type->symbol));
      else
        {
          AstExprTypeEnum *Enum = &Type->as.Enum;

          printf("enum\n");
          transpile_to_c_enum_values(&Enum->values, ident + TAB_SIZE);
        }

      break;
    }

  if (should_print_symbol_name && symbol)
    printf(" " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));
}

void
transpile_to_c_type(AstExpr *type, size_t ident)
{
  transpile_to_c_type_with_symbol(type, NULL, ident);
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

        transpile_to_c_type_with_symbol(Variable->type, symbol, ident);
        if (Variable->expr)
          {
            put_string(" = ");
            transpile_to_c_expr(Variable->expr, ident);
          }
        put_string(";");

        ast_flags = old_flags;
      }

      break;
    case Ast_Symbol_Parameter:
      {
        AstSymbolParameter *Parameter = &symbol->as.Parameter;

        transpile_to_c_type_with_symbol(Parameter->type, symbol, ident);
      }

      break;
    case Ast_Symbol_Procedure:
      {
        AstSymbolProcedure *Procedure = &symbol->as.Procedure;

        AstExprTypeProcedure *Type_Procedure = &Procedure->type->as.Type.as.Procedure;
        assert(Procedure->type->tag == Ast_Expr_Type && Procedure->type->as.Type.tag == Ast_Expr_Type_Procedure);

        put_spaces(ident);
        transpile_to_c_type(Type_Procedure->return_type, ident);
        printf(" " SYMBOL_NAME_SPECIFIER, FORMAT_SYMBOL_NAME(symbol));
        transpile_to_c_type_procedure_params(Type_Procedure, ident);
        put_string("\n");
        transpile_to_c_stmt_block(&Procedure->block, ident);
        put_string("\n");
      }

      break;
    case Ast_Symbol_Type:
      {
        AstExpr *Type = symbol->as.Type;
        assert(Type->tag == Ast_Expr_Type);

        if (!(symbol->flags & AST_SYMBOL_FLAG_IS_UNPACKED))
          {
            put_spaces(ident);

            AstExprType *Expr_Type = &Type->as.Type;

            // If aliasing unnamed type, just typedef it, since it's already defined.
            if (symbol != Expr_Type->symbol)
              {
                printf("typedef " SYMBOL_NAME_SPECIFIER " " SYMBOL_NAME_SPECIFIER ";\n", FORMAT_SYMBOL_NAME(Expr_Type->symbol), FORMAT_SYMBOL_NAME(symbol));
                return;
              }

            switch (Expr_Type->tag)
              {
              case Ast_Expr_Type_Procedure:
                {
                  AstExprTypeProcedure *Procedure = &Expr_Type->as.Procedure;

                  put_string("typedef ");
                  transpile_to_c_type_procedure(Procedure, symbol, ident);
                  put_string(";\n");
                }

                break;
              case Ast_Expr_Type_Struct:
                {
                  AstExprTypeStruct *Struct = &Expr_Type->as.Struct_Or_Union;

                  printf("struct " SYMBOL_NAME_SPECIFIER "\n", FORMAT_SYMBOL_NAME(symbol));
                  transpile_to_c_struct_fields(&Struct->fields, ident);
                  put_string(";\n");
                }

                break;
              case Ast_Expr_Type_Union:
                {
                  AstExprTypeStruct *Union = &Expr_Type->as.Struct_Or_Union;

                  printf("union " SYMBOL_NAME_SPECIFIER "\n", FORMAT_SYMBOL_NAME(symbol));
                  transpile_to_c_struct_fields(&Union->fields, ident);
                  put_string(";\n");
                }

                break;
              case Ast_Expr_Type_Enum:
                {
                  AstExprTypeEnum *Enum = &Expr_Type->as.Enum;

                  printf("enum " SYMBOL_NAME_SPECIFIER "\n", FORMAT_SYMBOL_NAME(symbol));
                  transpile_to_c_enum_values(&Enum->values, ident + TAB_SIZE);
                  put_string(";\n");
                }

                break;
              default:
                put_string("typedef ");
                transpile_to_c_type_with_symbol(Type, symbol, ident);
                put_string(";\n");
              }
          }
      }

      break;
    case Ast_Symbol_Struct_Field:
    case Ast_Symbol_Union_Field:
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
        put_string("if (");
        transpile_to_c_expr(If->cond, ident);
        put_string(")\n");
        transpile_to_c_stmt(If->if_true, ident + TAB_SIZE);
        put_string("\n");
        put_spaces(ident);
        put_string("else\n");
        if (If->if_false)
          transpile_to_c_stmt(If->if_false, ident + TAB_SIZE);
        else
          {
            put_spaces(ident + TAB_SIZE);
            put_string("{ /* empty */ }");
          }
        put_string("\n");
      }

      break;
    case Ast_Stmt_While:
      {
        AstStmtWhile *While = &stmt->as.While;

        if (While->is_do_while)
          {
            put_spaces(ident);
            put_string("do\n");
            transpile_to_c_stmt(While->block, ident + TAB_SIZE);
            put_string("\n");
            put_spaces(ident);
            put_string("while (");
            transpile_to_c_expr(While->cond, ident);
            put_string(");\n");
          }
        else
          {
            put_spaces(ident);
            put_string("while (");
            transpile_to_c_expr(While->cond, ident);
            put_string(")\n");
            transpile_to_c_stmt(While->block, ident + TAB_SIZE);
            put_string("\n");
          }
      }

      break;
    case Ast_Stmt_Break:
      put_spaces(ident);
      put_string("break;");
      break;
    case Ast_Stmt_Continue:
      put_spaces(ident);
      put_string("continue;");
      break;
    case Ast_Stmt_Return_Nothing:
      put_spaces(ident);
      put_string("return;");
      break;
    case Ast_Stmt_Return_Expr:
      put_spaces(ident);
      put_string("return ");
      transpile_to_c_expr(stmt->as.Return_Expr, ident);
      put_string(";");
      break;
    case Ast_Stmt_Switch:
      {
        AstStmtSwitch *Switch = &stmt->as.Switch;

        put_spaces(ident);
        put_string("switch (");
        transpile_to_c_expr(Switch->cond, ident);
        put_string(")\n");
        ident += TAB_SIZE;
        put_spaces(ident);
        put_string("{\n");
        transpile_to_c_stmt_block_no_curly(&Switch->cases, ident);
        if (Switch->default_case)
          {
            put_spaces(ident);
            put_string("default:\n");
            ident += TAB_SIZE;
            transpile_to_c_stmt(Switch->default_case, ident);
            ident -= TAB_SIZE;
          }
        put_string("\n");
        put_spaces(ident);
        put_string("}");
        ident -= TAB_SIZE;
      }

      break;
    case Ast_Stmt_Case:
      {
        AstStmtCase *Case = &stmt->as.Case;

        put_spaces(ident - (ident < TAB_SIZE ? 0 : TAB_SIZE));
        put_string("case ");
        transpile_to_c_expr(Case->expr, ident);
        put_string(":\n");
        transpile_to_c_stmt(Case->substmt, ident);
      }

      break;
    case Ast_Stmt_Default:
      {
        put_spaces(ident - (ident < TAB_SIZE ? 0 : TAB_SIZE));
        put_string("default:\n");
        transpile_to_c_stmt(stmt->as.Default, ident);
      }

      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        put_spaces(ident);
        transpile_to_c_expr(Assign->lhs, ident);
        put_string(" = ");
        transpile_to_c_expr(Assign->rhs, ident);
        put_string(";");
      }

      break;
    case Ast_Stmt_Symbol:
      transpile_to_c_symbol(stmt->as.Symbol, ident);
      break;
    case Ast_Stmt_Expr:
      put_spaces(ident);
      transpile_to_c_expr(stmt->as.Expr, ident);
      put_string(";");
      break;
    }
}

void
transpile_to_c_stmt_block_no_curly(AstStmtBlock *block, size_t ident)
{
  ident += TAB_SIZE;
  for (LinkedListNode *node = block->first; node; node = node->next)
    {
      AstStmt *stmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);
      transpile_to_c_stmt(stmt, ident);
      put_string("\n");
    }
  ident -= TAB_SIZE;
}

void
transpile_to_c_stmt_block(AstStmtBlock *block, size_t ident)
{
  put_spaces(ident);
  put_string("{\n");
  transpile_to_c_stmt_block_no_curly(block, ident);
  put_spaces(ident);
  put_string("}");
}

void
transpile_to_c(Ast *ast)
{
  for (LinkedListNode *node = ast->globals.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      transpile_to_c_symbol(symbol, 0);
      put_string("\n");
    }
}
