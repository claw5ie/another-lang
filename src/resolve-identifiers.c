AstSymbol *
find_symbol(Ast *ast, StringView name, Scope *scope, LineInfo line_info)
{
  AstSymbolKey key = {
    .name = name,
    .scope = scope,
  };

  do
    {
      AstSymbol *symbol = hash_table_find(&ast->symbols, &key);

      if (symbol)
        {
          switch (symbol->tag)
            {
            case Ast_Symbol_Variable:
            case Ast_Symbol_Parameter:
              if (line_info.offset > symbol->line_info.offset)
                return symbol;

              break;
            case Ast_Symbol_Procedure:
            case Ast_Symbol_Type:
            case Ast_Symbol_Alias:
              return symbol;
            case Ast_Symbol_Struct_Field:
            case Ast_Symbol_Enum_Value:
              UNREACHABLE();
            }
        }

      key.scope = key.scope->parent;
      if (!key.scope)
        break;
    }
  while (true);

  PRINT_ERROR(ast->filepath, line_info, "symbol '%.*s' is not defined", FORMAT_STRING_VIEW(name));
  exit(EXIT_FAILURE);
}

void resolve_identifiers_expr(Ast *, AstExpr *);

void
resolve_identifiers_expr_list(Ast *ast, LinkedList *list)
{
  for (LinkedListNode *node = list->first; node; node = node->next)
    {
      AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
      resolve_identifiers_expr(ast, expr);
    }
}

void
resolve_identifiers_procedure_header(Ast *ast, LinkedList *params, AstExpr *return_type)
{
  for (LinkedListNode *node = params->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolParameter *Parameter = &symbol->as.Parameter;

      resolve_identifiers_expr(ast, Parameter->type);
    }
  resolve_identifiers_expr(ast, return_type);
}

void
resolve_identifiers_struct_fields(Ast *ast, LinkedList *fields)
{
  for (LinkedListNode *node = fields->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolStructField *Struct_Field = &symbol->as.Struct_Field;

      resolve_identifiers_expr(ast, Struct_Field->type);
    }
}

void
resolve_identifiers_type(Ast *ast, AstExpr *expr)
{
  assert(expr->tag == Ast_Expr_Type);

  AstExprType *Type = &expr->as.Type;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
    case Ast_Expr_Type_Bool:
    case Ast_Expr_Type_Int:
      break;
    case Ast_Expr_Type_Pointer:
      resolve_identifiers_expr(ast, Type->as.Pointer);
      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        resolve_identifiers_procedure_header(ast, &Proc->params, Proc->return_type);
      }

      break;
    case Ast_Expr_Type_Array:
      {
        AstExprArrayAccess *Array = &Type->as.Array;

        resolve_identifiers_expr(ast, Array->lhs);
        resolve_identifiers_expr(ast, Array->index);
      }

      break;
    case Ast_Expr_Type_Struct:
      {
        AstSymbolStruct *Struct = &Type->as.Struct;

        resolve_identifiers_struct_fields(ast, &Struct->fields);
      }

      break;
    case Ast_Expr_Type_Union:
      {
        AstSymbolStruct *Union = &Type->as.Union;

        resolve_identifiers_struct_fields(ast, &Union->fields);
      }

      break;
    case Ast_Expr_Type_Enum:
      break;
    case Ast_Expr_Type_Symbol:
      UNREACHABLE();
    }
}

void resolve_identifiers_symbol(Ast *, AstSymbol *);

void
resolve_identifiers_expr(Ast *ast, AstExpr *expr)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        resolve_identifiers_expr(ast, Binary_Op->lhs);
        resolve_identifiers_expr(ast, Binary_Op->rhs);
      }

      break;
    case Ast_Expr_Unary_Op:
      {
        AstExprUnaryOp *Unary_Op = &expr->as.Unary_Op;

        resolve_identifiers_expr(ast, Unary_Op->subexpr);

        if (Unary_Op->subexpr->tag == Ast_Expr_Type && Unary_Op->tag == Ast_Expr_Unary_Op_Deref)
          {
            AstExpr *subexpr = Unary_Op->subexpr;
            expr->tag = Ast_Expr_Type;
            expr->as.Type = (AstExprType){
              .tag = Ast_Expr_Type_Pointer,
              .as = { .Pointer = subexpr },
            };
          }
      }

      break;
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        resolve_identifiers_expr(ast, Array_Access->lhs);
        resolve_identifiers_expr(ast, Array_Access->index);

        if (Array_Access->lhs->tag == Ast_Expr_Type)
          {
            AstExprArrayAccess tmp = *Array_Access;
            expr->tag = Ast_Expr_Type;
            expr->as.Type = (AstExprType){
              .tag = Ast_Expr_Type_Array,
              .as = { .Array = tmp },
            };
          }
      }

      break;
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call;

        resolve_identifiers_expr(ast, Call->lhs);
        resolve_identifiers_expr_list(ast, &Call->args);

        if (Call->lhs->tag == Ast_Expr_Type)
          {
            AstExprCall tmp = *Call;
            expr->tag = Ast_Expr_Type_Cons;
            expr->as.Type_Cons = tmp;
          }
      }

      break;
    case Ast_Expr_Type_Cons:
      {
        AstExprCall *Type_Cons = &expr->as.Type_Cons;

        resolve_identifiers_expr_list(ast, &Type_Cons->args);
      }

      break;
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        resolve_identifiers_expr(ast, Field_Access->lhs);
      }

      break;
    case Ast_Expr_Cast1:
      resolve_identifiers_expr(ast, expr->as.Cast1);
      break;
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        resolve_identifiers_expr(ast, Cast2->type);
        resolve_identifiers_expr(ast, Cast2->expr);
      }

      break;
    case Ast_Expr_Type:
      resolve_identifiers_type(ast, expr);
      break;
    case Ast_Expr_Designator:
      {
        AstExprDesignator *Designator = &expr->as.Designator;

        resolve_identifiers_expr(ast, Designator->expr);
      }

      break;
    case Ast_Expr_Identifier:
      {
        AstExprIdentifier *Identifier = &expr->as.Identifier;

        // Should I rely on expressions line info? What if it doesn't start identifier?
        AstSymbol *symbol = find_symbol(ast, Identifier->name, Identifier->scope, expr->line_info);

        switch (symbol->tag)
          {
          case Ast_Symbol_Type:
            expr->tag = Ast_Expr_Type;
            expr->as.Type = (AstExprType){
              .tag = Ast_Expr_Type_Symbol,
              .as = { .Symbol = symbol },
            };
            break;
          case Ast_Symbol_Alias:
            resolve_identifiers_symbol(ast, symbol);

            expr->tag = Ast_Expr_Type;
            expr->as.Type = (AstExprType){
              .tag = Ast_Expr_Type_Symbol,
              .as = { .Symbol = symbol },
            };
            break;
          default:
            expr->tag = Ast_Expr_Symbol;
            expr->as.Symbol = symbol;
            break;
          }
      }

      break;
    case Ast_Expr_Int64:
    case Ast_Expr_Bool:
    case Ast_Expr_Null:
      break;
    case Ast_Expr_Symbol:
      UNREACHABLE();
    }
}

void resolve_identifiers_stmt_block(Ast *, AstStmtBlock *);

void
resolve_identifiers_symbol(Ast *ast, AstSymbol *symbol)
{
  switch (symbol->resolving_stage)
    {
    case AST_SYMBOL_FLAG_NOT_RESOLVED:
      symbol->resolving_stage = AST_SYMBOL_FLAG_BEING_RESOLVED;
      break;
    case AST_SYMBOL_FLAG_BEING_RESOLVED:
      PRINT_ERROR0(ast->filepath, symbol->line_info, "detected cyclic reference");
      exit(EXIT_FAILURE);
    case AST_SYMBOL_FLAG_IS_RESOLVED:
      return;
    }

  switch (symbol->tag)
    {
    case Ast_Symbol_Variable:
      {
        AstSymbolVariable *Variable = &symbol->as.Variable;

        if (Variable->type)
          resolve_identifiers_expr(ast, Variable->type);

        if (Variable->expr)
          resolve_identifiers_expr(ast, Variable->expr);
      }

      break;
    case Ast_Symbol_Parameter:
      {
        AstSymbolParameter *Parameter = &symbol->as.Parameter;

        resolve_identifiers_expr(ast, Parameter->type);
      }

      break;
    case Ast_Symbol_Procedure:
      {
        AstSymbolProcedure *Procedure = &symbol->as.Procedure;

        resolve_identifiers_procedure_header(ast, &Procedure->params, Procedure->return_type);
        resolve_identifiers_stmt_block(ast, &Procedure->block);
      }

      break;
    case Ast_Symbol_Type:
      resolve_identifiers_expr(ast, symbol->as.Type);
      break;
    case Ast_Symbol_Alias:
      {
        AstSymbolAlias *Alias = &symbol->as.Alias;
        AstExpr *Alias_Type = Alias->type;

        resolve_identifiers_expr(ast, Alias_Type);

        if (Alias_Type->tag == Ast_Expr_Type && Alias_Type->as.Type.tag == Ast_Expr_Type_Symbol)
          {
            AstSymbol *underlying_symbol = Alias_Type->as.Type.as.Symbol;
            switch (symbol->tag)
              {
              case Ast_Symbol_Type:
                Alias->underlying_type = underlying_symbol->as.Type;
                break;
              case Ast_Symbol_Alias:
                Alias->underlying_type = underlying_symbol->as.Alias.underlying_type;
                break;
              default:
                UNREACHABLE();
              }
          }
      }

      break;
    case Ast_Symbol_Struct_Field:
    case Ast_Symbol_Enum_Value:
      UNREACHABLE();
    }

  symbol->resolving_stage = AST_SYMBOL_FLAG_IS_RESOLVED;
}

void
resolve_identifiers_stmt(Ast *ast, AstStmt *stmt)
{
  switch (stmt->tag)
    {
    case Ast_Stmt_Block:
      resolve_identifiers_stmt_block(ast, &stmt->as.Block);
      break;
    case Ast_Stmt_If:
      {
        AstStmtIf *If = &stmt->as.If;

        resolve_identifiers_expr(ast, If->cond);
        resolve_identifiers_stmt(ast, If->if_true);
        if (If->if_false)
          resolve_identifiers_stmt(ast, If->if_false);
      }

      break;
    case Ast_Stmt_While:
      {
        AstStmtWhile *While = &stmt->as.While;

        resolve_identifiers_expr(ast, While->cond);
        resolve_identifiers_stmt(ast, While->block);
      }

      break;
    case Ast_Stmt_Return_Expr:
      resolve_identifiers_expr(ast, stmt->as.Return_Expr);
      break;
    case Ast_Stmt_Switch:
      {
        AstStmtSwitch *Switch = &stmt->as.Switch;

        resolve_identifiers_expr(ast, Switch->cond);
        resolve_identifiers_stmt_block(ast, &Switch->cases);
        // Default case is always null for now.
      }

      break;
    case Ast_Stmt_Case:
      {
        AstStmtCase *Case = &stmt->as.Case;

        resolve_identifiers_expr(ast, Case->expr);
        resolve_identifiers_stmt(ast, Case->substmt);
      }

      break;
    case Ast_Stmt_Default:
      resolve_identifiers_stmt(ast, stmt->as.Default);
      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        resolve_identifiers_expr(ast, Assign->lhs);
        resolve_identifiers_expr(ast, Assign->rhs);
      }

      break;
    case Ast_Stmt_Symbol:
      resolve_identifiers_symbol(ast, stmt->as.Symbol);
      break;
    case Ast_Stmt_Expr:
      resolve_identifiers_expr(ast, stmt->as.Expr);
      break;
    case Ast_Stmt_Break:
    case Ast_Stmt_Continue:
    case Ast_Stmt_Return_Nothing:
      break;
    }
}

void
resolve_identifiers_stmt_block(Ast *ast, AstStmtBlock *block)
{
  for (LinkedListNode *node = block->first; node; node = node->next)
    {
      AstStmt *stmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);
      resolve_identifiers_stmt(ast, stmt);
    }
}

void
resolve_identifiers(Ast *ast)
{
  for (LinkedListNode *node = ast->globals.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      resolve_identifiers_symbol(ast, symbol);
    }
}
