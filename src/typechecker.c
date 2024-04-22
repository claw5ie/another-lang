typedef u16 TypeFlagsType;
#define TYPE_IS_INTEGER 0x4
#define TYPE_IS_POINTER 0x8
#define TYPE_IS_COMPARABLE 0x10
#define TYPE_HAS_ORDER 0x20
#define TYPE_CAN_DEREFERENCE 0x40
#define TYPE_HAS_NO_BITS 0x80
#define TYPE_POINTS_TO_VOID (0x100 | TYPE_IS_POINTER)

#define is_pointer_to_void(flags) (((flags) & TYPE_POINTS_TO_VOID) == TYPE_POINTS_TO_VOID)
#define is_pointer_to_non_void(flags) (((flags) & TYPE_POINTS_TO_VOID) == TYPE_IS_POINTER)

AstExpr g_void_type = {
  .tag = Ast_Expr_Type,
  .as = { .Type = {
      .tag = Ast_Expr_Type_Void,
    } },
};
AstExpr g_bool_type = {
  .tag = Ast_Expr_Type,
  .as = { .Type = {
      .tag = Ast_Expr_Type_Bool,
    } },
};
AstExpr g_generic_int_type = {
  .tag = Ast_Expr_Type,
  .as = { .Type = {
      .tag = Ast_Expr_Type_Generic_Int,
    } },
};
AstExpr g_null_type = {
  .tag = Ast_Expr_Type,
  .as = { .Type = {
      .tag = Ast_Expr_Type_Pointer,
      .as = { .Pointer = &g_void_type },
    } },
};
AstExpr g_max_bits_int_type = {
  .tag = Ast_Expr_Type,
  .as = { .Type = {
      .tag = Ast_Expr_Type_Int,
      .as = { .Int = {
          .bits = MAX_BITS_IN_INT,
          .is_signed = true,
        } },
    } },
};

AstExpr *typecheck_expr(Ast *, AstExpr *, AstExpr *);
void typecheck_type(Ast *, AstExpr *);
void typecheck_symbol(Ast *, AstSymbol *);
void typecheck_stmt_block(Ast *, AstStmtBlock *);
void eprint_type(AstExpr *);

TypeFlagsType
compare_type(AstExpr *type)
{
  assert(type->tag == Ast_Expr_Type);
  AstExprType *Type = &type->as.Type;
  TypeFlagsType flags = 0;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      flags |= TYPE_HAS_NO_BITS;
      break;
    case Ast_Expr_Type_Bool:
      break;
    case Ast_Expr_Type_Int:
      flags |= TYPE_IS_INTEGER | TYPE_IS_COMPARABLE | TYPE_HAS_ORDER;
      if (Type->as.Int.bits == 0)
        flags |= TYPE_HAS_NO_BITS;
      break;
    case Ast_Expr_Type_Generic_Int:
      flags |= TYPE_IS_INTEGER | TYPE_IS_COMPARABLE | TYPE_HAS_ORDER;
      break;
    case Ast_Expr_Type_Pointer:
      flags |= TYPE_IS_POINTER | TYPE_IS_COMPARABLE | TYPE_HAS_ORDER | TYPE_CAN_DEREFERENCE;

      if (Type->as.Pointer->as.Type.tag == Ast_Expr_Type_Void)
        {
          flags &= ~TYPE_CAN_DEREFERENCE;
          flags |= TYPE_POINTS_TO_VOID;
        }

      break;
    case Ast_Expr_Type_Proc:
      flags |= TYPE_IS_POINTER | TYPE_IS_COMPARABLE | TYPE_HAS_ORDER;
      break;
    case Ast_Expr_Type_Array:
    case Ast_Expr_Type_Struct:
    case Ast_Expr_Type_Union:
      break;
    case Ast_Expr_Type_Enum:
      flags |= TYPE_IS_COMPARABLE | TYPE_HAS_ORDER;
      break;
    }

  return flags;
}

bool
can_negate_int_type(AstExprTypeInt *dst, AstExpr *type)
{
  assert(type->tag == Ast_Expr_Type);
  AstExprType *Type = &type->as.Type;
  AstExprTypeInt result = { 0 };
  bool ok = true;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        if (Int->is_signed)
          result = *Int;
        else if (Int->bits < MAX_BITS_IN_INT)
          result = (AstExprTypeInt){
            .bits = Int->bits + 1,
            .is_signed = true,
          };
        else
          ok = false;
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      result = g_max_bits_int_type.as.Type.as.Int;
      break;
    default:
      ok = false;
      break;
    }

  if (ok && dst)
    *dst = result;

  return ok;
}

bool
can_cast_to_type(AstExpr *to_cast, AstExpr *type)
{
  assert(to_cast->tag == Ast_Expr_Type && type->tag == Ast_Expr_Type);

  AstExprType *To_Cast = &to_cast->as.Type;
  AstExprType *Type = &type->as.Type;

  switch (To_Cast->tag)
    {
    case Ast_Expr_Type_Void:
      return Type->tag == Ast_Expr_Type_Void;
    case Ast_Expr_Type_Bool:
    case Ast_Expr_Type_Int:
    case Ast_Expr_Type_Generic_Int:
    case Ast_Expr_Type_Pointer:
    case Ast_Expr_Type_Proc:
    case Ast_Expr_Type_Enum:
      switch (Type->tag)
        {
        case Ast_Expr_Type_Bool:
        case Ast_Expr_Type_Int:
        case Ast_Expr_Type_Generic_Int:
        case Ast_Expr_Type_Pointer:
        case Ast_Expr_Type_Proc:
        case Ast_Expr_Type_Enum:
          return true;
        case Ast_Expr_Type_Array:
          return To_Cast->tag == Ast_Expr_Type_Pointer;
        default:
          return false;
        }
    case Ast_Expr_Type_Array:
      switch (Type->tag)
        {
        case Ast_Expr_Type_Pointer:
        case Ast_Expr_Type_Array:
          return true;
        default:
          return false;
        }
    case Ast_Expr_Type_Struct:
    case Ast_Expr_Type_Union:
      return to_cast == type;
    }

  UNREACHABLE();
}

// u{n}, u{m} -> u{max(n, m)}
// i{n}, i{m} -> i{max(n, m)}
// u{n}, i{m} -> i{m}     m > n
//               i{n+1}   otherwise
bool
are_int_types_compatible(AstExprType *dst, AstExpr *lhs_type, AstExpr *rhs_type)
{
  assert(lhs_type->tag == Ast_Expr_Type && rhs_type->tag == Ast_Expr_Type);
  AstExprType *Lhs_Type = &lhs_type->as.Type;
  AstExprType *Rhs_Type = &rhs_type->as.Type;
  AstExprType result = { 0 };
  bool ok = true;

  switch (Lhs_Type->tag)
    {
    case Ast_Expr_Type_Int:
      switch (Rhs_Type->tag)
        {
        case Ast_Expr_Type_Int:
          {
            AstExprTypeInt *Lhs_Int = &Lhs_Type->as.Int;
            AstExprTypeInt *Rhs_Int = &Rhs_Type->as.Int;

            switch ((Lhs_Int->is_signed << 1) | Rhs_Int->is_signed)
              {
              case 0: // 0b00
              case 3: // 0b11
                result = (AstExprType){
                  .tag = Ast_Expr_Type_Int,
                  .as = { .Int = {
                      .bits = max_u16(Lhs_Int->bits, Rhs_Int->bits),
                      .is_signed = Lhs_Int->is_signed,
                    } },
                  .symbol = NULL,
                };
                break;
              case 1: // 0b01
                {
                  AstExprTypeInt *tmp = Lhs_Int;
                  Lhs_Int = Rhs_Int;
                  Rhs_Int = tmp;
                }

                // fallthrough
              case 2: // 0b10
                if (Lhs_Int->bits > Rhs_Int->bits)
                  result = (AstExprType){
                    .tag = Ast_Expr_Type_Int,
                    .as = { .Int = {
                        .bits = Lhs_Int->bits,
                        .is_signed = true,
                      } },
                    .symbol = NULL,
                  };
                else if (Rhs_Int->bits < MAX_BITS_IN_INT)
                  result = (AstExprType){
                    .tag = Ast_Expr_Type_Int,
                    .as = { .Int = {
                        .bits = Rhs_Int->bits + 1,
                        .is_signed = true,
                      } },
                    .symbol = NULL,
                  };
                else
                  ok = false;

                break;
              default:
                UNREACHABLE();
              }
          }

          break;
        case Ast_Expr_Type_Generic_Int:
          result = *Lhs_Type;
          break;
        default:
          ok = false;
        }

      break;
    case Ast_Expr_Type_Generic_Int:
      result = *Rhs_Type;
      break;
    default:
      ok = false;
    }

  if (ok && dst)
    *dst = result;

  return ok;
}

bool
are_types_equal(AstExpr *lhs_type, AstExpr *rhs_type)
{
  assert(lhs_type->tag == Ast_Expr_Type && rhs_type->tag == Ast_Expr_Type);
  AstExprType *Lhs_Type = &lhs_type->as.Type;
  AstExprType *Rhs_Type = &rhs_type->as.Type;

  if (Lhs_Type->symbol && Lhs_Type->symbol == Rhs_Type->symbol)
    return true;

  switch (Lhs_Type->tag)
    {
    case Ast_Expr_Type_Void:
    case Ast_Expr_Type_Bool:
      return (Lhs_Type->tag == Rhs_Type->tag);
    case Ast_Expr_Type_Int:
    case Ast_Expr_Type_Generic_Int:
      return are_int_types_compatible(NULL, lhs_type, rhs_type);
    case Ast_Expr_Type_Pointer:
      {
        TypeFlagsType lhs_flags = compare_type(lhs_type);
        TypeFlagsType rhs_flags = compare_type(rhs_type);

        if ((is_pointer_to_void(lhs_flags) && (rhs_flags & TYPE_IS_POINTER))
            || ((lhs_flags & TYPE_IS_POINTER) && is_pointer_to_void(rhs_flags)))
          return true;
        else if (Rhs_Type->tag != Ast_Expr_Type_Pointer)
          return false;
        else
          return are_types_equal(Lhs_Type->as.Pointer, Rhs_Type->as.Pointer);
      }
    case Ast_Expr_Type_Proc:
      {
        TypeFlagsType rhs_flags = compare_type(rhs_type);

        if (is_pointer_to_void(rhs_flags))
          return true;
        else if (Rhs_Type->tag != Ast_Expr_Type_Proc)
          return false;

        AstExprTypeProc *Lhs_Proc = &Lhs_Type->as.Proc;
        AstExprTypeProc *Rhs_Proc = &Rhs_Type->as.Proc;

        if (Lhs_Proc->params.count != Rhs_Proc->params.count)
          return false;

        LinkedListNode *lhs_node = Lhs_Proc->params.first;
        LinkedListNode *rhs_node = Rhs_Proc->params.first;
        while (lhs_node)
          {
            AstSymbolParameter *Lhs_Parameter = &LINKED_LIST_GET_NODE_DATA(AstSymbol *, lhs_node)->as.Parameter;
            AstSymbolParameter *Rhs_Parameter = &LINKED_LIST_GET_NODE_DATA(AstSymbol *, rhs_node)->as.Parameter;

            if (!are_types_equal(Lhs_Parameter->type, Rhs_Parameter->type))
              return false;

            lhs_node = lhs_node->next;
            rhs_node = rhs_node->next;
          }

        return are_types_equal(Lhs_Proc->return_type, Rhs_Proc->return_type);
      }
    case Ast_Expr_Type_Array:
      {
        if (Rhs_Type->tag != Ast_Expr_Type_Array)
          return false;

        AstExprArrayAccess *Lhs_Array = &Lhs_Type->as.Array;
        AstExprArrayAccess *Rhs_Array = &Rhs_Type->as.Array;

        // TODO: check if sizes are the same, but need to precompute them first.

        return are_types_equal(Lhs_Array->lhs, Rhs_Array->lhs);
      }
    case Ast_Expr_Type_Struct:
    case Ast_Expr_Type_Union:
    case Ast_Expr_Type_Enum:
      return lhs_type == rhs_type;
    }

  UNREACHABLE();
}

void
eprint_expr(AstExpr *expr)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        fputc('(', stderr);
        eprint_expr(Binary_Op->lhs);
        switch (Binary_Op->tag)
          {
          case Ast_Expr_Binary_Op_Or:  fputs(" || ", stderr); break;
          case Ast_Expr_Binary_Op_And: fputs(" && ", stderr); break;
          case Ast_Expr_Binary_Op_Eq:  fputs(" == ", stderr); break;
          case Ast_Expr_Binary_Op_Neq: fputs(" != ", stderr); break;
          case Ast_Expr_Binary_Op_Leq: fputs(" <= ", stderr); break;
          case Ast_Expr_Binary_Op_Geq: fputs(" >= ", stderr); break;
          case Ast_Expr_Binary_Op_Lt:  fputs(" < ", stderr); break;
          case Ast_Expr_Binary_Op_Gt:  fputs(" > ", stderr); break;
          case Ast_Expr_Binary_Op_Add: fputs(" + ", stderr); break;
          case Ast_Expr_Binary_Op_Sub: fputs(" - ", stderr); break;
          case Ast_Expr_Binary_Op_Mul: fputs(" * ", stderr); break;
          case Ast_Expr_Binary_Op_Div: fputs(" / ", stderr); break;
          case Ast_Expr_Binary_Op_Mod: fputs(" % ", stderr); break;
          }
        eprint_expr(Binary_Op->rhs);
        fputc(')', stderr);
      }

      break;
    case Ast_Expr_Unary_Op:
      {
        AstExprUnaryOp *Unary_Op = &expr->as.Unary_Op;

        fputc('(', stderr);
        switch (Unary_Op->tag)
          {
          case Ast_Expr_Unary_Op_Neg:
            {
              fputc('-', stderr);
              eprint_expr(Unary_Op->subexpr);
            }

            break;
          case Ast_Expr_Unary_Op_Not:
            {
              fputc('!', stderr);
              eprint_expr(Unary_Op->subexpr);
            }

            break;
          case Ast_Expr_Unary_Op_Ref:
            {
              fputc('&', stderr);
              eprint_expr(Unary_Op->subexpr);
            }

            break;
          case Ast_Expr_Unary_Op_Deref:
            {
              eprint_expr(Unary_Op->subexpr);
              fputc('*', stderr);
            }

            break;
          }
        fputc(')', stderr);
      }

      break;
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        eprint_expr(Array_Access->lhs);
        fputc('[', stderr);
        eprint_expr(Array_Access->index);
        fputc(']', stderr);
      }

      break;
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call;

        eprint_expr(Call->lhs);
        fputc('(', stderr);
        for (LinkedListNode *node = Call->args.first; node; node = node->next)
          {
            AstExpr *subexpr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
            eprint_expr(subexpr);

            if (node->next)
              fputs(", ", stderr);
          }
        fputc(')', stderr);
      }

      break;
    case Ast_Expr_Type_Cons:
      {
        AstExprCall *Type_Cons = &expr->as.Type_Cons;

        eprint_expr(Type_Cons->lhs);
        fputc('(', stderr);
        for (LinkedListNode *node = Type_Cons->args.first; node; node = node->next)
          {
            AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, node);
            eprint_expr(expr);

            if (node->next)
              fputs(", ", stderr);
          }
        fputc(')', stderr);
      }

      break;
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        eprint_expr(Field_Access->lhs);
        fprintf(stderr, ".%.*s", FORMAT_STRING_VIEW(Field_Access->name));
      }

      break;
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        fputs("cast(", stderr);
        eprint_type(Cast2->type);
        fputs(", ", stderr);
        eprint_expr(Cast2->expr);
        fputc(')', stderr);
      }

      break;
    case Ast_Expr_Type:
      eprint_type(expr);
      break;
    case Ast_Expr_Int64:
      fprintf(stderr, "%lu", expr->as.Int64);
      break;
    case Ast_Expr_Bool:
      fprintf(stderr, "%s", expr->as.Bool ? "true" : "false");
      break;
    case Ast_Expr_Designator:
      {
        AstExprDesignator *Designator = &expr->as.Designator;

        fprintf(stderr, "%.*s = ", FORMAT_STRING_VIEW(Designator->name));
        eprint_expr(Designator->expr);
      }

      break;
    case Ast_Expr_Null:
      fputs("null", stderr);
      break;
    case Ast_Expr_Symbol:
      {
        AstSymbol *symbol = expr->as.Symbol;

        fprintf(stderr, "%.*s", FORMAT_STRING_VIEW(symbol->name));
      }

      break;
    case Ast_Expr_Cast1:
    case Ast_Expr_Enum_Identifier:
    case Ast_Expr_Identifier:
      UNREACHABLE();
    }
}

void
eprint_type(AstExpr *type)
{
  assert(type->tag == Ast_Expr_Type);
  AstExprType *Type = &type->as.Type;

  if (Type->symbol)
    {
      fprintf(stderr, "%.*s", FORMAT_STRING_VIEW(Type->symbol->name));
      return;
    }

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      fputs("void", stderr);
      break;
    case Ast_Expr_Type_Bool:
      fputs("bool", stderr);
      break;
    case Ast_Expr_Type_Int:
      {
        AstExprTypeInt *Int = &Type->as.Int;

        fprintf(stderr, "%c%u", Int->is_signed ? 'i' : 'u', Int->bits);
      }

      break;
    case Ast_Expr_Type_Generic_Int:
      fputs("int", stderr);
      break;
    case Ast_Expr_Type_Pointer:
      eprint_type(Type->as.Pointer);
      fputc('*', stderr);
      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        fputs("proc(", stderr);
        for (LinkedListNode *node = Proc->params.first; node; node = node->next)
          {
            AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
            AstSymbolParameter *Parameter = &symbol->as.Parameter;

            eprint_type(Parameter->type);

            if (node->next)
              fputs(", ", stderr);
          }
        fputs(") -> ", stderr);
        eprint_type(Proc->return_type);
      }

      break;
    case Ast_Expr_Type_Array:
      {
        AstExprArrayAccess *Array = &Type->as.Array;

        eprint_type(Array->lhs);
        fputc('[', stderr);
        eprint_expr(Array->index);
        fputc(']', stderr);
      }

      break;
    case Ast_Expr_Type_Struct:
      fputs("<unnamed struct>", stderr);
      break;
    case Ast_Expr_Type_Union:
      fputs("<unnamed union>", stderr);
      break;
    case Ast_Expr_Type_Enum:
      fputs("<unnamed enum>", stderr);
      break;
    }
}

void
typecheck_type_proc(Ast *ast, AstExprTypeProc *type)
{
  for (LinkedListNode *node = type->params.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolParameter *Parameter = &symbol->as.Parameter;

      typecheck_type(ast, Parameter->type);
    }
  typecheck_type(ast, type->return_type);
}

void
typecheck_struct_fields(Ast *ast, LinkedList *fields)
{
  for (LinkedListNode *node = fields->first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      AstSymbolStructField *Struct_Field = &symbol->as.Struct_Field;

      typecheck_type(ast, Struct_Field->type);
    }
}

void
typecheck_type(Ast *ast, AstExpr *expr)
{
  // Would be nice to remove this? (like move to the 'resolve identifier' stage)
  if (expr->tag != Ast_Expr_Type)
    {
      PRINT_ERROR0_LN(ast->filepath, expr->line_info, "expected type, not expression");
      EXIT_ERROR();
    }

  switch (expr->typechecking_stage)
    {
    case Ast_Stage_Not_Typechecked:
      expr->typechecking_stage = Ast_Stage_Being_Typechecked;
      break;
    case Ast_Stage_Being_Typechecked:
      if (ast->flags & AST_FLAG_SKIP_CYCLE)
        return;

      PRINT_ERROR0_LN(ast->filepath, expr->line_info, "detected cyclic reference");
      EXIT_ERROR();
    case Ast_Stage_Is_Typechecked:
      return;
    }

  AstExprType *Type = &expr->as.Type;
  AstFlagsType old_flags = ast->flags;

  switch (Type->tag)
    {
    case Ast_Expr_Type_Void:
      if (ast->flags & AST_FLAG_REJECT_VOID_TYPE)
        {
          PRINT_ERROR0_LN(ast->filepath, expr->line_info, "unexpected 'void'");
          EXIT_ERROR();
        }

      break;
    case Ast_Expr_Type_Bool:
    case Ast_Expr_Type_Int:
    case Ast_Expr_Type_Generic_Int:
      break;
    case Ast_Expr_Type_Pointer:
      {
        ast->flags &= ~AST_FLAG_REJECT_VOID_TYPE;
        ast->flags |= AST_FLAG_SKIP_CYCLE;
        typecheck_type(ast, Type->as.Pointer);
      }

      break;
    case Ast_Expr_Type_Proc:
      {
        AstExprTypeProc *Proc = &Type->as.Proc;

        ast->flags &= ~AST_FLAG_REJECT_VOID_TYPE;
        ast->flags |= AST_FLAG_SKIP_CYCLE;
        typecheck_type_proc(ast, Proc);
      }

      break;
    case Ast_Expr_Type_Array:
      {
        AstExprArrayAccess *Array = &Type->as.Array;

        typecheck_type(ast, Array->lhs);
        AstExpr *index_type = typecheck_expr(ast, NULL, Array->index);
        TypeFlagsType index_flags = compare_type(index_type);

        if (!(index_flags & TYPE_IS_INTEGER))
          {
            PRINT_ERROR0(ast->filepath, expr->line_info, "expected integer, but got '");
            eprint_type(index_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }
      }

      break;
    case Ast_Expr_Type_Struct:
      {
        AstSymbolStruct *Struct = &Type->as.Struct;

        typecheck_struct_fields(ast, &Struct->fields);
      }

      break;
    case Ast_Expr_Type_Union:
      {
        AstSymbolStruct *Union = &Type->as.Union;

        typecheck_struct_fields(ast, &Union->fields);
      }

      break;
    case Ast_Expr_Type_Enum:
      {
        AstSymbolEnum *Enum = &Type->as.Enum;

        ast->flags |= AST_FLAG_SKIP_CYCLE | AST_FLAG_IS_TYPECHECKING_ENUM;

        for (LinkedListNode *node = Enum->values.first; node; node = node->next)
          {
            AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
            assert(symbol->tag == Ast_Symbol_Enum_Value);
						typecheck_symbol(ast, symbol);
          }
      }

      break;
    }

  ast->flags = old_flags;

  expr->typechecking_stage = Ast_Stage_Is_Typechecked;
}

void
typecheck_struct(Ast *ast, AstSymbolStruct *Struct, LinkedList *args, LineInfo line_info)
{
  LinkedListNode *arg_node = args->first;
  while (arg_node)
    {
      AstExpr *arg_expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, arg_node);

      if (arg_expr->tag != Ast_Expr_Designator)
        {
          PRINT_ERROR0_LN(ast->filepath, line_info, "expected designator to construct a type");
          EXIT_ERROR();
        }

      AstExprDesignator *Designator = &arg_expr->as.Designator;
      AstSymbol *symbol = find_symbol(ast, Designator->name, Struct->scope, line_info);
      assert(symbol->tag == Ast_Symbol_Struct_Field);
      AstExpr *field_type = symbol->as.Struct_Field.type;
      AstExpr *arg_type = typecheck_expr(ast, field_type, Designator->expr);

      if (!are_types_equal(field_type, arg_type))
        {
          PRINT_ERROR0(ast->filepath, line_info, "expected '");
          eprint_type(field_type);
          EPRINT0("', but got '");
          eprint_type(arg_type);
          EPRINT0("'\n");
          EXIT_ERROR();
        }

      arg_node = arg_node->next;
    }
}

AstExpr *
typecheck_expr(Ast *ast, AstExpr *type_hint, AstExpr *expr)
{
  switch (expr->tag)
    {
    case Ast_Expr_Binary_Op:
      {
        AstExprBinaryOp *Binary_Op = &expr->as.Binary_Op;

        AstExpr *lhs_type = typecheck_expr(ast, NULL, Binary_Op->lhs);
        AstExpr *rhs_type = typecheck_expr(ast, lhs_type, Binary_Op->rhs);
        TypeFlagsType lhs_flags = compare_type(lhs_type);
        TypeFlagsType rhs_flags = compare_type(rhs_type);

        switch (Binary_Op->tag)
          {
          case Ast_Expr_Binary_Op_Or:
          case Ast_Expr_Binary_Op_And:
            {
              if (!are_types_equal(lhs_type, &g_bool_type) || !are_types_equal(rhs_type, &g_bool_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "expected 'bool'/'bool', but got '");
                  eprint_type(lhs_type);
                  EPRINT0("'/'");
                  eprint_type(rhs_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              return &g_bool_type;
            }
          case Ast_Expr_Binary_Op_Eq:
          case Ast_Expr_Binary_Op_Neq:
            {
              if (!(lhs_flags & TYPE_IS_COMPARABLE) || !are_types_equal(lhs_type, rhs_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "can't compare '");
                  eprint_type(lhs_type);
                  EPRINT0("'/'");
                  eprint_type(rhs_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              return &g_bool_type;
            }
          case Ast_Expr_Binary_Op_Leq:
          case Ast_Expr_Binary_Op_Geq:
          case Ast_Expr_Binary_Op_Lt:
          case Ast_Expr_Binary_Op_Gt:
            {
              if (!(lhs_flags & TYPE_HAS_ORDER) || !are_types_equal(lhs_type, rhs_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "can't compare '");
                  eprint_type(lhs_type);
                  EPRINT0("'/'");
                  eprint_type(rhs_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              return &g_bool_type;
            }
          case Ast_Expr_Binary_Op_Add:
            {
              if (is_pointer_to_non_void(lhs_flags) && (rhs_flags & TYPE_IS_INTEGER))
                return lhs_type;
              else if ((lhs_flags & TYPE_IS_INTEGER) && is_pointer_to_non_void(rhs_flags))
                return rhs_type;
              else
                {
                  AstExprType Type = { 0 };

                  if (!are_int_types_compatible(&Type, lhs_type, rhs_type))
                    {
                      PRINT_ERROR0(ast->filepath, expr->line_info, "can't add '");
                      eprint_type(lhs_type);
                      EPRINT0("'/'");
                      eprint_type(rhs_type);
                      EPRINT0("'\n");
                      EXIT_ERROR();
                    }

                  AstExpr *result = ast_malloc(ast, sizeof(*result));
                  *result = (AstExpr){
                    .tag = Ast_Expr_Type,
                    .as = { .Type = Type },
                    .line_info = expr->line_info,
                    .typechecking_stage = Ast_Stage_Is_Typechecked,
                    .flags = 0,
                  };

                  return result;
                }
            }
          case Ast_Expr_Binary_Op_Sub:
            {
              if (is_pointer_to_non_void(lhs_flags) && (rhs_flags & TYPE_IS_INTEGER))
                return lhs_type;
              else
                {
                  AstExprType Type = { 0 };

                  if (!are_int_types_compatible(&Type, lhs_type, rhs_type))
                    {
                      PRINT_ERROR0(ast->filepath, expr->line_info, "can't subtract '");
                      eprint_type(lhs_type);
                      EPRINT0("'/'");
                      eprint_type(rhs_type);
                      EPRINT0("'\n");
                      EXIT_ERROR();
                    }

                  AstExpr *result = ast_malloc(ast, sizeof(*result));
                  *result = (AstExpr){
                    .tag = Ast_Expr_Type,
                    .as = { .Type = Type },
                    .line_info = expr->line_info,
                    .typechecking_stage = Ast_Stage_Is_Typechecked,
                    .flags = 0,
                  };

                  return result;
                }
            }
          case Ast_Expr_Binary_Op_Mul:
          case Ast_Expr_Binary_Op_Div:
          case Ast_Expr_Binary_Op_Mod:
            {
              AstExprType Type = {0 };

              if (!are_int_types_compatible(&Type, lhs_type, rhs_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "expected integer/intger, but got '");
                  eprint_type(lhs_type);
                  EPRINT0("'/'");
                  eprint_type(rhs_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              AstExpr *result = ast_malloc(ast, sizeof(*result));
              *result = (AstExpr){
                .tag = Ast_Expr_Type,
                .as = { .Type = Type },
                .line_info = expr->line_info,
                .typechecking_stage = Ast_Stage_Is_Typechecked,
                .flags = 0,
              };

              return result;
            }
          }
      }

      UNREACHABLE();
    case Ast_Expr_Unary_Op:
      {
        AstExprUnaryOp *Unary_Op = &expr->as.Unary_Op;

        AstExpr *subexpr_type = typecheck_expr(ast, NULL, Unary_Op->subexpr);

        switch (Unary_Op->tag)
          {
          case Ast_Expr_Unary_Op_Neg:
            {
              AstExprTypeInt Int = { 0 };

              if (!can_negate_int_type(&Int, subexpr_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "can't negate '");
                  eprint_type(subexpr_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              AstExpr *result = ast_malloc(ast, sizeof(*result));
              *result = (AstExpr){
                .tag = Ast_Expr_Type,
                .as = { .Type = {
                    .tag = Ast_Expr_Type_Int,
                    .as = { .Int = Int },
                    .symbol = NULL,
                  } },
                .line_info = expr->line_info,
                .typechecking_stage = Ast_Stage_Is_Typechecked,
                .flags = 0,
              };

              return result;
            }
          case Ast_Expr_Unary_Op_Not:
            {
              if (subexpr_type->as.Type.tag != Ast_Expr_Type_Bool)
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "expected 'bool', but got '");
                  eprint_type(subexpr_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              return &g_bool_type;
            }
          case Ast_Expr_Unary_Op_Ref:
            {
              if (!(Unary_Op->subexpr->flags & AST_EXPR_FLAG_IS_LVALUE))
                {
                  PRINT_ERROR0_LN(ast->filepath, Unary_Op->subexpr->line_info, "expression is not lvalue");
                  EXIT_ERROR();
                }

              AstExpr *result = ast_malloc(ast, sizeof(*result));
              *result = (AstExpr){
                .tag = Ast_Expr_Type,
                .as = { .Type = {
                    .tag = Ast_Expr_Type_Pointer,
                    .as = { .Pointer = subexpr_type },
                    .symbol = NULL,
                  } },
                .line_info = expr->line_info,
                .typechecking_stage = Ast_Stage_Is_Typechecked,
                .flags = 0,
              };

              return result;
            }
          case Ast_Expr_Unary_Op_Deref:
            {
              TypeFlagsType subexpr_flags = compare_type(subexpr_type);

              if (!(subexpr_flags & TYPE_CAN_DEREFERENCE))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "can't dereference '");
                  eprint_type(subexpr_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              expr->flags |= AST_EXPR_FLAG_IS_LVALUE;

              assert(subexpr_type->tag == Ast_Expr_Type && subexpr_type->as.Type.tag == Ast_Expr_Type_Pointer);

              return subexpr_type->as.Type.as.Pointer;
            }
          }
      }

      UNREACHABLE();
    case Ast_Expr_Array_Access:
      {
        AstExprArrayAccess *Array_Access = &expr->as.Array_Access;

        AstExpr *lhs_type = typecheck_expr(ast, NULL, Array_Access->lhs);
        AstExpr *index_type = typecheck_expr(ast, NULL, Array_Access->index);
        TypeFlagsType index_flags = compare_type(index_type);
        AstExpr *result = NULL;

        switch (lhs_type->as.Type.tag)
          {
          case Ast_Expr_Type_Pointer:
            result = lhs_type->as.Type.as.Pointer;

            if (result->as.Type.tag == Ast_Expr_Type_Void)
              {
                PRINT_ERROR0_LN(ast->filepath, expr->line_info, "can't dereference pointer to 'void'");
                EXIT_ERROR();
              }

            break;
          case Ast_Expr_Type_Array:
            result = lhs_type->as.Type.as.Array.lhs;
            break;
          default:
            {
              PRINT_ERROR0(ast->filepath, expr->line_info, "'");
              eprint_type(lhs_type);
              EPRINT0("' is not indexable\n");
              EXIT_ERROR();
            }
          }

        if (!(index_flags & TYPE_IS_INTEGER))
          {
            PRINT_ERROR0(ast->filepath, expr->line_info, "expected integer, but got '");
            eprint_type(index_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }

        if (Array_Access->lhs->flags & AST_EXPR_FLAG_IS_LVALUE)
          expr->flags |= AST_EXPR_FLAG_IS_LVALUE;

        return result;
      }
    case Ast_Expr_Call:
      {
        AstExprCall *Call = &expr->as.Call;

        AstExpr *lhs_type = typecheck_expr(ast, NULL, Call->lhs);

        if (lhs_type->as.Type.tag != Ast_Expr_Type_Proc)
          {
            PRINT_ERROR0(ast->filepath, expr->line_info, "'");
            eprint_type(lhs_type);
            EPRINT0("' is not callable\n");
            EXIT_ERROR();
          }

        AstExprTypeProc *Proc = &lhs_type->as.Type.as.Proc;

        if (Proc->params.count != Call->args.count)
          {
            PRINT_ERROR_LN(ast->filepath, expr->line_info, "expected %zu arguments, but got %zu", Proc->params.count, Call->args.count);
            EXIT_ERROR();
          }

        LinkedListNode *param_node = Proc->params.first;
        LinkedListNode *arg_node = Call->args.first;
        while (param_node)
          {
            AstSymbolParameter *Parameter = &LINKED_LIST_GET_NODE_DATA(AstSymbol *, param_node)->as.Parameter;
            AstExpr *arg_expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, arg_node);
            AstExpr *arg_type = typecheck_expr(ast, Parameter->type, arg_expr);

            if (!are_types_equal(Parameter->type, arg_type))
              {
                PRINT_ERROR0(ast->filepath, expr->line_info, "expected '");
                eprint_type(Parameter->type);
                EPRINT0("', but got '");
                eprint_type(arg_type);
                EPRINT0("'\n");
                EXIT_ERROR();
              }

            param_node = param_node->next;
            arg_node = arg_node->next;
          }

        return Proc->return_type;
      }
    case Ast_Expr_Type_Cons:
      {
        AstExprCall *Type_Cons = &expr->as.Type_Cons;

        if (!Type_Cons->lhs)
          {
            if (type_hint)
              Type_Cons->lhs = type_hint; // The line info is (probably) not set properly.
            else
              {
                PRINT_ERROR0_LN(ast->filepath, expr->line_info, "can't infere the type of type constructor");
                EXIT_ERROR();
              }
          }

        AstExpr *Lhs_Type = Type_Cons->lhs;
        assert(Lhs_Type->tag == Ast_Expr_Type);
        typecheck_type(ast, Lhs_Type);

        switch (Lhs_Type->as.Type.tag)
          {
          case Ast_Expr_Type_Void:
            PRINT_ERROR0_LN(ast->filepath, expr->line_info, "can't construct value of type 'void'");
            EXIT_ERROR();
          case Ast_Expr_Type_Bool:
          case Ast_Expr_Type_Int:
          case Ast_Expr_Type_Pointer:
          case Ast_Expr_Type_Proc:
          case Ast_Expr_Type_Enum:
            {
              if (Type_Cons->args.count != 1)
                {
                  PRINT_ERROR_LN(ast->filepath, expr->line_info, "expected 1 argument, but got %zu", Type_Cons->args.count);
                  EXIT_ERROR();
                }

              AstExpr *expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, Type_Cons->args.first);
              AstExpr *expr_type = typecheck_expr(ast, Lhs_Type, expr);

              if (!are_types_equal(Lhs_Type, expr_type))
                {
                  PRINT_ERROR0(ast->filepath, expr->line_info, "expected '");
                  eprint_type(Lhs_Type);
                  EPRINT0("', but got '");
                  eprint_type(expr_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }

              return Lhs_Type;
            }
          case Ast_Expr_Type_Array:
            {
              AstExpr *subexpr_type = Lhs_Type->as.Type.as.Array.lhs;

              LinkedListNode *arg_node = Type_Cons->args.first;
              while (arg_node)
                {
                  AstExpr *arg_expr = LINKED_LIST_GET_NODE_DATA(AstExpr *, arg_node);
                  AstExpr *arg_type = typecheck_expr(ast, subexpr_type, arg_expr);

                  if (!are_types_equal(subexpr_type, arg_type))
                    {
                      PRINT_ERROR0(ast->filepath, arg_expr->line_info, "expected '");
                      eprint_type(subexpr_type);
                      EPRINT0("', but got '");
                      eprint_type(arg_type);
                      EPRINT0("'\n");
                      EXIT_ERROR();
                    }

                  arg_node = arg_node->next;
                }

              return Lhs_Type;
            }
          case Ast_Expr_Type_Struct:
            typecheck_struct(ast, &Lhs_Type->as.Type.as.Struct, &Type_Cons->args, expr->line_info);
            return Lhs_Type;
          case Ast_Expr_Type_Union:
            typecheck_struct(ast, &Lhs_Type->as.Type.as.Union, &Type_Cons->args, expr->line_info);
            return Lhs_Type;
          case Ast_Expr_Type_Generic_Int:
            UNREACHABLE();
          }
      }

      UNREACHABLE();
    case Ast_Expr_Field_Access:
      {
        AstExprFieldAccess *Field_Access = &expr->as.Field_Access;

        if (Field_Access->lhs->tag == Ast_Expr_Type)
          {
            AstExpr *lhs_type = Field_Access->lhs;
            typecheck_type(ast, lhs_type);
            Scope *scope = NULL;

            switch (lhs_type->as.Type.tag)
              {
              case Ast_Expr_Type_Enum:
                scope = lhs_type->as.Type.as.Enum.scope;
                break;
              default:
                PRINT_ERROR0(ast->filepath, expr->line_info, "expected 'enum', but got '");
                eprint_type(lhs_type);
                EPRINT0("'\n");
                EXIT_ERROR();
              }

            AstSymbol *symbol = find_symbol(ast, Field_Access->name, scope, expr->line_info);
            assert(symbol->tag == Ast_Symbol_Enum_Value);

            // This needs to be done everytime we resolve expression to enum value. Could just check in the end if this expr was modified to enum value and do this.
            if (ast->flags & AST_FLAG_IS_TYPECHECKING_ENUM)
              typecheck_symbol(ast, symbol);

            expr->tag = Ast_Expr_Symbol;
            expr->as.Symbol = symbol;

            return lhs_type;
          }
        else
          {
            AstExpr *lhs_type = typecheck_expr(ast, NULL, Field_Access->lhs);
            Scope *scope = NULL;

            switch (lhs_type->as.Type.tag)
              {
              case Ast_Expr_Type_Pointer:
                {
                  AstExpr *subtype = lhs_type->as.Type.as.Pointer;
                  assert(subtype->tag == Ast_Expr_Type);

                  switch (subtype->as.Type.tag)
                    {
                    case Ast_Expr_Type_Struct:
                      scope = subtype->as.Type.as.Struct.scope;
                      break;
                    case Ast_Expr_Type_Union:
                      scope = subtype->as.Type.as.Union.scope;
                      break;
                    default:
                      PRINT_ERROR0(ast->filepath, expr->line_info, "expected pointer to 'struct'/'union', but got '");
                      eprint_type(lhs_type);
                      EPRINT0("'\n");
                      EXIT_ERROR();
                    }
                }

                break;
              case Ast_Expr_Type_Struct:
                scope = lhs_type->as.Type.as.Struct.scope;
                break;
              case Ast_Expr_Type_Union:
                scope = lhs_type->as.Type.as.Union.scope;
                break;
              default:
                PRINT_ERROR0(ast->filepath, expr->line_info, "expected 'struct'/'union' or pointer to 'struct'/'union', but got '");
                eprint_type(lhs_type);
                EPRINT0("'\n");
                EXIT_ERROR();
              }

            AstSymbol *symbol = find_symbol(ast, Field_Access->name, scope, expr->line_info);
            assert(symbol->tag == Ast_Symbol_Struct_Field);

            if (Field_Access->lhs->flags & AST_EXPR_FLAG_IS_LVALUE)
              expr->flags |= AST_EXPR_FLAG_IS_LVALUE;

            return symbol->as.Struct_Field.type;
          }
      }
    case Ast_Expr_Cast1:
      {
        if (!type_hint)
          {
            PRINT_ERROR0_LN(ast->filepath, expr->line_info, "can't deduce the type of expression");
            EXIT_ERROR();
          }

        AstExpr *expr_type = typecheck_expr(ast, NULL, expr->as.Cast1);

        // Need to add a new node to reflect the conversion being made.
        if (!can_cast_to_type(type_hint, expr_type))
          {
            PRINT_ERROR0(ast->filepath, expr->line_info, "can't cast '");
            eprint_type(type_hint);
            EPRINT0("' to '");
            eprint_type(expr_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }

        AstExpr *tmp = expr->as.Cast1;
        expr->tag = Ast_Expr_Cast2;
        expr->as.Cast2 = (AstExprCast2){
          .type = type_hint,
          .expr = tmp,
        };

        return type_hint;
      }
    case Ast_Expr_Cast2:
      {
        AstExprCast2 *Cast2 = &expr->as.Cast2;

        typecheck_type(ast, Cast2->type);
        AstExpr *expr_type = typecheck_expr(ast, NULL, Cast2->expr);

        // Add new node here as well (just like for 'Cast1').
        if (!can_cast_to_type(Cast2->type, expr_type))
          {
            PRINT_ERROR0(ast->filepath, expr->line_info, "can't cast '");
            eprint_type(type_hint);
            EPRINT0("' to '");
            eprint_type(expr_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }

        return Cast2->type;
      }
    case Ast_Expr_Type:
      PRINT_ERROR0_LN(ast->filepath, expr->line_info, "unexpected type here");
      EXIT_ERROR();
    case Ast_Expr_Int64:
      return &g_generic_int_type;
    case Ast_Expr_Bool:
      return &g_bool_type;
    case Ast_Expr_Null:
      return &g_null_type;
    case Ast_Expr_Designator:
      PRINT_ERROR0_LN(ast->filepath, expr->line_info, "unexpected designator");
      EXIT_ERROR();
    case Ast_Expr_Symbol:
      {
        AstSymbol *symbol = expr->as.Symbol;

        switch (symbol->tag)
          {
          case Ast_Symbol_Variable:
            {
              if (!(symbol->flags & AST_SYMBOL_FLAG_VARIABLE_IS_TYPECHECKED))
                {
                  PRINT_ERROR0_LN(ast->filepath, expr->line_info, "variable is used in its own definition");
                  EXIT_ERROR();
                }

              AstExpr *type = symbol->as.Variable.type;
              TypeFlagsType type_flags = compare_type(type);

              if (!(type_flags & TYPE_HAS_NO_BITS))
                expr->flags |= AST_EXPR_FLAG_IS_LVALUE;

              return type;
            }
          case Ast_Symbol_Parameter:
            {
              AstExpr *type = symbol->as.Parameter.type;
              TypeFlagsType type_flags = compare_type(type);

              if (!(type_flags & TYPE_HAS_NO_BITS))
                expr->flags |= AST_EXPR_FLAG_IS_LVALUE;

              return type;
            }
          case Ast_Symbol_Procedure:
            typecheck_symbol(ast, symbol);
            return symbol->as.Procedure.type;
          case Ast_Symbol_Type:
          case Ast_Symbol_Struct_Field:
          case Ast_Symbol_Enum_Value:
          case Ast_Symbol_Alias:
            UNREACHABLE();
          }
      }

      UNREACHABLE();
    case Ast_Expr_Enum_Identifier:
      {
        if (!type_hint)
          {
            PRINT_ERROR0_LN(ast->filepath, expr->line_info, "can't infere the type of enum identifier");
            EXIT_ERROR();
          }

        AstExprIdentifier *Enum_Identifier = &expr->as.Enum_Identifier;

        switch (type_hint->as.Type.tag)
          {
          case Ast_Expr_Type_Enum:
            {
              AstSymbolEnum *Enum = &type_hint->as.Type.as.Enum;

              AstSymbol *symbol = find_symbol(ast, Enum_Identifier->name, Enum->scope, expr->line_info);
              assert(symbol->tag == Ast_Symbol_Enum_Value);

              if (ast->flags & AST_FLAG_IS_TYPECHECKING_ENUM)
                typecheck_symbol(ast, symbol);

              expr->tag = Ast_Expr_Symbol;
              expr->as.Symbol = symbol;

              return type_hint;
            }
          default:
            PRINT_ERROR0(ast->filepath, expr->line_info, "expected 'enum' type, but got '");
            eprint_type(type_hint);
            EPRINT0("'\n");
            EXIT_ERROR();
          }
      }
    case Ast_Expr_Identifier:
      UNREACHABLE();
    }

  UNREACHABLE();
}

void
typecheck_symbol(Ast *ast, AstSymbol *symbol)
{
  switch (symbol->typechecking_stage)
    {
    case Ast_Stage_Not_Typechecked:
      symbol->typechecking_stage = Ast_Stage_Being_Typechecked;
      break;
    case Ast_Stage_Being_Typechecked:
      if (symbol->tag == Ast_Symbol_Procedure)
        return;

      PRINT_ERROR0_LN(ast->filepath, symbol->line_info, "detected cyclic reference");
      EXIT_ERROR();
    case Ast_Stage_Is_Typechecked:
      return;
    }

  switch (symbol->tag)
    {
    case Ast_Symbol_Variable:
      {
        AstSymbolVariable *Variable = &symbol->as.Variable;

        switch (((Variable->expr != NULL) << 1) | (Variable->type != NULL))
          {
          case 1: // 0b01
            typecheck_type(ast, Variable->type);
            break;
          case 2: // 0b10
            {
              AstExpr *expr_type = typecheck_expr(ast, NULL, Variable->expr);
              Variable->type = expr_type;
            }

            break;
          case 3: // 0b11
            {
              typecheck_type(ast, Variable->type);
              AstExpr *expr_type = typecheck_expr(ast, Variable->type, Variable->expr);

              if (!are_types_equal(Variable->type, expr_type))
                {
                  PRINT_ERROR0(ast->filepath, Variable->expr->line_info, "expected '");
                  eprint_type(Variable->type);
                  EPRINT0("', but got '");
                  eprint_type(expr_type);
                  EPRINT0("'\n");
                  EXIT_ERROR();
                }
            }

            break;
          default: UNREACHABLE();
          }

        symbol->flags |= AST_SYMBOL_FLAG_VARIABLE_IS_TYPECHECKED;
      }

      break;
    case Ast_Symbol_Parameter:
      {
        AstSymbolParameter *Parameter = &symbol->as.Parameter;

        typecheck_type(ast, Parameter->type);
      }

      break;
    case Ast_Symbol_Procedure:
      {
        AstSymbolProcedure *Procedure = &symbol->as.Procedure;

        AstExprTypeProc *Proc = &Procedure->type->as.Type.as.Proc;
        typecheck_type_proc(ast, Proc);

        AstExpr *old_return_type = ast->return_type;
        ast->return_type = Proc->return_type;

        typecheck_stmt_block(ast, &Procedure->block);

        ast->return_type = old_return_type;
      }

      break;
    case Ast_Symbol_Type:
      typecheck_type(ast, symbol->as.Type);
      break;
		case Ast_Symbol_Enum_Value:
      {
        AstSymbolEnumValue *Enum_Value = &symbol->as.Enum_Value;

        if (Enum_Value->depends_on && Enum_Value->depends_on->typechecking_stage == Ast_Stage_Being_Typechecked)
          {
            PRINT_ERROR0_LN(ast->filepath, symbol->line_info, "detected cyclic reference");
            EXIT_ERROR();
          }

        if (Enum_Value->expr)
          {
            AstExpr *expr_type = typecheck_expr(ast, Enum_Value->type, Enum_Value->expr);

            if (!are_types_equal(Enum_Value->type, expr_type))
              {
                PRINT_ERROR0(ast->filepath, Enum_Value->expr->line_info, "expected '");
                eprint_type(Enum_Value->type);
                EPRINT0("', but got '");
                eprint_type(expr_type);
                EPRINT0("'\n");
                EXIT_ERROR();
              }
          }
      }

      break;
    case Ast_Symbol_Struct_Field:
    case Ast_Symbol_Alias:
      UNREACHABLE();
    }

  symbol->typechecking_stage = Ast_Stage_Is_Typechecked;
}

void
typecheck_stmt(Ast *ast, AstStmt *stmt)
{
  switch (stmt->tag)
    {
    case Ast_Stmt_Block:
      typecheck_stmt_block(ast, &stmt->as.Block);
      break;
    case Ast_Stmt_If:
      {
        AstStmtIf *If = &stmt->as.If;

        AstExpr *cond_type = typecheck_expr(ast, &g_bool_type, If->cond);

        if (!are_types_equal(cond_type, &g_bool_type))
          {
            PRINT_ERROR0(ast->filepath, stmt->line_info, "expected 'bool', but got '");
            eprint_type(cond_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }

        typecheck_stmt(ast, If->if_true);
        if (If->if_false)
          typecheck_stmt(ast, If->if_false);
      }

      break;
    case Ast_Stmt_While:
      {
        AstStmtWhile *While = &stmt->as.While;

        AstExpr *cond_type = typecheck_expr(ast, &g_bool_type, While->cond);

        if (!are_types_equal(cond_type, &g_bool_type))
          {
            PRINT_ERROR0(ast->filepath, stmt->line_info, "expected 'bool', but got '");
            eprint_type(cond_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }

        AstFlagsType old_flags = ast->flags;
        ast->flags |= AST_FLAG_IS_IN_LOOP;

        typecheck_stmt(ast, While->block);

        ast->flags = old_flags;
      }

      break;
    case Ast_Stmt_Break:
      if (!(ast->flags & AST_FLAG_IS_IN_LOOP))
        {
          PRINT_ERROR0_LN(ast->filepath, stmt->line_info, "'break' outside of a loop");
          EXIT_ERROR();
        }

      break;
    case Ast_Stmt_Continue:
      if (!(ast->flags & AST_FLAG_IS_IN_LOOP))
        {
          PRINT_ERROR0_LN(ast->filepath, stmt->line_info, "'continue' outside of a loop");
          EXIT_ERROR();
        }

      break;
    case Ast_Stmt_Return_Nothing:
      break;
    case Ast_Stmt_Return_Expr:
      {
        AstExpr *Expr = stmt->as.Return_Expr;

        AstExpr *expr_type = typecheck_expr(ast, ast->return_type, Expr);

        if (are_types_equal(ast->return_type, &g_void_type))
          {
            PRINT_ERROR0_LN(ast->filepath, Expr->line_info, "unexpected expression when function returns 'void'");
            EXIT_ERROR();
          }
        else if (!are_types_equal(expr_type, ast->return_type))
          {
            PRINT_ERROR0(ast->filepath, Expr->line_info, "expected '");
            eprint_type(ast->return_type);
            EPRINT0("', but got '");
            eprint_type(expr_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }
      }

      break;
    case Ast_Stmt_Switch:
      {
        AstStmtSwitch *Switch = &stmt->as.Switch;

        AstExpr *cond_type = typecheck_expr(ast, NULL, Switch->cond);
        AstStmt *default_case = NULL;

        for (LinkedListNode *node = Switch->cases.first; node; node = node->next)
          {
            AstStmt *substmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);

            switch (substmt->tag)
              {
              case Ast_Stmt_Case:
              case Ast_Stmt_Default:
                break;
              default:
                PRINT_ERROR0_LN(ast->filepath, substmt->line_info, "expected 'case' or 'default' statement");
                EXIT_ERROR();
              }

            do
              {
                switch (substmt->tag)
                  {
                  case Ast_Stmt_Case:
                    {
                      AstStmtCase *Case = &substmt->as.Case;

                      AstExpr *case_type = typecheck_expr(ast, cond_type, Case->expr);

                      if (!are_types_equal(cond_type, case_type))
                        {
                          PRINT_ERROR0(ast->filepath, Case->expr->line_info, "expected '");
                          eprint_type(cond_type);
                          EPRINT0("', but got '");
                          eprint_type(case_type);
                          EPRINT0("'\n");
                          EXIT_ERROR();
                        }

                      substmt = Case->substmt;
                    }

                    break;
                  case Ast_Stmt_Default:
                    {
                      AstStmt *Default = substmt->as.Default;

                      if (default_case)
                        {
                          PRINT_ERROR0_LN(ast->filepath, substmt->line_info, "default statement was already defined");
                          PRINT_NOTE0_LN(ast->filepath, default_case->line_info, "first defined here");
                          EXIT_ERROR();
                        }

                      default_case = Default;
                      linked_list_remove_node(&Switch->cases, node);

                      substmt = Default;
                    }

                    break;
                  default:
                    goto finish_going_through_cases;
                  }
              }
            while (true);
          finish_going_through_cases:

            if (default_case)
              default_case = substmt;

            typecheck_stmt(ast, substmt);
          }

        Switch->default_case = default_case;
      }

      break;
    case Ast_Stmt_Case:
      PRINT_ERROR0_LN(ast->filepath, stmt->line_info, "'case' statement can't be here");
      EXIT_ERROR();
      break;
    case Ast_Stmt_Default:
      PRINT_ERROR0_LN(ast->filepath, stmt->line_info, "'default' statement can't be here");
      EXIT_ERROR();
      break;
    case Ast_Stmt_Assign:
      {
        AstStmtAssign *Assign = &stmt->as.Assign;

        AstExpr *lhs_type = typecheck_expr(ast, NULL, Assign->lhs);
        AstExpr *rhs_type = typecheck_expr(ast, lhs_type, Assign->rhs);

        if (!are_types_equal(lhs_type, rhs_type))
          {
            PRINT_ERROR0(ast->filepath, Assign->rhs->line_info, "expected '");
            eprint_type(lhs_type);
            EPRINT0("', but got '");
            eprint_type(rhs_type);
            EPRINT0("'\n");
            EXIT_ERROR();
          }
      }

      break;
    case Ast_Stmt_Symbol:
      typecheck_symbol(ast, stmt->as.Symbol);
      break;
    case Ast_Stmt_Expr:
      {
        AstExpr *Expr = stmt->as.Expr;

        typecheck_expr(ast, NULL, Expr);

        if (Expr->tag != Ast_Expr_Call)
          {
            PRINT_ERROR0_LN(ast->filepath, Expr->line_info, "can only call function here");
            EXIT_ERROR();
          }
      }

      break;
    }
}

void
typecheck_stmt_block(Ast *ast, AstStmtBlock *block)
{
  for (LinkedListNode *node = block->first; node; node = node->next)
    {
      AstStmt *stmt = &LINKED_LIST_GET_NODE_DATA(AstStmt, node);
      typecheck_stmt(ast, stmt);
    }
}

void
typecheck(Ast *ast)
{
  for (LinkedListNode *node = ast->globals.first; node; node = node->next)
    {
      AstSymbol *symbol = LINKED_LIST_GET_NODE_DATA(AstSymbol *, node);
      typecheck_symbol(ast, symbol);
    }
}
