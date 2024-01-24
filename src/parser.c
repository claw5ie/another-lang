#define MAX_EXPR_COUNT 2
#define MAX_SCOPE_COUNT 256

typedef struct Parser Parser;
struct Parser
{
  Lexer lexer;
  Arena arena;
  HashTable symbols;

  Scope *current_scope;
  Scope *global_scope;

  AstExpr *exprs[MAX_EXPR_COUNT];
};

// Helps keep in sync 'parse_highest_prec_base' with 'parse_highest_prec'.
enum ExprStartTag
  {
    Expr_Start_None,
    Expr_Start_Double_Reference,
    Expr_Start_Unary_Minus,
    Expr_Start_Unary_Not,
    Expr_Start_Unary_Reference,
    Expr_Start_Parenthesized,
    Expr_Start_Procedure_Type,
    Expr_Start_Unnamed_Struct_Type,
    Expr_Start_Unnamed_Union_Type,
    Expr_Start_Unnamed_Enum_Type,
    Expr_Start_Cast,
    Expr_Start_Void_Type,
    Expr_Start_Bool_Type,
    Expr_Start_Int_Type,
    Expr_Start_False,
    Expr_Start_True,
    Expr_Start_Null,
    Expr_Start_Integer,
    Expr_Start_Identifier,
  };
typedef enum ExprStartTag ExprStartTag;

#define LOWEST_PREC (INT_MIN + 1)

size_t
symbol_hash(void *key)
{
  AstSymbolKey *k = key;
  size_t hash0 = murmur2_hash(k->name.data, k->name.count);
  size_t hash1 = murmur2_hash(&k->scope, sizeof(k->scope));
  return hash0 + 33 * hash1;
}

bool
are_symbols_equal(void *key0, void *key1)
{
  AstSymbolKey *k0 = key0;
  AstSymbolKey *k1 = key1;
  return k0->scope == k1->scope && are_views_equal(k0->name, k1->name);
}

void *
parser_malloc(Parser *p, size_t size)
{
  void *data = arena_malloc(&p->arena, size);
  if (!data)
    abort();
  return data;
}

void
push_scope(Parser *p)
{
  Scope *scope = parser_malloc(p, sizeof(*scope));
  scope->parent = p->current_scope;
  p->current_scope = scope;
}

void
pop_scope(Parser *p)
{
  p->current_scope = p->current_scope->parent;
}

AstSymbol *
insert_symbol(Parser *p, Token *id_token)
{
  AstSymbolKey key = {
    .name = id_token->text,
    .scope = p->current_scope,
  };
  bool was_inserted = false;
  AstSymbol *symbol = hash_table_insert(&p->symbols, &key, &was_inserted);

  if (!was_inserted)
    {
      PRINT_ERROR(p->lexer.filepath, id_token->line_info, "symbol '%.*s' is already defined", FORMAT_STRING_VIEW(symbol->name));
      PRINT_NOTE0(p->lexer.filepath, symbol->line_info, "first defined here");
      exit(EXIT_FAILURE);
    }

  return symbol;
}

int
prec_of_op(TokenTag op)
{
  switch (op)
    {
    case Token_Or:  return 0;
    case Token_And: return 1;
    case Token_Eq:
    case Token_Neq:
    case Token_Leq:
    case Token_Geq:
    case Token_Lt:
    case Token_Gt:  return 2;
    case Token_Add:
    case Token_Sub: return 3;
    case Token_Mul:
    case Token_Div:
    case Token_Mod: return 4;
    default:        return LOWEST_PREC - 1;
    }
}

AstExprBinaryOpTag
to_binary_op_tag(TokenTag op)
{
  switch (op)
    {
    case Token_Or:  return Ast_Expr_Binary_Op_Or;
    case Token_And: return Ast_Expr_Binary_Op_And;
    case Token_Eq:  return Ast_Expr_Binary_Op_Eq;
    case Token_Neq: return Ast_Expr_Binary_Op_Neq;
    case Token_Leq: return Ast_Expr_Binary_Op_Leq;
    case Token_Geq: return Ast_Expr_Binary_Op_Geq;
    case Token_Lt:  return Ast_Expr_Binary_Op_Lt;
    case Token_Gt:  return Ast_Expr_Binary_Op_Gt;
    case Token_Add: return Ast_Expr_Binary_Op_Add;
    case Token_Sub: return Ast_Expr_Binary_Op_Sub;
    case Token_Mul: return Ast_Expr_Binary_Op_Mul;
    case Token_Div: return Ast_Expr_Binary_Op_Div;
    case Token_Mod: return Ast_Expr_Binary_Op_Mod;
    default:        assert(false);
    }
}

AstExprUnaryOpTag
to_unary_op_tag(ExprStartTag op)
{
  switch (op)
    {
    case Expr_Start_Unary_Minus:     return Ast_Expr_Unary_Op_Neg;
    case Expr_Start_Unary_Not:       return Ast_Expr_Unary_Op_Not;
    case Expr_Start_Unary_Reference: return Ast_Expr_Unary_Op_Ref;
    default:                         assert(false);
    }
}

AstExpr *parse_expr(Parser *);

AstExpr *
parse_type(Parser *p)
{
  return parse_expr(p);
}

LinkedList
parse_comma_separated_exprs(Parser *p, TokenTag start_list, TokenTag end_list)
{
  expect_token(&p->lexer, start_list);

  LinkedList exprs = { 0 };

  TokenTag tt = peek_token(&p->lexer);
  while (tt != Token_End_Of_File && tt != end_list)
    {
      AstExpr *expr = NULL;

      if (tt == Token_Identifier && peek_ahead_token(&p->lexer, 1) == Token_Equal)
        {
          Token token = grab_token(&p->lexer);
          advance_many_tokens(&p->lexer, 2);
          AstExpr *initializer = parse_expr(p);
          expr = parser_malloc(p, sizeof(*expr));
          *expr = (AstExpr){
            .tag = Ast_Expr_Designator,
            .as = { .Designator = {
                .name = token.text,
                .expr = initializer,
              } },
            .line_info = token.line_info,
          };
        }
      else
        expr = parse_expr(p);

      LinkedListNode *node = parser_malloc(p, sizeof(*node) + sizeof(expr));
      LINKED_LIST_PUT_NODE_DATA(AstExpr *, node, expr);
      linked_list_insert_last(&exprs, node);

      tt = peek_token(&p->lexer);
      if (tt != Token_End_Of_File && tt != end_list)
        {
          expect_token(&p->lexer, Token_Comma);
          tt = peek_token(&p->lexer);
        }
    }

  expect_token(&p->lexer, end_list);

  return exprs;
}

void
parse_procedure_header(Parser *p, LinkedList *params, AstExpr **return_type, bool insert_params_into_table)
{
  expect_token(&p->lexer, Token_Open_Paren);

  TokenTag tt = peek_token(&p->lexer);
  while (tt != Token_End_Of_File && tt != Token_Close_Paren)
    {
      Token param_id_token = grab_token(&p->lexer);
      bool has_name = false;

      if (peek_token(&p->lexer) == Token_Identifier
          && peek_ahead_token(&p->lexer, 1) == Token_Double_Colon)
        {
          advance_many_tokens(&p->lexer, 2);
          has_name = true;
        }

      AstExpr *type = parse_type(p);
      AstSymbol *symbol = insert_params_into_table && has_name
        ? insert_symbol(p, &param_id_token)
        : parser_malloc(p, sizeof(*symbol));
      *symbol = (AstSymbol){
        .tag = Ast_Symbol_Parameter,
        .as = { .Parameter = {
            .type = type,
            .has_name = has_name,
          } },
        .name = param_id_token.text,
        .line_info = param_id_token.line_info,
      };

      LinkedListNode *node = parser_malloc(p, sizeof(*node) + sizeof(symbol));
      LINKED_LIST_PUT_NODE_DATA(AstSymbol *, node, symbol);
      linked_list_insert_last(params, node);

      tt = peek_token(&p->lexer);
      if (tt != Token_End_Of_File && tt != Token_Close_Paren)
        {
          expect_token(&p->lexer, Token_Comma);
          tt = peek_token(&p->lexer);
        }
    }

  expect_token(&p->lexer, Token_Close_Paren);

  if (peek_token(&p->lexer) == Token_Arrow)
    {
      advance_token(&p->lexer);
      *return_type = parse_type(p);
    }
  else
    {
      AstExpr *type = parser_malloc(p, sizeof(*type));
      *type = (AstExpr){
        .tag = Ast_Expr_Type,
        .as = { .Type = {
            .tag = Ast_Expr_Type_Void,
          } },
        .line_info = { 0 }, // TODO: put line info here.
      };
      *return_type = type;
    }
}

LinkedList
parse_struct_fields(Parser *p)
{
  expect_token(&p->lexer, Token_Open_Curly);

  LinkedList fields = { 0 };

  push_scope(p);

  // Should empty structs be allowed?
  TokenTag tt = peek_token(&p->lexer);
  while (tt != Token_End_Of_File && tt != Token_Close_Curly)
    {
      Token id_token = grab_token(&p->lexer);
      expect_token(&p->lexer, Token_Identifier);
      expect_token(&p->lexer, Token_Double_Colon);
      AstExpr *type = parse_type(p);

      AstSymbol *symbol = insert_symbol(p, &id_token);
      *symbol = (AstSymbol){
        .tag = Ast_Symbol_Struct_Field,
        .as = { .Struct_Field = {
            .type = type,
          } },
        .name = id_token.text,
        .line_info = id_token.line_info,
      };

      LinkedListNode *node = parser_malloc(p, sizeof(*node) + sizeof(symbol));
      LINKED_LIST_PUT_NODE_DATA(AstSymbol *, node, symbol);
      linked_list_insert_last(&fields, node);

      tt = peek_token(&p->lexer);
      if (tt != Token_End_Of_File && tt != Token_Close_Curly)
        {
          expect_token(&p->lexer, Token_Comma);
          tt = peek_token(&p->lexer);
        }
    }

  pop_scope(p);

  expect_token(&p->lexer, Token_Close_Curly);

  return fields;
}

LinkedList
parse_enum_values(Parser *p)
{
  expect_token(&p->lexer, Token_Open_Curly);

  LinkedList fields = { 0 };

  push_scope(p);

  TokenTag tt = peek_token(&p->lexer);
  do
    {
      Token id_token = grab_token(&p->lexer);
      expect_token(&p->lexer, Token_Identifier);

      AstSymbol *symbol = insert_symbol(p, &id_token);
      *symbol = (AstSymbol){
        .tag = Ast_Symbol_Enum_Value,
        .as = { .Enum_Value = {
            .dummy = 42,
          } },
        .name = id_token.text,
        .line_info = id_token.line_info,
      };

      LinkedListNode *node = parser_malloc(p, sizeof(*node) + sizeof(symbol));
      LINKED_LIST_PUT_NODE_DATA(AstSymbol *, node, symbol);
      linked_list_insert_last(&fields, node);

      tt = peek_token(&p->lexer);
      if (tt != Token_End_Of_File && tt != Token_Close_Curly)
        {
          expect_token(&p->lexer, Token_Comma);
          tt = peek_token(&p->lexer);
        }
    }
  while (tt != Token_End_Of_File && tt != Token_Close_Curly);

  pop_scope(p);

  expect_token(&p->lexer, Token_Close_Curly);

  return fields;
}

ExprStartTag
can_token_start_expression(TokenTag tag)
{
  switch (tag)
    {
    case Token_And:         return Expr_Start_Double_Reference;
    case Token_Sub:         return Expr_Start_Unary_Minus;
    case Token_Not:         return Expr_Start_Unary_Not;
    case Token_Ref:         return Expr_Start_Unary_Reference;
    case Token_Open_Paren:  return Expr_Start_Parenthesized;
    case Token_Proc:        return Expr_Start_Procedure_Type;
    case Token_Struct:      return Expr_Start_Unnamed_Struct_Type;
    case Token_Union:       return Expr_Start_Unnamed_Union_Type;
    case Token_Enum:        return Expr_Start_Unnamed_Enum_Type;
    case Token_Cast:        return Expr_Start_Cast;
    case Token_Void_Type:   return Expr_Start_Void_Type;
    case Token_Bool_Type:   return Expr_Start_Bool_Type;
    case Token_Int_Type:    return Expr_Start_Int_Type;
    case Token_False:       return Expr_Start_False;
    case Token_True:        return Expr_Start_True;
    case Token_Null:        return Expr_Start_Null;
    case Token_Integer:     return Expr_Start_Integer;
    case Token_Identifier:  return Expr_Start_Identifier;
    default:                return Expr_Start_None;
    }
}

size_t
parse_fixed_size_arg_list(Parser *p)
{
  expect_token(&p->lexer, Token_Open_Paren);

  size_t count = 0;
  TokenTag tt = peek_token(&p->lexer);
  for (; tt != Token_End_Of_File && tt != Token_Close_Paren; count++)
    {
      AstExpr *expr = parse_expr(p);

      if (count < MAX_EXPR_COUNT)
        p->exprs[count] = expr;

      tt = peek_token(&p->lexer);
      if (tt != Token_End_Of_File && tt != Token_Close_Paren)
        {
          expect_token(&p->lexer, Token_Comma);
          tt = peek_token(&p->lexer);
        }
    }

  expect_token(&p->lexer, Token_Close_Paren);

  return count;
}

AstExpr *
parse_highest_prec_base(Parser *p)
{
  Token token = grab_token(&p->lexer);
  advance_token(&p->lexer);

  ExprStartTag expr_start_tag = can_token_start_expression(token.tag);
  switch (expr_start_tag)
    {
    case Expr_Start_None:
      {
        PRINT_ERROR(p->lexer.filepath, token.line_info, "'%.*s' doesn't look like an expression", FORMAT_STRING_VIEW(token.text));
        exit(EXIT_FAILURE);
      }
    case Expr_Start_Double_Reference: // '&&expr' is not tokenized as '& & expr'.
      {
        AstExpr *subsubexpr = parse_highest_prec_base(p);
        AstExpr *subexpr = parser_malloc(p, sizeof(*subexpr));
        *subexpr = (AstExpr){
          .tag = Ast_Expr_Unary_Op,
          .as = { .Unary_Op = {
              .tag = Ast_Expr_Unary_Op_Ref,
              .subexpr = subsubexpr,
            } },
          .line_info = token.line_info,
        };
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Unary_Op,
          .as = { .Unary_Op = {
              .tag = Ast_Expr_Unary_Op_Ref,
              .subexpr = subexpr,
            } },
          .line_info = token.line_info,
        };

        return expr;
      }
    case Expr_Start_Unary_Minus:
    case Expr_Start_Unary_Not:
    case Expr_Start_Unary_Reference:
      {
        AstExpr *subexpr = parse_highest_prec_base(p);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Unary_Op,
          .as = { .Unary_Op = {
              .tag = to_unary_op_tag(expr_start_tag),
              .subexpr = subexpr,
            } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Parenthesized:
      {
        AstExpr *expr = parse_expr(p);
        expect_token(&p->lexer, Token_Close_Paren);
        return expr;
      }
    case Expr_Start_Procedure_Type:
      {
        LinkedList params = { 0 };
        AstExpr *return_type = NULL;
        parse_procedure_header(p, &params, &return_type, false);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Proc,
              .as = { .Proc = {
                  .params = params,
                  .return_type = return_type,
                } } } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Unnamed_Struct_Type:
      {
        LinkedList fields = parse_struct_fields(p);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Struct,
              .as = { .Struct = {
                  .fields = fields,
                } } } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Unnamed_Union_Type:
      {
        LinkedList fields = parse_struct_fields(p);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Union,
              .as = { .Union = {
                  .fields = fields,
                } } } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Unnamed_Enum_Type:
      {
        LinkedList values = parse_enum_values(p);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Enum,
              .as = { .Enum = {
                  .values = values,
                } } } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Cast:
      {
        size_t count = parse_fixed_size_arg_list(p);

        switch (count)
          {
          case 1:
            {
              AstExpr *expr = parser_malloc(p, sizeof(*expr));
              *expr = (AstExpr){
                .tag = Ast_Expr_Cast1,
                .as = { .Cast1 = p->exprs[0] },
                .line_info = token.line_info,
              };
              return expr;
            }
          case 2:
            {
              AstExpr *expr = parser_malloc(p, sizeof(*expr));
              *expr = (AstExpr){
                .tag = Ast_Expr_Cast2,
                .as = { .Cast2 = {
                    .type = p->exprs[0],
                    .expr = p->exprs[1],
                  } },
                .line_info = token.line_info,
              };
              return expr;
            }
          default:
            {
              // Don't actually need to exit here? But need to return something.
              PRINT_ERROR(p->lexer.filepath, token.line_info, "expected 1 or 2 arguments, not %zu", count);
              exit(EXIT_FAILURE);
            }
          }
      }
    case Expr_Start_Void_Type:
      {
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Void,
            } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Bool_Type:
      {
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Bool,
            } },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Int_Type:
      {
        StringView text = token.text;
        ++text.data;
        --text.count;

        u16 bits = view_to_unsigned(text);
        bool is_signed = text.data[-1] == 'i';

        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Int,
              .as = { .Int = {
                  .bits = bits,
                  .is_signed = is_signed,
                } } } },
          .line_info = token.line_info,
        };

        return expr;
      }
    case Expr_Start_False:
    case Expr_Start_True:
      {
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Bool,
          .as = { .Bool = token.tag == Token_True },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Null:
      {
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Null,
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Integer:
      {
        u64 value = view_to_unsigned(token.text);
        AstExpr *expr = parser_malloc(p, sizeof(*expr));
        *expr = (AstExpr){
          .tag = Ast_Expr_Int64,
          .as = { .Int64 = value },
          .line_info = token.line_info,
        };
        return expr;
      }
    case Expr_Start_Identifier:
      {
        if (token.text.count == 1 && token.text.data[0] == '_' &&
            peek_token(&p->lexer) == Token_Open_Paren)
          {
            LinkedList expr_list = parse_comma_separated_exprs(p, Token_Open_Paren, Token_Close_Paren);
            AstExpr *expr = parser_malloc(p, sizeof(*expr));
            *expr = (AstExpr){
              .tag = Ast_Expr_Type_Cons,
              .as = { .Type_Cons = {
                  .lhs = NULL,
                  .args = expr_list,
                } },
              .line_info = token.line_info,
            };
            return expr;
          }
        else
          {
            AstExpr *expr = parser_malloc(p, sizeof(*expr));
            *expr = (AstExpr){
              .tag = Ast_Expr_Identifier,
              .as = { .Identifier = {
                  .name = token.text,
                  .scope = p->current_scope,
                } },
              .line_info = token.line_info,
            };
            return expr;
          }
      }
    }

  UNREACHABLE();
}

AstExpr *
parse_highest_prec(Parser *p)
{
  AstExpr *base = parse_highest_prec_base(p);

  do
    {
      switch (peek_token(&p->lexer))
        {
        case Token_Mul:
          {
            TokenTag next = peek_ahead_token(&p->lexer, 1);
            if (can_token_start_expression(next) != Expr_Start_None)
              goto finish_parsing_postfix_unary_operators;

            LineInfo line_info = grab_token(&p->lexer).line_info;
            advance_token(&p->lexer);

            AstExpr *new_base = parser_malloc(p, sizeof(*new_base));
            *new_base = (AstExpr){
              .tag = Ast_Expr_Unary_Op,
              .as = { .Unary_Op = {
                  .tag = Ast_Expr_Unary_Op_Deref,
                  .subexpr = base,
                } },
              .line_info = line_info,
            };
            base = new_base;
          }

          break;
        case Token_Open_Paren:
          {
            LineInfo line_info = grab_token(&p->lexer).line_info;
            LinkedList expr_list = parse_comma_separated_exprs(p, Token_Open_Paren, Token_Close_Paren);
            AstExpr *new_base = parser_malloc(p, sizeof(*new_base));
            *new_base = (AstExpr){
              .tag = Ast_Expr_Call,
              .as = { .Call = {
                  .lhs = base,
                  .args = expr_list,
                } },
              .line_info = line_info,
            };
            base = new_base;
          }

          break;
        case Token_Open_Bracket:
          {
            LineInfo line_info = grab_token(&p->lexer).line_info;
            advance_token(&p->lexer);
            AstExpr *expr = parse_expr(p);
            expect_token(&p->lexer, Token_Close_Bracket);

            AstExpr *new_base = parser_malloc(p, sizeof(*new_base));
            *new_base = (AstExpr){
              .tag = Ast_Expr_Array_Access,
              .as = { .Array_Access = {
                  .lhs = base,
                  .index = expr,
                } },
              .line_info = line_info,
            };
            base = new_base;
          }

          break;
        case Token_Dot:
          {
            advance_token(&p->lexer);

            Token token = grab_token(&p->lexer);
            expect_token(&p->lexer, Token_Identifier);

            AstExpr *new_base = parser_malloc(p, sizeof(*new_base));
            *new_base = (AstExpr){
              .tag = Ast_Expr_Field_Access,
              .as = { .Field_Access = {
                  .lhs = base,
                  .name = token.text,
                } },
              .line_info = token.line_info,
            };
            base = new_base;
          }

          break;
        default:
          goto finish_parsing_postfix_unary_operators;
        }
    }
  while (true);
 finish_parsing_postfix_unary_operators:

  return base;
}

AstExpr *
parse_prec(Parser *p, int min_prec)
{
  AstExpr *lhs = parse_highest_prec(p);
  TokenTag op = peek_token(&p->lexer);
  int curr_prec = prec_of_op(op);
  int prev_prec = INT_MAX;

  while (curr_prec < prev_prec && curr_prec >= min_prec)
    {
      do
        {
          advance_token(&p->lexer);

          AstExpr *rhs = parse_prec(p, curr_prec + 1);
          AstExpr *new_lhs = parser_malloc(p, sizeof(*new_lhs));
          *new_lhs = (AstExpr){
            .tag = Ast_Expr_Binary_Op,
            .as = { .Binary_Op = {
                .tag = to_binary_op_tag(op),
                .lhs = lhs,
                .rhs = rhs,
              } },
            .line_info = lhs->line_info,
          };
          lhs = new_lhs;

          op = peek_token(&p->lexer);
        }
      while (curr_prec == prec_of_op(op));

      prev_prec = curr_prec;
      curr_prec = prec_of_op(op);
    }

  return lhs;
}

AstExpr *
parse_expr(Parser *p)
{
  return parse_prec(p, LOWEST_PREC);
}

AstStmtBlock parse_stmt_block(Parser *);

AstSymbol *
parse_symbol(Parser *p)
{
  switch (peek_token(&p->lexer))
    {
    case Token_Proc:
      {
        advance_token(&p->lexer);

        Token id_token = grab_token(&p->lexer);
        expect_token(&p->lexer, Token_Identifier);

        push_scope(p);

        LinkedList params = { 0 };
        AstExpr *return_type = NULL;
        parse_procedure_header(p, &params, &return_type, true);
        AstStmtBlock block = parse_stmt_block(p);

        pop_scope(p);

        AstSymbol *symbol = insert_symbol(p, &id_token);
        *symbol = (AstSymbol){
          .tag = Ast_Symbol_Procedure,
          .as = { .Procedure = {
              .params = params,
              .return_type = return_type,
              .block = block,
            } },
          .name = id_token.text,
          .line_info = id_token.line_info,
        };

        return symbol;
      }
    case Token_Struct:
      {
        advance_token(&p->lexer);

        Token id_token = grab_token(&p->lexer);
        expect_token(&p->lexer, Token_Identifier);
        LinkedList fields = parse_struct_fields(p);

        AstExpr *type = parser_malloc(p, sizeof(*type));
        *type = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Struct,
              .as = { .Struct = {
                  .fields = fields,
                } },
            } },
          .line_info = id_token.line_info,
        };
        AstSymbol *symbol = insert_symbol(p, &id_token);
        *symbol = (AstSymbol){
          .tag = Ast_Symbol_Type,
          .as = { .Type = type },
          .name = id_token.text,
          .line_info = id_token.line_info,
        };

        return symbol;
      }
    case Token_Union:
      {
        advance_token(&p->lexer);

        Token id_token = grab_token(&p->lexer);
        expect_token(&p->lexer, Token_Identifier);
        LinkedList fields = parse_struct_fields(p);

        AstExpr *type = parser_malloc(p, sizeof(*type));
        *type = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Union,
              .as = { .Union = {
                  .fields = fields,
                } },
            } },
          .line_info = id_token.line_info,
        };
        AstSymbol *symbol = insert_symbol(p, &id_token);
        *symbol = (AstSymbol){
          .tag = Ast_Symbol_Type,
          .as = { .Type = type },
          .name = id_token.text,
          .line_info = id_token.line_info,
        };

        return symbol;
      }
    case Token_Enum:
      {
        advance_token(&p->lexer);

        Token id_token = grab_token(&p->lexer);
        expect_token(&p->lexer, Token_Identifier);
        LinkedList values = parse_enum_values(p);

        AstExpr *type = parser_malloc(p, sizeof(*type));
        *type = (AstExpr){
          .tag = Ast_Expr_Type,
          .as = { .Type = {
              .tag = Ast_Expr_Type_Enum,
              .as = { .Enum = {
                  .values = values,
                } },
            } },
          .line_info = id_token.line_info,
        };
        AstSymbol *symbol = insert_symbol(p, &id_token);
        *symbol = (AstSymbol){
          .tag = Ast_Symbol_Type,
          .as = { .Type = type },
          .name = id_token.text,
          .line_info = id_token.line_info,
        };

        return symbol;
      }
    case Token_Alias:
      {
        advance_token(&p->lexer);

        Token id_token = grab_token(&p->lexer);
        expect_token(&p->lexer, Token_Identifier);
        expect_token(&p->lexer, Token_Equal);
        AstExpr *type = parse_type(p);
        expect_token(&p->lexer, Token_Semicolon);

        AstSymbol *symbol = insert_symbol(p, &id_token);
        *symbol = (AstSymbol){
          .tag = Ast_Symbol_Alias,
          .as = { .Alias = {
              .type = type,
              .underlying_type = type,
            } },
          .name = id_token.text,
          .line_info = id_token.line_info,
        };

        return symbol;
      }
    case Token_Identifier:
      switch (peek_ahead_token(&p->lexer, 1))
        {
        case Token_Double_Colon:
          {
            Token id_token = grab_token(&p->lexer);
            advance_many_tokens(&p->lexer, 2);

            AstExpr *type = parse_type(p);
            AstExpr *expr = NULL;
            if (peek_token(&p->lexer) == Token_Equal)
              {
                advance_token(&p->lexer);
                expr = parse_expr(p);
              }
            AstSymbol *symbol = insert_symbol(p, &id_token);
            *symbol = (AstSymbol){
              .tag = Ast_Symbol_Variable,
              .as = { .Variable = {
                  .type = type,
                  .expr = expr,
                } },
              .name = id_token.text,
              .line_info = id_token.line_info,
            };

            expect_token(&p->lexer, Token_Semicolon);

            return symbol;
          }
        case Token_Double_Colon_Equal:
          {
            Token id_token = grab_token(&p->lexer);
            advance_many_tokens(&p->lexer, 2);

            AstExpr *expr = parse_expr(p);
            AstSymbol *symbol = insert_symbol(p, &id_token);
            *symbol = (AstSymbol){
              .tag = Ast_Symbol_Variable,
              .as = { .Variable = {
                  .type = NULL,
                  .expr = expr,
                } },
              .name = id_token.text,
              .line_info = id_token.line_info,
            };

            expect_token(&p->lexer, Token_Semicolon);

            return symbol;
          }
        default:
          return NULL;
        }
    default:
      return NULL;
    }
}

AstStmt parse_stmt_but_not_symbol(Parser *);

AstStmt
parse_stmt(Parser *p)
{
  LineInfo line_info = grab_token(&p->lexer).line_info;

  switch (peek_token(&p->lexer))
    {
    case Token_Open_Curly:
      {
        push_scope(p);

        AstStmtBlock block = parse_stmt_block(p);
        AstStmt stmt = {
          .tag = Ast_Stmt_Block,
          .as = { .Block = block },
          .line_info = line_info,
        };

        pop_scope(p);

        return stmt;
      }
    case Token_If:
      {
        advance_token(&p->lexer);

        AstExpr *expr = parse_expr(p);

        if (peek_token(&p->lexer) == Token_Then)
          advance_token(&p->lexer);

        AstStmt *if_true = parser_malloc(p, sizeof(*if_true));
        AstStmt *if_false = NULL;

        *if_true = parse_stmt_but_not_symbol(p);
        if (peek_token(&p->lexer) == Token_Else)
          {
            advance_token(&p->lexer);
            if_false = parser_malloc(p, sizeof(*if_false));
            *if_false = parse_stmt_but_not_symbol(p);
          }

        AstStmt stmt = {
          .tag = Ast_Stmt_If,
          .as = { .If = {
              .cond = expr,
              .if_true = if_true,
              .if_false = if_false,
            } },
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_While:
      {
        advance_token(&p->lexer);

        AstExpr *expr = parse_expr(p);
        if (peek_token(&p->lexer) == Token_Do)
          advance_token(&p->lexer);

        AstStmt *block = parser_malloc(p, sizeof(*block));
        *block = parse_stmt_but_not_symbol(p);

        AstStmt stmt = {
          .tag = Ast_Stmt_While,
          .as = { .While = {
              .cond = expr,
              .block = block,
              .is_do_while = false,
            } },
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Do:
      {
        advance_token(&p->lexer);

        AstStmt *block = parser_malloc(p, sizeof(*block));
        *block = parse_stmt_but_not_symbol(p);

        expect_token(&p->lexer, Token_While);
        AstExpr *expr = parse_expr(p);
        expect_token(&p->lexer, Token_Semicolon);

        AstStmt stmt = {
          .tag = Ast_Stmt_While,
          .as = { .While = {
              .cond = expr,
              .block = block,
              .is_do_while = true,
            } },
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Break:
      {
        advance_token(&p->lexer);
        expect_token(&p->lexer, Token_Semicolon);

        AstStmt stmt = {
          .tag = Ast_Stmt_Break,
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Continue:
      {
        advance_token(&p->lexer);
        expect_token(&p->lexer, Token_Semicolon);

        AstStmt stmt = {
          .tag = Ast_Stmt_Continue,
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Return:
      {
        advance_token(&p->lexer);
        if (peek_token(&p->lexer) != Token_Semicolon)
          {
            AstExpr *expr = parse_expr(p);
            expect_token(&p->lexer, Token_Semicolon);

            AstStmt stmt = {
              .tag = Ast_Stmt_Return_Expr,
              .as = { .Return_Expr = expr },
              .line_info = line_info,
            };

            return stmt;
          }

        expect_token(&p->lexer, Token_Semicolon);

        AstStmt stmt = {
          .tag = Ast_Stmt_Return_Nothing,
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Switch:
      {
        advance_token(&p->lexer);

        AstExpr *cond = parse_expr(p);

        push_scope(p);
        AstStmtBlock block = parse_stmt_block(p);
        pop_scope(p);

        AstStmt stmt = {
          .tag = Ast_Stmt_Switch,
          .as = { .Switch = {
              .cond = cond,
              .cases = block,
            } },
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Case:
      {
        advance_token(&p->lexer);
        AstExpr *expr = parse_expr(p);
        if (peek_token(&p->lexer) == Token_Then)
          advance_token(&p->lexer);

        AstStmt *substmt = parser_malloc(p, sizeof(*substmt));
        *substmt = parse_stmt_but_not_symbol(p);

        AstStmt stmt = {
          .tag = Ast_Stmt_Case,
          .as = { .Case = {
              .expr = expr,
              .substmt = substmt,
            } },
          .line_info = line_info,
        };

        return stmt;
      }
    case Token_Default:
      {
        advance_token(&p->lexer);
        AstStmt *substmt = parser_malloc(p, sizeof(*substmt));
        *substmt = parse_stmt_but_not_symbol(p);

        AstStmt stmt = {
          .tag = Ast_Stmt_Default,
          .as = { .Default = substmt },
          .line_info = line_info,
        };

        return stmt;
      }
    default:
      {
        AstSymbol *symbol = parse_symbol(p);

        if (symbol)
          {
            AstStmt stmt = {
              .tag = Ast_Stmt_Symbol,
              .as = { .Symbol = symbol },
              .line_info = line_info,
            };

            return stmt;
          }
        else
          {
            AstExpr *lhs = parse_expr(p);

            if (peek_token(&p->lexer) == Token_Equal)
              {
                advance_token(&p->lexer);
                AstExpr *rhs = parse_expr(p);
                expect_token(&p->lexer, Token_Semicolon);

                AstStmt stmt = {
                  .tag = Ast_Stmt_Assign,
                  .as = { .Assign = {
                      .lhs = lhs,
                      .rhs = rhs,
                    } },
                  .line_info = line_info,
                };

                return stmt;
              }

            expect_token(&p->lexer, Token_Semicolon);

            AstStmt stmt = {
              .tag = Ast_Stmt_Expr,
              .as = { .Expr = lhs },
              .line_info = line_info,
            };

            return stmt;
          }
      }
    }
}

AstStmtBlock
parse_stmt_block(Parser *p)
{
  expect_token(&p->lexer, Token_Open_Curly);

  AstStmtBlock block = { 0 };

  TokenTag tt = peek_token(&p->lexer);
  while (tt != Token_End_Of_File && tt != Token_Close_Curly)
    {
      AstStmt stmt = parse_stmt(p);
      LinkedListNode *node = parser_malloc(p, sizeof(*node) + sizeof(stmt));
      LINKED_LIST_PUT_NODE_DATA(AstStmt, node, stmt);
      linked_list_insert_last(&block, node);

      tt = peek_token(&p->lexer);
    }

  expect_token(&p->lexer, Token_Close_Curly);

  return block;
}

AstStmt
parse_stmt_but_not_symbol(Parser *p)
{
  AstStmt stmt = parse_stmt(p);
  if (stmt.tag == Ast_Stmt_Symbol)
    {
      PRINT_ERROR0(p->lexer.filepath, stmt.line_info, "can't define variables here");
      exit(EXIT_FAILURE);
    }
  return stmt;
}

Ast
parse(const char *filepath)
{
  size_t source_code_size = 0;
  char *source_code = read_entire_file(filepath, &source_code_size);

  Parser parser = {
    .lexer = {
      .token_start = 0,
      .token_count = 0,
      .line_info = { .line = 1, .column = 1, .offset = 0 },
      .source_code = source_code,
      .source_code_size = source_code_size,
      .filepath = filepath,
    },
    .arena = { 0 },
    .symbols = {
      .key_size = sizeof(AstSymbolKey),
      .data_size = sizeof(AstSymbol),
      .key_hash = symbol_hash,
      .are_keys_equal = are_symbols_equal,
    },
    .current_scope = NULL,
    .global_scope = NULL,
  };

  LinkedList globals = { 0 };

  Scope *global_scope = parser_malloc(&parser, sizeof(*global_scope));
  parser.current_scope = global_scope;
  parser.global_scope = global_scope;

  while (peek_token(&parser.lexer) != Token_End_Of_File)
    {
      AstSymbol *symbol = parse_symbol(&parser);
      if (!symbol)
        {
          Token token = grab_token(&parser.lexer);
          PRINT_ERROR0(parser.lexer.filepath, token.line_info, "expected symbol definition");
          exit(EXIT_FAILURE);
        }
      LinkedListNode *node = parser_malloc(&parser, sizeof(*node) + sizeof(symbol));
      LINKED_LIST_PUT_NODE_DATA(AstSymbol *, node, symbol);
      linked_list_insert_last(&globals, node);
    }

  assert(parser.current_scope == global_scope);

  Ast ast = {
    .globals = globals,
    .arena = parser.arena,
    .symbols = parser.symbols,
    .filepath = filepath,
  };

  return ast;
}
