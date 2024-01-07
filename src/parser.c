typedef struct Parser Parser;
struct Parser
{
  Lexer lexer;
  Arena arena;
};

#define LOWEST_PREC (INT_MIN + 1)

void *
parser_malloc(Parser *p, size_t size)
{
  void *data = arena_malloc(&p->arena, size);
  if (data == NULL)
    abort();
  return data;
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
to_unary_op_tag(TokenTag op)
{
  switch (op)
    {
    case Token_Sub: return Ast_Expr_Unary_Op_Neg;
    case Token_Not: return Ast_Expr_Unary_Op_Not;
    default:        assert(false);
    }
}

AstExpr *parse_expr(Parser *);

AstExpr *
parse_highest_prec(Parser *p)
{
  Token token = grab_token(&p->lexer);
  advance_token(&p->lexer);

  if (token.tag == Token_Open_Paren)
    {
      AstExpr *expr = parse_expr(p);
      expect_token(&p->lexer, Token_Close_Paren);
      return expr;
    }

  AstExpr *expr = parser_malloc(p, sizeof(AstExpr));
  expr->line_info = token.line_info;

  switch (token.tag)
    {
    case Token_Sub:
    case Token_Not:
      {
        AstExpr *subexpr = parse_highest_prec(p);
        expr->tag = Ast_Expr_Unary_Op;
        expr->as.Unary_Op = (AstExprUnaryOp){
          .tag = to_unary_op_tag(token.tag),
          .subexpr = subexpr,
        };
        return expr;
      }
    case Token_False:
    case Token_True:
      {
        expr->tag = Ast_Expr_Bool;
        expr->as.Bool = token.tag == Token_True;
        return expr;
      }
    case Token_Integer:
      {
        u64 value = 0;

        StringView text = token.text;
        while (text.count-- > 0)
          value = 10 * value + (*text.data++ - '0');

        expr->tag = Ast_Expr_Int64;
        expr->as.Int64 = value;

        return expr;
      }
    case Token_Identifier:
      {
        expr->tag = Ast_Expr_Identifier;
        expr->as.Identifier = token.text;
        return expr;
      }
    default:
      {
        PRINT_ERROR(p->lexer.filepath, token.line_info, "'%.*s' doesn't look like an expression", FORMAT_STRING_VIEW(token.text));
        exit(EXIT_FAILURE);
      }
    }
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
          AstExpr *new_lhs = parser_malloc(p, sizeof(AstExpr));
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
  };

  LinkedList exprs = { 0 };

  while (peek_token(&parser.lexer) != Token_End_Of_File)
    {
      AstExpr *expr = parse_expr(&parser);
      LinkedListNode *node = parser_malloc(&parser, sizeof(LinkedListNode));
      node->data = expr;
      linked_list_insert_last(&exprs, node);
      expect_token(&parser.lexer, Token_Semicolon);
    }

  Ast ast = {
    .exprs = exprs,
    .arena = parser.arena,
  };

  return ast;
}
