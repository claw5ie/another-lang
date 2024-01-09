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
    case Token_Ref: return Ast_Expr_Unary_Op_Ref;
    default:        assert(false);
    }
}

AstExpr *parse_expr(Parser *);

// Cases here and in "parse_highest_prec_base" should match one to one. Don't forget to keep them in sync!!
bool
can_token_start_expression(TokenTag tag)
{
  switch (tag)
    {
    case Token_Open_Paren:
    case Token_And:
    case Token_Sub:
    case Token_Not:
    case Token_Ref:
    case Token_Void_Type:
    case Token_Bool_Type:
    case Token_Int_Type:
    case Token_False:
    case Token_True:
    case Token_Integer:
    case Token_Identifier: return true;
    default:               return false;
    }
}

AstExpr *
parse_highest_prec_base(Parser *p)
{
  Token token = grab_token(&p->lexer);
  advance_token(&p->lexer);

  if (token.tag == Token_Open_Paren)
    {
      AstExpr *expr = parse_expr(p);
      expect_token(&p->lexer, Token_Close_Paren);
      return expr;
    }

  AstExpr *expr = parser_malloc(p, sizeof(*expr));
  expr->line_info = token.line_info;

  switch (token.tag)
    {
    case Token_And: // Double reference (as in '&&expr').
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
        expr->tag = Ast_Expr_Unary_Op;
        expr->as.Unary_Op = (AstExprUnaryOp){
          .tag = Ast_Expr_Unary_Op_Ref,
          .subexpr = subexpr,
        };

        return expr;
      }
    case Token_Sub:
    case Token_Not:
    case Token_Ref:
      {
        AstExpr *subexpr = parse_highest_prec_base(p);
        expr->tag = Ast_Expr_Unary_Op;
        expr->as.Unary_Op = (AstExprUnaryOp){
          .tag = to_unary_op_tag(token.tag),
          .subexpr = subexpr,
        };
        return expr;
      }
    case Token_Void_Type:
      {
        expr->tag = Ast_Expr_Type_Void;
        return expr;
      }
    case Token_Bool_Type:
      {
        expr->tag = Ast_Expr_Type_Bool;
        return expr;
      }
    case Token_Int_Type:
      {
        u16 bits = 0;

        StringView text = token.text;
        for (size_t i = 1; i < text.count; i++)
          bits = 10 * bits + (text.data[i] - '0');

        expr->tag = Ast_Expr_Type_Int;
        expr->as.Type_Int = (AstExprTypeInt){
          .bits = bits,
          .is_signed = text.data[0] == 'i',
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
            if (can_token_start_expression(next))
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

AstType *
parse_type(Parser *p)
{
  return parse_expr(p);
}

AstSymbol *
parse_symbol(Parser *p)
{
  if (peek_token(&p->lexer) == Token_Identifier)
    {
      switch (peek_ahead_token(&p->lexer, 1))
        {
        case Token_Double_Colon:
          {
            Token id_token = grab_token(&p->lexer);
            advance_many_tokens(&p->lexer, 2);

            AstType *type = parse_type(p);
            AstExpr *expr = NULL;
            if (peek_token(&p->lexer) == Token_Equal)
              {
                advance_token(&p->lexer);
                expr = parse_expr(p);
              }
            AstSymbol *symbol = parser_malloc(p, sizeof(*symbol));
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
        case Token_Colon_Equal:
          {
            Token id_token = grab_token(&p->lexer);
            advance_many_tokens(&p->lexer, 2);

            AstExpr *expr = parse_expr(p);
            AstSymbol *symbol = parser_malloc(p, sizeof(*symbol));
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
    }

  return NULL;
}

AstStmtBlock parse_stmt_block(Parser *);
AstStmtBlock parse_single_stmt_or_block(Parser *);

AstStmt
parse_stmt(Parser *p)
{
  AstStmt stmt;
  stmt.line_info = grab_token(&p->lexer).line_info;

  switch (peek_token(&p->lexer))
    {
    case Token_Open_Curly:
      {
        AstStmtBlock block = parse_stmt_block(p);
        stmt.tag = Ast_Stmt_Block;
        stmt.as.Block = block;

        return stmt;
      }
    case Token_If:
      {
        advance_token(&p->lexer);

        AstExpr *expr = parse_expr(p);
        if (peek_token(&p->lexer) == Token_Then)
          advance_token(&p->lexer);
        AstStmtBlock if_true = parse_single_stmt_or_block(p);
        AstStmtBlock if_false = { 0 };
        if (peek_token(&p->lexer) == Token_Else)
          {
            advance_token(&p->lexer);
            if_false = parse_single_stmt_or_block(p);
          }

        stmt.tag = Ast_Stmt_If;
        stmt.as.If = (AstStmtIf){
          .cond = expr,
          .if_true = if_true,
          .if_false = if_false,
        };

        return stmt;
      }
    case Token_While:
      {
        advance_token(&p->lexer);

        AstExpr *expr = parse_expr(p);
        if (peek_token(&p->lexer) == Token_Do)
          advance_token(&p->lexer);
        AstStmtBlock block = parse_single_stmt_or_block(p);

        stmt.tag = Ast_Stmt_While;
        stmt.as.While = (AstStmtWhile){
          .cond = expr,
          .block = block,
          .is_do_while = false,
        };

        return stmt;
      }
    case Token_Do:
      {
        advance_token(&p->lexer);

        AstStmtBlock block = parse_single_stmt_or_block(p);
        expect_token(&p->lexer, Token_While);
        AstExpr *expr = parse_expr(p);
        expect_token(&p->lexer, Token_Semicolon);

        stmt.tag = Ast_Stmt_While;
        stmt.as.While = (AstStmtWhile){
          .cond = expr,
          .block = block,
          .is_do_while = true,
        };

        return stmt;
      }
    case Token_Break:
      advance_token(&p->lexer);
      expect_token(&p->lexer, Token_Semicolon);
      stmt.tag = Ast_Stmt_Break;
      return stmt;
    case Token_Continue:
      advance_token(&p->lexer);
      expect_token(&p->lexer, Token_Semicolon);
      stmt.tag = Ast_Stmt_Continue;
      return stmt;
    case Token_Identifier:
      {
        AstSymbol *symbol = parse_symbol(p);
        if (symbol != NULL)
          {
            stmt.tag = Ast_Stmt_Symbol;
            stmt.as.Symbol = symbol;
            return stmt;
          }
      }
      // fall through
    default:
      {
        AstExpr *lhs = parse_expr(p);

        if (peek_token(&p->lexer) == Token_Equal)
          {
            advance_token(&p->lexer);
            AstExpr *rhs = parse_expr(p);
            expect_token(&p->lexer, Token_Semicolon);

            stmt.tag = Ast_Stmt_Assign;
            stmt.as.Assign = (AstStmtAssign){
              .lhs = lhs,
              .rhs = rhs,
            };

            return stmt;
          }

        expect_token(&p->lexer, Token_Semicolon);

        stmt.tag = Ast_Stmt_Expr;
        stmt.as.Expr = lhs;

        return stmt;
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
      AstStmt *stmt = parser_malloc(p, sizeof(*stmt));
      *stmt = parse_stmt(p);

      LinkedListNode *node = parser_malloc(p, sizeof(*node));
      node->data = stmt;
      linked_list_insert_last(&block, node);

      tt = peek_token(&p->lexer);
    }

  expect_token(&p->lexer, Token_Close_Curly);

  return block;
}

AstStmtBlock
parse_single_stmt_or_block(Parser *p)
{
  switch (peek_token(&p->lexer))
    {
    case Token_Open_Curly:
      return parse_stmt_block(p);
    default:
      {
        AstStmt *stmt = parser_malloc(p, sizeof(*stmt));
        *stmt = parse_stmt(p);

        AstStmtBlock block = { 0 };
        LinkedListNode *node = parser_malloc(p, sizeof(*node));
        node->data = stmt;
        linked_list_insert_last(&block, node);

        return block;
      }
    }
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

  LinkedList stmts = { 0 };

  while (peek_token(&parser.lexer) != Token_End_Of_File)
    {
      AstStmt *stmt = parser_malloc(&parser, sizeof(*stmt));
      *stmt = parse_stmt(&parser);

      LinkedListNode *node = parser_malloc(&parser, sizeof(*node));
      node->data = stmt;
      linked_list_insert_last(&stmts, node);
    }

  Ast ast = {
    .stmts = stmts,
    .arena = parser.arena,
  };

  return ast;
}
