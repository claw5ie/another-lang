struct Parser
{
  using LineInfo = Lexer::LineInfo;
  using Token = Lexer::Token;

  static constexpr int LOWEST_PREC = std::numeric_limits<int>::lowest() + 1;

  static Ast parse(const char *filepath)
  {
    auto parser = Parser{
      .lexer = Lexer::init(filepath),
      .ast = {
        .exprs = { },
        .arena = { },
      }
    };
    parser.parse();

    return parser.ast;
  }

  static int prec(Token::Tag op)
  {
    switch (op)
    {
    case Token::_Plus:
    case Token::_Minus:    return 0;
    case Token::_Asterisk:
    case Token::_Slash:    return 1;
    default:               return LOWEST_PREC - 1;
    }
  }

  static Ast::Expr::BinaryOp::Tag to_binary_op_tag(Token::Tag op)
  {
    switch (op)
    {
    case Token::_Plus:     return Ast::Expr::BinaryOp::Add;
    case Token::_Minus:    return Ast::Expr::BinaryOp::Sub;
    case Token::_Asterisk: return Ast::Expr::BinaryOp::Mul;
    case Token::_Slash:    return Ast::Expr::BinaryOp::Div;
    default:               UNREACHABLE();
    }
  }

  Ast::Expr *parse_expr_highest_prec()
  {
    auto token = lexer.grab();
    lexer.advance();

    switch (token.tag)
    {
    case Token::_Integer:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Integer,
        .as = { .Integer = token.as.Integer },
      };

      return expr;
    }
    default:
      report_error(token.line_info, "token doesn't start an expression");
      COMPILER_EXIT_ERROR();
    }
  }

  Ast::Expr *parse_expr_prec(int lowest_prec_limit)
  {
    auto lhs = parse_expr_highest_prec();
    auto op = lexer.peek();
    auto prev_prec = std::numeric_limits<int>::max(), curr_prec = prec(op);

    while (curr_prec < prev_prec && curr_prec >= lowest_prec_limit)
    {
      do
      {
        auto line_info = lexer.grab().line_info;
        lexer.advance();

        auto rhs = parse_expr_prec(curr_prec + 1);
        auto new_lhs = ast.alloc<Ast::Expr>();
        *new_lhs = {
          .line_info = line_info,
          .tag = Ast::Expr::_Binary_Op,
          .as = { .Binary_Op = {
              .tag = to_binary_op_tag(op),
              .lhs = lhs,
              .rhs = rhs,
            } },
        };
        lhs = new_lhs;

        op = lexer.peek();
      }
      while (curr_prec == prec(op));

      prev_prec = curr_prec;
      curr_prec = prec(op);
    }

    return lhs;
  }

  Ast::Expr *parse_expr()
  {
    return parse_expr_prec(LOWEST_PREC);
  }

  void parse()
  {
    auto expr_list = Ast::ExprList{ };

    while (lexer.peek() != Token::_End_Of_File)
    {
      auto expr = parse_expr();

      auto node = ast.alloc<Ast::ExprList::Node>();
      *node = Ast::ExprList::Node{
        .data = expr,
      };
      expr_list.insert_last(node);
    }
  }

  void report_error(LineInfo line_info, std::string_view text)
  {
    REPORT_ERROR_HELPER(lexer.filepath, line_info, "error", text);
  }

  Lexer lexer;
  Ast ast;
};
