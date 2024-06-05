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
        .stmt_list = { },
        .symbol_table = { },
        .arena = { },
        .filepath = filepath,
      },
      .had_error = false,
    };
    parser.parse();

    parser.ast.symbol_table.string_pool.swap(parser.lexer.string_pool);

    return std::move(parser.ast);
  }

  static int prec(Token::Tag op)
  {
    switch (op)
    {
    case Token::_Double_Bar:       return -4;
    case Token::_Double_Ampersand: return -3;
    case Token::_Double_Equal:
    case Token::_Not_Equal:        return -2;
    case Token::_Less:
    case Token::_Less_Equal:
    case Token::_Greater:
    case Token::_Greater_Equal:    return -1;
    case Token::_Plus:
    case Token::_Minus:            return 0;
    case Token::_Asterisk:
    case Token::_Slash:
    case Token::_Percent_Sign:     return 1;
    default:                       return LOWEST_PREC - 1;
    }
  }

  static Ast::Expr::BinaryOp::Tag to_binary_op_tag(Token::Tag op)
  {
    switch (op)
    {
    case Token::_Double_Bar:       return Ast::Expr::BinaryOp::Or;
    case Token::_Double_Ampersand: return Ast::Expr::BinaryOp::And;
    case Token::_Double_Equal:     return Ast::Expr::BinaryOp::Eq;
    case Token::_Not_Equal:        return Ast::Expr::BinaryOp::Neq;
    case Token::_Less:             return Ast::Expr::BinaryOp::Lt;
    case Token::_Less_Equal:       return Ast::Expr::BinaryOp::Leq;
    case Token::_Greater:          return Ast::Expr::BinaryOp::Gt;
    case Token::_Greater_Equal:    return Ast::Expr::BinaryOp::Geq;
    case Token::_Plus:             return Ast::Expr::BinaryOp::Add;
    case Token::_Minus:            return Ast::Expr::BinaryOp::Sub;
    case Token::_Asterisk:         return Ast::Expr::BinaryOp::Mul;
    case Token::_Slash:            return Ast::Expr::BinaryOp::Div;
    case Token::_Percent_Sign:     return Ast::Expr::BinaryOp::Mod;
    default:                       UNREACHABLE();
    }
  }

  static Ast::Expr::UnaryOp::Tag to_unary_op_tag(Token::Tag op)
  {
    switch (op)
    {
    case Token::_Plus:             return Ast::Expr::UnaryOp::Plus;
    case Token::_Minus:            return Ast::Expr::UnaryOp::Minus;
    case Token::_Exclamation_Mark: return Ast::Expr::UnaryOp::Not;
    default:                       UNREACHABLE();
    }
  }

  static bool can_token_start_expression(Token::Tag tag)
  {
    switch (tag)
    {
    case Token::_Double_Ampersand:
    case Token::_Plus:
    case Token::_Minus:
    case Token::_Exclamation_Mark:
    case Token::_Ampersand:
    case Token::_Open_Paren:
    case Token::_Void_Type:
    case Token::_Bool_Type:
    case Token::_Int_Type:
    case Token::_False:
    case Token::_True:
    case Token::_Null:
    case Token::_Integer:
    case Token::_Identifier: return true;
    default:                 return false;
    }
  }

  // Every case should also appear in 'can_token_start_expression'.
  Ast::Expr *parse_expr_highest_prec()
  {
    auto token = lexer.grab();
    lexer.advance();

    switch (token.tag)
    {
    case Token::_Double_Ampersand:
    {
      auto subsubexpr = parse_expr_highest_prec();
      auto subexpr = ast.alloc<Ast::Expr>();
      *subexpr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Ref,
        .as = { .Ref = subsubexpr },
        .flags = 0,
      };
      ++subexpr->line_info.column;
      ++subexpr->line_info.offset;

      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Ref,
        .as = { .Ref = subexpr },
        .flags = 0,
      };

      return expr;
    }
    case Token::_Plus:
    case Token::_Minus:
    case Token::_Exclamation_Mark:
    {
      auto subexpr = parse_expr_highest_prec();
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Unary_Op,
        .as = { .Unary_Op = {
            .tag = to_unary_op_tag(token.tag),
            .subexpr = subexpr,
          } },
        .flags = 0,
      };

      return expr;
    }
    case Token::_Ampersand:
    {
      auto subexpr = parse_expr_highest_prec();
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Ref,
        .as = { .Ref = subexpr },
        .flags = 0,
      };
      return expr;
    }
    case Token::_Open_Paren:
    {
      auto expr = parse_expr();
      lexer.expect(Token::_Close_Paren);
      return expr;
    }
    case Token::_Void_Type:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Type,
        .as = { .Type = {
            .tag = Ast::Type::_Void,
            .as = { },
          } },
        .flags = 0,
      };
      return expr;
    }
    case Token::_Bool_Type:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Type,
        .as = { .Type = {
            .tag = Ast::Type::_Bool,
            .as = { },
          } },
        .flags = 0,
      };
      return expr;
    }
    case Token::_Int_Type:
    {
      auto &Int_Type = token.as.Int_Type;

      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Type,
        .as = { .Type = {
            .tag = Ast::Type::_Int,
            .as = { .Int = {
                .bits = Int_Type.bits,
                .is_signed = Int_Type.is_signed,
              } },
          } },
        .flags = 0,
      };
      return expr;
    }
    case Token::_False:
    case Token::_True:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Boolean,
        .as = { .Boolean = token.tag == Token::_True },
        .flags = 0,
      };
      return expr;
    }
    case Token::_Null:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Null,
        .as = { },
        .flags = 0,
      };
      return expr;
    }
    case Token::_Integer:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Integer,
        .as = { .Integer = token.as.Integer },
        .flags = 0,
      };

      return expr;
    }
    case Token::_Identifier:
    {
      auto expr = ast.alloc<Ast::Expr>();
      *expr = Ast::Expr{
        .line_info = token.line_info,
        .tag = Ast::Expr::_Identifier,
        .as = { .Identifier = token.as.Identifier },
        .flags = 0,
      };
      return expr;
    }
    default:
      assert(!can_token_start_expression(token.tag));
      report_error(token.line_info, "token doesn't start an expression");
      COMPILER_EXIT_ERROR();
    }
  }

  Ast::Expr *parse_expr_base()
  {
    auto base = parse_expr_highest_prec();

    do
    {
      switch (lexer.peek())
      {
      case Token::_Asterisk:
      {
        if (can_token_start_expression(lexer.peek(1)))
          goto finish_parsing_unary_operators;

        lexer.advance();
        auto new_base = ast.alloc<Ast::Expr>();
        *new_base = Ast::Expr{
          .line_info = base->line_info,
          .tag = Ast::Expr::_Deref,
          .as = { .Deref = base },
          .flags = 0,
        };
        base = new_base;

      } break;
      default:
        goto finish_parsing_unary_operators;
      }
    }
    while (true);
  finish_parsing_unary_operators:

    return base;
  }

  Ast::Expr *parse_expr_prec(int lowest_prec_limit)
  {
    auto lhs = parse_expr_base();
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
          .line_info = lhs->line_info,
          .tag = Ast::Expr::_Binary_Op,
          .as = { .Binary_Op = {
              .tag = to_binary_op_tag(op),
              .lhs = lhs,
              .rhs = rhs,
              .line_info = line_info,
            } },
          .flags = 0,
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

  Ast::Stmt *parse_stmt()
  {
    auto must_end_with_semicolon = false;

    const auto go = [this, &must_end_with_semicolon]() -> Ast::Stmt *
    {
      auto base = parse_expr();

      switch (lexer.peek())
      {
      case Token::_Colon:
      {
        auto line_info = lexer.grab().line_info;

        lexer.advance();
        auto type = parse_expr();
        Ast::Expr *value = nullptr;

        if (lexer.peek() == Token::_Equal)
        {
          lexer.advance();
          value = parse_expr();
        }

        insert_symbol(base);

        auto symbol = ast.alloc<Ast::Symbol>();
        *symbol = Ast::Symbol{
          .name = { },
          .line_info = line_info,
          .tag = Ast::Symbol::_Variable,
          .as = { .Variable = {
              .pattern = base,
              .type = type,
              .value = value,
            } },
        };

        auto stmt = ast.alloc<Ast::Stmt>();
        *stmt = Ast::Stmt{
          .line_info = base->line_info,
          .tag = Ast::Stmt::_Symbol,
          .as = { .Symbol = symbol },
        };

        must_end_with_semicolon = true;

        return stmt;
      }
      case Token::_Equal:
      {
        auto line_info = lexer.grab().line_info;

        lexer.advance();
        auto rhs = parse_expr();

        auto stmt = ast.alloc<Ast::Stmt>();
        *stmt = Ast::Stmt{
          .line_info = line_info,
          .tag = Ast::Stmt::_Assign,
          .as = { .Assign = {
              .lhs = base,
              .rhs = rhs,
            } },
        };

        must_end_with_semicolon = true;

        return stmt;
      }
      default:
      {
        auto stmt = ast.alloc<Ast::Stmt>();
        *stmt = Ast::Stmt{
          .line_info = base->line_info,
          .tag = Ast::Stmt::_Expr,
          .as = { .Expr = base },
        };

        must_end_with_semicolon = true;

        return stmt;
      }
      }
    };

    auto stmt = go();

    if (must_end_with_semicolon)
    {
      if (lexer.peek() == Token::_Semicolon)
        lexer.advance();
      else
        report_error(lexer.grab().line_info, "expected ';'");
    }

    return stmt;
  }

  void insert_symbol(Ast::Expr *expr)
  {
    switch (expr->tag)
    {
    case Ast::Expr::_Binary_Op:
    {
      auto &Binary_Op = expr->as.Binary_Op;

      insert_symbol(Binary_Op.lhs);
      insert_symbol(Binary_Op.rhs);
    } break;
    case Ast::Expr::_Unary_Op:
    {
      auto &Unary_Op = expr->as.Unary_Op;

      insert_symbol(Unary_Op.subexpr);
    } break;
    case Ast::Expr::_Ref:
    {
      insert_symbol(expr->as.Ref);
    } break;
    case Ast::Expr::_Deref:
    {
      insert_symbol(expr->as.Deref);
    } break;
    case Ast::Expr::_Cast:
    {
      insert_symbol(expr->as.Cast.expr);
    } break;
    case Ast::Expr::_Type:
    {
      auto &Type = expr->as.Type;
      switch (Type.tag)
      {
      case Ast::Type::_Pointer:
      case Ast::Type::_Void:
      case Ast::Type::_Bool:
      case Ast::Type::_Int:
        break;
      }
    } break;
    case Ast::Expr::_Boolean:
    case Ast::Expr::_Integer:
    case Ast::Expr::_Null:
      break;
    case Ast::Expr::_Symbol:
      UNREACHABLE();
    case Ast::Expr::_Identifier:
    {
      auto &Identifier = expr->as.Identifier;

      auto [it, was_inserted] = ast.symbol_table.insert(Ast::SymbolKey{
          .name = Identifier,
        });

      if (!was_inserted)
      {
        report_error(expr->line_info, std::format("symbol '{}' is defined already", Identifier));
        COMPILER_EXIT_ERROR();
      }

      auto symbol = ast.alloc<Ast::Symbol>();
      *symbol = Ast::Symbol{
        .name = it->first.name,
        .line_info = expr->line_info,
        .tag = Ast::Symbol::_Identifier,
        .as = { .Identifier = {
            .type = nullptr,
            .value = nullptr,
          } },
      };
      it->second = symbol;

      expr->tag = Ast::Expr::_Symbol;
      expr->as.Symbol = symbol;
    }
    }
  }

  void parse()
  {
    auto stmt_list = Ast::StmtList{ };

    while (lexer.peek() != Token::_End_Of_File)
    {
      auto stmt = parse_stmt();

      auto node = ast.alloc<Ast::StmtList::Node>();
      *node = Ast::StmtList::Node{
        .data = stmt,
      };
      stmt_list.insert_last(node);
    }

    ast.stmt_list = stmt_list;

    if (had_error)
      exit(EXIT_FAILURE);
  }

  void report_error(LineInfo line_info, std::string_view text)
  {
    had_error = true;
    REPORT_ERROR_HELPER(lexer.filepath, line_info, "error", text);
  }

  Lexer lexer;
  Ast ast;
  bool had_error;
};
