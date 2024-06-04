#define COMPILER_EXIT_ERROR() exit(EXIT_FAILURE)

#define REPORT_ERROR_HELPER(filepath, line_info, header, text) std::cerr << filepath << ":" << (line_info).line << ":" << (line_info).column << ": " header ": " << text << "\n"

struct Lexer
{
  using StringPool = std::unordered_set<std::string>;

  struct LineInfo
  {
    u32 line, column;
    size_t offset;
  };

  struct Token
  {
    struct IntType
    {
      u16 bits;
      bool is_signed;
    };

    enum Tag
    {
      _Double_Bar,
      _Double_Ampersand,
      _Double_Equal,
      _Not_Equal,
      _Less,
      _Less_Equal,
      _Greater,
      _Greater_Equal,
      _Plus,
      _Minus,
      _Asterisk,
      _Slash,
      _Percent_Sign,

      _Exclamation_Mark,

      _Open_Paren,
      _Close_Paren,
      _Semicolon,
      _Colon,
      _Equal,

      _Bool_Type,
      _Int_Type,

      _False,
      _True,

      _Integer,
      _Identifier,

      _End_Of_File,
    };

    union Data
    {
      Token::IntType Int_Type;
      u64 Integer;
      std::string_view Identifier;
    };

    Tag tag;
    Data as;
    LineInfo line_info;
  };

  static constexpr u8 LOOKAHEAD = 2;

  static Lexer init(const char *filepath)
  {
    auto lexer = Lexer{
      .filepath = filepath,
      .source_code = Utils::read_entire_file(filepath),
    };
    return lexer;
  }

  Token &grab_ref(u8 index)
  {
    assert(index < LOOKAHEAD);
    while (index >= token_count)
      buffer_token();
    return token_buffer[(token_start + index) % LOOKAHEAD];
  }

  Token grab(u8 index = 0)
  {
    return grab_ref(index);
  }

  Token::Tag peek(u8 index = 0)
  {
    return grab_ref(index).tag;
  }

  void advance(u8 count = 1)
  {
    assert(count <= LOOKAHEAD);
    token_start += count;
    token_start %= LOOKAHEAD;
    token_count -= count;
  }

  void advance_line_info()
  {
    ++line_info.column;
    ++line_info.offset;
    if (source_code[line_info.offset - 1] == '\n')
    {
      line_info.column = 1;
      ++line_info.line;
    }
  }

  void expect(Token::Tag expected)
  {
    static constexpr auto to_string = [](Token::Tag tag) -> std::string_view
    {
      switch (tag)
      {
      case Token::_Double_Bar:       return "'||'";
      case Token::_Double_Ampersand: return "'&&'";
      case Token::_Double_Equal:     return "'=='";
      case Token::_Not_Equal:        return "'!='";
      case Token::_Less:             return "'<'";
      case Token::_Less_Equal:       return "'<='";
      case Token::_Greater:          return "'>'";
      case Token::_Greater_Equal:    return "'>='";
      case Token::_Plus:             return "'+'";
      case Token::_Minus:            return "'-'";
      case Token::_Asterisk:         return "'*'";
      case Token::_Slash:            return "'/'";
      case Token::_Percent_Sign:     return "'%'";
      case Token::_Exclamation_Mark: return "'!'";
      case Token::_Open_Paren:       return "'('";
      case Token::_Close_Paren:      return "')'";
      case Token::_Semicolon:        return "';'";
      case Token::_Colon:            return "':'";
      case Token::_Equal:            return "'='";
      case Token::_Bool_Type:        return "'bool'";
      case Token::_Int_Type:         return "integer type";
      case Token::_False:            return "'false'";
      case Token::_True:             return "'true'";
      case Token::_Integer:          return "integer literal";
      case Token::_Identifier:       return "identifier";
      case Token::_End_Of_File:      return "end-of-file";
      }

      UNREACHABLE();
    };

    if (peek() != expected)
    {
      report_error(grab().line_info, std::format("expected {}", to_string(expected)));
      COMPILER_EXIT_ERROR();

    }
    advance();
  }

  void buffer_token()
  {
    auto text = std::string_view{ source_code };
    auto i = line_info.offset;

    do
    {
      for (; isspace(text[i]); i++)
        advance_line_info();

      if (text[i] == '/' && text[i + 1] == '/')
        for (; text[i] != '\0' && text[i] != '\n'; i++)
          advance_line_info();
      else
        break;
    }
    while (text[i] != '\0');

    auto token = Token{
      .tag = Token::_End_Of_File,
      .as = { },
      .line_info = line_info,
    };

    if (text[i] == '\0') { }
    else if (isdigit(text[i]))
    {
      u64 value = 0;

      do
      {
        // TODO: check for overflow.
        value = 10 * value + (text[i++] - '0');
        advance_line_info();
      }
      while (isdigit(text[i]));

      token.tag = Token::_Integer;
      token.as = Token::Data{ .Integer = value };
    }
    else if (isalpha(text[i]) || text[i] == '_')
    {
      do
      {
        advance_line_info();
        ++i;
      }
      while (isalnum(text[i]) || text[i] == '_');

      auto identifier = std::string_view{ &source_code[token.line_info.offset], i - token.line_info.offset };

      if (identifier.size() >= 2 && identifier.size() <= 3)
      {
        auto is_int = false;
        auto is_signed = identifier[0] == 'i';

        {
          auto is_unsigned = identifier[0] == 'u';
          auto is_first_char_a_digit = isdigit(identifier[1]);
          auto is_rest_fine = true;
          if (identifier.size() == 3)
            is_rest_fine = isdigit(identifier[2]) && identifier[1] != '0';
          is_int = (is_signed || is_unsigned) && is_first_char_a_digit && is_rest_fine;
        }

        if (is_int)
        {
          u16 bits = 0;
          auto [ptr, ec] = std::from_chars(identifier.begin() + 1, identifier.end(), bits);
          assert(ec == std::errc{ });

          token.tag = Token::_Int_Type;
          token.as = Token::Data{ .Int_Type = {
              .bits = bits,
              .is_signed = is_signed,
            } };

          goto push_token;
        }
      }

      struct Keyword
      {
        std::string_view text;
        Token::Tag tag;
      };

      static constexpr Keyword keywords[] = {
        { .text = "false", .tag = Token::_False, },
        { .text = "true", .tag = Token::_True, },
        { .text = "bool", .tag = Token::_Bool_Type, },
      };

      for (auto &keyword: keywords)
      {
        if (keyword.text == identifier)
        {
          token.tag = keyword.tag;
          goto push_token;
        }
      }

      auto [string_it, _] = string_pool.emplace(identifier);
      token.tag = Token::_Identifier;
      token.as = Token::Data{ .Identifier = *string_it };
    }
    else
    {
      struct Symbol
      {
        std::string_view text;
        Token::Tag tag;
      };

      static constexpr Symbol symbols[] = {
        { .text = "||", .tag = Token::_Double_Bar, },
        { .text = "&&", .tag = Token::_Double_Ampersand, },
        { .text = "==", .tag = Token::_Double_Equal, },
        { .text = "!=", .tag = Token::_Not_Equal, },
        { .text = "<=", .tag = Token::_Less_Equal, },
        { .text = ">=", .tag = Token::_Greater_Equal, },
        { .text = "<", .tag = Token::_Less, },
        { .text = ">", .tag = Token::_Greater, },
        { .text = "+", .tag = Token::_Plus, },
        { .text = "-", .tag = Token::_Minus, },
        { .text = "*", .tag = Token::_Asterisk, },
        { .text = "/", .tag = Token::_Slash, },
        { .text = "%", .tag = Token::_Percent_Sign, },
        { .text = "!", .tag = Token::_Exclamation_Mark, },
        { .text = "(", .tag = Token::_Open_Paren, },
        { .text = ")", .tag = Token::_Close_Paren, },
        { .text = ";", .tag = Token::_Semicolon, },
        { .text = ":", .tag = Token::_Colon, },
        { .text = "=", .tag = Token::_Equal, },
      };

      auto rest = std::string_view{ &text[i], source_code.size() - i };

      for (auto &symbol: symbols)
      {
        if (Utils::is_prefix(symbol.text, rest))
        {
          auto count = symbol.text.size();
          i += count;
          line_info.column += count;
          line_info.offset += count;

          token.tag = symbol.tag;

          goto push_token;
        }
      }

      report_error(line_info, std::format("unexpected character '{}'", text[i]));
      COMPILER_EXIT_ERROR();
    }

  push_token:
    assert(token_count < LOOKAHEAD);
    auto index = (token_start + token_count) % LOOKAHEAD;
    token_buffer[index] = token;
    ++token_count;
  }

  void report_error(LineInfo line_info, std::string_view text)
  {
    REPORT_ERROR_HELPER(filepath, line_info, "error", text);
  }

  Token token_buffer[LOOKAHEAD] = { };
  u8 token_start = 0, token_count = 0;
  LineInfo line_info = { .line = 1, .column = 1, .offset = 0 };

  std::string_view filepath;
  std::string source_code;
  StringPool string_pool = { };
};
