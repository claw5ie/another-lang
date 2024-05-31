#ifdef NDEBUG
#define COMPILER_EXIT_ERROR() exit(EXIT_FAILURE)
#else
#define COMPILER_EXIT_ERROR() assert(false)
#endif

#define REPORT_ERROR_HELPER(filepath, line_info, header, text) std::cout << filepath << ":" << (line_info).line << ":" << (line_info).column << ": " header ": " << text << "\n"

struct Lexer
{
  struct LineInfo
  {
    u32 line, column;
    size_t offset;
  };

  struct Token
  {
    enum Tag
    {
      _Plus,
      _Minus,
      _Asterisk,
      _Slash,

      _Integer,

      _End_Of_File,
    };

    union Data
    {
      u64 Integer;
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
      token.as = { .Integer = value };
    }
    else
    {
      struct Symbol
      {
        std::string_view text;
        Token::Tag tag;
      };

      static constexpr Symbol symbols[] = {
        { .text = "+", .tag = Token::_Plus, },
        { .text = "-", .tag = Token::_Minus, },
        { .text = "*", .tag = Token::_Asterisk, },
        { .text = "/", .tag = Token::_Slash, },
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
};
