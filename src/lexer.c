#define PRINT_ERROR0(filepath, line_info, message) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column)
#define PRINT_ERROR(filepath, line_info, message, ...) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column,  __VA_ARGS__)

typedef struct LineInfo LineInfo;
struct LineInfo
{
  size_t line, column, offset;
};

enum TokenTag
  {
    Token_Or,
    Token_And,
    Token_Eq,
    Token_Neq,
    Token_Leq,
    Token_Geq,
    Token_Lt,
    Token_Gt,
    Token_Add,
    Token_Sub,
    Token_Mul,
    Token_Div,
    Token_Mod,

    Token_Not,
    Token_Ref,
    Token_Open_Paren,
    Token_Close_Paren,
    Token_Open_Curly,
    Token_Close_Curly,
    Token_Open_Bracket,
    Token_Close_Bracket,
    Token_Semicolon,
    Token_Dot,
    Token_Comma,
    Token_Equal,
    Token_Double_Colon,
    Token_Double_Colon_Equal,
    Token_Arrow,

    Token_If,
    Token_Then,
    Token_Else,
    Token_While,
    Token_Do,
    Token_Break,
    Token_Continue,
    Token_Return,
    Token_Case,

    Token_Proc,
    Token_Struct,
    Token_Union,
    Token_Enum,
    Token_Alias,
    Token_Cast,

    Token_Void_Type,
    Token_Bool_Type,
    Token_Int_Type,

    Token_False,
    Token_True,
    Token_Null,

    Token_Integer,
    Token_Identifier,

    Token_End_Of_File,
  };
typedef enum TokenTag TokenTag;

typedef struct Token Token;
struct Token
{
  TokenTag tag;
  StringView text;
  LineInfo line_info;
};

#define LOOKAHEAD 2

typedef struct Lexer Lexer;
struct Lexer
{
  Token tokens[LOOKAHEAD];
  u8 token_start;
  u8 token_count;
  LineInfo line_info;

  char *source_code;
  size_t source_code_size;
  const char *filepath;
};

bool
is_int_type(StringView text)
{
  return text.count >= 2 && text.count <= 3
    && (text.data[0] == 'i' || text.data[0] == 'u')
    && isdigit(text.data[1])
    && isdigit(text.data[2]);
}

void
advance_line_info(Lexer *lexer)
{
  ++lexer->line_info.column;
  ++lexer->line_info.offset;
  if (lexer->source_code[lexer->line_info.offset - 1] == '\n')
    {
      ++lexer->line_info.line;
      lexer->line_info.column = 1;
    }
}

void
buffer_token(Lexer *lexer)
{
  size_t at = lexer->line_info.offset;
  const char *text = lexer->source_code;

  do
    {
      for (; isspace(text[at]); at++)
        advance_line_info(lexer);

      if (text[at] == '/' && text[at + 1] == '/')
        for (; text[at] != '\0' && text[at] != '\n'; at++)
          advance_line_info(lexer);
      else
        break;
    }
  while (text[at] != '\0');

  Token token = {
    .tag = Token_End_Of_File,
    .text = { .data = &text[at], .count = 0 },
    .line_info = lexer->line_info,
  };

  if (text[at] == '\0')
    { }
  else if (isdigit(text[at]))
    {
      do
        {
          advance_line_info(lexer);
          ++at;
        }
      while (isdigit(text[at]));

      token.tag = Token_Integer;
      token.text.count = at - token.line_info.offset;
    }
  else if (isalpha(text[at]) || text[at] == '_')
    {
      do
        {
          advance_line_info(lexer);
          ++at;
        }
      while (isalnum(text[at]) || text[at] == '_');

      token.tag = Token_Identifier;
      token.text.count = at - token.line_info.offset;

      if (is_int_type(token.text))
        {
          token.tag = Token_Int_Type;
          goto push_token;
        }

      typedef struct Keyword Keyword;
      struct Keyword
      {
        StringView text;
        TokenTag tag;
      };

      const Keyword keywords[] = {
        { .text = STRING_VIEW_FROM_CSTRING("if"),       .tag = Token_If        },
        { .text = STRING_VIEW_FROM_CSTRING("then"),     .tag = Token_Then      },
        { .text = STRING_VIEW_FROM_CSTRING("else"),     .tag = Token_Else      },
        { .text = STRING_VIEW_FROM_CSTRING("while"),    .tag = Token_While     },
        { .text = STRING_VIEW_FROM_CSTRING("do"),       .tag = Token_Do        },
        { .text = STRING_VIEW_FROM_CSTRING("break"),    .tag = Token_Break     },
        { .text = STRING_VIEW_FROM_CSTRING("continue"), .tag = Token_Continue  },
        { .text = STRING_VIEW_FROM_CSTRING("return"),   .tag = Token_Return    },
        { .text = STRING_VIEW_FROM_CSTRING("proc"),     .tag = Token_Proc      },
        { .text = STRING_VIEW_FROM_CSTRING("case"),     .tag = Token_Case      },
        { .text = STRING_VIEW_FROM_CSTRING("struct"),   .tag = Token_Struct    },
        { .text = STRING_VIEW_FROM_CSTRING("union"),    .tag = Token_Union     },
        { .text = STRING_VIEW_FROM_CSTRING("enum"),     .tag = Token_Enum      },
        { .text = STRING_VIEW_FROM_CSTRING("alias"),    .tag = Token_Alias     },
        { .text = STRING_VIEW_FROM_CSTRING("cast"),     .tag = Token_Cast      },
        { .text = STRING_VIEW_FROM_CSTRING("void"),     .tag = Token_Void_Type },
        { .text = STRING_VIEW_FROM_CSTRING("bool"),     .tag = Token_Bool_Type },
        { .text = STRING_VIEW_FROM_CSTRING("false"),    .tag = Token_False     },
        { .text = STRING_VIEW_FROM_CSTRING("true"),     .tag = Token_True      },
        { .text = STRING_VIEW_FROM_CSTRING("null"),     .tag = Token_Null      },
      };

      for (size_t i = 0; i < ARRAY_COUNT(keywords); i++)
        {
          const Keyword *keyword = &keywords[i];
          if (are_string_views_equal(keyword->text, token.text))
            {
              token.tag = keyword->tag;
              break;
            }
        }
    }
  else
    {
      typedef struct Symbol Symbol;
      struct Symbol
      {
        StringView text;
        TokenTag tag;
      };

      const Symbol symbols[] = {
        { .text = STRING_VIEW_FROM_CSTRING("::="), .tag = Token_Double_Colon_Equal },
        { .text = STRING_VIEW_FROM_CSTRING("||"),  .tag = Token_Or            },
        { .text = STRING_VIEW_FROM_CSTRING("&&"),  .tag = Token_And           },
        { .text = STRING_VIEW_FROM_CSTRING("=="),  .tag = Token_Eq            },
        { .text = STRING_VIEW_FROM_CSTRING("!="),  .tag = Token_Neq           },
        { .text = STRING_VIEW_FROM_CSTRING("<="),  .tag = Token_Leq           },
        { .text = STRING_VIEW_FROM_CSTRING(">="),  .tag = Token_Geq           },
        { .text = STRING_VIEW_FROM_CSTRING("::"),  .tag = Token_Double_Colon  },
        { .text = STRING_VIEW_FROM_CSTRING("->"),  .tag = Token_Arrow         },
        { .text = STRING_VIEW_FROM_CSTRING("<"),   .tag = Token_Lt            },
        { .text = STRING_VIEW_FROM_CSTRING(">"),   .tag = Token_Gt            },
        { .text = STRING_VIEW_FROM_CSTRING("+"),   .tag = Token_Add           },
        { .text = STRING_VIEW_FROM_CSTRING("-"),   .tag = Token_Sub           },
        { .text = STRING_VIEW_FROM_CSTRING("*"),   .tag = Token_Mul           },
        { .text = STRING_VIEW_FROM_CSTRING("/"),   .tag = Token_Div           },
        { .text = STRING_VIEW_FROM_CSTRING("%"),   .tag = Token_Mod           },
        { .text = STRING_VIEW_FROM_CSTRING("!"),   .tag = Token_Not           },
        { .text = STRING_VIEW_FROM_CSTRING("&"),   .tag = Token_Ref           },
        { .text = STRING_VIEW_FROM_CSTRING("("),   .tag = Token_Open_Paren    },
        { .text = STRING_VIEW_FROM_CSTRING(")"),   .tag = Token_Close_Paren   },
        { .text = STRING_VIEW_FROM_CSTRING("{"),   .tag = Token_Open_Curly    },
        { .text = STRING_VIEW_FROM_CSTRING("}"),   .tag = Token_Close_Curly   },
        { .text = STRING_VIEW_FROM_CSTRING("["),   .tag = Token_Open_Bracket  },
        { .text = STRING_VIEW_FROM_CSTRING("]"),   .tag = Token_Close_Bracket },
        { .text = STRING_VIEW_FROM_CSTRING(";"),   .tag = Token_Semicolon     },
        { .text = STRING_VIEW_FROM_CSTRING("."),   .tag = Token_Dot           },
        { .text = STRING_VIEW_FROM_CSTRING(","),   .tag = Token_Comma         },
        { .text = STRING_VIEW_FROM_CSTRING("="),   .tag = Token_Equal         },
      };

      StringView rest = { .data = &text[at], .count = lexer->source_code_size - at };

      for (size_t i = 0; i < ARRAY_COUNT(symbols); i++)
        {
          const Symbol *symbol = &symbols[i];
          if (is_prefix(symbol->text, rest))
            {
              size_t count = symbol->text.count;

              token.tag = symbol->tag;
              token.text.count = count;

              at += count;
              lexer->line_info.column += count;
              lexer->line_info.offset += count;

              goto push_token;
            }
        }

      PRINT_ERROR(lexer->filepath, lexer->line_info, "unrecognized character '%c'", text[at]);
      exit(EXIT_FAILURE);
    }

 push_token:
  assert(lexer->token_count < LOOKAHEAD);
  size_t index = (lexer->token_start + lexer->token_count) % LOOKAHEAD;
  lexer->tokens[index] = token;
  ++lexer->token_count;
}

const char *
token_tag_to_string(TokenTag tag)
{
  switch (tag)
    {
    case Token_Open_Paren:    return "'('";
    case Token_Close_Paren:   return "')'";
    case Token_Open_Curly:    return "'{'";
    case Token_Close_Curly:   return "'}'";
    case Token_Open_Bracket:  return "'['";
    case Token_Close_Bracket: return "']'";
    case Token_Semicolon:     return "';'";
    case Token_Comma:         return "','";
    case Token_Double_Colon:  return "'::'";
    case Token_While:         return "'while'";
    case Token_Identifier:    return "identifier";
    case Token_Or:
    case Token_And:
    case Token_Eq:
    case Token_Neq:
    case Token_Leq:
    case Token_Geq:
    case Token_Lt:
    case Token_Gt:
    case Token_Add:
    case Token_Sub:
    case Token_Mul:
    case Token_Div:
    case Token_Mod:
    case Token_Not:
    case Token_Ref:
    case Token_Dot:
    case Token_Equal:
    case Token_Double_Colon_Equal:
    case Token_Arrow:
    case Token_If:
    case Token_Then:
    case Token_Else:
    case Token_Do:
    case Token_Break:
    case Token_Continue:
    case Token_Return:
    case Token_Case:
    case Token_Proc:
    case Token_Struct:
    case Token_Union:
    case Token_Enum:
    case Token_Alias:
    case Token_Cast:
    case Token_Void_Type:
    case Token_Bool_Type:
    case Token_Int_Type:
    case Token_False:
    case Token_True:
    case Token_Null:
    case Token_Integer:
    case Token_End_Of_File: UNREACHABLE();
    }

  UNREACHABLE();
}

Token
grab_token(Lexer *lexer)
{
  if (lexer->token_count == 0)
    buffer_token(lexer);

  return lexer->tokens[lexer->token_start];
}

void
putback_token(Lexer *lexer, Token *token)
{
  assert(lexer->token_count < LOOKAHEAD);
  --lexer->token_start;
  lexer->token_start %= LOOKAHEAD;
  ++lexer->token_count;
  lexer->tokens[lexer->token_start] = *token;
}

TokenTag
peek_ahead_token(Lexer *lexer, u8 index)
{
  assert(index < LOOKAHEAD);

  while (lexer->token_count <= index)
    buffer_token(lexer);

  return lexer->tokens[(lexer->token_start + index) % LOOKAHEAD].tag;
}

TokenTag
peek_token(Lexer *lexer)
{
  if (lexer->token_count == 0)
    buffer_token(lexer);

  return lexer->tokens[lexer->token_start].tag;
}

void
advance_many_tokens(Lexer *lexer, u8 count)
{
  assert(lexer->token_count >= count);

  lexer->token_start += count;
  lexer->token_start %= LOOKAHEAD;
  lexer->token_count -= count;
}

void
advance_token(Lexer *lexer)
{
  advance_many_tokens(lexer, 1);
}

void
expect_token(Lexer *lexer, TokenTag expected)
{
  if (peek_token(lexer) != expected)
    {
      Token token = grab_token(lexer);
      PRINT_ERROR(lexer->filepath, token.line_info, "expected %s, but got '%.*s'", token_tag_to_string(expected), FORMAT_STRING_VIEW(token.text));
      exit(EXIT_FAILURE);
    }

  advance_token(lexer);
}
