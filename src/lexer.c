#define PRINT_ERROR0(filepath, line_info, message) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column)
#define PRINT_ERROR(filepath, line_info, message, ...) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column,  __VA_ARGS__)

typedef struct LineInfo LineInfo;
struct LineInfo
{
  size_t line, column, offset;
};

enum TokenTag
  {
    Token_Add,
    Token_Mul,

    Token_Open_Paren,
    Token_Close_Paren,

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
        { .text = STRING_VIEW_FROM_CSTRING("+"), .tag = Token_Add },
        { .text = STRING_VIEW_FROM_CSTRING("*"), .tag = Token_Mul },
        { .text = STRING_VIEW_FROM_CSTRING("("), .tag = Token_Open_Paren },
        { .text = STRING_VIEW_FROM_CSTRING(")"), .tag = Token_Close_Paren },
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

Token
grab_token(Lexer *lexer)
{
  if (lexer->token_count == 0)
    buffer_token(lexer);

  return lexer->tokens[lexer->token_start];
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
