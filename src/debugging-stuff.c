const char *
debug_token_tag_to_string(TokenTag tag)
{
  switch (tag)
    {
    case Token_Add:         return "Add";
    case Token_Mul:         return "Mul";
    case Token_Open_Paren:  return "Open_Paren";
    case Token_Close_Paren: return "Close_Paren";
    case Token_Integer:     return "Integer";
    case Token_Identifier:  return "Identifier";
    case Token_End_Of_File: return "End_Of_File";
    }

  assert(false);
}

void
debug_print_all_tokens(Lexer *lexer)
{
  do
    {
      Token token = grab_token(lexer);
      advance_token(lexer);

      printf("Token.%s:\n"
             "    text: '%.*s'\n"
             "    pos:  (%zu, %zu, %zu)\n",
             debug_token_tag_to_string(token.tag),
             (int)token.text.count,
             token.text.data,
             token.line_info.line,
             token.line_info.column,
             token.line_info.offset);

      if (token.tag == Token_End_Of_File)
        break;
    }
  while (true);

  assert(lexer->token_count == 0);
  lexer->line_info = (LineInfo){ .line = 1, .column = 1, .offset = 0 };
}
