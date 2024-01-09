const char *
debug_token_tag_to_string(TokenTag tag)
{
  switch (tag)
    {
    case Token_Or:            return "Or";
    case Token_And:           return "And";
    case Token_Eq:            return "Eq";
    case Token_Neq:           return "Neq";
    case Token_Leq:           return "Leq";
    case Token_Geq:           return "Geq";
    case Token_Lt:            return "Lt";
    case Token_Gt:            return "Gt";
    case Token_Add:           return "Add";
    case Token_Sub:           return "Sub";
    case Token_Mul:           return "Mul";
    case Token_Div:           return "Div";
    case Token_Mod:           return "Mod";
    case Token_Not:           return "Not";
    case Token_Ref:           return "Ref";
    case Token_Open_Paren:    return "Open_Paren";
    case Token_Close_Paren:   return "Close_Paren";
    case Token_Open_Curly:    return "Open_Curly";
    case Token_Close_Curly:   return "Close_Curly";
    case Token_Open_Bracket:  return "Open_Bracket";
    case Token_Close_Bracket: return "Close_Bracket";
    case Token_Semicolon:     return "Semicolon";
    case Token_Dot:           return "Dot";
    case Token_Comma:         return "Comma";
    case Token_Equal:         return "Equal";
    case Token_Double_Colon:  return "Double_Colon";
    case Token_Colon_Equal:   return "Colon_Equal";
    case Token_If:            return "If";
    case Token_Then:          return "Then";
    case Token_Else:          return "Else";
    case Token_While:         return "While";
    case Token_Do:            return "Do";
    case Token_Break:         return "Break";
    case Token_Continue:      return "Continue";
    case Token_Void_Type:     return "Void_Type";
    case Token_Bool_Type:     return "Bool_Type";
    case Token_Int_Type:      return "Int_Type";
    case Token_False:         return "False";
    case Token_True:          return "True";
    case Token_Integer:       return "Integer";
    case Token_Identifier:    return "Identifier";
    case Token_End_Of_File:   return "End_Of_File";
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
             FORMAT_STRING_VIEW(token.text),
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
