#include <iostream>
#include <string>
#include <format>

#include <cstring>
#include <cstdint>
#include <cassert>

#include <sys/fcntl.h>
#include <sys/stat.h>
#include <sys/unistd.h>

using u8  = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using i8  = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

#include "utils.cpp"
#include "lexer.cpp"

int main(int argc, char **argv)
{
  using Token = Lexer::Token;

  auto lexer = Lexer::init(argc < 2 ? "examples/debug" : argv[1]);

  do
  {
    auto token = lexer.grab();
    lexer.advance();

    std::cout << "text: \"";

    switch (token.tag)
    {
    case Token::_Plus:
      std::cout << "+";
      break;
    case Token::_Minus:
      std::cout << "-";
      break;
    case Token::_Asterisk:
      std::cout << "*";
      break;
    case Token::_Slash:
      std::cout << "/";
      break;
    case Token::_Integer:
      std::cout << token.as.Integer;
      break;
    case Token::_End_Of_File:
      break;
    }

    std::cout << "\""
              << "\nline: (" << token.line_info.line << ", " << token.line_info.column << ", " << token.line_info.offset << ")\n";

    if (token.tag == Token::_End_Of_File)
      break;
  }
  while (true);
}
