#include <iostream>
#include <string>
#include <format>
#include <forward_list>
#include <unordered_map>

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
#include "notstd.cpp"
#include "lexer.cpp"
#include "ast.cpp"
#include "parser.cpp"
#include "typechecker.cpp"
#include "transpiler.cpp"

int main(int argc, char **argv)
{
  auto ast = Parser::parse(argc < 2 ? "examples/debug" : argv[1]);
  Typechecker::typecheck(ast);
  Transpiler::transpile(ast);
}
