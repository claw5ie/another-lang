#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <stddef.h>
#include <stdarg.h>
#include <math.h>
#include <assert.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

typedef unsigned uint;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

#include "utils.c"
#include "notstd.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"
#include "resolve-identifiers.c"
#include "typechecker.c"
#include "transpiler.c"
#include "debugging-stuff.c"

int
main(int argc, char **argv)
{
  Ast ast = parse(argc <= 1 ? "examples/debug" : argv[1]);
  resolve_identifiers(&ast);
  typecheck(&ast);
  transpile_to_c(&ast);
}
