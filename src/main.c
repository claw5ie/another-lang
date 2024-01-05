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
#include "lexer.c"
#include "debugging-stuff.c"

int
main(void)
{
  const char *filepath = "examples/debug";
  size_t source_code_size = 0;
  char *source_code = read_entire_file(filepath, &source_code_size);

  Lexer lexer = {
    .token_start = 0,
    .token_count = 0,
    .line_info = { .line = 1, .column = 1, .offset = 0 },
    .source_code = source_code,
    .source_code_size = source_code_size,
    .filepath = filepath,
  };

  debug_print_all_tokens(&lexer);

  return EXIT_SUCCESS;
}
