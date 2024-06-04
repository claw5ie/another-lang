#define UNREACHABLE() assert(false && "unreachable")

namespace Utils
{
  constexpr size_t align_by_void_pointer(size_t value)
  {
    value += sizeof(void *) - 1;
    value -= value % sizeof(void *);
    return value;
  }

  constexpr u8 log2(u64 value)
  {
    u8 result = 0;

    for (u8 i = 6; i-- > 0; )
    {
      auto bits = (bool)(value >> (1u << i)) << i;
      result += bits;
      value >>= bits;
    }

    return result;
  }

  constexpr bool is_prefix(std::string_view prefix, std::string_view string)
  {
    return prefix.size() <= string.size() && memcmp(&prefix[0], &string[0], prefix.size()) == 0;
  }

  std::string read_entire_file(const char *filepath)
  {
    auto file_data = std::string{ };
    auto fd = open(filepath, O_RDONLY);

    if (fd == -1)
      goto exit_error;

    {
      struct stat stats;
      if (fstat(fd, &stats) == -1)
        goto exit_error;
      file_data.resize(stats.st_size + 1);
    }

    if (read(fd, &file_data[0], file_data.size() - 1) == -1)
      goto exit_error;
    file_data.back() = '\0';

    close(fd);

    return file_data;

  exit_error:
    perror(strerror(errno));
    exit(EXIT_FAILURE);
  }
}
