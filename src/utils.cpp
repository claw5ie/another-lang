namespace Utils
{
  bool is_prefix(std::string_view prefix, std::string_view string)
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
