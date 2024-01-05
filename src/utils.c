char *
read_entire_file(const char *filepath)
{
  int fd = open(filepath, O_RDONLY);

  if (fd == -1)
    goto print_error;

  char *data = NULL;
  size_t size = 0;

  {
    struct stat stats;
    if (fstat(fd, &stats) == -1)
      goto print_error;
    size = stats.st_size;
  }

  data = malloc(size + 1);
  if (data == NULL)
    abort();
  else if (read(fd, data, size) == -1)
    goto print_error;

  data[size] = '\0';

  close(fd);

  return data;

 print_error:
  fprintf(stderr, "error: %s: '%s'\n", strerror(errno), filepath);
  exit(EXIT_FAILURE);
}
