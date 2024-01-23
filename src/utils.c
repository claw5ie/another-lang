#define UNREACHABLE() assert(false)
#define ARRAY_COUNT(array) (sizeof(array) / sizeof(*array))

char *
read_entire_file(const char *filepath, size_t *file_size)
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
  if (!data)
    abort();
  else if (read(fd, data, size) == -1)
    goto print_error;
  data[size] = '\0';

  close(fd);

  if (file_size)
    *file_size = size;

  return data;

 print_error:
  fprintf(stderr, "error: %s: '%s'\n", strerror(errno), filepath);
  exit(EXIT_FAILURE);
}

// MurMur2 hash by Austin Appleby
// Taken from https://github.com/abrandoned/murmur2/blob/5eccf83ccaa20a756d36f0688ac8b4fa94b09737/MurmurHash2.c#L74
// Slightly modified.
uint64_t
MurmurHash64A ( const void * key, int len, uint64_t seed )
{
  const uint64_t m = 0xc6a4a7935bd1e995LLU;
  const int r = 47;

  uint64_t h = seed ^ (len * m);

  const uint64_t * data = (const uint64_t *)key;
  const uint64_t * end = data + (len/8);

  while(data != end)
    {
      uint64_t k = *data++;

      k *= m;
      k ^= k >> r;
      k *= m;

      h ^= k;
      h *= m;
    }

  const unsigned char * data2 = (const unsigned char*)data;

  switch(len & 7)
    {
    case 7: h ^= ((uint64_t) data2[6]) << 48; // fallthrough
    case 6: h ^= ((uint64_t) data2[5]) << 40; // fallthrough
    case 5: h ^= ((uint64_t) data2[4]) << 32; // fallthrough
    case 4: h ^= ((uint64_t) data2[3]) << 24; // fallthrough
    case 3: h ^= ((uint64_t) data2[2]) << 16; // fallthrough
    case 2: h ^= ((uint64_t) data2[1]) << 8;  // fallthrough
    case 1: h ^= ((uint64_t) data2[0]);
      h *= m;
    };

  h ^= h >> r;
  h *= m;
  h ^= h >> r;

  return h;
}

size_t
murmur2_hash(const void *data, size_t size)
{
  return MurmurHash64A(data, size, 0xBEEFCAFEDEADE621);
}
