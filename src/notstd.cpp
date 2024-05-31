namespace NotStd
{
  template<typename T>
  struct LinkedList
  {
    struct Node
    {
      Node *next = nullptr, *prev = nullptr;
      T data;
    };

    void insert_last(Node *node)
    {
      if (count > 0)
      {
        node->next = nullptr;
        node->prev = last;
        last->next = node;
        last = node;
        ++count;
      }
      else
      {
        node->next = nullptr;
        node->prev = nullptr;
        first = last = node;
        count = 1;
      }
    }

    Node *first = nullptr, *last = nullptr;
    size_t count = 0;
  };

  struct ArenaAllocator
  {
    static constexpr size_t DEFAULT_BLOCK_SIZE = 4 * 1024;

    struct Block
    {
      Block *next;
      size_t size, capacity;
      char *data;
    };

    template<typename T>
    T *alloc(size_t count = 1)
    {
      return (T *)alloc_bytes(count * sizeof(T));
    }

    void *alloc_bytes(size_t size)
    {
      size = Utils::align_by_void_pointer(size);

      auto block = first;
      for (; block; block = block->next)
        if (block->size + size <= block->capacity)
          break;

      if (!block)
      {
        constexpr auto block_header_size = Utils::align_by_void_pointer(sizeof(Block));
        auto block_size = size < DEFAULT_BLOCK_SIZE ? DEFAULT_BLOCK_SIZE : size;

        block = (Block *)new (std::nothrow) char[block_header_size + block_size];
        if (!block)
          return nullptr;
        *block = Block{
          .next = nullptr,
          .size = 0,
          .capacity = block_size,
          .data = (char *)block + block_header_size,
        };

        if (last)
          last = last->next = block;
        else
          first = last = block;
      }

      auto data = &block->data[block->size];
      block->size += size;

      return data;
    }

    Block *first = nullptr, *last = nullptr;
  };
}
