typedef struct StringView StringView;
struct StringView
{
  const char *data;
  size_t count;
};

#define STRING_VIEW_FROM_CSTRING(string) (StringView){ .data = string, .count = sizeof(string) - 1 }
#define FORMAT_STRING_VIEW(view) (int)(view).count, (view).data

bool
are_string_views_equal(StringView v0, StringView v1)
{
  return v0.count == v1.count && memcmp(v0.data, v1.data, v0.count) == 0;
}

bool
is_prefix(StringView prefix, StringView rest)
{
  return prefix.count <= rest.count && memcmp(prefix.data, rest.data, prefix.count) == 0;
}

typedef struct LinkedListNode LinkedListNode;
struct LinkedListNode
{
  void *data;
  LinkedListNode *next, *prev;
};

typedef struct LinkedList LinkedList;
struct LinkedList
{
  LinkedListNode *first, *last;
  size_t count;
};

void
linked_list_insert_last(LinkedList *list, LinkedListNode *node)
{
  if (list->count != 0)
    {
      node->next = NULL;
      node->prev = list->last;
      list->last->next = node;
      list->last = node;
      ++list->count;
    }
  else
    {
      node->next = NULL;
      node->prev = NULL;
      list->first = list->last = node;
      list->count = 1;
    }
}

#define ARENA_BLOCK_DEFAULT_CAPACITY (4 * 1024)

typedef struct ArenaBlock ArenaBlock;
struct ArenaBlock
{
  size_t size;
  size_t capacity;
  ArenaBlock *next;
  char data[];
};

typedef struct Arena Arena;
struct Arena
{
  ArenaBlock *first, *last;
};

void *
arena_malloc(Arena *arena, size_t size)
{
  // Align by 'sizeof(void *)'.
  size += sizeof(void *) - 1;
  size -= size % sizeof(void *);

  ArenaBlock *block = arena->first;
  for (; block != NULL; block = block->next)
    if (block->size + size <= block->capacity)
      break;

  if (block == NULL)
    {
      size_t capacity = size <= ARENA_BLOCK_DEFAULT_CAPACITY ? ARENA_BLOCK_DEFAULT_CAPACITY : size;
      block = malloc(sizeof(ArenaBlock) + capacity);
      if (block == NULL)
        abort();
      *block = (ArenaBlock){
        .size = 0,
        .capacity = capacity,
        .next = NULL,
      };

      if (arena->last != NULL)
        {
          arena->last->next = block;
          arena->last = block;
        }
      else
        arena->first = arena->last = block;
    }

  void *ptr = &block->data[block->size];
  block->size += size;

  return ptr;
}
