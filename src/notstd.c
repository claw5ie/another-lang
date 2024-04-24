typedef struct StringView StringView;
struct StringView
{
  const char *data;
  size_t count;
};

#define STRING_VIEW_FROM_CSTRING(string) (StringView){ .data = string, .count = sizeof(string) - 1 }
#define FORMAT_STRING_VIEW(view) (int)(view).count, (view).data

bool
are_views_equal(StringView v0, StringView v1)
{
  return v0.count == v1.count && memcmp(v0.data, v1.data, v0.count) == 0;
}

bool
is_prefix(StringView prefix, StringView rest)
{
  return prefix.count <= rest.count && memcmp(prefix.data, rest.data, prefix.count) == 0;
}

unsigned long long
view_to_unsigned(StringView text)
{
  unsigned long long result = 0;
  while (text.count-- > 0)
    result = 10 * result + (*text.data++ - '0');
  return result;
}

#define LINKED_LIST_PUT_NODE_DATA(Type, node, node_data) *(Type *)(&(node)->data[0]) = node_data
#define LINKED_LIST_GET_NODE_DATA(Type, node) (*(Type *)(&(node)->data[0]))

typedef struct LinkedListNode LinkedListNode;
struct LinkedListNode
{
  LinkedListNode *next, *prev;
  char data[];
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

void
linked_list_remove_node(LinkedList *list, LinkedListNode *node)
{
  assert(list->count > 0);

  --list->count;

  LinkedListNode *next = node->next;
  LinkedListNode *prev = node->prev;

  if (next)
    next->prev = prev;
  else
    list->last = prev;

  if (prev)
    prev->next = next;
  else
    list->first = next;
}

void
linked_list_concat(LinkedList *self, LinkedList *other)
{
  if (self->count == 0)
    *self = *other;
  else if (other->count == 0)
    *other = *self;
  else
    {
      self->last->next = other->first;
      other->first->prev = self->last;

      self->last = other->last;
      other->first = self->first;

      size_t new_count = self->count + other->count;
      self->count = new_count;
      other->count = new_count;
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
  for (; block; block = block->next)
    if (block->size + size <= block->capacity)
      break;

  if (!block)
    {
      size_t capacity = size <= ARENA_BLOCK_DEFAULT_CAPACITY ? ARENA_BLOCK_DEFAULT_CAPACITY : size;
      block = malloc(sizeof(ArenaBlock) + capacity);
      if (!block)
        abort();
      *block = (ArenaBlock){
        .size = 0,
        .capacity = capacity,
        .next = NULL,
      };

      if (arena->last)
        arena->last = arena->last->next = block;
      else
        arena->first = arena->last = block;
    }

  void *ptr = &block->data[block->size];
  block->size += size;

  return ptr;
}

typedef size_t (*HashTableKeyHashFunc)(void *key);
typedef bool (*HashTableAreKeysEqualFunc)(void *key0, void *key1);

typedef struct HashTableNode HashTableNode;
struct HashTableNode
{
  void *key;
  void *data;
  HashTableNode *next;
  size_t hash;
};

typedef struct HashTable HashTable;
struct HashTable
{
  HashTableNode **data;
  size_t count, capacity;
  size_t key_size, data_size;
  HashTableKeyHashFunc key_hash;
  HashTableAreKeysEqualFunc are_keys_equal;
};

void
hash_table_maybe_expand(HashTable *t)
{
  if (t->capacity == 0 || (double)t->count / t->capacity > 0.75)
    {
      // 'NULL' is not guaranteed to be 0, I think, so can't calloc??
      size_t new_capacity = 2 * (t->capacity + 1);
      HashTableNode **new_data = calloc(new_capacity, sizeof(*new_data));
      if (!new_data)
        abort();

      for (size_t i = t->capacity; i-- > 0; )
        {
          HashTableNode *node = t->data[i];
          while (node)
            {
              size_t index = node->hash % new_capacity;

              HashTableNode *next = node->next;
              node->next = new_data[index];
              new_data[index] = node;

              node = next;
            }
        }

      free(t->data);
      t->data = new_data;
      t->capacity = new_capacity;
    }
}

void *
hash_table_find(HashTable *t, void *key)
{
  if (t->capacity > 0)
    {
      HashTableNode *node = t->data[t->key_hash(key) % t->capacity];
      for (; node; node = node->next)
        if (t->are_keys_equal(node->key, key))
          return node->data;
    }

  return NULL;
}

void *
hash_table_insert(HashTable *t, void *key, bool *was_inserted)
{
  assert(sizeof(HashTableNode) % sizeof(void *) == 0
         && t->key_size % sizeof(void *) == 0);

  *was_inserted = false;

  {
    void *data = hash_table_find(t, key);
    if (data)
      return data;
  }

  hash_table_maybe_expand(t);

  size_t hash = t->key_hash(key);
  size_t index = hash % t->capacity;
  HashTableNode *node = malloc(sizeof(*node) + t->key_size + t->data_size);
  if (!node)
    abort();
  node->key = (char *)node + sizeof(*node);
  node->data = (char *)node->key + t->key_size;
  node->hash = hash;
  node->next = t->data[index];
  t->data[index] = node;
  ++t->count;

  memcpy(node->key, key, t->key_size);

  *was_inserted = true;

  return node->data;
}
