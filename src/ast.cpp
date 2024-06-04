struct Ast
{
  struct Symbol;
  struct Expr;
  struct Stmt;

  using ArenaAllocator = NotStd::ArenaAllocator;
  using LineInfo = Lexer::LineInfo;
  using StringPool = Lexer::StringPool;
  using SymbolList = NotStd::LinkedList<Symbol *>;
  using ExprList = NotStd::LinkedList<Expr *>;
  using StmtList = NotStd::LinkedList<Stmt *>;

  struct SymbolKey
  {
    std::string_view name;
  };

  struct Symbol
  {
    struct Identifier
    {
      Expr *type, *value;
    };

    struct Variable
    {
      Expr *pattern, *type, *value;
    };

    enum Tag
    {
      _Identifier,
      _Variable,
    };

    union Data
    {
      Symbol::Identifier Identifier;
      Symbol::Variable Variable;
    };

    std::string_view name;
    LineInfo line_info;
    Tag tag;
    Data as;
  };

  struct Type
  {
    struct Int
    {
      u16 bits;
      bool is_signed;
    };

    enum Tag
    {
      _Bool,
      _Int,
    };

    union Data
    {
      Type::Int Int;
    };

    Tag tag;
    Data as;
  };

  struct Expr
  {
    struct BinaryOp
    {
      enum Tag
      {
        Or,
        And,
        Eq,
        Neq,
        Lt,
        Leq,
        Gt,
        Geq,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
      };

      Tag tag;
      Expr *lhs, *rhs;
      LineInfo line_info;
    };

    struct UnaryOp
    {
      enum Tag
      {
        Not,
        Plus,
        Minus,
      };

      Tag tag;
      Expr *subexpr;
    };

    struct Cast
    {
      Expr *type, *expr;
    };

    enum Tag
    {
      _Binary_Op,
      _Unary_Op,
      _Cast,
      _Type,
      _Boolean,
      _Integer,
      _Symbol,
      _Identifier,
    };

    union Data
    {
      Expr::BinaryOp Binary_Op;
      Expr::UnaryOp Unary_Op;
      Expr::Cast Cast;
      Ast::Type Type;
      bool Boolean;
      u64 Integer;
      Ast::Symbol *Symbol;
      std::string_view Identifier;
    };

    using Flag = u8;
    static constexpr Flag IS_LVALUE = 0x1;
    static constexpr Flag TYPE_HAS_ZERO_BITS = 0x2;

    std::string type_to_string()
    {
      static constexpr auto go = [](Ast::Expr *expr, std::string &dst)
      {
        assert(expr->tag == Ast::Expr::_Type);
        switch (expr->as.Type.tag)
        {
        case Type::_Bool:
        {
          dst += "bool";
        } break;
        case Type::_Int:
        {
          auto &Int = expr->as.Type.as.Int;
          dst += Int.is_signed ? 'i' : 'u';
          dst += std::to_string(Int.bits);
        } break;
        }
      };

      auto result = std::string{ };
      result.reserve(64);
      go(this, result);
      return result;
    }

    LineInfo line_info;
    Tag tag;
    Data as;
    Flag flags;
  };

  struct Stmt
  {
    struct Assign
    {
      Expr *lhs, *rhs;
    };

    enum Tag
    {
      _Assign,
      _Symbol,
      _Expr,
    };

    union Data
    {
      Stmt::Assign Assign;
      Ast::Symbol *Symbol;
      Ast::Expr *Expr;
    };

    LineInfo line_info;
    Tag tag;
    Data as;
  };

  struct SymbolTable
  {
    struct Hash
    {
      size_t operator()(SymbolKey key) const
      {
        auto hasher = std::hash<std::string_view>{};
        auto hash = hasher(key.name);
        return hash;
      }
    };

    struct Equal
    {
      bool operator()(SymbolKey key0, SymbolKey key1) const
      {
        return key0.name == key1.name;
      }
    };

    using Map = std::unordered_map<SymbolKey, Symbol *, Hash, Equal>;

    std::pair<Map::iterator, bool> insert(SymbolKey key)
    {
      auto it = map.find(key);
      if (it != map.end())
        return { it, false };
      else
      {
        auto [string_it, _] = string_pool.emplace(key.name);
        auto [it, was_inserted] = map.emplace(SymbolKey{ .name = *string_it }, nullptr);
        assert(was_inserted);
        return { it, true };
      }
    }

    Map map;
    StringPool string_pool;
  };

  template <typename T>
  T *alloc(size_t count = 1)
  {
    auto data = arena.alloc<T>(count);
    if (!data)
      abort();
    return data;
  }

  StmtList stmt_list;
  SymbolTable symbol_table;
  ArenaAllocator arena;

  std::string_view filepath;
};
