struct Ast
{
  struct Expr;

  using ArenaAllocator = NotStd::ArenaAllocator;
  using LineInfo = Lexer::LineInfo;
  using ExprList = NotStd::LinkedList<Expr *>;

  struct Expr
  {
    struct BinaryOp
    {
      enum Tag
      {
        Add,
        Sub,
        Mul,
        Div,
      };

      Tag tag;
      Expr *lhs, *rhs;
    };

    enum Tag
    {
      _Binary_Op,
      _Integer,
    };

    union Data
    {
      Expr::BinaryOp Binary_Op;
      u64 Integer;
    };

    LineInfo line_info;
    Tag tag;
    Data as;
  };

  template <typename T>
  T *alloc(size_t count = 1)
  {
    auto data = arena.alloc<T>(count);
    if (!data)
      abort();
    return data;
  }

  ExprList exprs;
  ArenaAllocator arena;
};
