struct Ast
{
  struct Expr;

  using ArenaAllocator = NotStd::ArenaAllocator;
  using LineInfo = Lexer::LineInfo;
  using ExprList = NotStd::LinkedList<Expr *>;

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

    enum Tag
    {
      _Binary_Op,
      _Unary_Op,
      _Type,
      _Boolean,
      _Integer,
      _Identifier,
    };

    union Data
    {
      Expr::BinaryOp Binary_Op;
      Expr::UnaryOp Unary_Op;
      Ast::Type Type;
      bool Boolean;
      u64 Integer;
      std::string_view Identifier;
    };

    LineInfo line_info;
    Tag tag;
    Data as;

    bool is_same_type(Ast::Expr *other)
    {
      assert(this->tag == Ast::Expr::_Type
             && other->tag == Ast::Expr::_Type);
      return this->as.Type.tag == other->as.Type.tag;
    }

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
  std::string_view filepath;
};
