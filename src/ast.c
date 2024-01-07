typedef struct Ast Ast;
struct Ast
{
  LinkedList exprs;
  Arena arena;
};

typedef struct AstExpr AstExpr;

enum AstExprBinaryOpTag
  {
    Ast_Expr_Binary_Op_Or,
    Ast_Expr_Binary_Op_And,
    Ast_Expr_Binary_Op_Eq,
    Ast_Expr_Binary_Op_Neq,
    Ast_Expr_Binary_Op_Leq,
    Ast_Expr_Binary_Op_Geq,
    Ast_Expr_Binary_Op_Lt,
    Ast_Expr_Binary_Op_Gt,
    Ast_Expr_Binary_Op_Add,
    Ast_Expr_Binary_Op_Sub,
    Ast_Expr_Binary_Op_Mul,
    Ast_Expr_Binary_Op_Div,
    Ast_Expr_Binary_Op_Mod,
  };
typedef enum AstExprBinaryOpTag AstExprBinaryOpTag;

typedef struct AstExprBinaryOp AstExprBinaryOp;
struct AstExprBinaryOp
{
  AstExprBinaryOpTag tag;
  AstExpr *lhs, *rhs;
};

enum AstExprUnaryOpTag
  {
    Ast_Expr_Unary_Op_Neg,
    Ast_Expr_Unary_Op_Not,
  };
typedef enum AstExprUnaryOpTag AstExprUnaryOpTag;

typedef struct AstExprUnaryOp AstExprUnaryOp;
struct AstExprUnaryOp
{
  AstExprUnaryOpTag tag;
  AstExpr *subexpr;
};

typedef union AstExprData AstExprData;
union AstExprData
{
  AstExprBinaryOp Binary_Op;
  AstExprUnaryOp Unary_Op;
  u64 Int64;
  bool Bool;
  StringView Identifier;
};

enum AstExprTag
  {
    Ast_Expr_Binary_Op,
    Ast_Expr_Unary_Op,
    Ast_Expr_Int64,
    Ast_Expr_Bool,
    Ast_Expr_Identifier,
  };
typedef enum AstExprTag AstExprTag;

struct AstExpr
{
  AstExprTag tag;
  AstExprData as;
  LineInfo line_info;
};
