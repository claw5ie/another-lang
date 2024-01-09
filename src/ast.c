typedef struct Ast Ast;
struct Ast
{
  LinkedList stmts;
  Arena arena;
};

typedef struct AstExpr AstExpr;
typedef AstExpr AstType;

typedef struct AstSymbolVariable AstSymbolVariable;
struct AstSymbolVariable
{
  AstType *type;
  AstExpr *expr;
};

typedef union AstSymbolData AstSymbolData;
union AstSymbolData
{
  AstSymbolVariable Variable;
};

enum AstSymbolTag
  {
    Ast_Symbol_Variable,
  };
typedef enum AstSymbolTag AstSymbolTag;

typedef struct AstSymbol AstSymbol;
struct AstSymbol
{
  AstSymbolTag tag;
  AstSymbolData as;
  StringView name;
  LineInfo line_info;
};

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
    Ast_Expr_Unary_Op_Ref,
    Ast_Expr_Unary_Op_Deref,
  };
typedef enum AstExprUnaryOpTag AstExprUnaryOpTag;

typedef struct AstExprUnaryOp AstExprUnaryOp;
struct AstExprUnaryOp
{
  AstExprUnaryOpTag tag;
  AstExpr *subexpr;
};

typedef struct AstExprTypeInt AstExprTypeInt;
struct AstExprTypeInt
{
  u16 bits;
  bool is_signed;
};

typedef union AstExprData AstExprData;
union AstExprData
{
  AstExprBinaryOp Binary_Op;
  AstExprUnaryOp Unary_Op;
  u64 Int64;
  bool Bool;
  AstExprTypeInt Type_Int;
  StringView Identifier;
};

enum AstExprTag
  {
    Ast_Expr_Binary_Op,
    Ast_Expr_Unary_Op,
    Ast_Expr_Type_Void,
    Ast_Expr_Type_Bool,
    Ast_Expr_Type_Int,
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

typedef LinkedList AstStmtBlock;

typedef struct AstStmtIf AstStmtIf;
struct AstStmtIf
{
  AstExpr *cond;
  AstStmtBlock if_true, if_false;
};

typedef struct AstStmtWhile AstStmtWhile;
struct AstStmtWhile
{
  AstExpr *cond;
  AstStmtBlock block;
  bool is_do_while;
};

typedef struct AstStmtAssign AstStmtAssign;
struct AstStmtAssign
{
  AstExpr *lhs, *rhs;
};

typedef union AstStmtData AstStmtData;
union AstStmtData
{
  AstStmtBlock Block;
  AstStmtIf If;
  AstStmtWhile While;
  AstStmtAssign Assign;
  AstSymbol *Symbol;
  AstExpr *Expr;
};

enum AstStmtTag
  {
    Ast_Stmt_Block,
    Ast_Stmt_If,
    Ast_Stmt_While,
    Ast_Stmt_Break,
    Ast_Stmt_Continue,
    Ast_Stmt_Assign,
    Ast_Stmt_Symbol,
    Ast_Stmt_Expr,
  };
typedef enum AstStmtTag AstStmtTag;

typedef struct AstStmt AstStmt;
struct AstStmt
{
  AstStmtTag tag;
  AstStmtData as;
  LineInfo line_info;
};
