typedef struct Ast Ast;
struct Ast
{
  LinkedList globals;
  Arena arena;
  HashTable symbols;
};

typedef struct AstExpr AstExpr;
typedef AstExpr AstType;
typedef LinkedList AstStmtBlock;

typedef u32 ScopeId;

typedef struct AstSymbolKey AstSymbolKey;
struct AstSymbolKey
{
  StringView name;
  ScopeId scope;
};

typedef struct AstSymbolVariable AstSymbolVariable;
struct AstSymbolVariable
{
  AstType *type;
  AstExpr *expr;
};

typedef struct AstSymbolParameter AstSymbolParameter;
struct AstSymbolParameter
{
  AstType *type;
  bool has_name;
};

typedef struct AstSymbolProcedure AstSymbolProcedure;
struct AstSymbolProcedure
{
  LinkedList params;
  AstType *return_type;
  AstStmtBlock block;
};

typedef struct AstSymbolStructField AstSymbolStructField;
struct AstSymbolStructField
{
  AstType *type;
};

typedef struct AstSymbolStruct AstSymbolStruct;
struct AstSymbolStruct
{
  LinkedList fields;
};

typedef struct AstSymbolEnumValue AstSymbolEnumValue;
struct AstSymbolEnumValue
{
  int dummy;
};

typedef struct AstSymbolEnum AstSymbolEnum;
struct AstSymbolEnum
{
  LinkedList values;
};

typedef struct AstSymbolAlias AstSymbolAlias;
struct AstSymbolAlias
{
  AstType *type;
};

typedef union AstSymbolData AstSymbolData;
union AstSymbolData
{
  AstSymbolVariable Variable;
  AstSymbolParameter Parameter;
  AstSymbolProcedure Procedure;
  AstSymbolStruct Struct;
  AstSymbolStruct Union;
  AstSymbolEnum Enum;
  AstSymbolAlias Alias;
  AstSymbolStructField Struct_Field;
  AstSymbolEnumValue Enum_Value;
};

enum AstSymbolTag
  {
    Ast_Symbol_Variable,
    Ast_Symbol_Parameter,
    Ast_Symbol_Procedure,
    Ast_Symbol_Struct,
    Ast_Symbol_Union,
    Ast_Symbol_Enum,
    Ast_Symbol_Alias,
    Ast_Symbol_Struct_Field,
    Ast_Symbol_Enum_Value,
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

typedef struct AstExprArrayAccess AstExprArrayAccess;
struct AstExprArrayAccess
{
  AstExpr *lhs, *index;
};

typedef struct AstExprTypeInt AstExprTypeInt;
struct AstExprTypeInt
{
  u16 bits;
  bool is_signed;
};

typedef struct AstExprTypeProc AstExprTypeProc;
struct AstExprTypeProc
{
  LinkedList params;
  AstType *return_type;
};

typedef struct AstExprCall AstExprCall;
struct AstExprCall
{
  AstExpr *lhs;
  LinkedList args;
};

typedef struct AstExprFieldAccess AstExprFieldAccess;
struct AstExprFieldAccess
{
  AstExpr *lhs;
  StringView name;
};

typedef struct AstExprCast2 AstExprCast2;
struct AstExprCast2
{
  AstExpr *type;
  AstExpr *expr;
};

typedef struct AstExprDesignator AstExprDesignator;
struct AstExprDesignator
{
  StringView name;
  AstExpr *expr;
};

typedef union AstExprData AstExprData;
union AstExprData
{
  AstExprBinaryOp Binary_Op;
  AstExprUnaryOp Unary_Op;
  AstExprArrayAccess Array_Access;
  AstExprTypeInt Type_Int;
  AstExprTypeProc Type_Proc;
  AstSymbolStruct Type_Struct;
  AstSymbolStruct Type_Union;
  AstSymbolEnum Type_Enum;
  AstExprCall Call;
  AstExprFieldAccess Field_Access;
  AstExpr *Cast1;
  AstExprCast2 Cast2;
  u64 Int64;
  bool Bool;
  AstExprDesignator Designator;
  StringView Identifier;
};

enum AstExprTag
  {
    Ast_Expr_Binary_Op,
    Ast_Expr_Unary_Op,
    Ast_Expr_Array_Access,
    Ast_Expr_Type_Void,
    Ast_Expr_Type_Bool,
    Ast_Expr_Type_Int,
    Ast_Expr_Type_Proc,
    Ast_Expr_Type_Struct,
    Ast_Expr_Type_Union,
    Ast_Expr_Type_Enum,
    Ast_Expr_Call,
    Ast_Expr_Field_Access,
    Ast_Expr_Cast1,
    Ast_Expr_Cast2,
    Ast_Expr_Int64,
    Ast_Expr_Bool,
    Ast_Expr_Designator,
    Ast_Expr_Null,
    Ast_Expr_Identifier,
  };
typedef enum AstExprTag AstExprTag;

struct AstExpr
{
  AstExprTag tag;
  AstExprData as;
  LineInfo line_info;
};

typedef struct AstStmt AstStmt;

typedef struct AstStmtIf AstStmtIf;
struct AstStmtIf
{
  AstExpr *cond;
  AstStmt *if_true, *if_false;
};

typedef struct AstStmtWhile AstStmtWhile;
struct AstStmtWhile
{
  AstExpr *cond;
  AstStmt *block;
  bool is_do_while;
};

typedef struct AstStmtSwitch AstStmtSwitch;
struct AstStmtSwitch
{
  AstExpr *cond;
  AstStmtBlock cases;
  AstStmt *default_case;
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
  AstExpr *Return_Expr;
  AstStmtSwitch Switch;
  AstExpr *Case;
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
    Ast_Stmt_Return_Nothing,
    Ast_Stmt_Return_Expr,
    Ast_Stmt_Switch,
    Ast_Stmt_Case,
    Ast_Stmt_Assign,
    Ast_Stmt_Symbol,
    Ast_Stmt_Expr,
  };
typedef enum AstStmtTag AstStmtTag;

struct AstStmt
{
  AstStmtTag tag;
  AstStmtData as;
  LineInfo line_info;
};
