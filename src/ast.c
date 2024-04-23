typedef struct AstExpr AstExpr;

#define MAX_BITS_IN_INT 64

typedef u8 AstFlagsType;
#define AST_FLAG_IS_IN_LOOP 0x1
#define AST_FLAG_REJECT_VOID_TYPE 0x2
#define AST_FLAG_SKIP_CYCLE 0x4
#define AST_FLAG_IS_TYPECHECKING_ENUM 0x8
#define AST_FLAG_DONT_CAST_EXPR_LIST 0x10

typedef struct Ast Ast;
struct Ast
{
  LinkedList globals;
  Arena arena;
  HashTable symbols;
  const char *filepath;

  AstFlagsType flags;
  AstExpr *return_type;
};

enum AstTypecheckingStage
  {
    Ast_Stage_Not_Typechecked = 0, // Make sure that is the default value.
    Ast_Stage_Being_Typechecked,
    Ast_Stage_Is_Typechecked,
  };
typedef enum AstTypecheckingStage AstTypecheckingStage;

typedef struct AstSymbol AstSymbol;
typedef LinkedList AstStmtBlock;

typedef struct Scope Scope;
struct Scope
{
  Scope *parent;
};

typedef struct AstSymbolKey AstSymbolKey;
struct AstSymbolKey
{
  StringView name;
  Scope *scope;
};

typedef struct AstSymbolVariable AstSymbolVariable;
struct AstSymbolVariable
{
  AstExpr *type;
  AstExpr *expr;
};

typedef struct AstSymbolParameter AstSymbolParameter;
struct AstSymbolParameter
{
  AstExpr *type;
  bool has_name;
};

typedef struct AstSymbolProcedure AstSymbolProcedure;
struct AstSymbolProcedure
{
  AstExpr *type;
  AstStmtBlock block;
};

typedef struct AstSymbolStructField AstSymbolStructField;
struct AstSymbolStructField
{
  AstExpr *type;
};

typedef struct AstSymbolEnumValue AstSymbolEnumValue;
struct AstSymbolEnumValue
{
  AstExpr *type; // Points to itself.
  AstExpr *expr;
  AstSymbol *depends_on;
};

enum AstSymbolResolvingStage
  {
    Ast_Symbol_Flag_Not_Resolved,
    Ast_Symbol_Flag_Being_Resolved,
    Ast_Symbol_Flag_Is_Resolved,
  };
typedef enum AstSymbolResolvingStage AstSymbolResolvingStage;

typedef union AstSymbolData AstSymbolData;
union AstSymbolData
{
  AstSymbolVariable Variable;
  AstSymbolParameter Parameter;
  AstSymbolProcedure Procedure;
  AstExpr *Type;
  AstSymbolStructField Struct_Field;
  AstSymbolEnumValue Enum_Value;
  AstExpr *Alias;
};

typedef u8 AstSymbolFlagsType;
#define AST_SYMBOL_FLAG_IS_UNPACKED 0x1
#define AST_SYMBOL_FLAG_VARIABLE_IS_TYPECHECKED 0x2

enum AstSymbolTag
  {
    Ast_Symbol_Variable,
    Ast_Symbol_Parameter,
    Ast_Symbol_Procedure,
    Ast_Symbol_Type,
    Ast_Symbol_Struct_Field,
    Ast_Symbol_Enum_Value,
    Ast_Symbol_Alias,
  };
typedef enum AstSymbolTag AstSymbolTag;

struct AstSymbol
{
  AstSymbolTag tag;
  AstSymbolData as;
  StringView name;
  LineInfo line_info;
  AstSymbolResolvingStage resolving_stage: 2;
  AstTypecheckingStage typechecking_stage: 2;
  AstSymbolFlagsType flags;
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
  AstExpr *return_type;
  Scope *scope;
};

typedef struct AstExprArrayAccess AstExprArrayAccess;
struct AstExprArrayAccess
{
  AstExpr *lhs, *index;
};

typedef struct AstExprTypeStruct AstExprTypeStruct;
struct AstExprTypeStruct
{
  LinkedList fields;
  Scope *scope;
};

typedef struct AstExprTypeEnum AstExprTypeEnum;
struct AstExprTypeEnum
{
  LinkedList values;
  Scope *scope;
};

typedef union AstExprTypeData AstExprTypeData;
union AstExprTypeData
{
  AstExprTypeInt Int;
  AstExpr *Pointer;
  AstExprTypeProc Proc;
  AstExprArrayAccess Array;
  AstExprTypeStruct Struct_Or_Union;
  AstExprTypeEnum Enum;
};

enum AstExprTypeTag
  {
    Ast_Expr_Type_Void,
    Ast_Expr_Type_Bool,
    Ast_Expr_Type_Int,
    Ast_Expr_Type_Generic_Int,
    Ast_Expr_Type_Pointer,
    Ast_Expr_Type_Proc,
    Ast_Expr_Type_Array,
    Ast_Expr_Type_Struct,
    Ast_Expr_Type_Union,
    Ast_Expr_Type_Enum,
  };
typedef enum AstExprTypeTag AstExprTypeTag;

typedef struct AstExprType AstExprType;
struct AstExprType
{
  AstExprTypeTag tag;
  AstExprTypeData as;
  AstSymbol *symbol;
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

typedef struct AstExprIdentifier AstExprIdentifier;
struct AstExprIdentifier
{
  StringView name;
  Scope *scope;
};

typedef union AstExprData AstExprData;
union AstExprData
{
  AstExprBinaryOp Binary_Op;
  AstExprUnaryOp Unary_Op;
  AstExprArrayAccess Array_Access;
  AstExprCall Call_Or_Type_Cons;
  AstExprFieldAccess Field_Access;
  AstExpr *Cast1;
  AstExprCast2 Cast2;
  AstExprType Type;
  u64 Int64;
  bool Bool;
  AstExprDesignator Designator;
  AstSymbol *Symbol;
  AstExprIdentifier Enum_Identifier;
  AstExprIdentifier Identifier;
};

typedef u8 AstExprFlagsType;
#define AST_EXPR_FLAG_IS_LVALUE 0x1

enum AstExprTag
  {
    Ast_Expr_Binary_Op,
    Ast_Expr_Unary_Op,
    Ast_Expr_Array_Access,
    Ast_Expr_Call,
    Ast_Expr_Type_Cons,
    Ast_Expr_Field_Access,
    Ast_Expr_Cast1,
    Ast_Expr_Cast2,
    Ast_Expr_Type,
    Ast_Expr_Int64,
    Ast_Expr_Bool,
    Ast_Expr_Null,
    Ast_Expr_Designator,
    Ast_Expr_Symbol,
    Ast_Expr_Enum_Identifier,
    Ast_Expr_Identifier,
  };
typedef enum AstExprTag AstExprTag;

struct AstExpr
{
  AstExprTag tag;
  AstExprData as;
  LineInfo line_info;
  AstTypecheckingStage typechecking_stage: 2;
  AstExprFlagsType flags;
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

typedef struct AstStmtCase AstStmtCase;
struct AstStmtCase
{
  AstExpr *expr;
  AstStmt *substmt;
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
  AstStmtCase Case;
  AstStmt *Default;
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
    Ast_Stmt_Default,
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

void *
ast_malloc(Ast *ast, size_t size)
{
  void *data = arena_malloc(&ast->arena, size);
  if (!data)
    abort();
  return data;
}
