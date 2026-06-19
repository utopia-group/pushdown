%{ 
  open Ast
  open Exns
%}

%token <string> STRING
%token <string> IDENT
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token          ADD              (* + *)
%token          SUB              (* - *)
%token          MUL              (* * *)
%token          DIV              (* / *)
%token          EQ               (* == *)
%token          GE               (* >= *)
%token          GT               (* > *)
%token          LE               (* <= *)
%token          LT               (* < *)
%token          AND              (* and *)
%token          OR               (* or *)
%token          NOT              (* not *)
%token          DECL             (* = *)
%token          BOOL_TYPE        (* bool *)
%token          INT_TYPE         (* int *)
%token          FLOAT_TYPE       (* float *)
%token          STRING_TYPE      (* str *)
%token          OPTION_TYPE      (* Optional *)
%token          LIST_TYPE        (* List *)
%token          FOLD             (* fold *)
%token          FILTER           (* filter *)
%token          INSERT           (* insert *)
%token          MATCH            (* match *)
%token          CASE             (* case *)
%token          LAMBDA           (* lambda *)
%token          FIX              (* fix *)
%token          NULL             (* None *)
%token          IF ELSE
%token          COMMA COLON
%token          LPAREN RPAREN LBRACKET RBRACKET
%token          EOF

%nonassoc COLON                  (* : *)
%right IF ELSE                   (* ... if ... else ... *)
%right OR                        (* or *)
%right AND                       (* and *)
%left EQ                         (* == *)
%left GE GT LE LT                (* >= > <= < *)
%left ADD SUB                    (* + - *)
%left MUL DIV                    (* * / *)
%right NOT                       (* not *)

%start start
%type <prog> start

%%

start:
  | stmts EOF
    { $1 }

stmts:
  | stmt stmts
    { $1 :: $2 }
  | stmt
    { [$1] }

stmt:
  | ident_def DECL expr
    { Decl ($1, None, $3) }
  | ident_def COLON LPAREN type_refs RPAREN DECL expr
    { Decl ($1, Some $4, $7) }
  | ident_def COLON LBRACKET type_refs RBRACKET DECL expr
    { Decl ($1, Some $4, $7) }

expr:
  | simple_expr
    { $1 }
  | expr ADD expr
    { Add ($1, $3) }
  | expr SUB expr
    { Sub ($1, $3) }
  | expr MUL expr
    { Mul ($1, $3) }
  | expr DIV expr
    { Div ($1, $3) }
  | expr EQ expr
    { Eq ($1, $3) }
  | expr GE expr
    { Ge ($1, $3) }
  | expr GT expr
    { Gt ($1, $3) }
  | expr LE expr
    { Le ($1, $3) }
  | expr LT expr
    { Lt ($1, $3) }
  | expr AND expr
    { And ($1, $3) }
  | expr OR expr
    { Or ($1, $3) }
  | NOT expr
    { Not $2 }
  | LPAREN type_refs RPAREN
    { Schema $2 }
  | LPAREN exprs RPAREN
    { Tuple $2 }
  | LBRACKET exprs RBRACKET
    { List ($2, None) }
  | LBRACKET type_refs RBRACKET
    { Schema $2 }
  | expr LBRACKET INT RBRACKET
    { Proj ($1, $3) }
  | expr LBRACKET INT COLON RBRACKET
    { if $3 = 1 then Tail $1 else raise (Compile_error (0, "Malformed program")) }
  | expr IF expr ELSE expr
    { If ($3, $1, $5) }
  | MATCH expr COLON cases
    { Match ($2, $4) }
  | LAMBDA params COLON expr
    { Lambda ($2, $4) }
  | FIX params COLON expr
    { Fix (Core.List.hd_exn $2, Core.List.tl_exn $2, $4) }
  | fun_name LPAREN exprs RPAREN
    { App ($1, $3) }

simple_expr:
  | value
    { $1 }
  | LPAREN expr RPAREN
    { $2 }

value:
  | BOOL
    { Bool $1 }
  | INT
    { Int $1 }
  | SUB INT
    { Int (-$2) }
  | FLOAT
    { Float $1 }
  | SUB FLOAT
    { Float (-. $2) }
  | STRING
    { String $1 }
  | ident_use
    { $1 }
  | NULL
    { Null None }

cases:
  | CASE value COLON expr cases
    { ($2, $4) :: $5 }
  | CASE value COLON expr
    { [($2, $4)] }

type_refs:
  | type_ref COMMA type_refs
    { $1 :: $3 }
  | type_ref COMMA
    { [$1] }
  | type_ref
    { [$1] }

type_ref:
  | INT_TYPE
    { IntType }
  | BOOL_TYPE
    { BoolType }
  | FLOAT_TYPE
    { FloatType }
  | STRING_TYPE
    { StringType }
  | OPTION_TYPE LBRACKET type_ref RBRACKET
    { OptionType $3 }
  | LIST_TYPE LBRACKET type_ref RBRACKET
    { ListType $3 }

exprs:
  | expr COMMA exprs
    { $1 :: $3 }
  | expr
    { [$1] }
  | { [] }

params:
  | ident_def COMMA params
    { $1 :: $3 }
  | ident_def
    { [$1] }
  | { [] }

fun_name:
  | FOLD
    { Fold }
  | FILTER
    { Filter }
  | INSERT
    { Insert }
  | ident_def
    { Other $1 }

ident_use:
  | ident_def
    { Var $1 }

ident_def:
  | IDENT
    { Ident $1 }
