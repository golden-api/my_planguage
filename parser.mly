%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> IDENT
%token <string> FILE
%token VECTOR MATRIX
%token INPUT PRINT
%token AND OR NOT
%token ADD SUB MUL DIV MOD
%token LCURLY RCURLY LPAREN RPAREN
%token LSQ RSQ ML_SQ MR_SQ
%token LT GT LTE GTE EQ ASSIGN
%token SEMICOLON COMMA COMMENT
%token IF THEN ELSE WHILE FOR TO DO DONE
%token CONTINUE BREAK
%token MAGNITUDE ANGLE DIMENSION DOT
%token TRANSPOSE DETERMINANT INVERSE
%token EOF INT_T FLOAT_T BOOL_T

%start program
%type <Ast.program> program

%nonassoc ELSE
%nonassoc THEN
%right ASSIGN NOT
%left OR AND EQ LT GT LTE GTE
%left ADD SUB MUL DIV MOD

%%

program:
  statement_list EOF { $1 }

statement_list:
  top_statement statement_list { $1 :: $2 }
| /* empty */ { [] }

top_statement:
  statement SEMICOLON { $1 }

statement:  
    VECTOR INT IDENT ASSIGN vector { Vector($2, $3, $5) }
  | MATRIX INT COMMA INT IDENT ASSIGN matrix { Matrix($2, $4, $5, $7) }
  | IF expr THEN statement ELSE statement { If($2, $4, Some($6)) }
  | LCURLY statement_list RCURLY { Block($2) }
  | WHILE expr DO statement DONE { While($2, $4) }
  | FOR IDENT ASSIGN expr TO expr DO statement DONE { For($2, $4, $6, $8) }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | BREAK { Break }
  | CONTINUE { Continue }
  | simple_statement { $1 }

simple_statement:
  expr_stmt { $1 }

expr_stmt:
  assign_expr { Expr($1) }

expr:
  assign_expr { $1 }

assign_expr:
    or_expr { $1 }
  | IDENT ASSIGN assign_expr { Assign($1, $3) }
  | IDENT LSQ index_list RSQ ASSIGN assign_expr { AssignIdx(Index(Var($1),$3),$6)}

or_expr:
    and_expr { $1 }
  | or_expr OR and_expr { BinOp($1, Or, $3) }

and_expr:
    eq_expr { $1 }
  | and_expr AND eq_expr { BinOp($1, And, $3) }

eq_expr:
    comp_expr { $1 }
  | eq_expr EQ comp_expr { BinOp($1, Eq, $3) }

comp_expr:
    add_expr { $1 }
  | comp_expr LT add_expr { BinOp($1, Lt, $3) }
  | comp_expr GT add_expr { BinOp($1, Gt, $3) }
  | comp_expr LTE add_expr { BinOp($1, Lte, $3) }
  | comp_expr GTE add_expr { BinOp($1, Gte, $3) }

add_expr:
    mul_expr { $1 }
  | add_expr ADD mul_expr { BinOp($1, Add, $3) }
  | add_expr SUB mul_expr { BinOp($1, Sub, $3) }

mul_expr:
    post_expr { $1 }
  | mul_expr MUL post_expr { BinOp($1, Mul, $3) }
  | mul_expr DIV post_expr { BinOp($1, Div, $3) }
  | mul_expr MOD post_expr { BinOp($1, Mod, $3) }
  | mul_expr DOT post_expr { BinOp($1, Dot, $3) }
  | mul_expr ANGLE post_expr { BinOp($1, Angle, $3) }

post_expr:
  |  unary_expr { $1 }
  
unary_expr:
    primary_expr { $1 }
  | NOT unary_expr { UnOp(Not, $2) }
  | SUB unary_expr %prec NOT { UnOp(Neg, $2) }

primary_expr:
    BOOL { ConstBool($1) }
  | INT { ConstInt($1) }
  | FLOAT { ConstFloat($1) }
  | IDENT { Var($1) }
  | LPAREN expr RPAREN { $2 }
  | IDENT LSQ index_list RSQ { Index(Var($1), $3) }
  | MAGNITUDE LPAREN expr RPAREN { UnOp(Magnitude, $3) }
  | DIMENSION LPAREN expr RPAREN { UnOp(Dimension, $3) }
  | TRANSPOSE LPAREN expr RPAREN { UnOp(Transpose, $3) }
  | DETERMINANT LPAREN expr RPAREN { UnOp(Determinant, $3) }
  | INVERSE LPAREN expr RPAREN { UnOp(Inverse, $3) }
  | type_specifier INPUT LPAREN RPAREN { Input($1,None) }
  | type_specifier INPUT LPAREN FILE RPAREN { Input($1,Some($4)) }
  | matrix { $1 }
  | vector { $1 }

index_list:
  | expr                  { [$1] }  | expr COMMA expr       { [$1; $3] }          

type_specifier:
  | INT_T  { TInt }  | FLOAT_T  { TFloat }  | BOOL_T { TBool } | VECTOR  { TV } | MATRIX { TM }

vector:
  LSQ vector_elems RSQ { ConstV($2) }

vector_elems:
    expr COMMA vector_elems { $1 :: $3 }
  | expr { [$1] }

matrix:
  ML_SQ matrix_rows MR_SQ { ConstM($2) }

matrix_rows:
    matrix_row { [$1] }
  | matrix_row COMMA matrix_rows { $1 :: $3 }

matrix_row:
  LSQ vector_elems RSQ { $2 }

%%
