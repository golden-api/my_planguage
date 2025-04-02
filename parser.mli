type token =
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | IDENT of (string)
  | FILE of (string)
  | VECTOR
  | MATRIX
  | INPUT
  | PRINT
  | AND
  | OR
  | NOT
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | LSQ
  | RSQ
  | ML_SQ
  | MR_SQ
  | LT
  | GT
  | LTE
  | GTE
  | EQ
  | ASSIGN
  | SEMICOLON
  | COMMA
  | COMMENT
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | TO
  | DO
  | DONE
  | CONTINUE
  | BREAK
  | MAGNITUDE
  | ANGLE
  | DIMENSION
  | DOT
  | TRANSPOSE
  | DETERMINANT
  | INVERSE
  | EOF
  | INT_T
  | FLOAT_T
  | BOOL_T

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
