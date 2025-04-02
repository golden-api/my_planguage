{
open Parser
exception LexerError of string
}

rule token = parse
| [' ' '\t' '\r' '\n']+ { token lexbuf }
| "//" [^'\n']* { token lexbuf }
| "if"           { IF } | "then"         { THEN } | "else"         { ELSE } 
| "while"        { WHILE } | "for"          { FOR } | "to"           { TO }
| "do"           { DO } | "done"         { DONE } | "continue"     { CONTINUE }| "break"        { BREAK }
| "vector"       { VECTOR } | "matrix"       { MATRIX }
| "magnitude"    { MAGNITUDE } | "angle"        { ANGLE }
| "dimension"    { DIMENSION } | "transpose"    { TRANSPOSE }
| "determinant"  { DETERMINANT } | "inverse"      { INVERSE }
| "dot"          { DOT }
| "input"        { INPUT } | "print"        { PRINT }
| "int" {INT_T} | "float" {FLOAT_T} | "bool" {BOOL_T}
| "true" | "TRUE" { BOOL true } | "false" | "FALSE" { BOOL false }
| "and"          { AND } | "or"           { OR } | "not"          { NOT }
| "+"            { ADD } | "-"            { SUB } | "*"            { MUL }
| "/"            { DIV } | "%"            { MOD }
| "{"            { LCURLY } | "}"            { RCURLY }
| "("            { LPAREN } | ")"            { RPAREN }
| "[["            { ML_SQ } | "]]"            { MR_SQ }
| "["            { LSQ } | "]"  { RSQ }
| "<="           { LTE } | ">="           { GTE } | "<"            { LT }
| ">"            { GT } | "=="           { EQ }
| ":="           { ASSIGN }
| ";"            { SEMICOLON }
| ","            { COMMA }
| ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+ ".txt" as s { FILE s }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
| ['0'-'9']+                        { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']+ '.' ['0'-'9']*          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| eof                                { EOF }
| _                                  {  raise (LexerError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
