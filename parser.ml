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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 63 "parser.ml"
let yytransl_const = [|
  262 (* VECTOR *);
  263 (* MATRIX *);
  264 (* INPUT *);
  265 (* PRINT *);
  266 (* AND *);
  267 (* OR *);
  268 (* NOT *);
  269 (* ADD *);
  270 (* SUB *);
  271 (* MUL *);
  272 (* DIV *);
  273 (* MOD *);
  274 (* LCURLY *);
  275 (* RCURLY *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LSQ *);
  279 (* RSQ *);
  280 (* ML_SQ *);
  281 (* MR_SQ *);
  282 (* LT *);
  283 (* GT *);
  284 (* LTE *);
  285 (* GTE *);
  286 (* EQ *);
  287 (* ASSIGN *);
  288 (* SEMICOLON *);
  289 (* COMMA *);
  290 (* COMMENT *);
  291 (* IF *);
  292 (* THEN *);
  293 (* ELSE *);
  294 (* WHILE *);
  295 (* FOR *);
  296 (* TO *);
  297 (* DO *);
  298 (* DONE *);
  299 (* CONTINUE *);
  300 (* BREAK *);
  301 (* MAGNITUDE *);
  302 (* ANGLE *);
  303 (* DIMENSION *);
  304 (* DOT *);
  305 (* TRANSPOSE *);
  306 (* DETERMINANT *);
  307 (* INVERSE *);
    0 (* EOF *);
  308 (* INT_T *);
  309 (* FLOAT_T *);
  310 (* BOOL_T *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* BOOL *);
  260 (* IDENT *);
  261 (* FILE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\008\000\009\000\
\007\000\010\000\010\000\010\000\011\000\011\000\013\000\013\000\
\014\000\014\000\015\000\015\000\015\000\015\000\015\000\016\000\
\016\000\016\000\017\000\017\000\017\000\017\000\017\000\017\000\
\018\000\019\000\019\000\019\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\012\000\012\000\021\000\021\000\021\000\
\021\000\021\000\005\000\022\000\022\000\006\000\023\000\023\000\
\024\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\002\000\005\000\007\000\006\000\003\000\
\005\000\009\000\004\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\006\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\002\000\002\000\001\000\001\000\001\000\001\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\005\000\001\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\001\000\003\000\001\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\046\000\047\000\045\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\012\000\000\000\000\000\000\000\000\000\
\000\000\062\000\063\000\064\000\074\000\000\000\000\000\000\000\
\059\000\058\000\014\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\041\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\065\000\066\000\043\000\
\044\000\000\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\002\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\008\000\049\000\000\000\067\000\000\000\070\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\038\000\040\000\039\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\068\000\073\000\
\072\000\000\000\000\000\000\000\051\000\052\000\053\000\054\000\
\055\000\000\000\056\000\061\000\000\000\005\000\000\000\050\000\
\000\000\009\000\000\000\057\000\020\000\000\000\007\000\000\000\
\006\000\000\000\000\000\010\000"

let yydgoto = "\002\000\
\029\000\030\000\031\000\032\000\033\000\034\000\061\000\035\000\
\036\000\060\000\038\000\093\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\062\000\064\000\065\000"

let yysindex = "\035\000\
\072\255\000\000\000\000\000\000\000\000\244\254\039\255\043\255\
\026\255\127\255\127\255\072\255\181\255\181\255\025\255\181\255\
\181\255\045\255\000\000\000\000\030\255\031\255\032\255\033\255\
\035\255\000\000\000\000\000\000\000\000\056\000\072\255\034\255\
\000\000\000\000\000\000\000\000\000\000\046\255\054\255\037\255\
\245\254\007\255\253\254\000\000\000\000\000\000\057\255\181\255\
\181\255\064\255\036\255\181\255\048\255\000\000\000\000\000\000\
\000\000\052\255\051\255\000\000\044\255\059\255\181\255\055\255\
\050\255\049\255\047\255\058\255\181\255\181\255\181\255\181\255\
\181\255\000\000\000\000\000\000\127\255\127\255\127\255\127\255\
\127\255\127\255\127\255\127\255\127\255\127\255\127\255\127\255\
\127\255\127\255\071\255\060\255\075\255\000\000\068\255\094\255\
\079\255\181\255\000\000\000\000\181\255\000\000\078\255\000\000\
\025\255\072\255\072\255\181\255\081\255\082\255\083\255\087\255\
\088\255\054\255\037\255\245\254\007\255\007\255\007\255\007\255\
\253\254\253\254\000\000\000\000\000\000\000\000\000\000\004\255\
\181\255\089\255\090\255\109\255\000\000\091\255\000\000\000\000\
\000\000\095\255\085\255\096\255\000\000\000\000\000\000\000\000\
\000\000\114\255\000\000\000\000\181\255\000\000\106\255\000\000\
\072\255\000\000\181\255\000\000\000\000\116\255\000\000\097\255\
\000\000\072\255\100\255\000\000"

let yyrindex = "\000\000\
\146\000\000\000\000\000\000\000\000\000\226\255\142\255\145\255\
\000\000\000\000\000\000\137\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\122\255\075\001\028\001\
\179\255\140\000\050\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\226\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\134\255\000\000\000\000\000\000\
\135\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\143\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\098\001\052\001\004\001\164\000\188\000\212\000\236\000\
\083\000\116\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\247\255\000\000\157\255\034\000\009\000\245\255\000\000\
\000\000\255\255\000\000\070\000\092\000\093\000\091\000\202\255\
\195\255\201\255\028\000\000\000\000\000\197\255\068\000\000\000"

let yytablesize = 652
let yytable = "\037\000\
\003\000\059\000\058\000\103\000\066\000\067\000\138\000\139\000\
\146\000\048\000\037\000\086\000\087\000\088\000\080\000\081\000\
\082\000\083\000\049\000\084\000\085\000\075\000\121\000\122\000\
\147\000\117\000\118\000\119\000\120\000\037\000\123\000\124\000\
\125\000\126\000\127\000\001\000\092\000\056\000\057\000\050\000\
\097\000\135\000\089\000\051\000\090\000\052\000\063\000\094\000\
\068\000\069\000\070\000\071\000\072\000\159\000\073\000\074\000\
\077\000\109\000\110\000\111\000\112\000\113\000\163\000\078\000\
\091\000\076\000\079\000\095\000\096\000\098\000\099\000\100\000\
\003\000\004\000\005\000\006\000\101\000\007\000\008\000\104\000\
\009\000\102\000\105\000\010\000\106\000\011\000\092\000\107\000\
\108\000\012\000\128\000\013\000\129\000\014\000\132\000\015\000\
\140\000\130\000\131\000\133\000\136\000\141\000\142\000\143\000\
\037\000\037\000\016\000\144\000\145\000\017\000\018\000\014\000\
\151\000\152\000\019\000\020\000\021\000\148\000\022\000\149\000\
\023\000\024\000\025\000\026\000\027\000\028\000\154\000\003\000\
\004\000\005\000\053\000\153\000\054\000\055\000\156\000\155\000\
\158\000\162\000\010\000\015\000\011\000\164\000\018\000\160\000\
\018\000\003\000\013\000\157\000\014\000\065\000\015\000\037\000\
\066\000\018\000\018\000\003\000\069\000\018\000\018\000\071\000\
\037\000\018\000\018\000\018\000\150\000\060\000\161\000\134\000\
\114\000\116\000\115\000\021\000\137\000\022\000\000\000\023\000\
\024\000\025\000\026\000\027\000\028\000\003\000\004\000\005\000\
\006\000\000\000\054\000\055\000\025\000\025\000\000\000\000\000\
\010\000\000\000\011\000\000\000\000\000\000\000\000\000\025\000\
\013\000\025\000\014\000\000\000\015\000\000\000\000\000\000\000\
\025\000\000\000\025\000\025\000\000\000\000\000\025\000\025\000\
\000\000\000\000\025\000\025\000\025\000\000\000\000\000\000\000\
\000\000\021\000\000\000\022\000\000\000\023\000\024\000\025\000\
\026\000\027\000\028\000\048\000\048\000\000\000\048\000\048\000\
\048\000\048\000\048\000\000\000\000\000\000\000\048\000\000\000\
\048\000\000\000\000\000\048\000\048\000\048\000\048\000\048\000\
\000\000\048\000\048\000\000\000\000\000\048\000\048\000\000\000\
\000\000\048\000\048\000\048\000\000\000\000\000\000\000\048\000\
\000\000\048\000\000\000\003\000\050\000\050\000\000\000\050\000\
\050\000\050\000\050\000\050\000\000\000\000\000\000\000\050\000\
\000\000\050\000\000\000\000\000\050\000\050\000\050\000\050\000\
\050\000\000\000\050\000\050\000\000\000\000\000\050\000\050\000\
\000\000\000\000\050\000\050\000\050\000\000\000\000\000\000\000\
\050\000\000\000\050\000\032\000\032\000\000\000\032\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\032\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
\000\000\032\000\032\000\000\000\000\000\032\000\032\000\000\000\
\000\000\032\000\032\000\032\000\033\000\033\000\000\000\033\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\000\000\033\000\000\000\000\000\033\000\033\000\033\000\033\000\
\033\000\000\000\033\000\033\000\000\000\000\000\033\000\033\000\
\000\000\000\000\033\000\033\000\033\000\034\000\034\000\000\000\
\034\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\034\000\000\000\000\000\034\000\034\000\034\000\
\034\000\034\000\000\000\034\000\034\000\027\000\027\000\034\000\
\034\000\000\000\000\000\034\000\034\000\034\000\000\000\000\000\
\027\000\000\000\027\000\000\000\000\000\027\000\027\000\027\000\
\027\000\027\000\000\000\027\000\027\000\028\000\028\000\027\000\
\027\000\000\000\000\000\027\000\027\000\027\000\000\000\000\000\
\028\000\000\000\028\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\000\000\028\000\028\000\029\000\029\000\028\000\
\028\000\000\000\000\000\028\000\028\000\028\000\000\000\000\000\
\029\000\000\000\029\000\000\000\000\000\029\000\029\000\029\000\
\029\000\029\000\000\000\029\000\029\000\030\000\030\000\029\000\
\029\000\000\000\000\000\029\000\029\000\029\000\000\000\000\000\
\030\000\000\000\030\000\000\000\000\000\030\000\030\000\030\000\
\030\000\030\000\000\000\030\000\030\000\031\000\031\000\030\000\
\030\000\000\000\000\000\030\000\030\000\030\000\000\000\000\000\
\031\000\000\000\031\000\000\000\000\000\031\000\031\000\031\000\
\031\000\031\000\000\000\031\000\031\000\026\000\026\000\031\000\
\031\000\000\000\000\000\031\000\031\000\031\000\000\000\000\000\
\026\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\026\000\026\000\023\000\023\000\026\000\
\026\000\000\000\000\000\026\000\026\000\026\000\000\000\000\000\
\023\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\023\000\024\000\024\000\023\000\
\023\000\000\000\000\000\023\000\023\000\023\000\000\000\000\000\
\024\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\024\000\021\000\000\000\024\000\
\024\000\000\000\000\000\024\000\024\000\024\000\000\000\021\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\021\000\022\000\000\000\021\000\021\000\
\000\000\000\000\021\000\021\000\021\000\000\000\022\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\022\000\000\000\000\000\022\000\022\000\000\000\
\000\000\022\000\022\000\022\000"

let yycheck = "\001\000\
\000\000\013\000\012\000\063\000\016\000\017\000\106\000\107\000\
\005\001\022\001\012\000\015\001\016\001\017\001\026\001\027\001\
\028\001\029\001\031\001\013\001\014\001\031\000\084\000\085\000\
\021\001\080\000\081\000\082\000\083\000\031\000\086\000\087\000\
\088\000\089\000\090\000\001\000\048\000\010\000\011\000\001\001\
\052\000\101\000\046\001\001\001\048\001\020\001\022\001\049\000\
\004\001\020\001\020\001\020\001\020\001\153\000\020\001\000\000\
\011\001\069\000\070\000\071\000\072\000\073\000\162\000\010\001\
\008\001\032\001\030\001\004\001\033\001\022\001\019\001\021\001\
\001\001\002\001\003\001\004\001\033\001\006\001\007\001\025\001\
\009\001\023\001\033\001\012\001\036\001\014\001\098\000\041\001\
\031\001\018\001\020\001\020\001\033\001\022\001\001\001\024\001\
\108\000\023\001\031\001\021\001\023\001\021\001\021\001\021\001\
\106\000\107\000\035\001\021\001\021\001\038\001\039\001\022\001\
\004\001\023\001\043\001\044\001\045\001\129\000\047\001\031\001\
\049\001\050\001\051\001\052\001\053\001\054\001\042\001\001\001\
\002\001\003\001\004\001\037\001\006\001\007\001\021\001\040\001\
\031\001\041\001\012\001\024\001\014\001\042\001\021\001\155\000\
\023\001\000\000\020\001\149\000\022\001\008\001\024\001\153\000\
\008\001\032\001\033\001\019\001\023\001\036\001\037\001\025\001\
\162\000\040\001\041\001\042\001\131\000\023\001\158\000\098\000\
\077\000\079\000\078\000\045\001\105\000\047\001\255\255\049\001\
\050\001\051\001\052\001\053\001\054\001\001\001\002\001\003\001\
\004\001\255\255\006\001\007\001\010\001\011\001\255\255\255\255\
\012\001\255\255\014\001\255\255\255\255\255\255\255\255\021\001\
\020\001\023\001\022\001\255\255\024\001\255\255\255\255\255\255\
\030\001\255\255\032\001\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\040\001\041\001\042\001\255\255\255\255\255\255\
\255\255\045\001\255\255\047\001\255\255\049\001\050\001\051\001\
\052\001\053\001\054\001\010\001\011\001\255\255\013\001\014\001\
\015\001\016\001\017\001\255\255\255\255\255\255\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\255\255\255\255\036\001\037\001\255\255\
\255\255\040\001\041\001\042\001\255\255\255\255\255\255\046\001\
\255\255\048\001\255\255\019\001\010\001\011\001\255\255\013\001\
\014\001\015\001\016\001\017\001\255\255\255\255\255\255\021\001\
\255\255\023\001\255\255\255\255\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\040\001\041\001\042\001\255\255\255\255\255\255\
\046\001\255\255\048\001\010\001\011\001\255\255\013\001\014\001\
\255\255\255\255\255\255\255\255\255\255\255\255\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\255\255\255\255\036\001\037\001\255\255\
\255\255\040\001\041\001\042\001\010\001\011\001\255\255\013\001\
\014\001\255\255\255\255\255\255\255\255\255\255\255\255\021\001\
\255\255\023\001\255\255\255\255\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\040\001\041\001\042\001\010\001\011\001\255\255\
\013\001\014\001\255\255\255\255\255\255\255\255\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\255\255\255\255\255\255\
\255\255\030\001\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\010\001\011\001\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\255\255\
\021\001\255\255\023\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\011\001\255\255\036\001\
\037\001\255\255\255\255\040\001\041\001\042\001\255\255\021\001\
\255\255\023\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\032\001\033\001\011\001\255\255\036\001\037\001\
\255\255\255\255\040\001\041\001\042\001\255\255\021\001\255\255\
\023\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\032\001\033\001\255\255\255\255\036\001\037\001\255\255\
\255\255\040\001\041\001\042\001"

let yynames_const = "\
  VECTOR\000\
  MATRIX\000\
  INPUT\000\
  PRINT\000\
  AND\000\
  OR\000\
  NOT\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  LSQ\000\
  RSQ\000\
  ML_SQ\000\
  MR_SQ\000\
  LT\000\
  GT\000\
  LTE\000\
  GTE\000\
  EQ\000\
  ASSIGN\000\
  SEMICOLON\000\
  COMMA\000\
  COMMENT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  TO\000\
  DO\000\
  DONE\000\
  CONTINUE\000\
  BREAK\000\
  MAGNITUDE\000\
  ANGLE\000\
  DIMENSION\000\
  DOT\000\
  TRANSPOSE\000\
  DETERMINANT\000\
  INVERSE\000\
  EOF\000\
  INT_T\000\
  FLOAT_T\000\
  BOOL_T\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  BOOL\000\
  IDENT\000\
  FILE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 36 "parser.mly"
                     ( _1 )
# 465 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'top_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement_list) in
    Obj.repr(
# 39 "parser.mly"
                               ( _1 :: _2 )
# 473 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
              ( [] )
# 479 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 43 "parser.mly"
                      ( _1 )
# 486 "parser.ml"
               : 'top_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'vector) in
    Obj.repr(
# 46 "parser.mly"
                                   ( Vector(_2, _3, _5) )
# 495 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'matrix) in
    Obj.repr(
# 47 "parser.mly"
                                             ( Matrix(_2, _4, _5, _7) )
# 505 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 48 "parser.mly"
                                          ( If(_2, _4, Some(_6)) )
# 514 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 49 "parser.mly"
                                 ( Block(_2) )
# 521 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 50 "parser.mly"
                                 ( While(_2, _4) )
# 529 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 51 "parser.mly"
                                                    ( For(_2, _4, _6, _8) )
# 539 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                             ( Print(_3) )
# 546 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
          ( Break )
# 552 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
             ( Continue )
# 558 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_statement) in
    Obj.repr(
# 55 "parser.mly"
                     ( _1 )
# 565 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_stmt) in
    Obj.repr(
# 58 "parser.mly"
            ( _1 )
# 572 "parser.ml"
               : 'simple_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign_expr) in
    Obj.repr(
# 61 "parser.mly"
              ( Expr(_1) )
# 579 "parser.ml"
               : 'expr_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign_expr) in
    Obj.repr(
# 64 "parser.mly"
              ( _1 )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expr) in
    Obj.repr(
# 67 "parser.mly"
            ( _1 )
# 593 "parser.ml"
               : 'assign_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assign_expr) in
    Obj.repr(
# 68 "parser.mly"
                             ( Assign(_1, _3) )
# 601 "parser.ml"
               : 'assign_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'index_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'assign_expr) in
    Obj.repr(
# 69 "parser.mly"
                                                ( AssignIdx(Index(Var(_1),_3),_6))
# 610 "parser.ml"
               : 'assign_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 72 "parser.mly"
             ( _1 )
# 617 "parser.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 73 "parser.mly"
                        ( BinOp(_1, Or, _3) )
# 625 "parser.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq_expr) in
    Obj.repr(
# 76 "parser.mly"
            ( _1 )
# 632 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eq_expr) in
    Obj.repr(
# 77 "parser.mly"
                         ( BinOp(_1, And, _3) )
# 640 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 80 "parser.mly"
              ( _1 )
# 647 "parser.ml"
               : 'eq_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eq_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 81 "parser.mly"
                         ( BinOp(_1, Eq, _3) )
# 655 "parser.ml"
               : 'eq_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 84 "parser.mly"
             ( _1 )
# 662 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 85 "parser.mly"
                          ( BinOp(_1, Lt, _3) )
# 670 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 86 "parser.mly"
                          ( BinOp(_1, Gt, _3) )
# 678 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 87 "parser.mly"
                           ( BinOp(_1, Lte, _3) )
# 686 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 88 "parser.mly"
                           ( BinOp(_1, Gte, _3) )
# 694 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mul_expr) in
    Obj.repr(
# 91 "parser.mly"
             ( _1 )
# 701 "parser.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mul_expr) in
    Obj.repr(
# 92 "parser.mly"
                          ( BinOp(_1, Add, _3) )
# 709 "parser.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mul_expr) in
    Obj.repr(
# 93 "parser.mly"
                          ( BinOp(_1, Sub, _3) )
# 717 "parser.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 96 "parser.mly"
              ( _1 )
# 724 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 97 "parser.mly"
                           ( BinOp(_1, Mul, _3) )
# 732 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 98 "parser.mly"
                           ( BinOp(_1, Div, _3) )
# 740 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 99 "parser.mly"
                           ( BinOp(_1, Mod, _3) )
# 748 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 100 "parser.mly"
                           ( BinOp(_1, Dot, _3) )
# 756 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'post_expr) in
    Obj.repr(
# 101 "parser.mly"
                             ( BinOp(_1, Angle, _3) )
# 764 "parser.ml"
               : 'mul_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 104 "parser.mly"
                ( _1 )
# 771 "parser.ml"
               : 'post_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expr) in
    Obj.repr(
# 107 "parser.mly"
                 ( _1 )
# 778 "parser.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 108 "parser.mly"
                   ( UnOp(Not, _2) )
# 785 "parser.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 109 "parser.mly"
                             ( UnOp(Neg, _2) )
# 792 "parser.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 112 "parser.mly"
         ( ConstBool(_1) )
# 799 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
        ( ConstInt(_1) )
# 806 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 114 "parser.mly"
          ( ConstFloat(_1) )
# 813 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
          ( Var(_1) )
# 820 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                       ( _2 )
# 827 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'index_list) in
    Obj.repr(
# 117 "parser.mly"
                             ( Index(Var(_1), _3) )
# 835 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                 ( UnOp(Magnitude, _3) )
# 842 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                 ( UnOp(Dimension, _3) )
# 849 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                 ( UnOp(Transpose, _3) )
# 856 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( UnOp(Determinant, _3) )
# 863 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                               ( UnOp(Inverse, _3) )
# 870 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_specifier) in
    Obj.repr(
# 123 "parser.mly"
                                       ( Input(_1,None) )
# 877 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'type_specifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parser.mly"
                                            ( Input(_1,Some(_4)) )
# 885 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'matrix) in
    Obj.repr(
# 125 "parser.mly"
           ( _1 )
# 892 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vector) in
    Obj.repr(
# 126 "parser.mly"
           ( _1 )
# 899 "parser.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                          ( [_1] )
# 906 "parser.ml"
               : 'index_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                                            ( [_1; _3] )
# 914 "parser.ml"
               : 'index_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
           ( TInt )
# 920 "parser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                ( TFloat )
# 926 "parser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                                     ( TBool )
# 932 "parser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                                                         ( TV )
# 938 "parser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                                                                         ( TM )
# 944 "parser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vector_elems) in
    Obj.repr(
# 135 "parser.mly"
                       ( ConstV(_2) )
# 951 "parser.ml"
               : 'vector))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vector_elems) in
    Obj.repr(
# 138 "parser.mly"
                            ( _1 :: _3 )
# 959 "parser.ml"
               : 'vector_elems))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
         ( [_1] )
# 966 "parser.ml"
               : 'vector_elems))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'matrix_rows) in
    Obj.repr(
# 142 "parser.mly"
                          ( ConstM(_2) )
# 973 "parser.ml"
               : 'matrix))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'matrix_row) in
    Obj.repr(
# 145 "parser.mly"
               ( [_1] )
# 980 "parser.ml"
               : 'matrix_rows))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'matrix_row) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'matrix_rows) in
    Obj.repr(
# 146 "parser.mly"
                                 ( _1 :: _3 )
# 988 "parser.ml"
               : 'matrix_rows))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vector_elems) in
    Obj.repr(
# 149 "parser.mly"
                       ( _2 )
# 995 "parser.ml"
               : 'matrix_row))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
;;
