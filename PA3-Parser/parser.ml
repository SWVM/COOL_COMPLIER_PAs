type token =
  | IDENTIFIER of (string * string)
  | INTEGER of (string * string)
  | STRING of (string * string)
  | TYPE of (string * string)
  | AT of (string)
  | CASE of (string)
  | CLASS of (string)
  | COMMA of (string)
  | COLON of (string)
  | DIVIDE of (string)
  | DOT of (string)
  | ELSE of (string)
  | EQUALS of (string)
  | ESAC of (string)
  | FALSE of (string)
  | FI of (string)
  | IF of (string)
  | IN of (string)
  | INHERITS of (string)
  | ISVOID of (string)
  | LARROW of (string)
  | LBRACE of (string)
  | LE of (string)
  | LET of (string)
  | LOOP of (string)
  | LPAREN of (string)
  | LT of (string)
  | MINUS of (string)
  | NEW of (string)
  | NOT of (string)
  | OF of (string)
  | PLUS of (string)
  | POOL of (string)
  | RARROW of (string)
  | RBRACE of (string)
  | RPAREN of (string)
  | SEMI of (string)
  | THEN of (string)
  | TILDE of (string)
  | TIMES of (string)
  | TRUE of (string)
  | WHILE of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Printf
type identifier = string * string
and case_      = identifier * identifier * expr
and cool_class =
                | ClassNoInherits of identifier * (feature list)
                | ClassInherits   of identifier * identifier * (feature list)

and feature    =
                | AttributeNoInit of identifier * identifier
                | AttributeInit   of identifier * identifier * expr
                | Method          of identifier * (formal list) * identifier * expr

and cool_prog  = cool_class list

and formal     = Formal of identifier * identifier

and expr_inner =
               | Assign       of identifier * expr
               | DDispatch    of expr * identifier * param
               | SDispatch    of expr * identifier * identifier * param
               | SelfDispatch of identifier * param
               | If           of expr * expr * expr
               | While        of expr * expr
               | Block        of expr list
               | New          of identifier
               | BinaryOP     of expr * expr * string
               | UniOp        of expr * string
               | Integer      of string
               | String       of string
               | Id           of identifier
               | Bool         of string
               | Case         of expr * case_ list
               | Binding      of (binding list) * expr
and expr      = string * expr_inner

and param      = expr list

and binding    =
               | BindingNoInit   of identifier * identifier
               | BindingInit of identifier * identifier * expr

# 91 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* INTEGER *);
  259 (* STRING *);
  260 (* TYPE *);
  261 (* AT *);
  262 (* CASE *);
  263 (* CLASS *);
  264 (* COMMA *);
  265 (* COLON *);
  266 (* DIVIDE *);
  267 (* DOT *);
  268 (* ELSE *);
  269 (* EQUALS *);
  270 (* ESAC *);
  271 (* FALSE *);
  272 (* FI *);
  273 (* IF *);
  274 (* IN *);
  275 (* INHERITS *);
  276 (* ISVOID *);
  277 (* LARROW *);
  278 (* LBRACE *);
  279 (* LE *);
  280 (* LET *);
  281 (* LOOP *);
  282 (* LPAREN *);
  283 (* LT *);
  284 (* MINUS *);
  285 (* NEW *);
  286 (* NOT *);
  287 (* OF *);
  288 (* PLUS *);
  289 (* POOL *);
  290 (* RARROW *);
  291 (* RBRACE *);
  292 (* RPAREN *);
  293 (* SEMI *);
  294 (* THEN *);
  295 (* TILDE *);
  296 (* TIMES *);
  297 (* TRUE *);
  298 (* WHILE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\005\000\
\005\000\005\000\006\000\006\000\009\000\009\000\008\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\011\000\011\000\010\000\010\000\014\000\014\000\012\000\
\012\000\015\000\015\000\013\000\013\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\007\000\005\000\000\000\003\000\009\000\
\005\000\003\000\000\000\002\000\000\000\003\000\003\000\003\000\
\008\000\006\000\004\000\007\000\005\000\003\000\004\000\005\000\
\002\000\002\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\003\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\002\000\003\000\000\000\002\000\003\000\000\000\001\000\
\003\000\003\000\005\000\001\000\002\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\055\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\004\000\000\000\000\000\000\000\000\000\
\012\000\000\000\038\000\039\000\000\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\019\000\000\000\
\000\000\000\000\000\000\043\000\000\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\053\000\000\000\
\000\000\021\000\000\000\000\000\008\000\046\000\000\000\000\000\
\000\000\000\000\018\000\000\000\020\000\000\000\000\000\017\000\
\054\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\015\000\016\000\025\000\080\000\026\000\
\033\000\081\000\059\000\061\000\105\000\102\000\062\000\106\000"

let yysindex = "\003\000\
\021\255\000\000\028\255\000\000\034\000\002\255\007\255\000\000\
\021\255\037\255\048\255\000\000\029\255\248\254\023\255\026\255\
\048\255\056\255\063\255\000\000\048\255\031\255\049\255\060\255\
\038\255\065\255\000\000\000\000\078\255\071\255\067\255\063\255\
\000\000\237\254\000\000\000\000\078\255\000\000\078\255\078\255\
\078\255\076\255\078\255\074\255\078\255\078\255\000\000\078\255\
\064\000\000\000\079\255\065\255\078\255\078\255\025\255\147\255\
\064\000\154\255\047\255\077\255\251\254\000\000\179\255\000\000\
\064\000\064\000\199\255\083\255\078\255\087\255\078\255\078\255\
\078\255\078\255\078\255\078\255\068\255\000\000\064\000\224\255\
\058\255\095\255\078\255\078\255\000\000\102\255\076\255\078\255\
\000\000\078\255\088\255\020\255\075\255\057\255\057\255\057\255\
\010\255\010\255\020\255\078\255\078\255\000\000\000\000\101\255\
\098\255\095\255\231\255\000\000\090\255\000\000\064\000\255\255\
\115\255\078\255\006\000\224\255\110\255\000\000\000\000\078\255\
\078\255\000\000\111\255\091\255\000\000\000\000\105\255\032\000\
\064\000\078\255\000\000\078\255\000\000\106\255\039\000\000\000\
\000\000"

let yyrindex = "\000\000\
\143\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\143\000\000\000\112\255\000\000\000\000\000\000\000\000\000\000\
\112\255\000\000\118\255\000\000\112\255\000\000\119\255\000\000\
\000\000\125\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\113\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\255\000\000\000\000\125\255\000\000\126\255\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\194\000\208\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\239\000\127\255\
\000\000\000\000\000\000\120\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\070\000\000\000\253\000\028\001\042\001\
\132\000\163\000\101\000\000\000\000\000\000\000\000\000\000\000\
\000\000\152\255\000\000\000\000\015\255\000\000\073\001\000\000\
\000\000\126\255\000\000\127\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\255\126\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\159\000\000\000\244\255\000\000\000\000\227\255\137\000\
\119\000\148\255\088\000\000\000\067\000\060\000\091\000\000\000"

let yytablesize = 623
let yytable = "\049\000\
\018\000\053\000\087\000\001\000\022\000\124\000\054\000\055\000\
\027\000\056\000\057\000\058\000\088\000\063\000\068\000\065\000\
\066\000\019\000\067\000\069\000\070\000\134\000\050\000\079\000\
\068\000\010\000\051\000\003\000\011\000\068\000\070\000\007\000\
\050\000\008\000\069\000\070\000\051\000\071\000\009\000\092\000\
\013\000\094\000\095\000\096\000\097\000\098\000\099\000\072\000\
\014\000\076\000\017\000\073\000\074\000\107\000\058\000\082\000\
\075\000\020\000\111\000\023\000\112\000\068\000\021\000\024\000\
\076\000\028\000\069\000\070\000\030\000\029\000\115\000\116\000\
\032\000\031\000\050\000\051\000\060\000\064\000\034\000\035\000\
\036\000\085\000\077\000\037\000\074\000\086\000\091\000\093\000\
\075\000\100\000\128\000\129\000\038\000\103\000\039\000\104\000\
\076\000\040\000\113\000\041\000\114\000\042\000\135\000\043\000\
\026\000\109\000\044\000\045\000\026\000\117\000\121\000\118\000\
\026\000\127\000\026\000\123\000\046\000\037\000\047\000\048\000\
\037\000\026\000\037\000\037\000\037\000\037\000\131\000\026\000\
\037\000\026\000\037\000\026\000\026\000\026\000\026\000\037\000\
\130\000\037\000\132\000\037\000\037\000\136\000\002\000\037\000\
\037\000\037\000\006\000\037\000\037\000\037\000\037\000\068\000\
\037\000\011\000\042\000\010\000\069\000\070\000\068\000\071\000\
\013\000\044\000\047\000\069\000\070\000\052\000\071\000\012\000\
\052\000\072\000\078\000\108\000\119\000\073\000\074\000\126\000\
\072\000\110\000\075\000\000\000\073\000\074\000\000\000\068\000\
\083\000\075\000\076\000\000\000\069\000\070\000\084\000\071\000\
\000\000\076\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000\000\000\068\000\000\000\073\000\074\000\000\000\
\069\000\070\000\075\000\071\000\000\000\000\000\089\000\000\000\
\000\000\000\000\076\000\000\000\000\000\072\000\000\000\090\000\
\000\000\073\000\074\000\000\000\068\000\000\000\075\000\101\000\
\000\000\069\000\070\000\068\000\071\000\000\000\076\000\000\000\
\069\000\070\000\120\000\071\000\000\000\000\000\072\000\000\000\
\000\000\000\000\073\000\074\000\000\000\072\000\000\000\075\000\
\000\000\073\000\074\000\068\000\000\000\000\000\075\000\076\000\
\069\000\070\000\068\000\071\000\000\000\000\000\076\000\069\000\
\070\000\000\000\071\000\000\000\000\000\072\000\000\000\000\000\
\000\000\073\000\074\000\000\000\072\000\000\000\075\000\122\000\
\073\000\074\000\000\000\000\000\068\000\075\000\076\000\000\000\
\125\000\069\000\070\000\068\000\071\000\076\000\000\000\133\000\
\069\000\070\000\000\000\071\000\000\000\000\000\072\000\000\000\
\000\000\000\000\073\000\074\000\000\000\072\000\000\000\075\000\
\000\000\073\000\074\000\000\000\068\000\000\000\075\000\076\000\
\000\000\069\000\070\000\137\000\071\000\030\000\076\000\030\000\
\000\000\030\000\030\000\000\000\000\000\030\000\072\000\030\000\
\000\000\000\000\073\000\074\000\030\000\000\000\030\000\075\000\
\030\000\030\000\000\000\000\000\030\000\030\000\030\000\076\000\
\030\000\030\000\030\000\030\000\029\000\030\000\029\000\000\000\
\029\000\029\000\000\000\000\000\029\000\000\000\029\000\000\000\
\000\000\000\000\000\000\029\000\000\000\029\000\000\000\029\000\
\029\000\000\000\000\000\029\000\029\000\029\000\000\000\029\000\
\029\000\029\000\029\000\028\000\029\000\000\000\000\000\028\000\
\028\000\000\000\000\000\028\000\000\000\028\000\000\000\000\000\
\000\000\000\000\028\000\000\000\028\000\000\000\028\000\028\000\
\000\000\000\000\028\000\028\000\028\000\000\000\028\000\028\000\
\028\000\028\000\027\000\000\000\000\000\000\000\027\000\027\000\
\000\000\000\000\027\000\000\000\027\000\000\000\000\000\000\000\
\000\000\027\000\000\000\027\000\000\000\027\000\027\000\000\000\
\000\000\027\000\027\000\027\000\000\000\027\000\027\000\027\000\
\027\000\035\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\035\000\000\000\035\000\000\000\000\000\000\000\031\000\
\000\000\000\000\035\000\031\000\000\000\000\000\000\000\031\000\
\035\000\031\000\035\000\000\000\035\000\035\000\035\000\035\000\
\031\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\031\000\000\000\031\000\031\000\031\000\031\000\016\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\016\000\000\000\
\016\000\000\000\000\000\000\000\034\000\000\000\000\000\016\000\
\034\000\000\000\000\000\000\000\034\000\016\000\034\000\016\000\
\000\000\016\000\016\000\016\000\016\000\034\000\000\000\000\000\
\000\000\000\000\000\000\034\000\000\000\034\000\000\000\034\000\
\034\000\034\000\034\000\033\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\033\000\000\000\033\000\000\000\000\000\
\000\000\032\000\000\000\000\000\033\000\032\000\000\000\000\000\
\000\000\032\000\033\000\032\000\033\000\000\000\033\000\033\000\
\033\000\033\000\032\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\032\000\000\000\032\000\032\000\032\000\032\000\
\023\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\023\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\023\000\000\000\023\000\023\000\023\000\023\000"

let yycheck = "\029\000\
\009\001\021\001\008\001\001\000\017\000\114\000\026\001\037\000\
\021\000\039\000\040\000\041\000\018\001\043\000\005\001\045\000\
\046\000\026\001\048\000\010\001\011\001\130\000\008\001\053\000\
\005\001\019\001\008\001\007\001\022\001\005\001\011\001\004\001\
\018\001\000\000\010\001\011\001\018\001\013\001\037\001\069\000\
\004\001\071\000\072\000\073\000\074\000\075\000\076\000\023\001\
\001\001\040\001\022\001\027\001\028\001\083\000\084\000\031\001\
\032\001\035\001\088\000\004\001\090\000\005\001\037\001\001\001\
\040\001\035\001\010\001\011\001\009\001\021\001\100\000\101\000\
\008\001\036\001\004\001\009\001\001\001\004\001\001\001\002\001\
\003\001\035\001\004\001\006\001\028\001\009\001\004\001\001\001\
\032\001\022\001\120\000\121\000\015\001\036\001\017\001\001\001\
\040\001\020\001\011\001\022\001\026\001\024\001\132\000\026\001\
\008\001\004\001\029\001\030\001\012\001\009\001\021\001\014\001\
\016\001\004\001\018\001\001\001\039\001\005\001\041\001\042\001\
\008\001\025\001\010\001\011\001\012\001\013\001\036\001\031\001\
\016\001\033\001\018\001\035\001\036\001\037\001\038\001\023\001\
\026\001\025\001\034\001\027\001\028\001\036\001\000\000\031\001\
\032\001\033\001\035\001\035\001\036\001\037\001\038\001\005\001\
\040\001\036\001\035\001\037\001\010\001\011\001\005\001\013\001\
\036\001\036\001\036\001\010\001\011\001\014\001\013\001\009\000\
\032\000\023\001\052\000\084\000\106\000\027\001\028\001\116\000\
\023\001\087\000\032\001\255\255\027\001\028\001\255\255\005\001\
\038\001\032\001\040\001\255\255\010\001\011\001\037\001\013\001\
\255\255\040\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\023\001\255\255\005\001\255\255\027\001\028\001\255\255\
\010\001\011\001\032\001\013\001\255\255\255\255\036\001\255\255\
\255\255\255\255\040\001\255\255\255\255\023\001\255\255\025\001\
\255\255\027\001\028\001\255\255\005\001\255\255\032\001\008\001\
\255\255\010\001\011\001\005\001\013\001\255\255\040\001\255\255\
\010\001\011\001\012\001\013\001\255\255\255\255\023\001\255\255\
\255\255\255\255\027\001\028\001\255\255\023\001\255\255\032\001\
\255\255\027\001\028\001\005\001\255\255\255\255\032\001\040\001\
\010\001\011\001\005\001\013\001\255\255\255\255\040\001\010\001\
\011\001\255\255\013\001\255\255\255\255\023\001\255\255\255\255\
\255\255\027\001\028\001\255\255\023\001\255\255\032\001\033\001\
\027\001\028\001\255\255\255\255\005\001\032\001\040\001\255\255\
\035\001\010\001\011\001\005\001\013\001\040\001\255\255\016\001\
\010\001\011\001\255\255\013\001\255\255\255\255\023\001\255\255\
\255\255\255\255\027\001\028\001\255\255\023\001\255\255\032\001\
\255\255\027\001\028\001\255\255\005\001\255\255\032\001\040\001\
\255\255\010\001\011\001\037\001\013\001\008\001\040\001\010\001\
\255\255\012\001\013\001\255\255\255\255\016\001\023\001\018\001\
\255\255\255\255\027\001\028\001\023\001\255\255\025\001\032\001\
\027\001\028\001\255\255\255\255\031\001\032\001\033\001\040\001\
\035\001\036\001\037\001\038\001\008\001\040\001\010\001\255\255\
\012\001\013\001\255\255\255\255\016\001\255\255\018\001\255\255\
\255\255\255\255\255\255\023\001\255\255\025\001\255\255\027\001\
\028\001\255\255\255\255\031\001\032\001\033\001\255\255\035\001\
\036\001\037\001\038\001\008\001\040\001\255\255\255\255\012\001\
\013\001\255\255\255\255\016\001\255\255\018\001\255\255\255\255\
\255\255\255\255\023\001\255\255\025\001\255\255\027\001\028\001\
\255\255\255\255\031\001\032\001\033\001\255\255\035\001\036\001\
\037\001\038\001\008\001\255\255\255\255\255\255\012\001\013\001\
\255\255\255\255\016\001\255\255\018\001\255\255\255\255\255\255\
\255\255\023\001\255\255\025\001\255\255\027\001\028\001\255\255\
\255\255\031\001\032\001\033\001\255\255\035\001\036\001\037\001\
\038\001\008\001\255\255\255\255\255\255\012\001\255\255\255\255\
\255\255\016\001\255\255\018\001\255\255\255\255\255\255\008\001\
\255\255\255\255\025\001\012\001\255\255\255\255\255\255\016\001\
\031\001\018\001\033\001\255\255\035\001\036\001\037\001\038\001\
\025\001\255\255\255\255\255\255\255\255\255\255\031\001\255\255\
\033\001\255\255\035\001\036\001\037\001\038\001\008\001\255\255\
\255\255\255\255\012\001\255\255\255\255\255\255\016\001\255\255\
\018\001\255\255\255\255\255\255\008\001\255\255\255\255\025\001\
\012\001\255\255\255\255\255\255\016\001\031\001\018\001\033\001\
\255\255\035\001\036\001\037\001\038\001\025\001\255\255\255\255\
\255\255\255\255\255\255\031\001\255\255\033\001\255\255\035\001\
\036\001\037\001\038\001\008\001\255\255\255\255\255\255\012\001\
\255\255\255\255\255\255\016\001\255\255\018\001\255\255\255\255\
\255\255\008\001\255\255\255\255\025\001\012\001\255\255\255\255\
\255\255\016\001\031\001\018\001\033\001\255\255\035\001\036\001\
\037\001\038\001\025\001\255\255\255\255\255\255\255\255\255\255\
\031\001\255\255\033\001\255\255\035\001\036\001\037\001\038\001\
\008\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\016\001\255\255\018\001\255\255\255\255\255\255\255\255\255\255\
\255\255\025\001\255\255\255\255\255\255\255\255\255\255\031\001\
\255\255\033\001\255\255\035\001\036\001\037\001\038\001"

let yynames_const = "\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  INTEGER\000\
  STRING\000\
  TYPE\000\
  AT\000\
  CASE\000\
  CLASS\000\
  COMMA\000\
  COLON\000\
  DIVIDE\000\
  DOT\000\
  ELSE\000\
  EQUALS\000\
  ESAC\000\
  FALSE\000\
  FI\000\
  IF\000\
  IN\000\
  INHERITS\000\
  ISVOID\000\
  LARROW\000\
  LBRACE\000\
  LE\000\
  LET\000\
  LOOP\000\
  LPAREN\000\
  LT\000\
  MINUS\000\
  NEW\000\
  NOT\000\
  OF\000\
  PLUS\000\
  POOL\000\
  RARROW\000\
  RBRACE\000\
  RPAREN\000\
  SEMI\000\
  THEN\000\
  TILDE\000\
  TIMES\000\
  TRUE\000\
  WHILE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_list) in
    Obj.repr(
# 61 "parser.mly"
                                                                      ( _1  )
# 444 "parser.ml"
               : cool_prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                                                      ( [] )
# 450 "parser.ml"
               : 'class_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cool_class) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'class_list) in
    Obj.repr(
# 64 "parser.mly"
                                                                      (_1 :: _3)
# 459 "parser.ml"
               : 'class_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                                                                      ( ClassInherits(_2, _4, _6) )
# 472 "parser.ml"
               : 'cool_class))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                                                                      ( ClassNoInherits(_2, _4) )
# 483 "parser.ml"
               : 'cool_class))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                                                      ( [] )
# 489 "parser.ml"
               : 'feature_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'feature) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'feature_list) in
    Obj.repr(
# 70 "parser.mly"
                                                                      ( _1 :: _3)
# 498 "parser.ml"
               : 'feature_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formal_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                                                                                        ( Method(_1, _3, _6, _8) )
# 513 "parser.ml"
               : 'feature))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                                                                        ( AttributeInit(_1, _3, _5) )
# 524 "parser.ml"
               : 'feature))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 74 "parser.mly"
                                                                                        ( AttributeNoInit(_1, _3) )
# 533 "parser.ml"
               : 'feature))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                                                      ( [] )
# 539 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_tail) in
    Obj.repr(
# 77 "parser.mly"
                                                                      ( _1 :: _2 )
# 547 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                                                      ( [] )
# 553 "parser.ml"
               : 'formal_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_tail) in
    Obj.repr(
# 80 "parser.mly"
                                                                      ( _2 :: _3 )
# 562 "parser.ml"
               : 'formal_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 82 "parser.mly"
                                                                      ( Formal(_1, _3) )
# 571 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                                                      ( let line, _ = _1 in (line, Assign(_1, _3)) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                                                                      ( let line, _ = _1 in (line, SDispatch(_1, _3, _5, _7)) )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                                                                      ( let line, _ = _1 in (line, DDispatch(_1, _3, _5)) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                                                                      ( let line, _ = _1 in (line, SelfDispatch(_1, _3)) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                                                                      ( (_1, If(_2, _4, _6)) )
# 629 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                                                                      ( (_1, While(_2, _4)) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                                                                      ( (_1, Block(_2)))
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'binding_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                                                                      ( (_1, Binding((List.rev _2), _4)) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'case_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                                                      ( (_1, Case(_2, _4)) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 93 "parser.mly"
                                                                      ( (_1, New(_2)) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                                                                      ( (_1, UniOp(_2, "isvoid")) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "plus")) )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "minus")) )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "times")) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "divide")) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                                                      ( (_1, UniOp(_2, "negate")))
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "lt")) )
# 739 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "le")) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                                                                      ( let line, _ = _1 in (line, BinaryOP(_1, _3, "eq")) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                                                                      ( (_1, UniOp(_2, "not")) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                                                                      ( _2 )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 105 "parser.mly"
                                                                      ( let line, _   = _1 in (line, Id(_1)))
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 106 "parser.mly"
                                                                      ( let line, v = _1 in (line, Integer(v)) )
# 788 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 107 "parser.mly"
                                                                      ( let line, v = _1 in (line, String(v)) )
# 795 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                                                                      ( (_1, Bool("true")) )
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                                                                      ( (_1, Bool("false")) )
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
                                        ( _1 :: [] )
# 817 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 112 "parser.mly"
                                        ( _1 :: _3 )
# 826 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                        ( [] )
# 832 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param_tail) in
    Obj.repr(
# 115 "parser.mly"
                                        ( _1 :: _2)
# 840 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_tail) in
    Obj.repr(
# 116 "parser.mly"
                                        ( _2 :: _3)
# 849 "parser.ml"
               : 'param_tail))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                        ( [] )
# 855 "parser.ml"
               : 'param_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 120 "parser.mly"
                                                      (_1::[])
# 862 "parser.ml"
               : 'binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 121 "parser.mly"
                                                      ( _3 :: _1 )
# 871 "parser.ml"
               : 'binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 123 "parser.mly"
                                                      ( BindingNoInit(_1, _3))
# 880 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                                      ( BindingInit(_1, _3, _5) )
# 891 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'case_) in
    Obj.repr(
# 126 "parser.mly"
                                                          ( _1 :: [])
# 898 "parser.ml"
               : 'case_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'case_) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'case_list) in
    Obj.repr(
# 127 "parser.mly"
                                                          ( _1 :: _2 )
# 906 "parser.ml"
               : 'case_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                                                              ( (_1, _3, _5))
# 918 "parser.ml"
               : 'case_))
(* Entry cool_prog_entry *)
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
let cool_prog_entry (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : cool_prog)
;;
# 134 "parser.mly"
    (* converte token to back to string for error reporting*)
let token_str tt =
  match tt with
  |AT(l) -> "@"
  |CASE(l) -> "case"
  |CLASS(l) -> "class"
  |COMMA(l) -> ","
  |COLON(l) -> ":"
  |DIVIDE(l) -> "/"
  |DOT(l) -> "."
  |ELSE(l) -> "else"
  |EQUALS(l) -> "="
  |ESAC(l) -> "esac"
  |FALSE(l) -> "false"
  |FI(l) -> "fi"
  |IDENTIFIER(l,s) -> s
  |IF(l) -> "if"
  |IN(l) -> "in"
  |INHERITS(l) -> "inherits"
  |INTEGER(l,s) -> s
  |ISVOID(l) -> "isvoid"
  |LARROW(l) -> "<-"
  |LBRACE(l) -> "{"
  |LE(l) -> "<="
  |LET(l) -> "let"
  |LOOP(l) -> "loop"
  |LPAREN(l) -> "("
  |LT(l) -> "<"
  |MINUS(l) -> "-"
  |NEW(l) -> "new"
  |NOT(l) -> "not"
  |OF(l) -> "of"
  |PLUS(l) -> "+"
  |POOL(l) -> "pool"
  |RARROW(l) -> "=>"
  |RBRACE(l) -> "}"
  |RPAREN(l) -> ")"
  |SEMI(l) -> ";"
  |STRING(l,s) -> s
  |THEN(l) -> "then"
  |TILDE(l) -> "~"
  |TIMES(l) -> "*"
  |TRUE(l) -> "true"
  |TYPE(l,s) -> s
  |WHILE(l) -> "while"
  |EOF  -> "EOF";;

    (*parse lex file*)
let read_token filename =
  let fin = open_in filename in
  let tokens = Queue.create () in
  let get_line () = String.trim (input_line fin) in
  (try while true do
    let l = get_line () in
    let token_type = get_line () in
    let token = match token_type with
      |"at"       ->  AT(l)
      |"case"     ->  CASE(l)
      |"class"    ->  CLASS(l)
      |"comma"    ->  COMMA(l)
      |"colon"    ->  COLON(l)
      |"divide"   ->  DIVIDE(l)
      |"dot"      ->  DOT(l)
      |"else"     ->  ELSE(l)
      |"equals"   ->  EQUALS(l)
      |"esac"     ->  ESAC(l)
      |"false"    ->  FALSE(l)
      |"fi"       ->  FI(l)
      |"identifier"-> IDENTIFIER(l,get_line())
      |"if"       ->  IF(l)
      |"in"       ->  IN(l)
      |"inherits" ->  INHERITS(l)
      |"integer"  ->  INTEGER(l,get_line())
      |"isvoid"   ->  ISVOID(l)
      |"larrow"   ->  LARROW(l)
      |"lbrace"   ->  LBRACE(l)
      |"le"       ->  LE(l)
      |"let"      ->  LET(l)
      |"loop"     ->  LOOP(l)
      |"lparen"   ->  LPAREN(l)
      |"lt"       ->  LT(l)
      |"minus"    ->  MINUS(l)
      |"new"      ->  NEW(l)
      |"not"      ->  NOT(l)
      |"of"       ->  OF(l)
      |"plus"     ->  PLUS(l)
      |"pool"     ->  POOL(l)
      |"rarrow"   ->  RARROW(l)
      |"rbrace"   ->  RBRACE(l)
      |"rparen"   ->  RPAREN(l)
      |"semi"     ->  SEMI(l)
      |"string"   ->  STRING(l,get_line())
      |"then"     ->  THEN(l)
      |"tilde"    ->  TILDE(l)
      |"times"    ->  TIMES(l)
      |"true"     ->  TRUE(l)
      |"type"     ->  TYPE(l,get_line())
      |"while"    ->  WHILE(l)
      | _         -> begin
        printf "unexpeted token type: %s\n" token_type;
        exit 1
      end in
      Queue.add (l,token) tokens
  done with _ -> ());
  close_in fin;
  tokens





let main() = begin
  let filename = Sys.argv.(1) in
  let tokens = read_token filename in

  let last_line = ref "1"
  and last_token= ref "prog_start" in

  let lexbuf = Lexing.from_string "" in
  let lexer_token lb =
    if Queue.is_empty tokens then
      EOF
    else begin
      let line, next_token = Queue.take tokens in
      last_line := line;
      last_token := token_str next_token;
      next_token
    end
  in

  let result =
    try
      cool_prog_entry lexer_token lexbuf
    with _ -> begin
      printf "ERROR: %s: Parser: syntax error near %s\n" !last_line !last_token;
      exit 1
    end
  in

  let output_filename = Filename.chop_extension (Sys.argv.(1))^ ".cl-ast" in
  let f = open_out output_filename in

    (*recursive printing functions*)
  let rec print_list func l =
    fprintf f  "%d\n" (List.length l);
    List.iter func l;
  in

  let print_id id =
      let line, name = id in
      fprintf f  "%s\n%s\n" line name
  in

  let rec print_expr e =
    let line, inner = e in
    fprintf f "%s\n" line;
    match inner with
    | Assign(name, rhs) ->
      fprintf f  "assign\n";
      print_id name;
      print_expr rhs
    | DDispatch(exp, methdd, param) ->
      fprintf f  "dynamic_dispatch\n";
      print_expr exp;
      print_id methdd;
      print_list print_expr param
    | SDispatch(exp, parent, methdd, param) ->
      fprintf f  "static_dispatch\n";
      print_expr exp;
      print_id parent;
      print_id methdd;
      print_list print_expr param
    | SelfDispatch(methdd, param) ->
      fprintf f  "self_dispatch\n";
      print_id methdd;
      print_list print_expr param
    | If(condi, thnn, elss) ->
      fprintf f  "if\n";
      print_expr condi;
      print_expr thnn;
      print_expr elss
    | While(condi, body) ->
      fprintf f  "while\n";
      print_expr condi;
      print_expr body
    | Block(exp) ->
      fprintf f  "block\n";
      print_list print_expr exp
    | New(id) ->
      fprintf f  "new\n";
      print_id id
    | BinaryOP(exp1, exp2, op) ->
      fprintf f  "%s\n" op;
      print_expr exp1;
      print_expr exp2
    | UniOp(exp, op) ->
      fprintf f  "%s\n" op;
      print_expr exp
    | Integer(str) ->
      fprintf f  "integer\n%s\n" str
    | String(str) ->
      fprintf f  "string\n%s\n" str
    | Id(id) ->
      fprintf f  "identifier\n";
      print_id id
    | Bool(bool) ->
      fprintf f  "%s\n" bool
    | Case(exp, c_list) ->
      fprintf f  "case\n";
      print_expr exp;
      print_list print_case c_list
    | Binding(b_list, exp) ->
      fprintf f  "let\n";
      print_list print_binding b_list;
      print_expr exp

  and print_case c =
    match c with
    | (name, typp, exp) ->
      print_id name;
      print_id typp;
      print_expr exp

  and print_binding b =
    match b with
    | BindingNoInit(name, typp) ->
      fprintf f "let_binding_no_init\n";
      print_id name;
      print_id typp
    | BindingInit(name, typp, exp) ->
      fprintf f "let_binding_init\n";
      print_id name;
      print_id typp;
      print_expr exp
  in

  let print_formal ff =
    match ff with
    | Formal(name, typp) ->
      print_id name;
      print_id typp
  in

  let print_feature ff =
    match ff with
    | AttributeNoInit(name, typp) ->
      fprintf f  "attribute_no_init\n";
      print_id name;
      print_id typp
    | AttributeInit(name, typp, init) ->
      fprintf f  "attribute_init\n";
      print_id name;
      print_id typp;
      print_expr init
    | Method(name, formal_list, typp, body) ->
      fprintf f  "method\n";
      print_id name;
      print_list print_formal formal_list;
      print_id typp;
      print_expr body
  in

  let print_class c =
    match c with
    | ClassNoInherits(name, f_list) ->
      print_id name;
      fprintf f  "no_inherits\n";
      print_list print_feature f_list
    | ClassInherits(name, parent, f_list) ->
      print_id name;
      fprintf f  "inherits \n";
      print_id parent;
      print_list print_feature f_list
  in

  print_list print_class result
end ;;
main() ;;
# 1222 "parser.ml"
