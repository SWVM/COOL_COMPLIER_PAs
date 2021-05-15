%{
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

%}



/* File parser.mly */
%token <string * string> IDENTIFIER INTEGER STRING TYPE
%token <string> AT CASE CLASS COMMA COLON DIVIDE DOT ELSE EQUALS ESAC FALSE FI IF IN INHERITS ISVOID LARROW LBRACE LE LET LOOP LPAREN LT MINUS NEW NOT OF PLUS POOL RARROW RBRACE RPAREN SEMI THEN TILDE TIMES TRUE WHILE
%token EOF

%nonassoc  LE LT EQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%right ASSIGN

%start cool_prog_entry             /* the entry point */
%type <cool_prog> cool_prog_entry
%%
cool_prog_entry : class_list EOF                                      { $1  }
                ;
class_list      : /* empty lilst */                                   { [] }
                | cool_class SEMI class_list                          {$1 :: $3}
                ;
cool_class      : CLASS TYPE INHERITS TYPE LBRACE feature_list RBRACE { ClassInherits($2, $4, $6) }
                | CLASS TYPE LBRACE feature_list RBRACE               { ClassNoInherits($2, $4) }
                ;
feature_list    : /* empty lilst */                                   { [] }
                | feature SEMI feature_list                           { $1 :: $3}
                ;
feature         : IDENTIFIER LPAREN formal_list RPAREN COLON TYPE LBRACE expr RBRACE    { Method($1, $3, $6, $8) }
                | IDENTIFIER COLON TYPE LARROW expr                                     { AttributeInit($1, $3, $5) }
                | IDENTIFIER COLON TYPE                                                 { AttributeNoInit($1, $3) }
                ;
formal_list     :  /* empty lilst */                                  { [] }
                | formal formal_tail                                  { $1 :: $2 }
                ;
formal_tail     : /* empty lilst */                                   { [] }
                | COMMA formal formal_tail                            { $2 :: $3 }
                ;
formal          : IDENTIFIER COLON TYPE                               { Formal($1, $3) }
                ;
expr            : IDENTIFIER LARROW expr                              { let line, _ = $1 in (line, Assign($1, $3)) }
                | expr AT TYPE DOT IDENTIFIER LPAREN param RPAREN     { let line, _ = $1 in (line, SDispatch($1, $3, $5, $7)) }
                | expr DOT IDENTIFIER LPAREN param RPAREN             { let line, _ = $1 in (line, DDispatch($1, $3, $5)) }
                | IDENTIFIER LPAREN param RPAREN                      { let line, _ = $1 in (line, SelfDispatch($1, $3)) }
                | IF expr THEN expr ELSE expr FI                      { ($1, If($2, $4, $6)) }
                | WHILE expr LOOP expr POOL                           { ($1, While($2, $4)) }
                | LBRACE block RBRACE                                 { ($1, Block($2))}
                | LET binding_list IN expr                            { ($1, Binding((List.rev $2), $4)) }
                | CASE expr OF case_list ESAC                         { ($1, Case($2, $4)) }
                | NEW TYPE                                            { ($1, New($2)) }
                | ISVOID expr                                         { ($1, UniOp($2, "isvoid")) }
                | expr PLUS expr                                      { let line, _ = $1 in (line, BinaryOP($1, $3, "plus")) }
                | expr MINUS expr                                     { let line, _ = $1 in (line, BinaryOP($1, $3, "minus")) }
                | expr TIMES expr                                     { let line, _ = $1 in (line, BinaryOP($1, $3, "times")) }
                | expr DIVIDE expr                                    { let line, _ = $1 in (line, BinaryOP($1, $3, "divide")) }
                | TILDE expr                                          { ($1, UniOp($2, "negate"))}
                | expr LT expr                                        { let line, _ = $1 in (line, BinaryOP($1, $3, "lt")) }
                | expr LE expr                                        { let line, _ = $1 in (line, BinaryOP($1, $3, "le")) }
                | expr EQUALS expr                                    { let line, _ = $1 in (line, BinaryOP($1, $3, "eq")) }
                | NOT expr                                            { ($1, UniOp($2, "not")) }
                | LPAREN expr RPAREN                                  { $2 }
                | IDENTIFIER                                          { let line, _   = $1 in (line, Id($1))}
                | INTEGER                                             { let line, v = $1 in (line, Integer(v)) }
                | STRING                                              { let line, v = $1 in (line, String(v)) }
                | TRUE                                                { ($1, Bool("true")) }
                | FALSE                                               { ($1, Bool("false")) }
                ;
block           : expr SEMI             { $1 :: [] }
                | expr SEMI block       { $1 :: $3 }
                ;
param           :                       { [] }
                | expr param_tail       { $1 :: $2}
param_tail      : COMMA expr param_tail { $2 :: $3}
                |                       { [] }
                ;
                ;
binding_list    : binding                             {$1::[]}
                | binding_list COMMA binding          { $3 :: $1 }
                ;
binding         : IDENTIFIER COLON TYPE               { BindingNoInit($1, $3)}
                | IDENTIFIER COLON TYPE LARROW expr   { BindingInit($1, $3, $5) }
                ;
case_list       :  case_                                  { $1 :: []}
                |  case_  case_list                       { $1 :: $2 }
                ;
case_           : IDENTIFIER COLON TYPE RARROW expr SEMI      { ($1, $3, $5)}
                ;


%%
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
