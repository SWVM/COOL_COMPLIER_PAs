{
let line_number = ref 1
let depth       = ref 0   (*for nested comment*)

    (* report error and exit  *)
let report str  = Printf.printf "Error: %d Lexer: %s\n" !line_number str;
                  exit 1;;
    (*  remove double quote at the tail  *)
let rm_last str = String.sub str 0 ((String.length str)-1);;
    (*  Buffer  *)
let string_buff    = Buffer.create 1024
let buf_add str    = Buffer.add_string  string_buff str
let buf_chr chr    = Buffer.add_char string_buff chr
let buf_clear ()   = Buffer.clear string_buff
    (*  exit if string too long  *)
let check()        = if (Buffer.length string_buff) > 1024 then
                      begin
                        let     len = Buffer.length string_buff in
                        let message = Printf.sprintf "string constant is too long (%d > 1024)" len in
                        report message
                      end
let buf_get ()     = check(); Buffer.contents string_buff


type token =
  | Identifier of string
  | Integer of string
  | String of string
  | Type of string
  | Token of string

exception Eof

}


(* ================ hard coded (for case insensitive)  ================ *)
let aZ_0 = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let lower= ['a'-'z']
let cap  = ['A'-'Z']
let digit= ['0'-'9']
let a = ['a' 'A']
let b = ['b' 'B']
let c = ['c' 'C']
let d = ['d' 'D']
let e = ['e' 'E']
let f = ['f' 'F']
let g = ['g' 'G']
let h = ['h' 'H']
let i = ['i' 'I']
let j = ['j' 'J']
let k = ['k' 'K']
let l = ['l' 'L']
let m = ['m' 'M']
let n = ['n' 'N']
let o = ['o' 'O']
let p = ['p' 'P']
let q = ['q' 'Q']
let r = ['r' 'R']
let s = ['s' 'S']
let t = ['t' 'T']
let u = ['u' 'U']
let v = ['v' 'V']
let w = ['w' 'W']
let x = ['x' 'X']
let y = ['y' 'Y']
let z = ['z' 'Z']

(* ================ main  ================ *)
rule token = parse
  ['\t' '\b' '\r' ' ' '\012' '\011']  { token lexbuf }     (* skip blanks *)
  | ['\n' ]                           { incr line_number; token lexbuf }
  | "(*"                              { depth:=1;comment lexbuf}
  | "--"                              { comment_ lexbuf}
  | '@'                               {Token("at")}
  | c a s e                           {Token("case")}
  | c l a s s                         {Token("class")}
  | ','                               {Token("comma")}
  | ':'                               {Token("colon")}
  | '/'                               {Token("divide")}
  | '.'                               {Token("dot")}
  | e l s e                           {Token("else")}
  | '='                               {Token("equals")}
  | e s a c                           {Token("esac")}
  | f i                               {Token("fi")}
  | i f                               {Token("if")}
  | i n                               {Token("in")}
  | i n h e r i t s                   {Token("inherits")}
  | i s v o i d                       {Token("isvoid")}
  | "<-"                              {Token("larrow")}
  | '{'                               {Token("lbrace")}
  | "<="                              {Token("le")}
  | l e t                             {Token("let")}
  | l o o p                           {Token("loop")}
  | '('                               {Token("lparen")}
  | '<'                               {Token("lt")}
  | '-'                               {Token("minus")}
  | n e w                             {Token("new")}
  | n o t                             {Token("not")}
  | o f                               {Token("of")}
  | '+'                               {Token("plus")}
  | p o o l                           {Token("pool")}
  | "=>"                              {Token("rarrow")}
  | '}'                               {Token("rbrace")}
  | ')'                               {Token("rparen")}
  | ';'                               {Token("semi")}
  | t h e n                           {Token("then")}
  | '~'                               {Token("tilde")}
  | '*'                               {Token("times")}
  | w h i l e                         {Token("while")}
  | digit+           as lxm           { try
                                          Integer(Int32.to_string(Int32.of_string lxm))
                                        with | _ -> report ("not a non-negative 32-bit signed integer:" ^ lxm)
                                      }
  | '\"'                              { buf_clear(); str lexbuf }
  | cap aZ_0*        as lxm           { Type(lxm)}
  | t r u e                           {Token("true")}
  | f a l s e                         {Token("false")}
  | lower aZ_0*      as lxm           { Identifier(lxm)}
  | eof                               { raise Eof }
  | _                as lxm           { report (Printf.sprintf "invalid character: %c" lxm)}

(* ================ multi-line comment ================ *)
and comment = parse
    '\n'                              { incr line_number; comment lexbuf}
  | "(*"                              { incr depth; comment lexbuf}
  | "*)"                              { decr depth; if !depth=0 then token lexbuf else comment lexbuf}
  | _                                 { comment lexbuf}
  | eof                               { report "EOF in (* comment *)"}

(* ================ single-line comment ================ *)
and comment_= parse
    '\n'                              {incr line_number; token lexbuf}
  | _                                 { comment_ lexbuf}
  | eof                               { raise Eof }

(* ================ string ================ *)
and str = parse
    ['\n' '\000']                     { report "invalid character \""}
  | "\\" "\000"                       { report "invalid character \""}
  | '\"'                              { String(buf_get())}
  | "\\" [^'\n']     as lxm           { buf_add lxm; str lexbuf}
  | _  as lxm                         { buf_chr lxm; str lexbuf}
  | eof                               { report "invalid character \""}



{

let outbuf = Buffer.create 1024 in

try

    let filename    = Sys.argv.(1) in
    let file_handle = open_in filename in
    let lexbuf      = Lexing.from_channel file_handle in

  while true do
    let result = token lexbuf in
    match result with
     |Identifier(i) -> Printf.bprintf outbuf "%d\nidentifier\n%s\n" !line_number i
     |Integer(i)    -> Printf.bprintf outbuf "%d\ninteger\n%s\n" !line_number i
     |String(i)     -> Printf.bprintf outbuf "%d\nstring\n%s\n" !line_number i
     |Type(i)       -> Printf.bprintf outbuf "%d\ntype\n%s\n" !line_number i
     |Token(i)      -> Printf.bprintf outbuf "%d\n%s\n" !line_number i
  done
with | Eof -> begin
        let filename = Sys.argv.(1) ^ "-lex" in
        let file_handle = open_out filename in
        Printf.fprintf file_handle "%s" (Buffer.contents outbuf);
        close_out file_handle
        end
}
