{
open AcParser;;
let line=ref 1
(* Raised when parsing ends *)
exception Eof;;
exception LexErr of string

let inc_lines s = 
  String.iter (function '\n' -> incr line | _ -> ()) s

}

let digits = ['0'-'9']+

rule lexer = parse
(* eat blank characters *)
    [' ' '\t'] {lexer lexbuf}
  | "EOF" {EOF}
  | digits '.' digits (['e' 'E'] ['+' '-']? digits)? 
      {Tweight (float_of_string(Lexing.lexeme lexbuf))}
  | digits {Tint (int_of_string (Lexing.lexeme lexbuf))}
  | '+' {Tplus} 
  | '*' {Ttimes} 
  | 'v' {Tvar} 
  | 'n' {Tnum} 
  | '(' {Tlparen}
  | ')' {Trparen}
  | ('\r'? '\n')+ {inc_lines (Lexing.lexeme lexbuf); TEOL}
  | eof {EOF}
  | _ {raise (LexErr ((Lexing.lexeme lexbuf) ^ 
       ": error on line " ^ string_of_int !line)) }
  
