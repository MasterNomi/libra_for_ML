{
open CircuitParser;;
let line=ref 1
(* Raised when parsing ends *)
exception Eof;;
}

let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let nbegin = ['a'-'u' 'w'-'z' 'A'-'Z' '_']
let digits = ['0'-'9']+

rule lexer = parse
(* eat blank characters *)
    [' ' '\t'] {lexer lexbuf}
  | "EOF" {raise Eof} 
  | (['1'-'9']['0'-'9']*) as i {Tint( int_of_string i )}
  | digits ('.' digits)? (['e' 'E'] '-'? digits)? 
      {Tweight( float_of_string(Lexing.lexeme lexbuf))}
  | nbegin alnum*
      {Tnodename(Lexing.lexeme lexbuf)}
  | 'v' (digits as var) '_' (digits as value)
      {Tvar( int_of_string var, int_of_string value )}
  | '=' {Teq}
  | '+' {Tplus}
  | '*' {Ttimes}
  | '(' {Tlparen}
  | ')' {Trparen}
  | ['\n' '\r']+ {TEOL}
  | eof {EOF}
  | _ {failwith((Lexing.lexeme lexbuf) ^ 
       ": mistake at circuit " ^ string_of_int !line)}
  
