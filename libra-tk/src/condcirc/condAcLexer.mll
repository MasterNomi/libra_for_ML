{
open CondAcParser;;
let line=ref 1
(* Raised when parsing ends *)
exception Eof;;
}

let digits = ['0'-'9']+

rule lexer = parse
(* eat blank characters *)
    [' ' '\t'] {lexer lexbuf}
  | "EOF" {EOF}
  | digits '.' digits (['e' 'E'] '-'? digits)? 
      {Tweight (float_of_string(Lexing.lexeme lexbuf))}
  | digits {Tint (int_of_string (Lexing.lexeme lexbuf))}
  | '+' {Tplus} 
  | '*' {Ttimes} 
  | 'v' {Tvar} 
  | 'n' {Tnum} 
  | '(' {Tlparen}
  | ')' {Trparen}
	| '/' {Tinverse}
	|	'e' {Tevidence}
	|	'#' {Texponentiate}
  | ['\n' '\r']+ {TEOL}
  | eof {EOF}
  | _ {failwith((Lexing.lexeme lexbuf) ^ 
       ": mistake at circuit " ^ string_of_int !line)}
  
