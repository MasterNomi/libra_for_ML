%{
(* Ocamlyacc header *)
open CondAcParseType;;
open Printf

let line = ref 1

let parse_error s =
  print_string "Parse error: ";
  print_endline s;
  printf "Line %d\n" !line;
  flush stdout;;

let var_to_string (var, value) = sprintf "v%d_%d" var value

%}

/* token declarations */
%token <float> Tweight
%token <int> Tint
%token Tevidence
%token Tplus
%token Ttimes
%token Tnum
%token Tvar
%token TEOL
%token Tlparen
%token Trparen
%token Tinverse
%token Texponentiate
%token EOF

%type <int list * int * CondAcParseType.parsenode list> pcircuit
/* start symbol */
%start pcircuit

%%

/* Ocamlyacc grammar and action rules */
pcircuit:
| intset evnum pnodelist {($1, $2, $3)}
;

evnum:
|	Tint TEOL {line := !line + 1; $1}
;

intset:
| Tlparen intsetbody {$2}
;

intsetbody:
| Tint intsetbody {$1 :: $2}
| Trparen TEOL { line := !line + 1 ; []}
| Trparen {[]}
| error TEOL { line := !line + 1; []}
;

pnodelist:
| pnode pnodelist { line := !line + 1; $1 :: $2}
| EOF {[]}
| error TEOL { line := !line + 1; []}
;

pnode:
| Tplus intlist {PPlusNode $2}
| Ttimes intlist {PTimesNode $2}
| Tnum Tweight TEOL {PConstNode (log $2)}
| Tnum Tint TEOL {PConstNode (log (float_of_int $2))}
| Tvar Tint Tint TEOL {PVarNode ($2, $3)}
| Tinverse intlist {PInverseNode $2}
| Texponentiate Tint TEOL {PExpNode ($2)}
| Tevidence Tint TEOL {PEvNode $2}
;

intlist:
| Tint intlist { $1 :: $2 }
| TEOL { [] }
;

%%
