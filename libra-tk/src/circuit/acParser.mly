%{
(* Ocamlyacc header *)
open AcParseType;;
open Printf
open Lexing
open Parsing

exception ParseErr of string 

let parser_line = ref 1

let parse_error s =
  let msg = sprintf "%s on line %d" s !parser_line in
  raise (ParseErr msg)

let var_to_string (var, value) = sprintf "v%d_%d" var value

%}

/* token declarations */
%token <float> Tweight
%token <int> Tint
%token Tplus
%token Ttimes
%token Tnum
%token Tvar
%token TEOL
%token Tlparen
%token Trparen
%token EOF

%type <int list * AcParseType.parsenode list> pcircuit
/* start symbol */
%start pcircuit

%%

/* Ocamlyacc grammar and action rules */
pcircuit:
| intset pnodelist {($1, $2)}
;

intset:
| Tlparen intsetbody {$2}
;

intsetbody:
| Tint intsetbody {$1 :: $2}
| Trparen TEOL {incr parser_line; []}
| Trparen {[]}
| error TEOL {incr parser_line; []}
;

pnodelist:
| pnode pnodelist {$1 :: $2}
| EOF {[]}
| error TEOL {incr parser_line; []}
;

pnode:
| Tplus intlist {PPlusNode $2}
| Ttimes intlist {PTimesNode $2}
| Tnum Tweight TEOL {PConstNode (log $2)}
| Tnum Tint TEOL {PConstNode (log (float_of_int $2))}
| Tvar Tint Tint TEOL {PVarNode ($2, $3)}
;

intlist:
| Tint intlist { $1 :: $2 }
| TEOL {incr parser_line; [] }
;

%%
