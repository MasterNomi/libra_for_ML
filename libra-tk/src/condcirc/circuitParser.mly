%{
(* Ocamlyacc header *)
open CircuitParseType
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
%token <string> Tnodename
%token <int * int> Tvar
%token <int> Tint
%token Teq
%token Tplus
%token Ttimes
%token TEOL
%token Tlparen
%token Trparen
%token EOF

%type <int list * CircuitParseType.parsenode list> pcircuit
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
  timesnode TEOL {$1}
| plusnode TEOL {$1}
| constnode TEOL {$1}
| wplusnode TEOL {$1}
| splitnode TEOL {$1}
| distnode TEOL {$1}
;

timesnode:
  Tnodename Teq timesbody  {PTimesNode($1,$3)}
;

timesbody:
  Tnodename {[$1]}
| Tweight   {[string_of_float $1]}
| Tnodename Ttimes timesbody {$1 :: $3}
| Tvar      {[var_to_string $1]}
| Tvar      Ttimes timesbody {(var_to_string $1) :: $3}
| Tweight   Ttimes timesbody {(string_of_float $1) :: $3}
;

plusnode:
  Tnodename Teq plusbody  {PPlusNode($1,$3)}
;

plusbody:
  Tnodename Tplus Tnodename {[$1; $3]}
| Tvar Tplus Tnodename {[var_to_string $1; $3]}
| Tnodename Tplus Tvar {[$1; var_to_string $3]}
| Tvar Tplus Tvar {[var_to_string $1; var_to_string $3]}
| Tnodename Tplus plusbody {$1 :: $3}
| Tvar      Tplus plusbody {(var_to_string $1) :: $3}
;

constnode:
  Tnodename Teq weight {PConstNode($1, $3)}
;

wplusnode:
  Tnodename Teq wplusbody  {PWPlusNode(($1),($3))}
;

wplusbody:
  weight Tnodename {[(($1),($2))]}
| weight Tnodename Tplus wplusbody {(($1),($2)) :: $4}
;

weight:
  Tweight {(log $1)}
| Tint {(log (float_of_int $1))}
;

splitnode:
  Tnodename Teq splitbody  {PSplitNode(($1),($3))}
;

splitbody:
  weight Tvar Tnodename {[(($1),(fst $2),($3))]}
| weight Tvar Tnodename Tplus splitbody {(($1),(fst $2),($3)) :: $5}
;

distnode:
  Tnodename Teq distbody  {PDistNode(($1),($3))}
;

distbody:
  weight Tvar {[(($1),(fst $2))]}
| weight Tvar Tplus distbody {(($1),(fst $2)) :: $4}
;

%%
