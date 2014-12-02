(* example specification of a grammar file *)
open P1_lib

let header = until_a ("<<"^"g<<")
let start_of_g = a ("<<"^"g<<")
let end_of_g = a (">>"^"g>>")
let grammar = until_a (">>"^"g>>")
let footer = until_EOF
let ws = parse_RE "[ \n]*"

let nt = parse_RE "[A-Z]+"
let not_dquote = until_a "\""
let not_squote = until_a "'"
let not_q = until_a "?"

let fixme = parse_RE "FIXME"

let act = (a "{{") **> until_a "}}" **> (a "}}") >> (fun (_,(x,_)) -> x)

let c = content
let id = fun x -> x

<<g<<

S -> ?header? ?start_of_g? ?ws? G ?ws? ?end_of_g? ?footer? 
  {{ fun (h,(_,(_,(g,(_,(_,f)))))) -> print_endline "parsed (content g)"; (h,g,f) }}

G -> RULES {{ fun x -> `RULES x }}
| ?fixme? {{ fun x -> `RULES [] (* FIXME *) }}

RULES -> RULE     {{ fun x -> [x] }}
| RULE ?ws? RULES {{ fun (x,(_,y)) -> x::y }}

RULE -> ?nt? ?ws? "->" ?ws? RHS {{ fun (nt,(_,(_,(_,rhs)))) -> `RULE(nt,rhs) }}

RHS -> SYMSACT              {{ fun x -> [x] }}
| SYMSACT ?ws? "|" ?ws? RHS {{ fun (x,(_,(_,(_,xs)))) -> x::xs }}

SYMSACT -> SYMS ?ws? ACT {{ fun (x,(_,y)) -> `SYMSACT(x,y) }}

SYMS -> SYM     {{ fun x -> [x] }}
| SYM ?ws? SYMS {{ fun (x,(_,y)) -> x::y }}

ACT -> ?act? {{ id }}

SYM -> ?nt?             {{ fun x -> `NT (c x) }}
| "?" ?not_q? "?"       {{ fun (_,(x,_)) -> `TM ("?"^(c x)^"?") }}
| '"' ?not_dquote? '"'  {{ fun (_,(x,_)) -> `TM ("\""^(c x)^"\"") }}
| "'" ?not_squote? "'"  {{ fun (_,(x,_)) -> `TM ("\""^(c x)^"\"") }}

>>g>>

let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let _ = run_parser_string parse_S txt in
  ()

let _ = main ()

(*
Local Variables:
mode: tuareg
End:
*)
