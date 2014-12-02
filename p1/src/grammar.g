(* example specification of a grammar file *)
open P1_lib

let header = until_a ("<<"^"g<<")
let start_of_g = a ("<<"^"g<<")
let end_of_g = a (">>"^"g>>")
let grammar = until_a (">>"^"g>>")
let footer = until_EOF
let ws = parse_RE "[ \n]*"

<<g<<

S -> ?header? ?start_of_g? ?ws? G ?ws? ?end_of_g? ?footer? 
  {{ fun (h,(_,(_,(g,(_,(_,f)))))) -> print_endline (content g); (h,g,f) }}

G -> ?grammar? {{ fun x -> x }}

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
