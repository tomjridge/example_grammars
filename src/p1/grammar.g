(* example specification of a grammar file *)
open P1_lib

let header = until_a "<<g<<"
let footer = until_EOF
let ws = parse_RE "[ \n]*"

<<g<<
S -> ?header? "<<g<<" ?ws? G ?ws? ">>g>>" ?footer? {{ fun (h,(_,(_,(g,(_,(_,f)))))) -> (h,g,f) }}
>>g>>

let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let _ = run_parser_string parse_S txt in
  ()

let _ = main ()


Local Variables:
mode: tuareg
End:
