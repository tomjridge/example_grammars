open P4_lib
open Abnf_grammar_p4

let main () = 
  let fname = Sys.argv.(1) in
  let Some txt = read_file_as_string fname in
  let eg = run_parser_string parse_S txt in
  ()

let _ = main ()


(*
Local Variables:
mode: tuareg
End:
*)

