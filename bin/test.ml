open Example_grammars

let _ = match 2 with
| 1 -> Parse_abnf_grammar_txt.main()
| 2 -> Generate_abnf_parser.main()
| _ -> failwith __LOC__

(*
let _ = Parse_abnf_grammar_txt.test ()

let _ = Generate_abnf_parser.pretty_print()  
*)
