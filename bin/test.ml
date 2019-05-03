open Example_grammars

let main i = 
  match i with
  | 1 -> Parse_abnf_grammar_txt.main()
  | 2 -> Generate_abnf_parser.main()
  | 3 -> Abnf_parser.test()
  | _ -> failwith __LOC__

let _ = 
  List.iter main 
    (* [1;2] *)
    [3]
(*
let _ = Parse_abnf_grammar_txt.test ()

let _ = Generate_abnf_parser.pretty_print()  
*)
