open Example_grammars

let main i = 
  match i with
  | 1 -> Pbnf_parser.main()
  | 2 -> Generate_abnf_parser.main()
  | 3 -> Bin_util.time (fun () -> Abnf_parser.test()) |> fun f ->
         Printf.printf "Abnf_parser.test in %f\n" f
  | _ -> failwith __LOC__

let _ = 
  List.iter main 
    (* [1;2] *)
    [3]
