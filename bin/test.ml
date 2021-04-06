open Example_grammars

let main i = 
  match i with
  | 1 -> Pbnf_parser.main()
  | 2 -> Generate_abnf_parser.main()
  | 3 -> Bin_util.time (fun () -> Abnf_parser.test()) |> fun f ->
         Printf.printf "Abnf_parser.test in %f\n" f
  | 4 -> let module X = Scala_example.Test() in ignore(X.test_12())
  | 5 -> Scala_example.Handwritten_parser.test()
  | _ -> failwith __LOC__

let _ = 
  match Sys.argv |> Array.to_list |> List.tl with
  | [] -> 
    List.iter main 
      (* [1;2] *)
      [2;3]
  | [n] -> main (n|>int_of_string)
  | _ -> failwith 
           (Printf.sprintf "Unrecognized number of command line arguments: %s\n" __FILE__)
