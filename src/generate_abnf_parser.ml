(** Use abnf_grammar.txt,sexp to generate an ABNF parser. *)
include struct
  open Parse_abnf_grammar_txt.Grammar_of_grammars

  let it = Blobs.abnf_grammar_sexp

  let abnf_grammar_sexp = it |> Core_kernel.Sexp.of_string

  let abnf_export = export_of_sexp abnf_grammar_sexp

  let _ : export = abnf_export
end

(** Generate fun clause: fun ((x1,x2),x3) -> ... *)
let _x = 
  let rec xs = function 
    | 1 -> "x1"
    | n -> Printf.sprintf "%s,x%d" (xs (n-1)) n
  in
  function 
  | 1 -> "x1"
  | n -> Printf.sprintf "(%s)" (xs n)

(** Generate an ABNF parser *)
let pretty_print () = 
  let tbl_nts = Hashtbl.create 100 in
  let tbl_tms = Hashtbl.create 100 in (* not including literals a"..." *)
  let open Parse_abnf_grammar_txt.Grammar_type in
  abnf_export |> List.map (fun (nt,syms_act_list) -> 
    let pp_nt ?(add_nt_prefix=true) nt = 
      "_"^nt |> fun s ->
      Hashtbl.replace tbl_nts s ();
      (if add_nt_prefix then "nt " else "")^s
    in
    let pp_tm = function
      | Tm_lit (s1,s2,s3) -> s1^s2^s3 |> fun c -> Printf.sprintf "a%s" c
      | Tm_qu s -> 
        Hashtbl.replace tbl_tms s ();
        s  
    in
    let pp_sym = function
      | Nt nt -> pp_nt nt
      | Tm tm -> pp_tm tm
    in
    let pp_syms = function
      | syms -> syms |> List.map pp_sym |> String.concat "," |> fun s -> "("^s^")" in
    let pp_act n s = Printf.sprintf "(fun %s -> %s)" (_x n) s  in
    let pp_syms_act (syms,act) = 
      Printf.sprintf "%s    %s" 
        (pp_syms syms)
        (pp_act (List.length syms) act) 
    in
    let rec pp_rule (nt,rhss) = rhss |> List.map (fun rhs -> pp_rule' (nt,rhs)) |> String.concat "\n"
    and pp_rule' (nt,(syms,act)) = 
      Printf.sprintf "%s -->_%d %s;" 
        (pp_nt ~add_nt_prefix:false nt)   (* NOTE just print raw on lhs *)
        (List.length syms) 
        (pp_syms_act (syms,act))
    in
    pp_rule (nt,syms_act_list))
  |> String.concat "\n\n"
  |> fun s ->
  Hashtbl.to_seq_keys tbl_nts |> List.of_seq |> fun nts -> 
  Hashtbl.to_seq_keys tbl_tms |> List.of_seq |> fun tms -> 
  Printf.printf {|
(** NOTE this is generated code, see %s*)

(** NOTE terminals:
%s
*)

(** NOTE nonterminals:
%s
*)

(** Non-terminals *)
let %s = %s 


(** Rules; NOTE these have to be added in the given order due to short-circuit alternatives *)
let  = begin
%s
end
|}
    __FILE__
    (String.concat "\n" tms)
    (String.concat "\n" nts)
    (String.concat "," nts)
    (nts |> List.map (fun nt ->
         Printf.sprintf {|mk_nt "%s"|} nt) |> String.concat ",")
    s;
  print_endline "";
  ()

let main () = pretty_print ()
