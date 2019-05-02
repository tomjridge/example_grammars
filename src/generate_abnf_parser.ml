(** Use abnf_grammar.{txt,sexp} to generate an ABNF parser. *)
include struct
  open Parse_abnf_grammar_txt.Grammar_of_grammars

  let it = Blobs.abnf_grammar_sexp

  let abnf_grammar_sexp = it |> Core_kernel.Sexp.of_string

  let abnf_export = export_of_sexp abnf_grammar_sexp

  let _ : export = abnf_export
end

(** Generate fun clause: fun ((x1,x2),x3) -> ... *)
let rec _x n = match n with
  | 1 -> "x1"
  | _ -> _x (n-1) |> fun left ->
         Printf.sprintf "(%s,x%d)" left n

(** Generate an ABNF parser *)
let pretty_print () = 
  let tbl = Hashtbl.create 100 in
  let open Parse_abnf_grammar_txt.Grammar_type in
  abnf_export |> List.map (fun (nt,syms_act_list) -> 
    let pp_nt nt = 
      "_"^nt |> fun s ->
      Hashtbl.replace tbl s ();
      s
    in
    let pp_tm = function
      | Tm_lit (s1,s2,s3) -> s1^s2^s3 |> fun c -> Printf.sprintf "a%s" c
      | Tm_qu s -> s  (* FIXME may want to remove q marks *)
    in
    let pp_sym = function
      | Nt nt -> pp_nt nt
      | Tm tm -> pp_tm tm
    in
    let pp_syms = function
      | [sym] -> pp_sym sym
      | syms -> syms |> List.map pp_sym |> String.concat "," |> fun s -> "("^s^")" in
    let pp_act n s = Printf.sprintf "(fun %s -> %s)" (_x n) s  in
    let pp_syms_act (syms,act) = 
      Printf.sprintf "%s    %s" 
        (pp_syms syms)
        (pp_act (List.length syms) act) 
    in
    let rec pp_rule (nt,rhss) = rhss |> List.map (fun rhs -> pp_rule' (nt,rhs)) |> String.concat "\n"
    and pp_rule' (nt,(syms,act)) = 
      Printf.sprintf "%s -->_%d %s;" (pp_nt nt) (List.length syms) (pp_syms_act (syms,act))
    in
    pp_rule (nt,syms_act_list))
  |> String.concat "\n\n"
  |> fun s ->
  Hashtbl.to_seq_keys tbl |> List.of_seq |> fun nts -> 
  Printf.printf {|
(** NOTE this is generated code, see %s*)

let %s = %s in
[
%s
]
|}
    __FILE__
    (String.concat "," nts)
    (nts |> List.map (fun _ -> "mk_nt()") |> String.concat ",")
    s;
  print_endline
