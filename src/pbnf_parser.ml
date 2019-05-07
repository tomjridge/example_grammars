(** Parse the grammar description in abnf_grammar.txt; this is a
   simple format, with actions *)

(** Helper to avoid dependence on associativity of -- *)
let _3 ((x1,x2),x3) = (x1,x2,x3)

module Grammar_type = struct
  open Core_kernel
  type nt = string [@@deriving sexp]
  type tm = Tm_lit of (string * string * string) | Tm_qu of string [@@deriving sexp]
  type sym = Nt of nt | Tm of tm [@@deriving sexp]
  type syms = sym list [@@deriving sexp]
(*
  type action = string [@@deriving sexp]
  type syms_act = syms * action [@@deriving sexp]
  type rhs = syms_act list [@@deriving sexp]
  type rule = nt * rhs list [@@deriving sexp]
  type grammar = rule list [@@deriving sexp]

  let grammar_to_string g = 
    g |> sexp_of_grammar |> Core_kernel.Sexp.to_string_hum 
*)
end
(* let grammar_to_string = Grammar_type.grammar_to_string *)


module Grammar_of_grammars = struct 
  open Grammar_type
  open P0_lib

  let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)

  let _whitespace = re Re.(rep (set " \n")) 

  let rec ws s = 
    to_fun
      (_whitespace -- opt (comm -- of_fun ws) >>= fun _ -> return ())
      s
  let ws = of_fun ws

  let nt = re Re.(rep1 (alt[rg 'A' 'Z';char '_']))

  let tm_lit = 
    let sq = "'" in
    let dq = "\"" in
    ((a sq -- upto_a sq -- a sq) ||
     (a dq -- upto_a dq -- a dq))
    >>= fun ((q1,s),q2) -> Tm_lit (q1,s,q2) |> return

  let tm_re = 
    a"?" -- re Re.(rep1 (alt [alnum;char '_'])) -- a"?" >>= fun ((_,s),_) ->
    return (Tm_qu s)

  let tm = tm_lit || tm_re

  let sym = 
    (* debug ~msg:"XXXsymXXX" () >>= fun _ ->   *)
    (nt >>= fun x -> return (Nt x)) || 
    (tm >>= fun x -> return (Tm x))

  let syms = plus ~sep:ws sym

  let action = 
    (* debug ~msg:"XXXactXXX" () >>= fun _ ->   *)
    a"{{" -- upto_a "}}" -- a"}}" >>= fun ((_,x),_) -> return x

  let syms_act = syms -- ws -- action >>= (fun ((x,_),y) -> return (x,y))

  (* the rhs is a sequence of syms_act *)
  let rhs = (opt (a"|" -- ws)) -- (plus ~sep:(ws -- a"|" -- ws) syms_act)
        >>= fun (a,b) -> return b

  let rule = 
    (* debug ~msg:"XXXruleXXX" () >>= fun _ ->  *)
    nt -- (ws -- a "->" -- ws) -- rhs
    >>= fun ((nt,_),rhs) -> return (nt,rhs)

  let rules = star ~sep:ws rule 

  let grammar = ws -- rules -- ws >>= fun ((_,rs),_) -> return rs

  let _ : (string * (sym list * string) list) list m = grammar

  (** for pretty printing, we use type export, which avoids some of
     the complexity of the full Grammar_type *)

  open Core_kernel
  type export = (string * (sym list * string) list) list [@@deriving sexp]
  let export_to_string g = 
    g |> sexp_of_export |> Core_kernel.Sexp.to_string_hum 

end
open Grammar_of_grammars

open Core_kernel

let main () = 
  P0_lib.to_fun grammar Blobs.abnf_pbnf |> fun (Some(g,"")) -> 
  g |> export_to_string |> fun str -> 
  print_endline str

let test () = 
  P0_lib.to_fun grammar Blobs.abnf_pbnf |> fun (Some(g,"")) -> 
  g |> export_to_string |> fun str -> 
  print_endline str;
  str |> Sexp.of_string |> fun sexp ->
  sexp |> Sexp.to_string_hum |> fun str' ->
  print_endline str';
  assert(str=str');
  sexp |> export_of_sexp |> fun e -> 
  print_endline "OK";
  print_endline Blobs.abnf_sexp;
  (* Printf.printf "%s %d %d\n%!" __LOC__ (String.length str) (String.length Blobs.abnf_grammar_sexp); *)
  assert(str = Blobs.abnf_sexp)
(*
  export_of_sexp |> fun g' ->
  g |> export_to_string |> Core_kernel.sexp_of_string |> export_of_sexp |> fun g' ->
  assert(g=g')
*)
