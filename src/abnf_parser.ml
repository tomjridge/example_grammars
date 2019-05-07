open Abnf_parser_prelude
open Abnf_parser_prelude.Abnf_grammar_datatype

module Internal_generated_parser
    (Reqs: sig 
       include INTERNAL_REQS 
       with type 'a rhs = 'a P0.m 
        and type rule = unit end)
  : sig val _S : rulelist Reqs.nt end 
= struct
  open Reqs

  module Terminals = struct
    (** NOTE the following terminals are defined using Str format regexps *)

    let _str_regexp (s:string) : string sym = regexp_string_to_tm s
    let rulename = _str_regexp "[A-Za-z][-A-Za-z0-9]*"
    let repeat = _str_regexp "[0-9]*[*][0-9]*\\|[0-9]+"
    let dquote = a{|"|}
    let char_vals = _str_regexp "[^\"]*"
    let one_star_bit = _str_regexp "[01]+"
    let bin_val_rest = _str_regexp "\\([.][0-1]+\\)+\\|[-][0-1]+"
    let one_star_digit = _str_regexp "[0-9]+"
    let dec_val_rest = _str_regexp "\\([.][0-9]+\\)+\\|[-][0-9]+"
    let one_star_hexdig = _str_regexp "[0-9A-Fa-f]+"
    let hex_val_rest = _str_regexp "\\([.][0-9A-Fa-f]+\\)+\\|[-][0-9A-Fa-f]+"
    let prose_val_chars = _str_regexp "[^>]*"
    let crlf = _str_regexp "[\n]" (* FIXME really CR LF *)
    let vchar = _str_regexp "[\x21-\x7E]"
    let wsp = _str_regexp "[ \x09]"
    let ws = _str_regexp "[ \n]*"
    let wsplus = _str_regexp "[ \n]+"
    let eps = a""
  end  (* Terminals *)

  include Abnf_parser_generated.Make(Reqs)(Terminals)

  let _ : rulelist nt = _S

end  (* Internal_generated_parser *)


(** Instantiate the Internal functor, and export the results. This
   interpretation takes 'a rhs to be (univ sym list * act), which is
   the most concrete choice. An alternative would be (univ m list *
   act). This would allow the user to edit the generated code
   directly. *)
module Internal2 = struct

  module Typed_syms = struct
    (** human readable names *)
    let nt_to_hum_tbl = Hashtbl.create 100
    let nt_to_hum i = 
      Hashtbl.find_opt nt_to_hum_tbl i |> function
      | None -> failwith (
          Printf.sprintf "unknown nonterminal with id %d (%s)" i __FILE__)
      | Some s -> s

    type 'a nt = int

    let mk_nt = 
      let free = ref 0 in
      fun s -> 
        let x = !free in
        free:=!free+1;
        Hashtbl.replace nt_to_hum_tbl x s;
        Printf.printf "Non-term %s with id %d\n%!" s x;
        x

    type 'a sym = Nt of 'a nt | Tm of string P0.m

    let a s = Tm (P0.a s)
  end


  module Reqs = struct
    include Typed_syms

    type univ

    (* rhs is just a parser; not possible to inspect the structure  *)
    type 'a rhs = 'a P0.m

    type s2p = {
      s2p: 'a. 'a sym -> 'a P0.m
    }

    let s2p: s2p ref = ref {s2p = (fun _ -> failwith __LOC__) }

    (* This representation converts syms*act directly to 'a m *)
    module Underscores_2 : sig 
      val _1: 'a sym -> ('a -> 'z) -> 'z rhs
      val _2: ('a sym * 'b sym) -> ('a * 'b -> 'z) -> 'z rhs
      val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'z) -> 'z rhs
      val _4: ('a sym * 'b sym * 'c sym * 'd sym) -> ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
      val _5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) -> ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs
    end = struct
      open P0
      let p sym = of_fun (fun s -> to_fun (!s2p.s2p sym) s) (* NOTE the delayed evaluation of s2p *)
      let _ = p
      let _1 s f = p s >>= fun x -> return (f x)
      let _2 (s1,s2) f = p s1 -- p s2 >>= fun (x1,x2) -> 
        return (f (x1,x2))
      let _3 (s1,s2,s3) f = p s1 -- p s2 -- p s3 >>= fun ((x1,x2),x3) -> 
        return (f (x1,x2,x3))
      let _4 (s1,s2,s3,s4) f = p s1 -- p s2 -- p s3 -- p s4 >>= fun (((x1,x2),x3),x4) -> 
        return (f (x1,x2,x3,x4))
      let _5 (s1,s2,s3,s4,s5) f = p s1 -- p s2 -- p s3 -- p s4 -- p s5 >>= fun ((((x1,x2),x3),x4),x5) -> 
        return (f (x1,x2,x3,x4,x5))
    end
    include Underscores_2

    type rule = unit 
    (* Rule: 'a nt * 'a rhs -> rule ; we mutate 'a nt directly *)

    module Rules = struct
      open P0

      (* store rules *)
      let tbl = Hashtbl.create 100    

      let delay () = of_fun (fun s -> Some((),s))

      let new_rhs_id =
        let tbl = Hashtbl.create 100 in
        fun nt -> 
          Hashtbl.find_opt tbl nt 
          |> (function
              | None -> (ref 0 |> fun r -> Hashtbl.replace tbl nt r; r)
              | Some r -> r)
          |> fun r -> 
          let y = !r in
          r:=!r+1;
          y

      let get_state : unit -> State.t m = fun () -> 
        of_fun (fun s -> Some(s,s))

      let set_state : State.t -> unit m = fun s -> 
        of_fun (fun _ -> Some((),s))

      (* open P0 *)

      let debug_nt ~nt_hum ~input =
        get_state () >>= fun s ->
        set_state {s with debug=(nt_hum,input)::s.debug}

      let replace_newlines ~char s = 
        Core_kernel.String.tr ~target:'\n' ~replacement:char s

      let add_rule (type a) ~(debug:bool) (nt:a nt) (rhs:a rhs) : unit = 
        let rhs: univ rhs = Obj.magic rhs in
        let rhs = 
          match debug with
          | false -> rhs 
          | true -> (
            let nt_hum = nt_to_hum nt in
            let id = new_rhs_id nt in
            get_state () >>= fun s -> 
            set_state {s with debug=(nt_hum,s.input)::s.debug} >>= fun () ->
            Printf.printf "(%s) %d%s%d\n" 
              (s.input |> fun s -> String.sub s 0 (min (String.length s) 10) |> replace_newlines ~char:'@')
              (*(String.make (List.length s.debug) '.') *) (List.length s.debug)
              nt_hum 
              id;
            rhs)
        in
        Hashtbl.find_opt tbl nt |> (function
            | None -> Hashtbl.replace tbl nt rhs
            | Some p -> Hashtbl.replace tbl nt (p || rhs))

      let _ = add_rule
    end  (* Rules *)

    (** FIXME change this to enable debugging *)
    let debug = false

    let ( --> ) nt rhs = Rules.add_rule ~debug nt rhs

    let nt (nt:'a nt) : 'a sym = Nt nt

    let regexp_string_to_tm s = Tm (P0.str_re s)


    module Internal3 = struct
      let rec nt_to_parser (nt:univ nt) : univ P0.m = 
        (* Printf.printf "nt_to_parser called with nt %s\n%!" (nt_to_hum nt); *)
        (* we look up the nt in the hashtbl, and convert to a parser *)
        Hashtbl.find_opt Rules.tbl nt |> function
        | None -> failwith (Printf.sprintf "No rules found for non-terminal %s" (nt_to_hum nt))
        | Some p -> p
      and sym_to_parser = function
        | Nt nt -> nt_to_parser nt
        | Tm p -> Obj.magic p

      let _ = nt_to_parser
    end

    let nt_to_parser : 'a nt -> 'a P0.m = 
      fun nt -> Obj.magic (Internal3.nt_to_parser nt)

    let sym_to_parser : 'a sym -> 'a P0.m = 
      fun s -> Obj.magic (Internal3.sym_to_parser s)

    let _ = s2p:={s2p=sym_to_parser}

    let _ = nt_to_parser
  end  (* Reqs *)

  (** Here we finally instantiate our generated parser code *)
  module Internal_instance = Internal_generated_parser(Reqs)


  (** What we export *)
  module Export : sig
    type 'a nt
    val abnf_parser_start_nonterm : rulelist nt
    val nt_to_parser: 'a nt -> 'a P0.m
  end = struct
    type 'a nt = int
    let abnf_parser_start_nonterm = Internal_instance._S
    let nt_to_parser = Reqs.nt_to_parser
  end
    
end  (* Internal2 *)

include Internal2.Export

let _S = nt_to_parser abnf_parser_start_nonterm 

open P0
open State

let test () = 
  let make_state input = {State.empty_state with input} in
  let s = make_state Blobs.imap_protocol_abnf in
  to_fun _S s
  |> function
  | None -> failwith __LOC__
  | (Some(_,rest)) -> 
    Printf.printf 
      "Parsed IMAP grammar. Remaining input: %s  (%s)\n%!"
      rest.input __FILE__


(*
let debug ?(msg="") p = P0.(
    of_fun (fun s -> 
        Printf.printf "debug called; msg=%s; input=>>%s<<\n%!" msg s.input;
        to_fun p s))
*)

