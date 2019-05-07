(** A parser for ABNF format grammars *)

module P0 = P0_with_debug

let string_concat xs = String.concat "" xs 

(* let dest_Some = function | Some x -> x | _ -> failwith __LOC__ *)

module Abnf_grammar_datatype = struct
  type ty_option = OPTION of alternation 
  and group = GROUP of alternation 
  and element = EL_RULENAME of rulename | EL_GROUP of group | EL_OPTION of ty_option | EL_CHAR_VAL (* FIXME *) | EL_NUM_VAL | EL_PROSE_VAL 
  and repeat = REPEAT (* FIXME *) 
  and repetition = REP of (repeat option * element) 
  and concatenation = CONCATENATION of repetition list 
  and alternation = ALTERNATION of concatenation list 
  and elements = ELEMENTS of alternation 
  and defined_as = DAS_EQUAL | DAS_EQUAL_SLASH 
  and rulename = RULENAME of string 
  and rule = RULE of (rulename * defined_as * elements * string) 
  and rulelist_elt = RE_RULE of rule | RE_CWSP_CNL of string
  and rulelist = RULELIST of rulelist_elt list (* nonempty *)
end
open Abnf_grammar_datatype


module type INTERNAL_TYPED_SYMS = sig
  type 'a nt
  type 'a sym
  (* type 'a tm *)

  val mk_nt: string -> 'a nt

  val a: string -> string sym

  (** for defining terminals without exposing the sym types, we have
     an extra argument to convert a string regexp into a (terminal)
     sym *)
  val regexp_string_to_tm: string -> string sym
end


module type INTERNAL_REQS = sig 
  include INTERNAL_TYPED_SYMS
  type 'a rhs 
  type rule 
  val _1: 'a sym -> ('a -> 'z) -> 'z rhs
  val _2: ('a sym * 'b sym) -> ('a * 'b -> 'z) -> 'z rhs
  val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'z) -> 'z rhs
  val _4: ('a sym * 'b sym * 'c sym * 'd sym) -> ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
  val _5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) -> ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs

  val ( --> ) : 'a nt -> 'a rhs -> rule
  val nt : 'a nt -> 'a sym
end


module Internal_generated_parser
    (Reqs: sig 
       include INTERNAL_REQS 
       with type 'a rhs = 'a P0.m 
        and type rule = unit end)
  : sig val _S : rulelist Reqs.nt end 
= struct
  open Reqs

(*
let debug ?(msg="") p = P0.(
    of_fun (fun s -> 
        Printf.printf "debug called; msg=%s; input=>>%s<<\n%!" msg s.input;
        to_fun p s))
*)

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
  open Terminals

(* FIXME prefer to ppx_include the following? *)

(* NOTE in the following generated code, pairs and lists evaluate
   right to left; so later rules in a list take precendence; but this
   is not what we expect; so we just use a seq of statements *)

(** NOTE this is generated code, see src/generate_abnf_parser.ml*)

(** NOTE terminals:
dec_val_rest
vchar
crlf
prose_val_chars
one_star_bit
bin_val_rest
eps
dquote
one_star_digit
hex_val_rest
rulename
ws
char_vals
repeat
wsp
one_star_hexdig
wsplus
*)

(** NOTE nonterminals:
_NUM_VAL_REST
_CWSP
_PROSE_VAL
_REPEAT
_CHAR_VAL
_ALTERNATION
_REPETITION
_ELEMENT
_RULELIST
_CONCATENATION
_ELEMENTS
_CNL
_RULENAME
_EQUAL_OR_EQUAL_SLASH
_STAR_WSP_VCHAR
_S
_BIN_VAL
_GROUP
_DEC_VAL
_STAR_CONCATENATION_REST
_OPTION
_STAR_ALTERNATION_REST
_STAR_CWSP_CNL
_RULE
_HEX_VAL
_ONE_STAR_CWSP
_COMMENT
_DEFINED_AS
_NUM_VAL
_STAR_CWSP
_RULELIST_ELT
*)

(** Non-terminals *)
let _NUM_VAL_REST,_CWSP,_PROSE_VAL,_REPEAT,_CHAR_VAL,_ALTERNATION,_REPETITION,_ELEMENT,_RULELIST,_CONCATENATION,_ELEMENTS,_CNL,_RULENAME,_EQUAL_OR_EQUAL_SLASH,_STAR_WSP_VCHAR,_S,_BIN_VAL,_GROUP,_DEC_VAL,_STAR_CONCATENATION_REST,_OPTION,_STAR_ALTERNATION_REST,_STAR_CWSP_CNL,_RULE,_HEX_VAL,_ONE_STAR_CWSP,_COMMENT,_DEFINED_AS,_NUM_VAL,_STAR_CWSP,_RULELIST_ELT = mk_nt "_NUM_VAL_REST",mk_nt "_CWSP",mk_nt "_PROSE_VAL",mk_nt "_REPEAT",mk_nt "_CHAR_VAL",mk_nt "_ALTERNATION",mk_nt "_REPETITION",mk_nt "_ELEMENT",mk_nt "_RULELIST",mk_nt "_CONCATENATION",mk_nt "_ELEMENTS",mk_nt "_CNL",mk_nt "_RULENAME",mk_nt "_EQUAL_OR_EQUAL_SLASH",mk_nt "_STAR_WSP_VCHAR",mk_nt "_S",mk_nt "_BIN_VAL",mk_nt "_GROUP",mk_nt "_DEC_VAL",mk_nt "_STAR_CONCATENATION_REST",mk_nt "_OPTION",mk_nt "_STAR_ALTERNATION_REST",mk_nt "_STAR_CWSP_CNL",mk_nt "_RULE",mk_nt "_HEX_VAL",mk_nt "_ONE_STAR_CWSP",mk_nt "_COMMENT",mk_nt "_DEFINED_AS",mk_nt "_NUM_VAL",mk_nt "_STAR_CWSP",mk_nt "_RULELIST_ELT" 

(** Rules *)
let _ = (
_S -->_3 (ws,nt _RULELIST,ws)    (fun (x1,x2,x3) ->  x2 );

_RULELIST -->_2 (nt _RULELIST_ELT,nt _RULELIST)    (fun (x1,x2) ->  match x2 with RULELIST(xs) -> RULELIST(x1 :: xs) );
_RULELIST -->_1 (nt _RULELIST_ELT)    (fun x1 ->  RULELIST[x1] );

_RULELIST_ELT -->_1 (nt _RULE)    (fun x1 ->  RE_RULE(x1) );
_RULELIST_ELT -->_1 (wsplus)    (fun x1 ->  RE_CWSP_CNL(x1) );

_STAR_CWSP_CNL -->_2 (nt _CWSP,nt _STAR_CWSP_CNL)    (fun (x1,x2) ->  string_concat [x1;x2] );
_STAR_CWSP_CNL -->_1 (nt _CNL)    (fun x1 ->  x1 );

_RULE -->_4 (nt _RULENAME,nt _DEFINED_AS,nt _ELEMENTS,nt _CNL)    (fun (x1,x2,x3,x4) ->  RULE(x1,x2,x3,x4) );

_RULENAME -->_1 (rulename)    (fun x1 ->  RULENAME x1 );

_DEFINED_AS -->_3 (nt _STAR_CWSP,nt _EQUAL_OR_EQUAL_SLASH,nt _STAR_CWSP)    (fun (x1,x2,x3) ->  x2 );

_STAR_CWSP -->_2 (nt _CWSP,nt _STAR_CWSP)    (fun (x1,x2) ->  () );
_STAR_CWSP -->_1 (eps)    (fun x1 ->  () );

_EQUAL_OR_EQUAL_SLASH -->_1 (a"=/")    (fun x1 ->  DAS_EQUAL_SLASH );
_EQUAL_OR_EQUAL_SLASH -->_1 (a"=")    (fun x1 ->  DAS_EQUAL );

_ELEMENTS -->_2 (nt _ALTERNATION,nt _STAR_CWSP)    (fun (x1,x2) ->  ELEMENTS(x1) );

_CWSP -->_2 (nt _CNL,wsp)    (fun (x1,x2) ->  string_concat [x1;x2] );
_CWSP -->_1 (wsp)    (fun x1 ->  x1 );

_CNL -->_1 (nt _COMMENT)    (fun x1 ->  x1 );
_CNL -->_1 (crlf)    (fun x1 ->  x1 );

_COMMENT -->_3 (a";",nt _STAR_WSP_VCHAR,crlf)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_STAR_WSP_VCHAR -->_2 (vchar,nt _STAR_WSP_VCHAR)    (fun (x1,x2) ->  string_concat [x1;x2] );
_STAR_WSP_VCHAR -->_2 (wsp,nt _STAR_WSP_VCHAR)    (fun (x1,x2) ->  string_concat [x1;x2] );
_STAR_WSP_VCHAR -->_1 (eps)    (fun x1 ->  x1 );

_ALTERNATION -->_2 (nt _CONCATENATION,nt _STAR_ALTERNATION_REST)    (fun (x1,x2) ->  ALTERNATION(x1::x2) );

_STAR_ALTERNATION_REST -->_5 (nt _STAR_CWSP,a"/",nt _STAR_CWSP,nt _CONCATENATION,nt _STAR_ALTERNATION_REST)    (fun (x1,x2,x3,x4,x5) ->  x4::x5 );
_STAR_ALTERNATION_REST -->_1 (eps)    (fun x1 ->  [] );

_CONCATENATION -->_2 (nt _REPETITION,nt _STAR_CONCATENATION_REST)    (fun (x1,x2) ->  CONCATENATION(x1::x2) );

_STAR_CONCATENATION_REST -->_3 (nt _ONE_STAR_CWSP,nt _REPETITION,nt _STAR_CONCATENATION_REST)    (fun (x1,x2,x3) ->  x2::x3 );
_STAR_CONCATENATION_REST -->_1 (eps)    (fun x1 ->  [] );

_ONE_STAR_CWSP -->_2 (nt _CWSP,nt _STAR_CWSP)    (fun (x1,x2) ->  () );

_REPETITION -->_2 (nt _REPEAT,nt _ELEMENT)    (fun (x1,x2) ->  REP(Some(x1),x2) );
_REPETITION -->_1 (nt _ELEMENT)    (fun x1 ->  REP(None,x1) );

_REPEAT -->_1 (repeat)    (fun x1 ->  REPEAT );

_ELEMENT -->_1 (nt _RULENAME)    (fun x1 ->  EL_RULENAME x1 );
_ELEMENT -->_1 (nt _GROUP)    (fun x1 ->  EL_GROUP x1 );
_ELEMENT -->_1 (nt _OPTION)    (fun x1 ->  EL_OPTION x1 );
_ELEMENT -->_1 (nt _CHAR_VAL)    (fun x1 ->  EL_CHAR_VAL );
_ELEMENT -->_1 (nt _NUM_VAL)    (fun x1 ->  EL_NUM_VAL );
_ELEMENT -->_1 (nt _PROSE_VAL)    (fun x1 ->  EL_PROSE_VAL );

_GROUP -->_5 (a"(",nt _STAR_CWSP,nt _ALTERNATION,nt _STAR_CWSP,a")")    (fun (x1,x2,x3,x4,x5) ->  GROUP(x3) );

_OPTION -->_5 (a"[",nt _STAR_CWSP,nt _ALTERNATION,nt _STAR_CWSP,a"]")    (fun (x1,x2,x3,x4,x5) ->  OPTION(x3) );

_CHAR_VAL -->_3 (dquote,char_vals,dquote)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_NUM_VAL -->_2 (a"%",nt _NUM_VAL_REST)    (fun (x1,x2) ->  string_concat [x1;x2] );

_NUM_VAL_REST -->_1 (nt _BIN_VAL)    (fun x1 ->  x1 );
_NUM_VAL_REST -->_1 (nt _DEC_VAL)    (fun x1 ->  x1 );
_NUM_VAL_REST -->_1 (nt _HEX_VAL)    (fun x1 ->  x1 );

_BIN_VAL -->_3 (a"b",one_star_bit,bin_val_rest)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_DEC_VAL -->_3 (a"d",one_star_digit,dec_val_rest)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_HEX_VAL -->_3 (a"x",one_star_hexdig,hex_val_rest)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_PROSE_VAL -->_3 (a"<",prose_val_chars,a">")    (fun (x1,x2,x3) ->  x2 );
)

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
  module Internal_instance = Internal_generated_parser(Reqs)


  (** What we export *)
  module Export : sig
    type 'a nt
    val _S : rulelist nt
    val nt_to_parser: 'a nt -> 'a P0.m
  end = struct
    type 'a nt = int
    let _S = Internal_instance._S
    let nt_to_parser = Reqs.nt_to_parser
  end
    
end  (* Internal2 *)

include Internal2.Export

let _S = nt_to_parser _S 

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
