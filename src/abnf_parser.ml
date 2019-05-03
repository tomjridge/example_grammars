(** A parser for ABNF format grammars *)

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
  val mk_nt: unit -> 'a nt
  val a: string -> string sym
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

(*
  type 'a grammar 
  val grammar: name:string -> descr:string -> initial_nt:'a nt ->
    rules:rule list -> 'a grammar
*)

  (** for defining terminals without exposing the sym types, we have
     an extra argument to convert a string regexp into a (terminal)
     sym *)
  val regexp_string_to_tm: string -> string sym
end


module Internal(Reqs: INTERNAL_REQS) : sig 
  val _S : rulelist Reqs.nt
end = struct

let string_concat xs = String.concat "" xs 

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
end
open Terminals


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
let _NUM_VAL_REST,_CWSP,_PROSE_VAL,_REPEAT,_CHAR_VAL,_ALTERNATION,_REPETITION,_ELEMENT,_RULELIST,_CONCATENATION,_ELEMENTS,_CNL,_RULENAME,_EQUAL_OR_EQUAL_SLASH,_STAR_WSP_VCHAR,_S,_BIN_VAL,_GROUP,_DEC_VAL,_STAR_CONCATENATION_REST,_OPTION,_STAR_ALTERNATION_REST,_STAR_CWSP_CNL,_RULE,_HEX_VAL,_ONE_STAR_CWSP,_COMMENT,_DEFINED_AS,_NUM_VAL,_STAR_CWSP,_RULELIST_ELT = mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt() 

(** Rules *)
let rules = [
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
]

let _ : rulelist nt = _S

(*
(** Wrap up the result and export *)
let grammar = Reqs.grammar ~name:"ABNF" ~descr:("ABNF parser, see "^__FILE__)
    ~initial_nt:_S
    ~rules
*)
end


(** Instantiate the Internal functor, and export the results *)
module Internal2 = struct

  module Typed_syms = struct
    type 'a nt = int

    type 'a sym = Nt of 'a nt | Tm of string P0_lib.m

    let mk_nt = 
      let free = ref 0 in
      fun () -> 
        let x = !free in
        free:=!free+1;
        x

    let a s = Tm (P0_lib.a s)
  end
  module Reqs = struct
    include Typed_syms

    (*
    (** To store the defns of nts *)
    type 'z rhs = 
      | Rhs1: 'a sym * ('a -> 'z) -> 'z rhs
      | Rhs2: ('a sym * 'b sym) * ('a * 'b -> 'z) -> 'z rhs
      | Rhs3: ('a sym * 'b sym * 'c sym) * ('a * 'b * 'c -> 'z) -> 'z rhs
      | Rhs4: ('a sym * 'b sym * 'c sym * 'd sym) * ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
      | Rhs5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) * ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs
*)


    type univ
(*
    let to_univ: 'a rhs list -> univ = fun x -> Obj.magic x
    let from_univ: univ -> 'a rhs list = fun x -> Obj.magic x
*)

    type 'a rhs = univ sym list * (univ list -> univ)

    type rule = unit (* Rule: 'a nt * 'a rhs -> rule ; we mutate 'a nt directly *)

(*
    include struct
      (* open P0_lib *)
      let _1 s f = Rhs1(s,f)
      let _2 (s1,s2) f = Rhs2((s1,s2),f)
      let _3 ss f = Rhs3(ss,f)
      let _4 ss f = Rhs4(ss,f)
      let _5 ss f = Rhs5(ss,f)
    end
*)

    module Underscores : sig 
      val _1: 'a sym -> ('a -> 'z) -> 'z rhs
      val _2: ('a sym * 'b sym) -> ('a * 'b -> 'z) -> 'z rhs
      val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'z) -> 'z rhs
      val _4: ('a sym * 'b sym * 'c sym * 'd sym) -> ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
      val _5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) -> ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs
    end = struct
      let _1 : 'a sym -> ('a -> 'z) -> 'z rhs = fun s f -> 
        [Obj.magic s],
        fun [u1] -> Obj.magic (f (Obj.magic u1))
      let _2 = fun (s1,s2) f -> 
        [Obj.magic s1;Obj.magic s2],
        fun [u1;u2] -> Obj.magic (f (Obj.magic u1, Obj.magic u2))
      let _3 (s1,s2,s3) f = 
        [Obj.magic s1; Obj.magic s2; Obj.magic s3],
        fun [u1;u2;u3] -> Obj.magic (f (Obj.magic u1, Obj.magic u2, Obj.magic u3))
      let _4 (s1,s2,s3,s4) f = 
        [Obj.magic s1; Obj.magic s2; Obj.magic s3; Obj.magic s4],
        fun [u1;u2;u3;u4] -> Obj.magic (f (Obj.magic u1, Obj.magic u2, Obj.magic u3, Obj.magic u4))
      let _5 (s1,s2,s3,s4,s5) f = 
        [Obj.magic s1; Obj.magic s2; Obj.magic s3; Obj.magic s4; Obj.magic s5],
        fun [u1;u2;u3;u4;u5] -> Obj.magic (f (Obj.magic u1, Obj.magic u2, Obj.magic u3, Obj.magic u4, Obj.magic u5))
    end
    include Underscores

    let tbl = Hashtbl.create 100    

    let ( --> ) (type a) (nt:a nt) (rhs:a rhs) : unit = 
      let rhss = Hashtbl.find_opt tbl nt |> function
        | None -> []
        | Some rhss -> (rhss : a rhs list)
      in
      Hashtbl.replace tbl nt (rhss@[rhs])

    let nt (nt:'a nt) : 'a sym = Nt nt

    type 'a grammar = { name:string; descr:string; initial_nt: 'a nt }
    let grammar  ~name ~descr ~initial_nt ~(rules:rule list) = 
      { name; descr; initial_nt }

    let regexp_string_to_tm s = Tm (P0_lib.Str_.re s)

    open P0_lib 
    
    module Internal3 = struct
      let rec nt_to_parser (nt:univ nt) : univ P0_lib.m = 
        (* we look up the rhss in the hashtbl, and convert to a parser *)
        let rhss : univ rhs list = 
          Hashtbl.find_opt tbl nt |> function
          | None -> []
          | Some rhss -> rhss
        in
        (* FIXME in the following it would be nicer if we had a list of
           sym and an action that took a list *)
        let rhs_to_parser (rhs:univ rhs) : univ P0_lib.m = match rhs with
          | syms,act -> (a"" >>= fun _ -> 
              syms |> List.map sym_to_parser |> fun ps ->
              P0_lib.sequence ps >>= fun xs -> 
              let r = act xs in
              let _ = r in  (* NOTE univ *)
              return (Obj.magic r))
        in
        let rec alts = function
          | [] -> of_fun (fun s -> None)
          | [rhs] -> rhs_to_parser rhs
          | rhs::rest -> (rhs_to_parser rhs) || (alts rest)
        in
        let r = alts rhss in
        let _ = r in
        r
      and sym_to_parser = function
        | Nt nt -> nt_to_parser nt
        | Tm p -> Obj.magic p

      let _ = nt_to_parser
    end

    let nt_to_parser : 'a nt -> 'a P0_lib.m = 
      fun nt -> Obj.magic (Internal3.nt_to_parser nt)

  end
  module Internal_instance = Internal(Reqs)

  
  
  (* include (Internal_instance : sig val grammar : rulelist Reqs.grammar end) *)

  module Export : sig
    type 'a nt
    val _S : rulelist nt
    val nt_to_parser: 'a nt -> 'a P0_lib.m
  end = struct
    type 'a nt = int
    let _S = Internal_instance._S
    let nt_to_parser = Reqs.nt_to_parser
  end
    
end

include Internal2.Export

let _S = nt_to_parser _S 

open P0_lib

let test () = 
  to_fun _S {|
address         = "(" addr-name SP addr-adl SP addr-mailbox SP
                  addr-host ")"
|}
  |> function
| None -> failwith __LOC__
| (Some(_,rest)) -> Printf.printf "Remaining input: %s  (%s)\n%!" rest __FILE__
