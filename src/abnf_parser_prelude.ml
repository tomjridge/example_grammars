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
(* open Abnf_grammar_datatype *)
include Abnf_grammar_datatype

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
