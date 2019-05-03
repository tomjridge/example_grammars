(** A parser for ABNF format grammars *)

let c x = x

let string_concat xs = String.concat "" xs 

let dest_Some = function | Some x -> x | _ -> failwith __LOC__

module type INTERNAL_TYPED_SYMS = sig
  type 'a nt
  type 'a sym
  type 'a tm
  val mk_nt: unit -> 'a nt
  val a: string -> string sym
end

module type INTERNAL_REQS = sig 
  include INTERNAL_TYPED_SYMS
  type 'a rhs 
  type rule 
  val _1: 'a sym -> ('a -> 'b) -> 'b rhs
  val _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs
  val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs
  val _4: ('a sym * 'b sym * 'c sym * 'd sym) -> ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
  val _5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) -> ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs

  val ( --> ) : 'a nt -> 'a rhs -> rule
  val nt : 'a nt -> 'a sym

  type 'a grammar 
  val grammar: name:string -> descr:string -> initial_nt:'a nt ->
    rules:rule list -> 'a grammar
end

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

module Terminals = struct
  let parse_RE re = failwith ""
  let rulename = parse_RE "[A-Za-z][-A-Za-z0-9]*"
  let repeat = parse_RE "[0-9]*[*][0-9]*\\|[0-9]+"
  let dquote = failwith ""
  let char_vals = parse_RE "[^\"]*"
  let one_star_bit = parse_RE "[01]+"
  let bin_val_rest = parse_RE "\\([.][0-1]+\\)+\\|[-][0-1]+"
  let one_star_digit = parse_RE "[0-9]+"
  let dec_val_rest = parse_RE "\\([.][0-9]+\\)+\\|[-][0-9]+"
  let one_star_hexdig = parse_RE "[0-9A-Fa-f]+"
  let hex_val_rest = parse_RE "\\([.][0-9A-Fa-f]+\\)+\\|[-][0-9A-Fa-f]+"
  let prose_val_chars = parse_RE "[^>]*"
  let crlf = parse_RE "[\n]" (* FIXME really CR LF *)
  let vchar = parse_RE "[\x21-\x7E]"
  let wsp = parse_RE "[ \x09]"
  let ws = (parse_RE "[ \n]*")
  let wsplus = (parse_RE "[ \n]+")

  let eps = failwith ""

  let eof = failwith ""
end
open Terminals

module Internal(Reqs: INTERNAL_REQS) : sig 
  val grammar : rulelist Reqs.grammar
end = struct

open Reqs

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

_RULELIST -->_1 (nt _RULELIST_ELT)    (fun x1 ->  RULELIST[x1] );
_RULELIST -->_2 (nt _RULELIST_ELT,nt _RULELIST)    (fun (x1,x2) ->  match x2 with RULELIST(xs) -> RULELIST(x1 :: xs) );

_RULELIST_ELT -->_1 (nt _RULE)    (fun x1 ->  RE_RULE(x1) );
_RULELIST_ELT -->_1 (wsplus)    (fun x1 ->  RE_CWSP_CNL(x1) );

_STAR_CWSP_CNL -->_1 (nt _CNL)    (fun x1 ->  x1 );
_STAR_CWSP_CNL -->_2 (nt _CWSP,nt _STAR_CWSP_CNL)    (fun (x1,x2) ->  string_concat [x1;x2] );

_RULE -->_4 (nt _RULENAME,nt _DEFINED_AS,nt _ELEMENTS,nt _CNL)    (fun (x1,x2,x3,x4) ->  RULE(x1,x2,x3,x4) );

_RULENAME -->_1 (rulename)    (fun x1 ->  RULENAME x1 );

_DEFINED_AS -->_3 (nt _STAR_CWSP,nt _EQUAL_OR_EQUAL_SLASH,nt _STAR_CWSP)    (fun (x1,x2,x3) ->  x2 );

_STAR_CWSP -->_1 (eps)    (fun x1 ->  () );
_STAR_CWSP -->_2 (nt _CWSP,nt _STAR_CWSP)    (fun (x1,x2) ->  () );

_EQUAL_OR_EQUAL_SLASH -->_1 (a"=")    (fun x1 ->  DAS_EQUAL );
_EQUAL_OR_EQUAL_SLASH -->_1 (a"=/")    (fun x1 ->  DAS_EQUAL_SLASH );

_ELEMENTS -->_2 (nt _ALTERNATION,nt _STAR_CWSP)    (fun (x1,x2) ->  ELEMENTS(x1) );

_CWSP -->_1 (wsp)    (fun x1 ->  x1 );
_CWSP -->_2 (nt _CNL,wsp)    (fun (x1,x2) ->  string_concat [x1;x2] );

_CNL -->_1 (nt _COMMENT)    (fun x1 ->  x1 );
_CNL -->_1 (crlf)    (fun x1 ->  x1 );

_COMMENT -->_3 (a";",nt _STAR_WSP_VCHAR,crlf)    (fun (x1,x2,x3) ->  string_concat [x1;x2;x3] );

_STAR_WSP_VCHAR -->_1 (eps)    (fun x1 ->  x1 );
_STAR_WSP_VCHAR -->_2 (wsp,nt _STAR_WSP_VCHAR)    (fun (x1,x2) ->  string_concat [x1;x2] );
_STAR_WSP_VCHAR -->_2 (vchar,nt _STAR_WSP_VCHAR)    (fun (x1,x2) ->  string_concat [x1;x2] );

_ALTERNATION -->_2 (nt _CONCATENATION,nt _STAR_ALTERNATION_REST)    (fun (x1,x2) ->  ALTERNATION(x1::x2) );

_STAR_ALTERNATION_REST -->_1 (eps)    (fun x1 ->  [] );
_STAR_ALTERNATION_REST -->_5 (nt _STAR_CWSP,a"/",nt _STAR_CWSP,nt _CONCATENATION,nt _STAR_ALTERNATION_REST)    (fun (x1,x2,x3,x4,x5) ->  x4::x5 );

_CONCATENATION -->_2 (nt _REPETITION,nt _STAR_CONCATENATION_REST)    (fun (x1,x2) ->  CONCATENATION(x1::x2) );

_STAR_CONCATENATION_REST -->_1 (eps)    (fun x1 ->  [] );
_STAR_CONCATENATION_REST -->_3 (nt _ONE_STAR_CWSP,nt _REPETITION,nt _STAR_CONCATENATION_REST)    (fun (x1,x2,x3) ->  x2::x3 );

_ONE_STAR_CWSP -->_2 (nt _CWSP,nt _STAR_CWSP)    (fun (x1,x2) ->  () );

_REPETITION -->_1 (nt _ELEMENT)    (fun x1 ->  REP(None,x1) );
_REPETITION -->_2 (nt _REPEAT,nt _ELEMENT)    (fun (x1,x2) ->  REP(Some(x1),x2) );

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

let _ = _S

(** Wrap up the result and export *)
let grammar = Reqs.grammar ~name:"ABNF" ~descr:("ABNF parser, see "^__FILE__)
    ~initial_nt:_S
    ~rules

end
