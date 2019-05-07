open Abnf_parser_prelude

(** NOTE this is generated code, see src/generate_abnf_parser.ml *)

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

module Make(Reqs:INTERNAL_REQS with type rule=unit)(Terms:sig
    open Reqs
        val dec_val_rest : string sym
    val vchar : string sym
    val crlf : string sym
    val prose_val_chars : string sym
    val one_star_bit : string sym
    val bin_val_rest : string sym
    val eps : string sym
    val dquote : string sym
    val one_star_digit : string sym
    val hex_val_rest : string sym
    val rulename : string sym
    val ws : string sym
    val char_vals : string sym
    val repeat : string sym
    val wsp : string sym
    val one_star_hexdig : string sym
    val wsplus : string sym end) = struct

open Reqs
open Terms

(** Non-terminals *)
let _NUM_VAL_REST,_CWSP,_PROSE_VAL,_REPEAT,_CHAR_VAL,_ALTERNATION,_REPETITION,_ELEMENT,_RULELIST,_CONCATENATION,_ELEMENTS,_CNL,_RULENAME,_EQUAL_OR_EQUAL_SLASH,_STAR_WSP_VCHAR,_S,_BIN_VAL,_GROUP,_DEC_VAL,_STAR_CONCATENATION_REST,_OPTION,_STAR_ALTERNATION_REST,_STAR_CWSP_CNL,_RULE,_HEX_VAL,_ONE_STAR_CWSP,_COMMENT,_DEFINED_AS,_NUM_VAL,_STAR_CWSP,_RULELIST_ELT = mk_nt "_NUM_VAL_REST",mk_nt "_CWSP",mk_nt "_PROSE_VAL",mk_nt "_REPEAT",mk_nt "_CHAR_VAL",mk_nt "_ALTERNATION",mk_nt "_REPETITION",mk_nt "_ELEMENT",mk_nt "_RULELIST",mk_nt "_CONCATENATION",mk_nt "_ELEMENTS",mk_nt "_CNL",mk_nt "_RULENAME",mk_nt "_EQUAL_OR_EQUAL_SLASH",mk_nt "_STAR_WSP_VCHAR",mk_nt "_S",mk_nt "_BIN_VAL",mk_nt "_GROUP",mk_nt "_DEC_VAL",mk_nt "_STAR_CONCATENATION_REST",mk_nt "_OPTION",mk_nt "_STAR_ALTERNATION_REST",mk_nt "_STAR_CWSP_CNL",mk_nt "_RULE",mk_nt "_HEX_VAL",mk_nt "_ONE_STAR_CWSP",mk_nt "_COMMENT",mk_nt "_DEFINED_AS",mk_nt "_NUM_VAL",mk_nt "_STAR_CWSP",mk_nt "_RULELIST_ELT" 


(** Rules; NOTE these have to be added in the given order due to short-circuit alternatives *)
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

end
