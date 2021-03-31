(** Scala example: Scala meta-grammar, Scala grammar, Scala parser; WIP *)


(** Following:

* https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

* https://github.com/scala/scala/blob/2.13.x/spec/13-syntax-summary.md - a copy should be in this directory

*)

(* from Tjr_lib_core.Iter *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x


let example = {|
  Literal           ::=  [‘-’] integerLiteral
                      |  [‘-’] floatingPointLiteral
                      |  booleanLiteral
                      |  characterLiteral
                      |  stringLiteral
                      |  interpolatedString
                      |  symbolLiteral
                      |  ‘null’

  QualId            ::=  id {‘.’ id}
  ids               ::=  id {‘,’ id}

  Path              ::=  StableId
                      |  [id ‘.’] ‘this’
|}

(** Scala metagrammar - the grammar in which the Scala grammar is expressed *)
let scala_metagrammar (type sym) p = 
  let open (struct

    (* NOTE the string argument is just for debugging and
       pretty-printing during code generation *)
    let nt (x:string) : sym = p#nt x

    let predef = p#predef

    (* whitespace, no new line; maybe eps *)
    let ws_nnl = predef#ws_nnl

    let ws = predef#ws (* may include nl *)

    (* careful: don't want to clash with some other terminal FIXME *)
    let a (s:string) = predef#a s 

    let eps = a ""


    (* this allows whitespace-nnl between each elt of rule *)
    let with_ws (rule:sym list) : sym list = 
      (rule,[]) |> iter_k (fun ~k (xs,acc) -> 
          match xs with 
          | [] -> failwith "impossible"
          | [sym] -> xs@[sym]
          | x::xs -> k (xs, acc@[x;ws_nnl]))

    let _ = with_ws

    let add_rule' nt rule = p#add_rule nt (with_ws rule)


    let _G = nt "G" 

    let _NT = nt "NT"  (* capital letter to start *)

    let _TM = nt "TM"  (* lowercase to start; terminals can also be literals *)

    let _LITERAL = nt "LITERAL"

    let _TICK = nt "TICK"

    let _SPECIAL = nt "SPECIAL"

    let _SYM = nt "SYM"



    (** Terminals *)

    (* FIXME question about whether these are ASCII chars, or Unicode
       (possibly multi-byte) chars *)
    let tick_chars = ["'"; "’"; "‘"]

    let _ : unit = p#add_rule _TICK [ p#any_of tick_chars ]


    (* NOTE this allows mixing ticks - they don't have to match *)
    let _ : unit = p#add_rule _LITERAL [ _TICK; predef#any_but tick_chars; _TICK ]        
        
    let _ : unit = p#add_rules _TM [
        [predef#starts_with_lower];
        [_LITERAL]
      ]


    (** Nonterminals *)

    let _ : unit = p#add_rule _NT [ predef#starts_with_upper ]


    (** Symbols *)

    (* we allow "symbols" to also include "specials" - options,
       repetitions etc *)
    let _ : unit = p#add_rules _SYM [
        [_NT];
        [_TM];
        [_SPECIAL]
      ]

    let _OPTION = nt "OPTION"

            
    let _ : unit = add_rule' _OPTION [ a"["; p#list_with_sep ~sep:ws_nnl _SYM; a"]"; ]


    let _REPETITION = nt "REPETITION"

    let _ : unit = add_rule' _REPETITION [ a"{"; p#list_with_sep ~sep:ws_nnl _SYM; a"}"; ]
    

    let _ : unit = p#add_rules _SPECIAL [
        [_OPTION];
        [_REPETITION];
      ]



    (** Sym list *)

    let _SYM_LIST = nt "SYM_LIST"

    (* The sym list can be empty in some cases - possibly just a typo in the Scala grammar? eg third alternative here:

      RefineStat        ::=  Dcl
                      |  ‘type’ TypeDef
                      |

    *)
    let _ : unit = p#add_rules _SYM_LIST [
        [eps];
        [_SYM;_SYM_LIST]
      ]



    (** RHS *)

    let _RHS = nt "RHS"

    (* bar_sep is: ws_nnl nl ws_nnl "|" ws_nnl *)
    let _ : unit = p#add_rule _RHS [ p#list_with_sep ~sep:predef#bar_sep _SYM_LIST ]



    (** RULE *)

    let _RULE = nt "RULE"

    let _ : unit = add_rule' _RULE [ _NT; a"::="; _RHS ]



    (** GRAMMAR *)
    
    let _GRAMMAR = nt "GRAMMAR"
        
    let _ : unit = p#add_rule _GRAMMAR [ p#list_with_sep ~sep:ws _RULE  ]
    


    (** Start symbol S *)

    let _S = nt "S"

    let _ : unit = p#add_rule _S [ ws; _GRAMMAR; ws; predef#end_of_input  ]    
    
    
  end)
  in
  ()
    
let (_ :
      < add_rule : 'sym -> 'sym list -> unit
      ; add_rules : 'sym -> 'sym list list -> unit
      ; any_of : string list -> 'sym
      ; list_with_sep : sep:'sym -> 'sym -> 'sym
      ; nt : string -> 'sym
      ; predef :
          < a : string -> 'sym
          ; any_but : string list -> 'sym
          ; bar_sep : 'sym
          ; end_of_input : 'sym
          ; starts_with_lower : 'sym
          ; starts_with_upper : 'sym
          ; ws : 'sym
          ; ws_nnl : 'sym
          ; .. >
      ; .. > ->
      unit) =
  scala_metagrammar
