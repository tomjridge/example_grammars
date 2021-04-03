(** Scala example: Scala meta-grammar, Scala grammar, Scala parser; WIP *)


(** Following:

* https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

* https://github.com/scala/scala/blob/2.13.x/spec/13-syntax-summary.md - a copy should be in this directory

*)

(* from Tjr_lib_core.Iter *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

(* FIXME move to List *)
let rec intersperse x ys = 
  match ys with 
  | [] -> []
  | [y] -> [y]
  | y::ys -> y::x::(intersperse x ys)


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
    let with_ws (rule:sym list) : sym list = intersperse ws rule

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

    let _ : unit = p#add_rule _TICK [ predef#any_of tick_chars ]


    (* NOTE this allows mixing ticks - they don't have to match *)
    let _ : unit = p#add_rule _LITERAL [ _TICK; predef#any_but tick_chars; _TICK ]        
        
    let _ : unit = p#add_rules _TM [
        [predef#starts_with_lower];
        [_LITERAL]
      ]


    (** Nonterminals *)


    (* FIXME note that "ids" is defined as a nonterminal, but starts
       with a non-capital; typo? *)
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
    let _ : unit = p#add_rule _SYM_LIST [ p#list_with_sep ~sep:ws_nnl _SYM ]



    (** RHS *)

    let _RHS = nt "RHS"
        
    let _BARSEP = nt "BARSEP"

    (* bar_sep is: ws_nnl nl ws_nnl "|" ws_nnl *)
    let _ : unit = p#add_rule _BARSEP [ ws_nnl; a"\n"; ws_nnl; a"|"; ws_nnl ]
    
    let _ : unit = p#add_rule _RHS [ p#list_with_sep ~sep:_BARSEP _SYM_LIST ]



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
      ; list_with_sep : sep:'sym -> 'sym -> 'sym
      ; nt : string -> 'sym
      ; predef :
          < a : string -> 'sym
          ; any_but : string list -> 'sym
          ; any_of : string list -> 'sym
          ; end_of_input : 'sym
          ; starts_with_lower : 'sym
          ; starts_with_upper : 'sym
          ; ws : 'sym
          ; ws_nnl : 'sym
          ; .. >
      ; .. > ->
      unit) =
  scala_metagrammar


(** Fill in the blanks using P0 2021 *)
module With_P0 = struct
  open P0_lib.P0.P0_2021

  let starts_with_lower = exec Re.(seq [lower;rep alnum] |> compile)

  let starts_with_upper = exec Re.(seq [upper;rep alnum] |> compile)

  let tm_counter = ref 1
  let tm_tbl = Hashtbl.create 10
  let tm f = 
    let n = !tm_counter in
    Hashtbl.add tm_tbl n f;
    tm_counter:=n+2;
    n

  let a s = tm (a s)
  let any_but s = tm (any_but s)
  let any_of s = tm (any_of s)
  let end_of_input = -1 (* tm (end_of_input) (* FIXME *) *)
  let starts_with_upper = tm starts_with_upper
  let starts_with_lower = tm starts_with_lower
  let ws = tm ws
  let ws_nnl = tm ws_nnl

  let predef = object
    method a = a
    method any_but = any_but
    method any_of = any_of
    method end_of_input = end_of_input
    method starts_with_lower = starts_with_lower
    method starts_with_upper = starts_with_upper
    method ws = ws
    method ws_nnl = ws_nnl
  end

  let add_rule s ss = ()
  let add_rules s xs = ()
  let list_with_sep ~sep p = -3 (* FIXME *)

  let nt_tbl = Hashtbl.create 10
  let nt_counter = ref 0
  let nt s = 
    let n = !nt_counter in
    Hashtbl.add nt_tbl s n;
    nt_counter := n+2;
    n
   
  let p = object
    method add_rule=add_rule
    method add_rules=add_rules
    method list_with_sep=list_with_sep
    method nt=nt
    method predef=predef
  end

  (* FIXME we need a to return a sym, so we need to enumerate
     terminals as well as nonterms *)
  let scala_metagrammar = scala_metagrammar p
  
end
