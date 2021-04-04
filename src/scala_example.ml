(** Scala example: Scala meta-grammar, Scala grammar, Scala parser; WIP *)


(** Following:

* https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

* https://github.com/scala/scala/blob/2.13.x/spec/13-syntax-summary.md - a copy should be in this directory

{[
let scala_grammar_fragment = {|
  Literal           ::=  ['-'] integerLiteral
                      |  ['-'] floatingPointLiteral
                      |  booleanLiteral
                      |  characterLiteral
                      |  stringLiteral
                      |  interpolatedString
                      |  symbolLiteral
                      |  'null'

  QualId            ::=  id {'.' id}
  ids               ::=  id {',' id}

  Path              ::=  StableId
                      |  [id '.'] 'this'
|}
]}

*)

module P0_2021 = P0_lib.P0_2021

let dont_log = true

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


(** Mutable map to list; FIXME move *)
type ('k,'v) map_to_list = {
  find_ref : 'k -> 'v list ref;
  find     : 'k -> 'v list;
  add      : 'k -> 'v -> unit;
}

let mk_map_to_list tbl = 
  let find_ref k =
    Hashtbl.find_opt tbl k |> function
    | None -> 
      let v = ref [] in
      Hashtbl.replace tbl k v;
      v
    | Some r -> r
  in
  let find k = !(find_ref k) in
  let add k v = 
    find_ref k |> fun vs -> 
    vs := !vs @ [v];
    ()
  in
  {find_ref;find;add}



(** Scala metagrammar - the grammar in which the Scala grammar is
   expressed. NOTE this function is completely independent of the
   parsing details - we fill those in later via the parameter p *)
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
    let tick_chars = ["'"; "`"]

    let _ : unit = p#add_rule _TICK [ predef#any_of tick_chars ]


    (* NOTE this allows mixing ticks - they don't have to match *)
    let _ : unit = p#add_rule _LITERAL [ _TICK; predef#rep_any_but tick_chars; _TICK ]        
        
    let _ : unit = p#add_rules _TM [
        [predef#starts_with_lower];
        [_LITERAL]
      ]


    (** Nonterminals *)


    (* FIXME note that "ids" is defined as a nonterminal, but starts
       with a non-capital; typo? *)
    let _ : unit = p#add_rule _NT [ predef#starts_with_upper ]


    (** Symbols *)

    (* A subrule of the form S | S | S *)
    let _WSNNL_BAR_WSNNL = nt "WSNNL_BAR_WSNNL"

    let _ : unit = p#add_rule _WSNNL_BAR_WSNNL [ws; a"|";ws]

    let _SYM_LIST = nt "SYM_LIST"



    (* we allow "symbols" to also include "specials" - options,
       repetitions etc *)
    let _ : unit = p#add_rules _SYM [
        [_NT];
        [_TM];
        [_SPECIAL]
      ]

    let _OPTION = nt "OPTION"

    (** NOTE subrules can be found in options (like the rule for
       PrefixExpr) and brackets; subrules are all on one line *)
    let _SUBRULE_BODY = nt "SUBRULE_BODY"

    let _ : unit = p#add_rule _SUBRULE_BODY [ p#list_with_sep ~sep:_WSNNL_BAR_WSNNL _SYM_LIST ]
            
    let _ : unit = add_rule' _OPTION [ a"["; _SUBRULE_BODY; a"]"; ]


    let _REPETITION = nt "REPETITION"

    let _ : unit = add_rule' _REPETITION [ a"{"; _SUBRULE_BODY; a"}"; ]
    

    let _BRACKET = nt "BRACKET"


    (** NOTE brackets are used to group "subrules" eg for the rule for EXPR *)
    let _ : unit = add_rule' _BRACKET [ a"("; _SUBRULE_BODY; a")"; ]

    let _ : unit = p#add_rules _SPECIAL [
        [_OPTION];
        [_REPETITION];
        [_BRACKET]
      ]



    (** Sym list *)

    (* The sym list can be empty in some cases - possibly just a typo in the Scala grammar? eg third alternative here:

      RefineStat        ::=  Dcl
                      |  ‘type’ TypeDef
                      |

       NOTE in 3 or 4 places a symlist is allowed to contain syms separated
       by ws (including nl); we remove these in scala_grammar.ebnf to make it
       easier to parse.  *)
    let _ : unit = p#add_rule _SYM_LIST [ p#list_with_sep ~sep:ws_nnl _SYM ]
        


    (** RHS *)

    let _RHS = nt "RHS"
        
    let _BARSEP = nt "BARSEP"

    (** bar_sep is: ws_nnl nl ws_nnl "|" ws_nnl; NOTE in a couple of
       places, the bar is on the same line; we adjust these in
       scala_grammar.ebnf to make it more uniform and easier to parse
       *)
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

    let _ : unit = p#declare_start_symbol _S
    
  end)
  in
  ()
    
let (_ :
      < add_rule : 'sym -> 'sym list -> unit
      ; add_rules : 'sym -> 'sym list list -> unit
      ; list_with_sep : sep:'sym -> 'sym -> 'sym
      ; nt : string -> 'sym
    ; declare_start_symbol: 'sym -> unit
      ; predef :
          < a : string -> 'sym
          ; rep_any_but : string list -> 'sym
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
module With_P0() = struct
  [@@@warning "-33"]

  (* open P0_lib.P0 *)
  open P0_lib.P0.P0_2021

  let p_starts_with_lower = exec Re.(seq [start; rg 'a' 'z';longest (rep alnum)] |> compile)

  let p_starts_with_upper = exec Re.(seq [start; rg 'A' 'Z';longest (rep alnum)] |> compile)

  let tm_counter = ref 101

  let tm_tbl = Hashtbl.create 10

  let _ : (_, [`String of string m | `Unit of unit m ]) Hashtbl.t = tm_tbl

  type sym = Tm of int | Nt of int

  let tm f = 
    let n = !tm_counter in
    Hashtbl.add tm_tbl n f;
    tm_counter:=n+2;
    n
    
  let pp_tm (tm:int) = Printf.sprintf "TM(%d)" tm

  let tm_find tm = 
    Hashtbl.find_opt tm_tbl tm |> function
    | None -> failwith (Printf.sprintf "Unknown parser for terminal: %d" tm)
    | Some f -> f


  let tm_s f = tm (`String f)

  let eps = tm_s (a "")

  let a s = 
    tm_s (a s) |> fun n -> 
    Printf.printf "terminal (a %s) numbered %d\n%!" s n;
    n

  let rep_any_but s = 
    tm_s (rep_any_but s) |> fun n -> 
    Printf.printf "terminal (rep_any_but _) numbered %d\n%!" n;
    n
    
  let any_of s = tm_s (any_of s)
  let end_of_input = tm (`Unit (end_of_input))

  let starts_with_upper = 
    tm_s p_starts_with_upper |> fun n -> 
    Printf.printf "terminal (starts_with_upper) numbered %d\n%!" n;
    n

  let starts_with_lower = 
    tm_s p_starts_with_lower |> fun n -> 
    Printf.printf "terminal (starts_with_lower) numbered %d\n%!" n;
    n

  let ws = 
    tm_s ws |> fun n -> 
    Printf.printf "terminal (ws) numbered %d\n%!" n;
    n

  let ws_nnl = 
    tm_s ws_nnl |> fun n -> 
    Printf.printf "terminal (ws_nnl) numbered %d\n%!" n;
    n

  let predef = object
    method a = a
    method rep_any_but = rep_any_but
    method any_of = any_of
    method end_of_input = end_of_input
    method starts_with_lower = starts_with_lower
    method starts_with_upper = starts_with_upper
    method ws = ws
    method ws_nnl = ws_nnl
  end

  let nt_tbl = Hashtbl.create 10
  let nt_counter = ref 0

  (** The string argument is for debugging *)
  let nt (s:string) = 
    let n = !nt_counter in
    Hashtbl.add nt_tbl n s;
    nt_counter := n+2;
    n

  let is_nt n = n mod 2 = 0

  let pp_nt (nt:int) = Printf.sprintf "NT(%s)" (Hashtbl.find nt_tbl nt)

  let rules_tbl = Hashtbl.create 10

  let rules = mk_map_to_list rules_tbl

  let add_rule s ss = 
    Printf.printf "Adding rule for %s\n" (pp_nt s);
    rules.add s ss

  let add_rules s xs = 
    Printf.printf "Adding rules for %s\n" (pp_nt s);
    List.iter (fun x -> add_rule s x) xs;
    ()

  let list_with_sep ~sep p = 
    (* Create a new nonterminal, add the rules, and return *)
    let n = nt "list_with_sep(?,?)" in
    add_rules n [
      [p; sep; n];
      [p];
      [eps]
    ];
    n

  let start_symbol = ref (-1)

  (** NOTE currently this must be a nonterminal *)
  let declare_start_symbol s = 
    assert(is_nt s);
    start_symbol:=s
   
  let p = object
    method add_rule             = add_rule
    method add_rules            = add_rules
    method list_with_sep        = list_with_sep
    method nt                   = nt
    method predef               = predef
    method declare_start_symbol = declare_start_symbol
  end


  (** {2 Run the parse, using P0, with the associated rules *)

  (* let delay p = inject @@ fun s -> run p s *)

  let rec parse_nt nt =       
    rules.find nt |> function
    | [] -> failwith (Printf.sprintf "No rules found for nt: %s\n" (pp_nt nt))
    | (xs : int list list) -> 
      inject (fun s ->
          assert(dont_log || (Printf.printf "Parsing nt: %s at position %d\n%!" (pp_nt nt) s.i; true));
          run (parse_rules xs) s)
      >>= fun rs -> return (`Parse_nt(pp_nt nt,rs))

  and parse_tm tm = 
    (* P0_2021.debug ~msg:(fun st -> 
        Printf.printf "parse_tm: %d at position %d\n" tm st.i) *)
    begin
      tm_find tm |> function
      | `Unit f -> 
        f >>= fun () -> return `Parse_tm_unit
      | `String f -> 
        f >>= fun s -> 
        assert(dont_log || (Printf.printf "Terminal %d parsed string: (%s)\n%!" tm s; true));
        return (`Parse_tm_string s)
    end

  and parse_sym sym = 
    match is_nt sym with
    | true -> parse_nt sym
    | false -> parse_tm sym

  and parse_syms syms = 
    seq_list (List.map parse_sym syms) >>= fun xs -> return (`Parse_syms xs)

  and parse_rules rs = 
    alt_list (List.map parse_syms rs) >>= fun xs -> return (`Parse_rules xs)


  (* Pass p to the scala grammar, thereby initializing the state in this module *)

  let _ : unit = scala_metagrammar p

  let parse_grammar s =
    let start_symbol = !start_symbol in 
    assert(is_nt start_symbol);
    P0_2021.parse ~debug:true (parse_nt start_symbol) s
end


module Test = struct

  module With_P0 = With_P0()
  open With_P0


  let _ : unit = assert(P0_2021.parse ~debug:true p_starts_with_upper "['-'] integerLiteral" = None)

  let test_1 () = 
    Printf.printf "\n\ntest_parse\n";
    P0_2021.parse ~debug:true (parse_nt !start_symbol)
      "Literal           ::=  ['-'] integerLiteral"

  let test_2 () = 
    Printf.printf "\n\ntest_2\n";
    P0_2021.parse ~debug:true (parse_nt !start_symbol)  {|
Literal ::=  [`-`] integerLiteral
                      |  [`-`] floatingPointLiteral
                      |  booleanLiteral
                      |  characterLiteral
                      |  stringLiteral
                      |  interpolatedString
                      |  symbolLiteral
|}

  let scala_grammar_fragment = {|
  Literal           ::=  ['-'] integerLiteral
                      |  ['-'] floatingPointLiteral
                      |  booleanLiteral
                      |  characterLiteral
                      |  stringLiteral
                      |  interpolatedString
                      |  symbolLiteral
                      |  'null'

  QualId            ::=  id {'.' id}
  Ids               ::=  id {',' id}

  Path              ::=  StableId
                      |  [id '.'] 'this'
|}

  let test_11 () = parse_grammar scala_grammar_fragment

  let test_12 () = parse_grammar Blobs.scala_grammar

end
