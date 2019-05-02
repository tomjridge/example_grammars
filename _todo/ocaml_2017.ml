(* a cleaned up version suitable for the newly-revamped p1 2017-07-01 *)

(* worth working with indexes rather than strings? *)

open P0

(* grammar of grammars ---------------------------------------------- *)

let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)
(* ws: 0 or more; re is hopefully longest match (not true for Str) *)
let rec ws s = (re "[ \n]*") --- (opt (comm --- ws)) @@ s
let nt = re "[A-Z]+" 
let tm = 
  let sq = "'" in
  let dq = "\"" in
  (a"?" -- re"[a-z_][a-zA-Z0-9]*" -- a"?") || 
  (a sq -- upto_a sq -- a sq) ||
  (a dq -- upto_a dq -- a dq) 
let sym = 
  (nt |>> fun x -> return (`NT x)) || 
  (tm |>> fun x -> return (`TM (_3 x)))
let var_eq = 
  let v = re "[a-z][a-z0-9]*" in
  let v_eq = v -- a"=" in
  opt v_eq -- sym |>> fun (v,s) -> return (v,s)
let syms = plus ~sep:ws var_eq
let bar = ws -- a "|" -- ws 
let rhs = plus ~sep:bar syms  (* plus and star are greedy *)
let rule = 
  sym -- (ws -- a "->" -- ws) -- rhs |>> fun x -> 
  _3 x |> fun (sym,_,rhs) -> return (sym,rhs)
let rules = star ~sep:(ws -- a";" -- ws) rule 
let grammar = ws -- rules -- ws |>> fun x -> _3 x |> fun (_,x2,_) -> return x2


(* example ---------------------------------------------------------- *)

module X_ = functor(_:sig end) -> struct

  let example = {|?w?|}

  let _ = tm example

  let _ = example |> a "?" -- re"[a-z]+"

  let example = {|

(* the expressions we want to parse at top-level *)
S -> ?w? DEFN ?w? ?eof?
| ?w? TYPEDEFINITIONS ?w? ?eof?
| ?w? TYPEXPR ?w? ?eof?

|}

  let _ = grammar example

end

(* ocaml grammar ---------------------------------------------------- *)

let g = {|

(* the expressions we want to parse at top-level *)
S -> ?w? DEFN ?w? ?eof?
| ?w? TYPEDEFINITIONS ?w? ?eof?
| ?w? TYPEXPR ?w? ?eof?
| ?w? "e:" EXPR ?w? ?eof?
| w1=?w? "val" w2=?w? i=EXPR w3=?w? ":" w4=?w? ty=TYPEXPR ?w? ?eof?
;

DEFN ->
    "let" w1=?w? e1=LETBINDING
  | "let" w0=?w? "rec" w1=?w? e1=LETBINDING
;

TYPEDEFINITIONS -> TYPEDEFINITION
  | TYPEDEFINITION ?w? TYPEDEFINITIONS
;

(* 6.3 Names *)

VALUENAME -> ?ident? ;

INFIXOP -> "="
  | "++"
  | "::"
  | "<"
  | "<="

  | "+"
  | "@"
  | "&&"
  | "||"
  | "***>"
  | "||||"
  | ">>>>"
  | ">>="
  | "|||"
;

FIELDNAME -> ?ident? ;

VALUEPATH -> VALUENAME
  | MODULEPATH "." VALUENAME
;

(* this was ?Ident? but we don't want List.map interpreted as CONSTR.FIELDNAME *)
CONSTR ->
    ?constr?
;

MODULEPATH -> ?_Ident?
  | MODULEPATH "." ?_Ident?
;


(* 6.4 Type expressions; following for type defns FIXME needs tidying up  *)

TYPEXPR -> "'" ?ident?
  | "(" w1=?w? t=TYPEXPR w2=?w? ")"
  | t1=TYPEXPR w1=?w? "->" w2=?w? t2=TYPEXPR
  | s1=TYPEXPR w1=?w? "*" w2=?w? s2=TYPEXPR
  | TYPECONSTR
  | s2=TYPECONSTR w1=?w? s1=TYPEXPRA
;

TYPEXPRA -> TYPEXPR
  | TYPEXPR ?w? TYPEXPRA
;

OCAMLTYPEXPR -> "'" ?ident?
  | "(" w1=?w? t=TYPEXPR w2=?w? ")"
  | t1=TYPEXPR w1=?w? "->" w2=?w? t2=TYPEXPR
  | s1=TYPEXPR w1=?w? "*" w2=?w? s2=TYPEXPR
  | TYPECONSTR
  | s1=TYPEXPR w1=?w? s2=TYPECONSTR
  | "(" w1=?w? s1=TYPEXPRA w2=?w? ")" w3=?w? s2=TYPECONSTR
;

OCAMLTYPEXPRA -> TYPEXPR
  | TYPEXPR ?w? "," ?w? TYPEXPRA
;

(*
  | "(" ?w? TYPE ?w? ")"
  | TYPE ?w? ?ident?
  | MODULEPATH "." TYPEXPR
  | TYPEXPR ?w? TYPECONSTR
*)

TYPECONSTR -> TYPECONSTRNAME
  | MODULEPATH "." TYPECONSTRNAME
;

POLYTYPEXPR -> TYPEXPR
;




(* 6.5 Constants *)

CONSTANT -> CONSTR
  | "[" ?w? "]"
  | ?num?
  | "-" ?num?
  | '"' s=?notdquote? '"'
  | "'" s=?notsquote? "'"
  | "()"
;



(* 6.6 Patterns *)

PATTERN -> EXPR
;


(* 6.7 Expressions ; grammar is too ambiguous so we identify atomic expressions which can be arguments to functions *)

EXPR -> ATEXPR
  | e=EXPR ":" t=TYPEXPR
  | EXPR ?w? "," ?w? EXPRA
  | CONSTR ?w? EXPR
  | EXPR ?w? "::" ?w? EXPR
  | FNARGS
  | EXPR ?w? INFIXOP ?w? EXPR
  | "if" w1=?w? e1=EXPR w2=?w? "then" w3=?w? e2=EXPR w4=?w? "else" w5=?w? e3=EXPR

  | "match" w1=?w? e=EXPR w2=?w? "with" w3=?w? cs=PATTERNMATCHING w4=?w? "end"

  | "let" w1=?w? e1=LETBINDING w2=?w? "in" w3=?w? e2=EXPR

  | "let" w0=?w? "rec" w1=?w? e1=LETBINDING w2=?w? "in" w3=?w? e2=EXPR

  | "fun" w1=?w? e1=MULTIPLEMATCHING
;

(* FIXME List.map parses as ATEXPR "." FIELDNAME, where ATEXPR is CONSTANT CONSTR *)

ATEXPR ->
    VALUEPATH
  | CONSTANT
  | "(" w1=?w? e1=EXPR w2=?w? ")"
  | "[" w1=?w? ss=EXPRLIST w2=?w? "]"
  | RECORD
  | s1=ATEXPR "." s2=FIELDNAME
  | s1=ATEXPR "." "[" s2=EXPR "]"
  | "_"
  | "<fun>"
  | "(" w1=?w? s1=INFIXOP w2=?w? ")"
;

EXPRA -> EXPR
  | EXPR ?w? "," ?w? EXPRA
;


EXPRLIST ->
    EXPR
  | EXPR ?w? ";" ?w? EXPRLIST
;


PATTERNMATCHING -> CASESB
  | "|" w1=?w? x=CASESB
;


CASESB ->
  CASE
  | c1=CASE w1=?w? "|" w2=?w? cs=CASESB
(* above clause erroneously allows cases to start with a bar *)
;

CASE -> e1=PATTERN w1=?w? "->" w2=?w? e2=EXPR
;


MULTIPLEMATCHING -> PATTERNMATCHING ;

LETBINDING -> PATTERN ?w? "=" ?w? EXPR ;



FNARGS -> ATEXPR ?w? ATEXPR
  | ATEXPR ?w? FNARGS
;

RECORD ->
    "<|" w1=?w? fs=FIELDS w2=?w? "|>"
  | "<|" w1=?w? i=ATEXPR w2=?w? "with" w3=?w? fs=FIELDS w4=?w? "|>"
;

FIELDS ->
    FIELD
  | FIELD ?w? ";" ?w? FIELDS
;

FIELD ->
    f=?ident? w1=?w? "=" w2=?w? e=EXPR
;


(* 6.8 Type and exception definitions *)

TYPEDEFINITION -> "type" w1=?w? s2=TYPEDEF
;

TYPEDEF -> TYPECONSTRNAME ?w? TYPEINFORMATION

  | TYPECONSTRNAME ?w? TYPEPARAMS ?w? TYPEINFORMATION

      (* FIXME what about params? may cause problems because hol lists params in order they occur in defn :( *)
;

TYPEPARAMS -> TYPEPARAM
  | TYPEPARAM ?w? TYPEPARAMS
;

OCAMLTYPEPARAMS -> TYPEPARAM
  | "(" ?w? TYPEPARAMSA ?w? ")"
;

TYPEPARAMSA -> TYPEPARAM
  | TYPEPARAM ?w? "," ?w? TYPEPARAMSA
;

TYPEPARAM -> "'" ?ident?
;

TYPECONSTRNAME -> ?ident?
;

TYPEINFORMATION -> TYPEEQUATION
  | TYPEREPRESENTATION
;

TYPEEQUATION -> "=" ?w? TYPEXPR ;

TYPEREPRESENTATION -> "=" ?w? CONSTRDECLS
  | "=" w3=?w? "<|" w1=?w? s2=FIELDDECLS w2=?w? "|>"
;


CONSTRDECLS -> CONSTRDECL
  | s1=CONSTRDECL w1=?w? "|" w2=?w? s2=CONSTRDECLS
;

CONSTRDECL -> CONSTRNAME
  | s1=CONSTRNAME w1=?w? "of" w2=?w? s2=TYPEXPR
;

CONSTRNAME -> ?_Ident? ;

(* following can end in optional ; *)
FIELDDECLS -> FIELDDECLSA
  | s1=FIELDDECLSA w1=?w? s2=";"
;

FIELDDECLSA -> FIELDDECL
  | s1=FIELDDECL w1=?w? ";" w2=?w? s2=FIELDDECLSA
;

(* FIXME the pattern is we map nonterms to strings; this causes lots of messing about with c and ^ *)

FIELDDECL -> s1=FIELDNAME w1=?w? ":" w2=?w? s2=POLYTYPEXPR 

|}

let _ = print_endline "Parsing grammar (x100)..."

let g' = for i = 1 to 100 do ignore(grammar g) done

let _ = grammar g |> function Some x -> x | None -> failwith __LOC__ 

let _ = print_endline "finished!"

(*

$ time ./a.out
Parsing grammar (x100)...
finished!

real	0m0.228s
user	0m0.212s
sys	0m0.016s

*)
