(* a cleaned up version suitable for the newly-revamped p1 2017-07-01 *)

(* worth working with indexes rather than strings? *)

let (starts_with,drop) = Tjr_string.(starts_with,drop)

let upto_a lit = Tjr_substring.upto_re ~re:Str.(regexp_string lit)

(* naive monadic parsing -------------------------------------------- *)

(* experiment with monadic parsing; 'a m takes a string and returns an
   'a * string or an error/noparse indication *)

type 'a m = string -> ('a * string) option

let bind (f:'a -> 'b m) (x:'a m) :'b m = 
  fun s -> x s |> function | None -> None | Some (v,s) -> f v s
let ( |>> ) x f = x |> bind f

let return x s = Some(x,s)

let then_ a b = a |>> fun x -> b |>> fun y -> return (x,y)
let ( -- ) = then_

(* FIXME improve this by using the result of the parse subsequently *)
(* let can x s = Some (x s <> None,s) *)

let a lit s = 
  if starts_with ~prefix:lit s 
  then drop (String.length lit) s |> fun s' -> Some(lit,s') 
  else None

let upto_a lit = ( 
  let p = upto_a lit in
  fun s -> 
    p {s_=s;i_=0} |> fun xs ->
    if xs <> [] 
    then Tjr_string.split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
    else None) [@@warning "-w-40"]

let re re' = (
  let re' = Str.regexp re' in
  fun s ->
    Tjr_substring.(re ~re:re' {s_=s;i_=0}) |> fun xs ->
    if xs <> []
    then Tjr_string.split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
    else None) [@@warning "-w-40"]

let try_ p s = 
  p s |> function
  | None -> Some(None,s) 
  | Some(x,s) -> Some(Some x,s)

let rec plus ~sep p = 
  p |>> fun x ->
  (try_ (sep -- plus ~sep p)) |>> function
  | None -> return [x]
  | Some (_,xs) -> return (x::xs)

let save s = Some(s,s)

let restore s' s = Some((),s')

(* a bit fiddly! *)
let star ~sep p =
  try_ p |>> function
  | None -> return []
  | Some x -> 
    save |>> fun state ->
    try_ sep |>> function
    | None -> restore state |>> fun _ -> return [x]
    | _ -> 
      try_ (plus ~sep p) |>> function
      | None -> restore state |>> fun _ -> return [x]
      | Some xs -> return (x::xs)

(* shortcut alternative *)
let alt a b = 
  try_ a |>> function
  | None -> b
  | Some x -> return x

let ( || ) = alt       

let discard p = p |>> fun _ -> return ()

let _0 = return ()

let ( --- ) a b = (a -- b) |>> fun _ -> _0
           
let _Some x = Some x
let opt x = try_ x 
let _ = opt

(* grammar of grammars ---------------------------------------------- *)

let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)
let rec ws s = 
  s |> (* 0 or more *) (* re is longest match *)
  (re "[ \n]*") --- 
  (try_ comm |>> function
    | None -> _0
    | Some _ -> ws |>> fun _ -> _0)
let nt = re "[A-Z]+" 
let tm = 
  let sq = "'" in
  let dq = "\"" in
  (a"?" -- re"[a-z_][a-zA-Z0-9]*" -- a"?") || 
  (a sq -- upto_a sq -- a sq) ||
  (a dq -- upto_a dq -- a dq) 
let sym = 
  (nt |>> fun x -> return (`NT x)) || 
  (tm |>> fun x -> return (`TM x))
let var_eq = 
  let v = re "[a-z][a-z0-9]*" in
  let v_eq = v -- a"=" in
  opt v_eq -- sym |>> fun (v,s) -> return (v,s)
let syms = plus ~sep:ws var_eq
let bar = ws -- a "|" -- ws 
let rhs = plus ~sep:bar syms  (* plus and star are greedy *)
let rule = 
  sym -- (ws -- a "->" -- ws) -- rhs |>> fun ((sym,_),rhs) -> return (sym,rhs)
let rules = star ~sep:(ws -- a";" -- ws) rule 
let grammar = ws -- rules -- ws |>> fun ((_,x2),_) -> return x2 


(* example ---------------------------------------------------------- *)

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

let _ = print_endline "Parsing grammar..."

let g' = grammar g 

(* let _ = g' |> function Some x -> x | None -> failwith __LOC__ *)

let _ = print_endline "finished!"

(*

$ time ./a.out
Parsing grammar...
finished!

real	0m0.010s
user	0m0.000s
sys	0m0.004s

*)
