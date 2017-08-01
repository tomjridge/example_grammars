(* a cleaned up version suitable for the newly-revamped p1 2017-07-01 *)

(* experiment with monadic parsing; 'a m takes a string and returns an
   'a * string or an error/noparse indication *)

type 'a m = string -> ('a * string) option

let bind (f:'a -> 'b m) (x:'a m) :'b m = 
  fun s -> x s |> function | None -> None | Some (v,s) -> f v s

let return x s = Some(x,s)

let then_ a b = a |> bind @@ fun x -> b |> bind @@ fun y -> return (x,y)
let ( -- ) = then_

let can x s = Some (x s <> None,s)

let a lit s = 
  if Tjr_string.starts_with ~prefix:lit s 
  then String.sub s 0 (String.length lit) |> fun s' -> Some(lit,s') 
  else None

let upto_a lit s =
  Tjr_substring.(upto_a lit {s_=s;i_=0}) |> fun xs ->
  if xs <> [] 
  then Tjr_string.split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
  else None

let re s = 
  Tjr_substring.(re ~re:(Str.regexp s) {s_=s;i_=0}) |> fun xs ->
  if xs <> []
  then Tjr_string.split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
  else None

let rec plus ~sep p = 
  p |> bind @@ fun x ->
  can (sep -- p) |> bind @@ fun more ->
  if more then plus ~sep p |> bind @@ fun xs -> return (x::xs)
  else return [x]

let star ~sep p =
  can p |> bind @@ fun at_least_one ->
  if at_least_one then plus ~sep p else return []

(* shortcut alternative *)
let ( || ) a b = can a |> bind @@ fun x -> if x then a else b
                   
let f ~a ~upto_a ~return ~re ~plus ~star ~can = 
  let ( -- ) a b = failwith __LOC__ in
  let ( || ) a b = failwith __LOC__ in
  let comm = "(*" -- upto_a "*)" -- a "*)" in  (* FIXME nested comments *)
  let rec ws = (* 0 or more *)
    re "[ \n]*" --  (* re is longest match *)
    if can comm then comm -- ws  else return ()
  in
  let nt = re "[A-Z]+" in
  let tm = 
    let sq = "'" in
    let dq = "\"" in
    (a"?" -- re"[a-z]+" -- a"?") || 
    (a sq -- upto_a sq -- a sq) ||
    (a dq -- upto_a dq -- a dq) 
  in
  let sym = nt || tm (* should return a string *) in
  let bar = ws -- a "|" -- ws in
  let rhs = plus ~sep:bar sym in (* plus and star are greedy *)
  let rule = sym -- ws -- a "->" -- ws -- rhs in
  let rules = star ~sep:(ws -- a";" -- ws) in
  rule,rules
    


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
