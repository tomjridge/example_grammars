;; -*- scheme -*-
(S (?ws? RULELIST ?ws? ?eof?) 
   "x2 |> sexp_of_rulelist |> Sexplib.Sexp.to_string_hum |> print_endline_")

(S (?ws? RULELIST ?ws? ?eof?) S_act.s2)





#| ambiguity in rfc 2234: <<r = a b c \n ; comment>> the comment can attach to elements or be a separate rulelist_elt |#

(RULELIST 
 (RULE WS_AND_COMMENTS RULELIST_REST) "match x3 with RULELIST(xs) -> RULELIST(RE_RULE(x1)::xs)" )

(RULELIST_REST 
 ((?eps?                                (RULELIST([])))
  (RULELIST                             x1)))

(WS_AND_COMMENTS
 ((?eps?                                 x1)
  (STAR_CWSP CNL WS_AND_COMMENTS         "ss_concat [x1;x2;x3] |> dest_Some")))

;; NOTE allow actions to be a string, a bracket-delineated expression, or a bracketed string? and also just an atom (eg x1, or some other function that is defined

(RULE -> (RULENAME STAR_CWSP DEFINED_AS STAR_CWSP ELEMENTS) (RULE(x1,x3,x5)))



;; an alternative with reduced whitespace:

(S -> ?ws? RULELIST ?ws? ?eof? / S_act.s2)


(WS_AND_COMMENTS -> 
  | ?eps?                                 / x1
  | STAR_CWSP CNL WS_AND_COMMENTS         / "ss_concat [x1;x2;x3] |> dest_Some")


;; or even just use sexp as a tokenizer...


WS_AND_COMMENTS -> 
  | ?eps?                                 / x1
  | STAR_CWSP CNL WS_AND_COMMENTS         / "ss_concat [x1;x2;x3] |> dest_Some"

;; and parse using a stupid p0 parser...
