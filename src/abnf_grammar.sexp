((S
  ((((Tm (Tm_qu ?ws?)) (Nt RULELIST) (Tm (Tm_qu ?ws?)) (Tm (Tm_qu ?eof?)))
    " x2 |> sexp_of_rulelist |> Sexplib.Sexp.to_string_hum |> print_endline ")))
 (RULELIST
  ((((Nt RULELIST_ELT)) " RULELIST[x1] ")
   (((Nt RULELIST_ELT) (Nt RULELIST))
    " match x2 with RULELIST(xs) -> RULELIST(x1 :: xs) ")))
 (RULELIST_ELT
  ((((Nt RULE)) " RE_RULE(x1) ")
   (((Tm (Tm_qu ?wsplus?))) " RE_CWSP_CNL(c x1) ")))
 (STAR_CWSP_CNL
  ((((Nt CNL)) " x1 ")
   (((Nt CWSP) (Nt STAR_CWSP_CNL)) " ss_concat [x1;x2] |> dest_Some ")))
 (RULE
  ((((Nt RULENAME) (Nt DEFINED_AS) (Nt ELEMENTS) (Nt CNL))
    " RULE(x1,x2,x3,c x4) ")))
 (RULENAME ((((Tm (Tm_qu ?rulename?))) " RULENAME (c x1) ")))
 (DEFINED_AS
  ((((Nt STAR_CWSP) (Nt EQUAL_OR_EQUAL_SLASH) (Nt STAR_CWSP)) " x2 ")))
 (STAR_CWSP
  ((((Tm (Tm_qu ?eps?))) " () ") (((Nt CWSP) (Nt STAR_CWSP)) " () ")))
 (EQUAL_OR_EQUAL_SLASH
  ((((Tm (Tm_lit ("\"" = "\"")))) " DAS_EQUAL ")
   (((Tm (Tm_lit ("\"" =/ "\"")))) " DAS_EQUAL_SLASH ")))
 (ELEMENTS ((((Nt ALTERNATION) (Nt STAR_CWSP)) " ELEMENTS(x1) ")))
 (CWSP
  ((((Tm (Tm_qu ?wsp?))) " x1 ")
   (((Nt CNL) (Tm (Tm_qu ?wsp?))) " ss_concat [x1;x2] |> dest_Some ")))
 (CNL ((((Nt COMMENT)) " x1 ") (((Tm (Tm_qu ?crlf?))) " x1 ")))
 (COMMENT
  ((((Tm (Tm_lit ("\"" ";" "\""))) (Nt STAR_WSP_VCHAR) (Tm (Tm_qu ?crlf?)))
    " ss_concat [x1;x2;x3] |> dest_Some ")))
 (STAR_WSP_VCHAR
  ((((Tm (Tm_qu ?eps?))) " x1 ")
   (((Tm (Tm_qu ?wsp?)) (Nt STAR_WSP_VCHAR))
    " ss_concat [x1;x2] |> dest_Some ")
   (((Tm (Tm_qu ?vchar?)) (Nt STAR_WSP_VCHAR))
    " ss_concat [x1;x2] |> dest_Some ")))
 (ALTERNATION
  ((((Nt CONCATENATION) (Nt STAR_ALTERNATION_REST)) " ALTERNATION(x1::x2) ")))
 (STAR_ALTERNATION_REST
  ((((Tm (Tm_qu ?eps?))) " [] ")
   (((Nt STAR_CWSP) (Tm (Tm_lit ("\"" / "\""))) (Nt STAR_CWSP)
     (Nt CONCATENATION) (Nt STAR_ALTERNATION_REST))
    " x4::x5 ")))
 (CONCATENATION
  ((((Nt REPETITION) (Nt STAR_CONCATENATION_REST)) " CONCATENATION(x1::x2) ")))
 (STAR_CONCATENATION_REST
  ((((Tm (Tm_qu ?eps?))) " [] ")
   (((Nt ONE_STAR_CWSP) (Nt REPETITION) (Nt STAR_CONCATENATION_REST))
    " x2::x3 ")))
 (ONE_STAR_CWSP ((((Nt CWSP) (Nt STAR_CWSP)) " () ")))
 (REPETITION
  ((((Nt ELEMENT)) " REP(None,x1) ")
   (((Nt REPEAT) (Nt ELEMENT)) " REP(Some(x1),x2) ")))
 (REPEAT ((((Tm (Tm_qu ?repeat?))) " REPEAT ")))
 (ELEMENT
  ((((Nt RULENAME)) " EL_RULENAME x1 ") (((Nt GROUP)) " EL_GROUP x1 ")
   (((Nt OPTION)) " EL_OPTION x1 ") (((Nt CHAR_VAL)) " EL_CHAR_VAL ")
   (((Nt NUM_VAL)) " EL_NUM_VAL ") (((Nt PROSE_VAL)) " EL_PROSE_VAL ")))
 (GROUP
  ((((Tm (Tm_lit ("\"" "(" "\""))) (Nt STAR_CWSP) (Nt ALTERNATION)
     (Nt STAR_CWSP) (Tm (Tm_lit ("\"" ")" "\""))))
    " GROUP(x3) ")))
 (OPTION
  ((((Tm (Tm_lit ("\"" [ "\""))) (Nt STAR_CWSP) (Nt ALTERNATION)
     (Nt STAR_CWSP) (Tm (Tm_lit ("\"" ] "\""))))
    " OPTION(x3) ")))
 (CHAR_VAL
  ((((Tm (Tm_qu ?dquote?)) (Tm (Tm_qu ?char_vals?)) (Tm (Tm_qu ?dquote?)))
    " ss_concat [x1;x2;x3] |> dest_Some ")))
 (NUM_VAL
  ((((Tm (Tm_lit ("\"" % "\""))) (Nt NUM_VAL_REST))
    " ss_concat [x1;x2] |> dest_Some ")))
 (NUM_VAL_REST
  ((((Nt BIN_VAL)) " x1 ") (((Nt DEC_VAL)) " x1 ") (((Nt HEX_VAL)) " x1 ")))
 (BIN_VAL
  ((((Tm (Tm_lit ("\"" b "\""))) (Tm (Tm_qu ?one_star_bit?))
     (Tm (Tm_qu ?bin_val_rest?)))
    " ss_concat [x1;x2;x3] |> dest_Some ")))
 (DEC_VAL
  ((((Tm (Tm_lit ("\"" d "\""))) (Tm (Tm_qu ?one_star_digit?))
     (Tm (Tm_qu ?dec_val_rest?)))
    " ss_concat [x1;x2;x3] |> dest_Some ")))
 (HEX_VAL
  ((((Tm (Tm_lit ("\"" x "\""))) (Tm (Tm_qu ?one_star_hexdig?))
     (Tm (Tm_qu ?hex_val_rest?)))
    " ss_concat [x1;x2;x3] |> dest_Some ")))
 (PROSE_VAL
  ((((Tm (Tm_lit ("\"" < "\""))) (Tm (Tm_qu ?prose_val_chars?))
     (Tm (Tm_lit ("\"" > "\""))))
    " x2 "))))