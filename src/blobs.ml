(** We include text files via ppx_blob *)

let abnf_grammar_txt = [%blob "abnf_grammar.txt"]

let abnf_grammar_sexp = [%blob "abnf_grammar.sexp"]
