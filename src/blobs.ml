(** We include text files via ppx_blob *)

let abnf_pbnf = [%blob "abnf.pbnf"]

let abnf_sexp = [%blob "abnf.sexp"]

let imap_protocol_abnf = [%blob "imap_protocol.abnf"]

let scala_grammar = [%blob "scala_grammar.ebnf"]
