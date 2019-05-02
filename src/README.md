The sequence of parsing and generating parsers is:

- abnf_grammar.txt is a handwritten grammar used for parsing ABNF format files
- parse_abnf_grammar_txt.ml parses this file and generates abnf_grammar.sexp
- generate_abnf_parser.ml then takes the result and generates a parser
  for ABNF format files
  - abnf_parser_rules.txt is the resulting code fragment, in a parsing
    DSL (OCaml subset) from tjr_simple_earley
- we can now use the ABNF format parser to parse the (ABNF format)
  definition of the IMAP protocol
- having done *this*, we can then generate a further parser to parse
  IMAP protocol messages (using some specific ABNF code generation
  combinators)
  
