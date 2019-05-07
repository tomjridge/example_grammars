# example_grammars/src

The sequence of parsing and generating parsers is:

| File                     | Description                                                  |
| ------------------------ | ------------------------------------------------------------ |
| p0_with_debug.ml         | A version of P0 with extra state info, for debugging         |
| abnf.pbnf                | A handwritten specification of the ABNF format, in "Plain BNF" |
| pbnf_parser.ml           | PBNF parser; parses abnf.pbnf and generates abnf.sexp        |
| abnf.sexp                | A .sexp version of the ABNF specification                    |
| generate_abnf_parser.ml  | Takes abnf.sexp and generates a parser for ABNF format files |
| abnf_parser.ml.generated | Generated code fragment, in a parsing DSL (OCaml subset) from tjr_simple_earley |
| abnf_parser.ml           | Generated parser for ABNF-format files (with some generic additional code that should be factored out) |
| imap_protocol.abnf       | ABNF-format definition of the IMAP protocol                  |
| imap_protocol.sexp       | sexp version of the IMAP protocol                            |
| generate_imap_parser.ml  |                                                              |
| imap_parser.ml.generated |                                                              |
| imap_parser.ml           | Parser for IMAP protocol messages                            |

