(*

Combinators for rfc [245]234 parsing

#directory "../../src_ext/p1/build";;
#load "p1.cma";;
#directory "../../src_ext/e3/build";;
#load "e3.cma";;
#directory "../../src_ext/p4/build";;
#load "p4.cma";;

*)

open P4_lib

let parse_1 = (a "1") >>> (fun (`SS(s,i,j)) -> 1)
let parse_eps = (a "") >>> (fun _ -> 0)

let rec itern n p = (
  let rec rhs' m = (match m with 
    | 0 -> (rhs parse_eps >> (fun _ -> []))
    | _ -> ((rhs' (m-1) >- p) >> (fun (xs,x) -> xs@[x])))
  in
  let alts = alts[rhs' n] in
  mkntparser (mk_pre_parser ()) (fun () -> alts))

let p = itern 5 parse_1
let txt = "11111"
let _ = assert ([[1; 1; 1; 1; 1]] = run_parser_string p txt)
let _ = assert ([] = run_parser_string p "1111")
let _ = assert ([] = run_parser_string p "111111")

(* no memo; star aka many *)
let star p = (
  let star_p = ref(mk_pre_parser ()) in
  let alts = lazy(alts[
    rhs parse_eps >> (fun _ -> []);
    (p >-- !star_p) >> (fun (x,xs) -> x::xs)])
  in
  let _ = star_p := mkntparser_lazy !star_p alts in
  !star_p)


let rec parse_E = (star parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = run_parser_string p "11111")
let _ = assert ([[]] = run_parser_string p "")


let parse_maybe p =
  let alts = alts [
    (rhs parse_eps) >> (fun _ -> None);
    (rhs p) >> (fun x -> Some x)]
  in
  mkntparser (mk_pre_parser()) (fun () -> alts)

let p = (parse_maybe parse_1)
let _ = assert ([Some 1] = run_parser_string p "1")
let _ = assert ([None] = run_parser_string p "")
let _ = assert ([] = run_parser_string p "11")

(*
parse p at most h times; simplest to express as: at_most h p = itern h (parse_maybe p); but this is highly ambiguous; an alternative is to construct the rhss directly
*)

let rec upto m n = if m <= n then m::upto (m+1) n else []

let at_most h p = (
  let rec rhs' m = (match m with 
    | 0 -> (rhs parse_eps >> (fun _ -> []))
    | _ -> ((rhs' (m-1) >- p) >> (fun (xs,x) -> xs@[x])))
  in
  let rhss = List.map rhs' (upto 0 h) in
  let alts = alts rhss in
  mkntparser (mk_pre_parser ()) (fun () -> alts))

let p = at_most 5 parse_1
let _ = assert([[1]] = run_parser_string p "1")
let _ = assert([[1;1]] = run_parser_string p "11")
let _ = assert([[1;1;1;1;1]] = run_parser_string p "11111")
let _ = assert([] = run_parser_string p "111111")

let parse_range l h p = (
  let alts = alts[
    (rhs (itern l p) >- at_most (h-l) p) >> (fun (x,y) -> x@y)]
  in
  mkntparser (mk_pre_parser()) (fun () -> alts))

let p = parse_range 3 5 parse_1
let _ = assert([[1;1;1]] = run_parser_string p "111")
let _ = assert([[1;1;1;1;1]] = run_parser_string p "11111")
let _ = assert([] = run_parser_string p "111111")
let _ = assert([] = run_parser_string p "11")


(* grouping *)

let parse_2 = (a "2") >>> (fun _ -> 2)

let p = (
  let alts = lazy(alts[
      ((rhs parse_1) >- (
          (* the following might arise from a nested group *)
          let alts = lazy(alts[
              ((rhs parse_1) >- parse_2 >- parse_2 >- parse_1) >> (fun _ -> ())])
          in
          mkntparser_lazy (mk_pre_parser()) alts)
       >- parse_1) >> (fun _ -> ());     
    ])
  in
  mkntparser_lazy (mk_pre_parser()) alts)

let _ = run_parser_string p "112211" 
