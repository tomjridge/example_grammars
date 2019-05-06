(** An instance of P0 with extra state to record debug information. *)

module State = struct
  type t = { input:string; debug:(string*string) list }  
  (** debug field is the stack of current executing parsers, and the
     input they were called on *)

  let empty_state = { input=""; debug=[] }
end

module P0 = struct

  open P0_lib.Internal

  open State

  module StringIM = struct
    module I = StringI
    type t = State.t 
    let get_input () = P0_lib.Monad.Internal.of_fun (fun t -> 
        Some(t.input,t))
    let set_input i = P0_lib.Monad.Internal.of_fun (fun t ->
        Some((),{t with input=i}))
  end

  module Instance = struct
    include (struct include StringI end : module type of StringI with type t:=string)
    include StringIM
    include Re_
    include Make(StringI)(StringIM)(Re_)
  end

  module Export : sig

    (** {2 String utils} *)

    val drop : int -> string -> string
    val len : string -> int
    val split_at : int -> string -> string * string
    (* type t = string *)


    (** {2 Regular expressions} *)
    
    type re = Re.t (* Re_.re *)
    (* type compiled_re = Re.re (\* Re_.compiled_re *\) *)
    val literal : string -> re
    val longest : re -> re
    (* val compile_bos_longest : re -> compiled_re *)

    type group = Re.Group.t
    val group_stop : group -> int
    (* val exec_opt : compiled_re -> string -> group option *)


    (** {2 Monad type and ops} *)

    type 'a m = ('a, State.t) P0_lib.Monad.m
    val return : 'a -> 'a m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val of_fun : (State.t -> ('a * State.t) option) -> 'a m
    val to_fun : 'a m -> State.t -> ('a * State.t) option
    val get_input : unit -> string m
    val set_input : string -> unit m

    (** {2 Regular expressions and the monad} *)

    (* val raw_exec_cre_no_drop : compiled_re -> group m *)
    (* val raw_exec_cre_and_drop : compiled_re -> (string * group) m *)

    (** The main interface to regular expressions *)
    val re : re -> string m

    (** {2 Standard combinators} *)
    val a : string -> string m
    val opt : 'a m -> 'a option m
    val then_ : 'a m -> 'b m -> ('a * 'b) m
    val ( -- ) : 'a m -> 'b m -> ('a * 'b) m
    val plus : sep:'a m -> 'b m -> 'b list m
    val star : sep:'a m -> 'b m -> 'b list m
    val alt : 'a m -> 'a m -> 'a m
    val ( || ) : 'a m -> 'a m -> 'a m
    val end_of_string : unit m
    val alternatives : 'a m list -> 'a m
    val sequence : 'a m list -> 'a list m

    (** {2 Support for OCaml's Str regexp lib *)
    val str_re : string -> string m
  end = struct
    include Instance 
    open P0_lib
    let ( >>= ) = Monad.( >>= )
    let return = Monad.return
    let of_fun = Monad.Internal.of_fun
    let to_fun = Monad.Internal.to_fun

    let str_re (re:string) = 
      let open Str in
      let re = Str.regexp re in
      of_fun (fun s -> 
          string_match re s.input 0 |> function
          | false -> None
          | true -> matched_string s.input |> fun mat -> 
                    Some(mat,{s with input=drop (String.length mat) s.input}))
    let _ = str_re

  end
    

end

include P0.Export
