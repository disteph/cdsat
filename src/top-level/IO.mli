(* IO tools *)

val write_to_file : string -> string -> unit

(* converts a file to a string with its contents *)

val read_from_file : string -> string

exception InvalidInput of exn

val read_from_stdin : ?stream:in_channel -> unit -> string
