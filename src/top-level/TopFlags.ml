(* This module is the control panel to run the program with different options *)
(* Psyche flags *)

let sizesort  = ref true         (* Sorts input files by size *)
let fname = ref ([]:string list) (* List of files or directories to treat *)
let clear4each = ref true    (* Clears structures, including table of learned lemmas,
                                between each problem *)    

let skipsat     = ref false  (* Skips instances expected to be sat *)
let skipunsat   = ref false  (* Skips instances expected to be unsat *)
let skipunknown = ref false  (* Skips instances without any result expectation *)

(* Variables containing the user-provided info about theories and plugins (from command-line) *)

let parser   : string list option ref  = ref None (* List of parsers to try. None = try them all *)
let myplugin : string ref = ref "basic"           (* Default plugin *)
let notheories: string list option ref = ref (Some[]) (* List of forbidden theories *)
let addtheories: string list option ref = ref (Some[]) (* List of added theories *)

let dtag     = ref ""      (* Temp variable to construct following ref *)
let dtags : (string*int*bool) list ref
             = ref []      (* Activates debug mode (displays fails, etc) *)
