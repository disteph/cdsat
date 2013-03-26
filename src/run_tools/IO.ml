(* IO tools *)

open Printf
open String

let write_to_file filename s =
  let chan = open_out filename in 
    fprintf chan "%s\n" s;  
    close_out chan

(* converts a file to a string with its contents *)

let read_from_file filename =
  let lines = ref "" in
  let chan = open_in filename in
    try
      while true; do
	let line = input_line chan in
	  if (length line>0)&&(not (line.[0]='c')) then 
	    lines := (!lines)^"\n"^line
      done; ""
    with End_of_file ->
      close_in chan;
      !lines

exception InvalidInput of exn

let read_from_stdin = fun ?(stream = stdin) () ->
  let respace = Str.regexp "^[ \t]*$" in
  let rec input = fun data ->
    let line =
      try  Some (input_line stream)
      with End_of_file -> None
    in
      match line with
      | Some line ->
          if   Str.string_match respace line 0
          then input data
          else input (data^"\n"^line)
      | None      -> data
  in
    input ""
