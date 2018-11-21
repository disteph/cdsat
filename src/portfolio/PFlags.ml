(* Plugin flags *)

let bool_decay  = ref 1.3   (* decay factor for Bool plugin *)
let decnumb   = ref 0 (* Number of decisions being made *)
let decnumbB  = ref 0 (* Number of Boolean decisions being prepared *)
let decwidth  = ref 0 (* Number of decision options *)
let forgetlemmas = ref true (* enable lemma forgetting *)
let lemmasincrmt = ref 1.1 (* increment for lemma forgetting *)
let lemmasmax = ref 1000 (* max lemma count for lemma forgetting *)
