(**********)
(* Timers *)
(**********)

type t
val newtimer:string->t
val start: t->unit
val stop: t->unit
val name: t->string
val watch: t->float
val reset: t->unit
val transfer: t->t->unit
