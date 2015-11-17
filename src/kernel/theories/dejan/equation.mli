
type var = int
type value = Num.num
type equation

val create : (var, value) Hashtbl.t -> value -> bool -> equation list -> equation
val createFromList : (var*value) list -> value -> bool -> equation list -> equation

val getCoeff : equation -> var -> value
val getSup : equation -> value
val isStrict : equation -> bool
val isAtomic : equation -> bool
val isTrivial : equation -> bool
val getActiveVar : equation -> var
val getAnotherActiveVar : equation -> var -> var
val getPrevious : equation -> equation list
val getPreviousEqs : equation list -> equation list

val affectVar : equation -> var -> value -> equation
val addDependance : equation -> equation list -> equation
val setDependance : equation -> equation list -> equation

val multiply : equation -> value -> equation
val add : equation -> equation -> equation
val combine : value -> equation -> value -> equation -> equation
val print : equation -> unit
val print_eqs : equation list -> unit
