type num = Core.Num.num
 
type constraint_t  = [ (*`Eq |*) `Le | `Ge | `Lt | `Gt ]
 
type simplex_K_t
type simplex_N_t

exception InvalidProblem of string

type 'a simplex_t = 
       ?copy:bool
    -> num array array
    -> (num * constraint_t) array
    -> 'a

val simplex_K : simplex_K_t simplex_t
val simplex_N : simplex_N_t simplex_t

type ucert_K = num array

type ucert_N =
  [ `NC_Split of int * Big_int.big_int * ucert_N * ucert_N
  | `NC_Base  of num array * num array ]

type ('num, 'ucert) result =
  [ `Satisfiable   of 'num array
  | `Unsatisfiable of 'ucert ]

val solve_K : simplex_K_t -> (num, ucert_K) result
val solve_N : simplex_N_t -> (Big_int.big_int, ucert_N) result

val string_of_ucert_N : ucert_N -> string
