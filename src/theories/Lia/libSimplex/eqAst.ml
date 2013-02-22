(* -------------------------------------------------------------------- *)
type equation = {
  eq_coeffs : Num.num Core.StringMap.t;
  eq_sign   : [ (*`Eq |*) `Le | `Ge | `Lt | `Gt ];
  eq_bound  : Num.num;
  id        : int
}

(* -------------------------------------------------------------------- *)
let ids_of_equations = fun equations ->
  let ids =
    (List.fold_left
       (fun ids eq ->
          Core.StringSet.union ids (Core.StringMap.keys eq.eq_coeffs))
         Core.StringSet.empty
       equations)
  in
    Core.StringSet.elements ids
