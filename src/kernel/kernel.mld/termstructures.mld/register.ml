module TSHandler = struct

  type _ t =
    | LitF    : Literals.TS.t t
    | Clauses : Clauses.TS.t t

  let equal (type a) (type b) : a t -> b t -> (a -> b) option =
    fun ts1 ts2 ->
    match ts1,ts2 with
    | LitF, LitF -> Some(fun x->x)
    | Clauses,Clauses -> Some(fun x->x)
    | _ -> None

  let getTS (type a) : a t -> (module Top.Specs.DataType with type t = a) = function
    | LitF    -> (module Literals.TS)
    | Clauses -> (module Clauses.TS)
    
                             
end
