open General
open Patricia
open Kernel.Top.Messages

module type Config = sig

  include TwoWatchedLits.Config

  type sign
  type tset
  type constraints

  val init_fixed : fixed
  val init_constraints : constraints

  type result = 
    | UNSAT of (sign,tset,thProvable) thsays
    | Propagate of fixed * Var.t
    | Meh

  val extract_msg: constraints -> (sign,tset,thStraight) thsays option 
  val constreat : Constraint.t -> constraints 
    -> (constraints * result, Var.t * Var.t) Sums.sum

end

module Make(C: Config) = struct

  module WL = TwoWatchedLits.Make(C)

  type t = {
    fixed : C.fixed;
    constraints : C.constraints;
    watch : WL.t
  }

  let init = {
    fixed = C.init_fixed;
    constraints = C.init_constraints;
    watch = WL.init
  }

  let rec treat c t = 
    match C.constreat c t.constraints with
    | Sums.A(constraints', res) ->
       begin
         match res with
         | C.UNSAT msg -> C.extract_msg constraints', Sums.A msg
         | C.Propagate(fixed',var) ->
            run { fixed = fixed';
                  constraints = constraints';
                  watch = WL.fix var t.watch
                }
         | C.Meh ->
            run { fixed = t.fixed;
                  constraints = constraints';
                  watch = t.watch
                }
       end
    | Sums.F(var1,var2) ->
       run {t with watch = WL.addconstraint c var1 var2 t.watch}

  and run t = 
    let res, watch' = WL.next t.fixed t.watch in
    let t' = { t with watch = watch' } in
    match res with
    | Some c -> treat c t'
    | None   -> C.extract_msg t.constraints, Sums.F t'

end
