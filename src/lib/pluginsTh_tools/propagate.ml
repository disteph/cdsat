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

  val constreat :
    Constraint.t -> (Var.t*Var.t, constraints->(constraints*result)) Sums.sum

  val extract_msg: constraints -> (sign,tset,thStraight) thsays option * constraints

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
    match C.constreat c with
    | Sums.A(var1,var2) ->
       run {t with watch = WL.addconstraint c var1 var2 t.watch}
    | Sums.F affect ->
       let constraints', res = affect t.constraints in
       begin
         match res with
         | C.UNSAT msgConflict -> 
            let msgProp,_ = C.extract_msg constraints' in
            msgProp, Sums.A msgConflict
         | C.Propagate(fixed',var) ->
            run { fixed = fixed';
                  constraints = constraints';
                  watch = WL.fix var t.watch
                }
         | C.Meh ->
            run { t with constraints = constraints' }
       end

  and run t = 
    let res, watch' = WL.next t.fixed t.watch in
    match res with
    | Some c -> treat c { t with watch = watch' }
    | None   ->
       let msg,constraints' = C.extract_msg t.constraints in
       msg, Sums.F { t with constraints = constraints'; watch = watch' }

end
