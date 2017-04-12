open General
open Patricia
open Kernel.Top.Messages


module type Config = sig

  include TwoWatchedLits.Config

  val howmany: int
  type stop
  type msg

  val init_fixed : fixed

  type result =
    | UNSAT     of stop
    | Propagate of fixed * Var.t list

  val constreat  : Constraint.t -> fixed -> result

  val extract_msg: fixed -> (msg * fixed) option

end

module Make(C: Config) = struct

  module WL = TwoWatchedLits.Make(C)

  type t = {
    fixed : C.fixed;
    watch : WL.t
  }

  let init = {
    fixed = C.init_fixed;
    watch = WL.init
  }

  let rec analyse t = function
    | C.UNSAT stop -> Sums.Case1 stop
    | C.Propagate(fixed',varlist) ->
       run
         {
           fixed = fixed';
           watch = List.fold WL.fix varlist t.watch
         }
               
  and run t = 
    let res, watch' = WL.next t.fixed ~howmany:C.howmany t.watch in
    let t = { t with watch = watch' } in
    match res with
    | Some(c,_) -> analyse t (C.constreat c t.fixed)
    | None   -> Sums.Case2 t
                           
  let add_constraint c t =
    { t with watch = WL.addconstraintNflag c [] t.watch }

  let fix f t = analyse t (f t.fixed)
        
  let extract_msg t =
    match C.extract_msg t.fixed with
    | None -> None, t
    | Some(msg,fixed') -> Some msg,{ t with fixed = fixed' }


end
