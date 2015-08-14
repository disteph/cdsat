(*******************************************)
(* This defines the first-rder constraints *)
(*******************************************)

open Format

open Top
open Basic
open Variables

open Unification

type t = { ar : World.t;
           mk : MKcorr.t;
           unifier : IU.t}

let topconstraint = { ar  = World.init;
                      mk  = MKcorr.empty;
                      unifier = IU.empty }

let print_in_fmt fmt sigma = 
  Format.fprintf fmt "{ar = {%a} || mk = {%a} || u = {%a}}"
    World.print_in_fmtEM sigma.ar
    MKcorr.print_in_fmt  sigma.mk
    IU.print_in_fmt      sigma.unifier
    
let proj sigma =
  let oldfv,newar = World.proj sigma.ar in
  match FreeVar.reveal oldfv with
  | FreeVar.Meta mv ->
    { ar      = newar;
      mk      = MKcorr.remove sigma.mk mv;
      unifier = sigma.unifier }
  | FreeVar.Eigen _ -> 
    { ar      = newar;
      mk      = sigma.mk;
      unifier = sigma.unifier }
      
let rec lift newar sigma =
  if World.equal sigma.ar newar then sigma
  else let oldfv,par = World.proj newar in
       let sigma = lift par sigma in
       match FreeVar.reveal oldfv with
       | FreeVar.Meta mv ->
         let so = Meta.get_sort mv in
         let newkey,newu = IU.new_key so sigma.unifier in
         { ar      = newar;
           mk      = MKcorr.add sigma.mk mv newkey;
           unifier = newu }
       | FreeVar.Eigen _ -> 
         { ar      = newar;
           mk      = sigma.mk;
           unifier = sigma.unifier }

let unif_aux b sigma1 sigma2 l1 l2 =
  match Unification.unif b sigma2.ar sigma1.mk sigma2.mk sigma1.unifier sigma2.unifier (Unification.combine [] (l1,l2)) with
  | None        -> None
  | Some(u1,u2) -> Some({ar = sigma1.ar; mk = sigma1.mk; unifier = u1},{ar = sigma2.ar; mk = sigma2.mk; unifier = u2})


let meet_aux b sigma1 sigma2 = 
  if not(World.prefix sigma1.ar sigma2.ar)
  then None
  else
    let sigma1 = lift sigma2.ar sigma1 in
    let l1 = 
      MKcorr.fold
        (fun _ key l -> (IU.key2val key)::l)
        sigma1.mk
        []
    in
    let l2 = 
      MKcorr.fold
        (fun _ key l -> (IU.key2val key)::l)
        sigma2.mk
        []
    in
    match unif_aux b sigma1 sigma2 l1 l2 with
    | None -> None
    | Some(res,_) -> Some res

let meet = meet_aux true

let unif sigma t1 t2 = 
  let internalise = IU.internalise (MKcorr.get_key sigma.mk) in
  match unif_aux true sigma sigma [internalise t1] [internalise t2] with
  | None                -> None
  | Some(sigma1,sigma2) -> meet sigma1 sigma2

let compare sigma1 sigma2 =
  match meet_aux false sigma1 sigma2 with 
  | Some _ -> 0
  | None   -> 1
