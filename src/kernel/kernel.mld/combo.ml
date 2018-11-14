(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Terms
open Sassigns
open Messages

open Theories
open Theory
open Register

include Combo_sig

let make dsKeys theories (module Proof:Proof) : (module API) =

  let theoriesWeq = HandlersMap.add Handlers.Eq () theories in

  let dsKeys = 
    let aux hdl () sofar =
      match hdl with
      | Handlers.Handler tag -> List.rev_append (Tags.dsKeys tag) sofar
      | Handlers.Eq -> List.rev_append Eq.MyTheory.ds sofar
    in
    HandlersMap.fold aux theoriesWeq dsKeys
  in

  let (module W) as writable = build dsKeys in
  let (module EGraph) = Eq.MyTheory.make writable in

  (module struct

    module WB = struct

      module W = W                 

      type 'a proof = 'a Proof.t
        
      type 'a t = WB of unit HandlersMap.t * (unit,'a) message * 'a proof
      type any = Any : _ t -> any [@@unboxed]

      let pp fmt (type a)(type proof) (WB(hdls,msg,_) : a t) =
        match msg with
        | Propa _ -> Format.fprintf fmt "%a propagate(s) %a"
                       HandlersMap.pp hdls
                       pp_message msg
        | Sat assign -> Format.fprintf fmt "%a declares %a"
                          HandlersMap.pp (HandlersMap.diff theoriesWeq hdls)
                          pp_message msg

      let pp_any fmt (Any msg) = pp fmt msg
      let show_any = Format.stringOf pp_any
      
      let sign hdl (type a) : (_,a) message -> a t =
        let hdl = Handlers.Handler hdl in
        function
        | Propa(assign,o) ->
          if HandlersMap.mem hdl theories
          then WB(HandlersMap.singleton hdl (),
                  Messages.propa () assign o,
                  Proof.from_th)
          else failwith "Using a theory that is not allowed"
        | Sat{ assign; sharing; myvars } ->
          WB(HandlersMap.remove hdl theoriesWeq,
             Messages.sat () assign ~sharing ~myvars,
             Proof.sat)

      let sign_Eq (type a) : (_,a) message -> a t = function
        | Propa(assign,o) ->
          WB(HandlersMap.singleton Handlers.Eq (),
             Messages.propa () assign o,
             Proof.from_th)
        | Sat{ assign; sharing; myvars } ->
          WB(HandlersMap.remove Handlers.Eq theoriesWeq,
             Messages.sat () assign ~sharing ~myvars,
             Proof.sat)


      let resolve
          (WB(hdls1,Propa(oldset,Straight bassign),proof1))
          (WB(hdls2,Propa(thset,o),proof2))
        =
        let res = Assign.remove (SAssign.build bassign) thset in
        WB(HandlersMap.union hdls1 hdls2,
           Messages.propa () (Assign.union res oldset) o,
           Proof.resolve proof1 proof2)

      let unsat (WB(hdls,Propa(thset,Straight(t,b)),proof)) =
        WB(hdls,
           unsat () (Assign.add (SAssign.build(negation(t, b))) thset),
           Proof.unsat)

      let curryfy
          ?(assign = Assign.empty)
          ?flip
          (WB(hdls,Propa(thset,Unsat),proof)) =
        let thset,(t,Values.Boolean b) =
          match flip with
          | None -> thset, (W.bC Symbols.False [], Values.Boolean false)
          | Some bassign -> Assign.remove (SAssign.build bassign) thset, bassign
        in
        let aux a ((thset,clause,b) as sofar) =
          let SAssign(term,value) = SAssign.reveal a in
          match value with
          | Values.Boolean b' when Assign.mem a thset && [%eq:bool] b b'
            -> Assign.remove a thset,
               W.bC Symbols.Imp (if b then [term;clause] else [clause;term]),
               true                 
          | Values.Boolean false when Assign.mem a thset
            -> Assign.remove a thset,
               W.bC (if b then Symbols.Or else Symbols.And) [term;clause],
               b
          | _ -> sofar
        in
        let thset,rhs,b = Assign.fold aux assign (thset, t, not b) in
        WB(hdls,straight () thset (rhs,Values.Boolean b),
           Proof.curryfy ~assign ?flip proof)

      type sat_tmp = { assign  : Assign.t;
                       sharing : TSet.t;
                       left    : unit HandlersMap.t;
                       varlist : TSet.t Lazy.t list }

      type sat_ans =
        | GoOn of sat_tmp
        | Share of TSet.t
        | Done of Assign.t * TSet.t
        | NoModelMatch of Assign.t
        | NoSharingMatch of TSet.t


      let sat_init assign ~sharing = { assign; sharing; left = theoriesWeq; varlist = [] }


      let isnt_var t = match Term.reveal t with
        | Terms.V _ -> false
        | _ -> true

      (* disjoint_check assign sharing myvars list
         list is a list of (lazy) sets of terms.
         Checks whether those sets are pairwise disjoint,
         ignoring elements that are already in sharing. *)
      let rec disjoint_check assign sharing myvars = function
        | [] -> Done(assign, sharing)
        | myvars'::tail ->
          let newvars   = TSet.diff (Lazy.force myvars') sharing in
          let novars    = TSet.filter isnt_var newvars in
          let newshared = TSet.union novars (TSet.inter newvars myvars) in
          if TSet.is_empty newshared
          then
            let myvars = TSet.union newvars myvars in
            disjoint_check assign sharing myvars tail
          else Share newshared

      let sat
          (WB(left',Sat{assign=assign'; sharing=sharing'; myvars=myvars'},_))
          { left; assign; sharing; varlist } =
        if Assign.equal assign assign'
        then
          if TSet.equal sharing sharing'
          then
            let left = HandlersMap.inter left left' in
            if HandlersMap.is_empty left
            then disjoint_check assign sharing TSet.empty varlist
            else GoOn { assign; sharing; left; varlist = myvars'::varlist }
          else NoSharingMatch sharing
        else NoModelMatch assign

    end

    let th_modules =
      let aux hdl () sofar =
        match hdl with
        | Handlers.Handler tag -> (Modules.make writable tag)::sofar
        | Handlers.Eq -> failwith "Eq should not be there"
      in
      HandlersMap.fold aux theories []

    module EGraph = EGraph

    let parse parser input =
      let ths,termB,_ = Parsers.Register.parse parser input in
      begin match ths with
        | Some l when not(HandlersMap.equal (fun ()()->true) theories (Register.get l)) ->
          print_endline(Format.toString(fun p ->
              p
                "Warning: using theories %a but just parsed %a"
                HandlersMap.pp theories
                HandlersMap.pp (Register.get l)))
        | _ -> ()
      end;
      List.map (W.lift []) termB

  end)
