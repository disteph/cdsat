open General.Sums
open PluginsTh.PluginTh
open Kernel.Top.Messages
   
module type Fix = sig
  include Kernel.Export.API
  val solve : WB.DS.Assign.t
              -> (Kernel.Top.Messages.unsat WB.t, WB.sat_ans) General.Sums.sum
  val clear : unit -> unit
end
                
let fix : (module Fix) =
  let theories =
    let open Kernel.Theories.Register in
    HandlersMap.singleton (Handlers.Handler Tags.Bool) ()
  in
  let (module K) = Kernel.Combo.make theories in
  let module Input = struct
      include K

      open Kernel.Theories.Register
         
      module Plugin = PluginsTh.Register.Make(WB.DS)

      let add_plugin
            (Modules.Module(tag,_) as plugin)
            (plugins_sofar, clear_sofar)
        =
        let init,clear = Plugin.make plugin in
        init::plugins_sofar,
        (fun () -> clear_sofar (); clear ())

      let pluginsTh, clear =
        List.fold
          add_plugin
          K.th_modules
          ([],(fun () -> ()))
    end
  in

  let (module Pl)  = Plugins.Register.get "" in
  (module struct
     include K
     include Pl.Make(Input)
               end: Fix)

module type Syntax = sig
  
  type a = string

  module Formula : sig
    type t = private
           | Top
           | Btm
           | Atom of a
           | And of t * t
           | Or of t * t
           | Implies of t * t
                     
    val atom_t : a -> t
    val and_t : t -> t -> t
    val or_t : t -> t -> t
    val implies_t : t -> t -> t
    val top_t : unit -> t
    val btm_t : unit -> t
    val atom_equal : t -> t -> bool
  end

  module Sequent : sig
    type t
    val build_sequent : Formula.t list -> Formula.t -> t
    val left : t -> Formula.t list
    val right : t -> Formula.t
  end

end



module Convert(S:Syntax) = struct
  open Kernel

  type w_rhs = private WRHS
  type wo_rhs = private WORHS

  open S
  type sequent = Formula.t list * Formula.t list
  type proof =
    | Trivial : sequent -> proof
    | Unsat   : sequent * proof -> proof
    | Resolve : sequent * proof * proof -> proof
    | Curry   : sequent * proof -> proof

  module Make(WB:Kernel.Export.WhiteBoard) = struct
    open WB.DS

    let rec encode =
      let open Top.Symbols in
      function
      | S.Formula.Top -> Term.bC True []
      | S.Formula.Btm -> Term.bC False []
      | S.Formula.Atom a -> Term.bC (User(a,(Top.Sorts.Prop,[]))) []
      | S.Formula.And(t1,t2) ->
        let t1,t2 = encode t1, encode t2 in
        Term.bC And [t1;t2]
      | S.Formula.Or(t1,t2) ->
        let t1,t2 = encode t1, encode t2 in
        Term.bC Or [t1;t2]
      | S.Formula.Implies(t1,t2) ->
        let t1,t2 = encode t1, encode t2 in
        Term.bC Imp [t1;t2]

    exception WeirdTerm

    let rec decode term =
      let open Top in
      let open Symbols in
      match Terms.reveal term with
      | Terms.C(True,[]) -> S.Formula.top_t ()
      | Terms.C(False,[]) -> S.Formula.btm_t ()
      | Terms.C(User(a,_),[]) -> S.Formula.atom_t a
      | Terms.C(And,[t1;t2]) ->
        let t1,t2 = decode t1, decode t2 in
        S.Formula.and_t t1 t2
      | Terms.C(Or,[t1;t2]) ->
        let t1,t2 = decode t1, decode t2 in
        S.Formula.or_t t1 t2
      | Terms.C(Imp,[t1;t2]) ->
        let t1,t2 = decode t1, decode t2 in
        S.Formula.implies_t t1 t2
      | _ -> raise WeirdTerm

    let encode_s sequent =
      let left = S.Sequent.left sequent in
      let right = S.Sequent.right sequent in
      let add formula = Assign.add (Top.Sassigns.boolassign (encode formula)) in
      List.fold add left (Assign.singleton(Top.Sassigns.boolassign ~b:false (encode right)))

    exception NonBooleanAssignment

    let core assign =
      let aux sassign (left,right) =
        match sassign with
        | Top.Sassigns.SAssign(formula,Top.Values.Boolean true)
          -> (decode formula::left), right
        | Top.Sassigns.SAssign(formula,Top.Values.Boolean false)
          -> left, (decode formula::right)
        | _ -> raise NonBooleanAssignment
      in
      Assign.fold aux assign ([],[])

    let rec proof_wo : unsat WB.t -> proof
      = function ans ->
        let WB(_,Propa(assign,Unsat),p) = ans in
        match p with
        | WB.Blob              -> Trivial(core assign)
        | WB.UnsatProof s      -> Unsat(core assign,proof_w s)
        | WB.ResolveProof(s,c) -> Resolve(core assign,proof_w s,proof_wo c)

    and proof_w : straight WB.t -> proof
      = function ans ->
        let WB(_,Propa(assign,Straight(formula,Top.Values.Boolean b)),p) = ans in
        let lhs,rhs = core assign in
        let seq = if b then lhs, ((decode formula)::rhs)
          else ((decode formula)::lhs), rhs
        in match p with
        | WB.Blob               -> Trivial seq
        | WB.ResolveProof(s,s') -> Resolve(core assign,proof_w s,proof_w s')
        | WB.CurryProof(_,c)    -> Curry(seq,proof_wo c)



    let model assign =
      let aux sassign valuation =
        match sassign with
        | Top.Sassigns.SAssign(term,Top.Values.Boolean b)
          -> begin match Top.Terms.reveal term with
              | Top.Terms.C(Top.Symbols.User(a,_),[]) -> (a,b)::valuation
              | _ -> valuation
            end
        | _ -> raise NonBooleanAssignment
      in
      Assign.fold aux assign []

  end
end

module Solve(S:Syntax) = struct

  include Convert(S)
    
  type answer = SAT of (S.a*bool) list | UNSAT of proof

  let prove problem0 =
    let (module Fix) = fix in
    let module Conv = Make(Fix.WB) in
    let open Fix in
    let problem = Conv.encode_s problem0 in
    match solve problem with
    | Case1(WB.WB(_,Propa(_,Unsat),_) as p) (* when WB.DS.Assign.subset assign problem *)
      -> UNSAT(Conv.proof_wo p)
    | Case2(WB.Done(assign,sharing)) (* when WB.DS.Assign.subset problem assign *)
      -> SAT(Conv.model assign)
    | _ -> failwith "Not answering problem"

end



