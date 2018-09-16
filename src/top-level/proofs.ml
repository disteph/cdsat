open Kernel
open Top.Sassigns
open Top.Messages


module NoProof = struct

  type 'a t = unit

  let from_th = ()
  let unsat   = ()
  let sat     = ()

  (* Resolves H⊢L and H',L⊢A into H∪H'⊢A *)
  let resolve  _ _ = ()

  (* Turns H,H',L⊢⊥ into (essentially) H⊢H'⇒~L
     First argument is H' (empty if not given), second is L (⊤↦true if not given) *)
  let curryfy ?assign ?flip _ = ()

end

module TreeProof = struct

  type 'a t =
    | From_th : _ propa t
    | Unsat   : unsat t
    | Sat     : sat t
    | Resolve : (straight t * 'a propa t) -> 'a propa t
    | Curryfy : (Assign.t option * BAssign.t option * unsat t) -> straight t
    
  let from_th = From_th
  let unsat   = Unsat
  let sat     = Sat

  (* Resolves H⊢L and H',L⊢A into H∪H'⊢A *)
  let resolve  p1 p2 = Resolve(p1,p2)

  (* Turns H,H',L⊢⊥ into (essentially) H⊢H'⇒~L
     First argument is H' (empty if not given), second is L (⊤↦true if not given) *)
  let curryfy ?assign ?flip p = Curryfy(assign,flip,p)

end
