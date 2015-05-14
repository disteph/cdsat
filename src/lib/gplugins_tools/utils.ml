(*******************************************************************)
(* This module contains extensions of the kernel's FrontEnd module:
   some useful abbreviations/functions *)
(*******************************************************************)


open Kernel.Interfaces_basic
open Kernel.Interfaces_plugin


module PHCons_ext(A:PHCons) = struct
  include A
  let equal a1 a2 = (id a1 = id a2)
  let hash = id
end
  
module FEext(FE:FrontEndType)
  = struct
    open FE
    (* A function to systematically accept answers *)
    let accept _ = ()
    let fNone () = None
    let isProvable = function Provable _ -> true | _ -> false
    let isNotProvable = function NotProvable _ -> true | _ -> false
    let model seq = let (a,_)=Seq.forPlugin seq in a
  end
