open Top.Specs
open Interfaces
       
type sign

module Make(DS: GlobalDS) : API with type sign     = sign
                                 and type termdata = DS.Term.datatype
                                 and type value    = DS.Value.t
                                 and type cval     = DS.CValue.t
                                 and type assign   = DS.Assign.t

(* include Theory.Type *)
(*         with type ('t,'v,'a) api = (module API with type sign = sign *)
(*                                                 and type termdata = 't *)
(*                                                 and type value    = 'v *)
(*                                                 and type assign   = 'a) *)
                        
