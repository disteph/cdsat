open Async
open Lib

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register

open Interfaces

module Make(WB: WhiteBoardExt)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign = Theories.Eq.MyTheory.sign
           and type termdata = WB.DS.Term.datatype
           and type value  = WB.DS.Value.t
           and type cval   = WB.DS.CValue.t
           and type assign = WB.DS.Assign.t) = struct

  open WB
  open DS
  open EGraph

  let rec flush_write writer unsat_msg = function
    | [] ->
       Lib.write writer unsat_msg
    | msg::l ->
       Lib.write writer msg
       >>= fun () ->
       flush_write writer unsat_msg l
         
  let rec flush reader writer unsat_msg l =
    let aux = function
      | MsgStraight _ -> flush reader writer unsat_msg l

      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2) -> 
         Deferred.all_unit
           [
             flush_write newwriter1 unsat_msg l;
             flush_write newwriter2 unsat_msg l;
             flush newreader1 newwriter1 unsat_msg l;
             flush newreader2 newwriter2 unsat_msg l
           ]
      | KillYourself _ -> return()
    in
    Lib.read reader aux


  let rec loop_read egraph from_pl to_pl = 
    let aux msg =
      Dump.print ["egraph",1] (fun p-> p "The E-graph reads %a" print2th_in_fmt msg);
      match msg with
      | MsgStraight(sassign,chrono)
        -> loop_write (egraph.add sassign) chrono from_pl to_pl
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2)
        -> Deferred.all_unit
             [loop_read egraph newreader1 newwriter1 ;
              loop_read egraph newreader2 newwriter2 ]
      | KillYourself(WB(_,Propa(assign,Unsat)),_,_) -> return()
    in
    Lib.read
      ~onkill:(fun ()->return(Dump.print ["egraph",2] (fun p-> p "E-graph dies")))
      from_pl aux

  and loop_write output chrono from_pl to_pl =

    let hhdl = Some Handlers.Eq in
    let msg_make msg = Msg(hhdl,Say(WB.stamp_Eq msg),chrono) in

    Dump.print ["egraph",1] (fun p-> p "E-graph looks at its output_msg");

    match output with
    | UNSAT(propas,conflict) -> 
       Dump.print ["egraph",1] (fun p-> p "E-graph: UNSAT discovered");
       
       let unsat_msg = msg_make conflict in
       let l = List.map msg_make propas in
       Deferred.all_unit
         [
           flush_write to_pl unsat_msg l ;
           flush from_pl to_pl unsat_msg l
         ]

    | SAT(msg,egraph) ->
       Dump.print ["egraph",1] (fun p-> p "E-graph: Message %a" Msg.pp msg);
       Deferred.all_unit
         [
           Lib.write to_pl (msg_make msg) ;
           loop_read egraph from_pl to_pl
         ]

  let make = loop_read EGraph.init

end
