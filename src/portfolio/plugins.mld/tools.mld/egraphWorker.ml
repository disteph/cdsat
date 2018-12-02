open Async
open Lib

open General
open Patricia
open Patricia_tools

open Kernel
open Top.Messages
open Theories.Theory
open Theories.Eq.MyTheory

open PluginsTh
open PluginsTh.Tools
    
let handler = Some Handlers.Eq


module Make
    (WB: WhiteBoardExt.S)
    (Kern: Theories.Eq.MyTheory.API
     with type sign := Theories.Eq.MyTheory.sign) = struct

  open WhiteBoardExt
  open WB
  open PluginsTh.Eq.Pl1

  module Arg = struct
    include Constraint
    include TypesFromHConsed(Constraint)
    include EmptyInfo
    type values = regular msg2th Pipe.Writer.t
  end

  module Clients = Map.MakeNH(Arg)

  let rec loop_read state clients ports = 
    let aux msg =
      Print.print ["egraph",1] (fun p-> p "The E-graph reads %a" pp_msg2th msg);
      match msg with
      | MsgStraight{ sassign; level; chrono }
        -> loop_write (add sassign ~level state) clients chrono ports
      | MsgSharing{ tset; chrono }
        -> loop_write (share tset state) clients chrono ports
      | MsgPropose{ chrono }
        ->
        Print.print ["egraph",1] (fun p-> p "E-graph: I have nothing to propose");
        Deferred.all_unit
          [ Lib.write ports.writer (Msg{ handler; answer = Try []; chrono }) ;
            loop_read state clients ports ]
      | MsgSpawn newports
        -> Deferred.all_unit
             [loop_read state clients ports ;
              loop_read state clients newports ]
      | KillYourself _ -> return()
      | WatchThis { reply_to; constr }
        ->
        Print.print ["egraph",1] (fun p-> p "E-graph: Watching stuff in %a" Constraint.pp constr);
        let aux = function None -> reply_to | Some _ -> failwith "Constraint is already registered" in
        let clients = Clients.add constr aux clients in
        let state   = watchthis constr state in
        loop_read state clients ports
      | TheoryAsk{ reply_to; node }
        -> let ans, state = ask node state in
        match ans with
        | None -> loop_read state clients ports
        | Some(normal_form, values, forbidden) ->
          Deferred.all_unit
            [ Lib.write reply_to (Infos{ node; normal_form; values; forbidden });
              loop_read state clients ports ]
    in
    Lib.read
      ~onkill:(fun ()->return(Print.print ["egraph",2] (fun p-> p "E-graph dies")))
      ports.reader aux

  and loop_write state clients chrono ports =

    let msg_make msg = Msg{ handler; answer = Say(WB.sign_Eq msg); chrono } in

    let output, state = speak state in
    Print.print ["egraph",1] (fun p-> p "E-graph looks at its output_msg");

    match output with
    | Nothing ->
      let msg = Msg{ handler; answer = Ack; chrono } in
      Deferred.all_unit
      [ Lib.write ports.writer msg ;
        loop_read state clients ports ]

    | Quid t -> 
      let msg = Msg{ handler; answer = Quid t; chrono } in
      Deferred.all_unit
        [ Lib.write ports.writer msg ;
          loop_read state clients ports ]
    
    | Propa(msg,_) -> 
      Print.print ["egraph",1] (fun p-> p "E-graph: Message %a" pp_message msg);
      Deferred.all_unit
        [ Lib.write ports.writer (msg_make msg) ;
          loop_read state clients ports ]

    | Sat msg -> 
      Print.print ["egraph",1] (fun p-> p "E-graph: Message %a" pp_message msg);
      Deferred.all_unit
        [ Lib.write ports.writer (msg_make msg) ;
          loop_read state clients ports ]

    | Detect c ->
      let port = Clients.find c clients in
      (* let clients = Clients.remove c clients in *)
      Deferred.all_unit
        [ Lib.write port (WatchFailed c) ;
          loop_read state clients ports ]

  module EGraph = Make(Kern)
  let make = loop_read EGraph.init Clients.empty

end
