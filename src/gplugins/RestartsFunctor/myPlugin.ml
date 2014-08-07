open Kernel.Interfaces_I

module GenPluginWRestart(MyGenPlugin:Plugins.GenType)(IAtom: IAtomType)
  :(Plugins.Type with type literals = IAtom.Atom.t
                 and type iliterals = IAtom.t) = struct
  
  type iliterals = IAtom.t
  type literals  = IAtom.Atom.t
  type delsubsts = IAtom.DSubst.t

  module UASet = MyGenPlugin.UASet
  module UF    = MyGenPlugin.UF
  module UFSet = MyGenPlugin.UFSet

  module Strategy(FE:FrontEndType with type Form.lit    = literals
				  and  type Form.datatype = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t
				  and  type ilit        = iliterals
				  and  type dsubsts     = delsubsts) = struct

    module Restarts = Common.RestartStrategies.RestartStrategies(UASet)
    let restart_strategy = Restarts.getbyname !Flags.restarts_strategy !Flags.restarts_p1 !Flags.restarts_p2

    module MyStrategy = MyPlugin.Strategy(FE)
    type data = MyStrategy.data
    let initial_data = MyStrategy.initial_data

    let reset () = restart_strategy#reset() 

    (* solve_restart handles restarts, launching the solve function of
       the input plugin once, then re-launching it every time a Restart
       exception is raised. *) 

    let rec solve_restart input = 
      try
        MyStrategy.solve input 
      with Restarts.Restart lits -> reset();
          (* Queue.push lits !priority_lits; *)
        restart_strategy#increment ();
        Dump.Kernel.toPlugin ();
        Dump.Kernel.reset_branches ();
          (* if !Flags.debug>0 then *)
        print_endline (Printf.sprintf "Restarting (next restart: %d)" restart_strategy#next);
        solve_restart input
          
    let solve input = let a = solve_restart input in reset();a
      
  end
    
end
