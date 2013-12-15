module RestartStrategies (UASet: Kernel.Interfaces.CollectImplem) = struct
  exception Restart of UASet.t

  class virtual strategy (enabled : bool) =
  object 
    method virtual next : int
    method virtual increment : unit -> unit
    method is_enabled = enabled
  end

  class none () = 
  object(self)
    inherit strategy (false) as super
      
    method next = failwith "no restarts"
    method increment () = failwith "no restarts"
  end

  class constant (threshold) =
  object(self)
    inherit strategy (true) as super
    
    method next = threshold
    method increment () = ()
  end

  class arithmetic (start, step)  = 
  object (self)
    inherit strategy (true) as super
      
    val mutable _next = start

    method next = _next
    method increment () = _next <- _next + step
  end

  class geometric (start, first_step, multiplier)  = 
  object (self)
    inherit strategy (true) as super
      
    val mutable _next = start
    val mutable _step = first_step

    method next = _next
    method increment () = 
      _next <- _next + _step;
      _step <- _step * multiplier
  end

  class exponential (start, multiplier)  = 
  object (self)
    inherit strategy (true) as super
      
    val mutable _next = start

    method next = _next
    method increment () = _next <- _next * multiplier
  end

  class luby (base)  = 
  object (self)
    inherit strategy (true) as super
      
    val mutable pow = 1
    val mutable steps = [1]
    val mutable luby = [1]
    val mutable _next = base

    method private update_steps () =
      if steps = [] then (
        pow <- pow * 2;
        steps <- luby @ [pow];
        luby <- luby @ luby @ [pow];
      );

    method next = _next
      
    method increment () = 
      self#update_steps ();
      _next <- base * (List.hd steps);
      steps <- List.tl steps;
  end

  exception NotFound of string
      
  let getbyname name p1 p2 = 
    match name with
    | "constant"    -> (new constant (p1) :> strategy) 
    | "arithmetic"  -> (new arithmetic (p1, p2) :> strategy)
    | "geometric"   -> (new geometric (p1, p1, p2) :> strategy)
    | "exponential" -> (new exponential(p1, p2) :> strategy)
    | "luby"        -> (new luby(p1) :> strategy)
    | "none"        -> (new none() :> strategy)
    | s -> raise (NotFound ("Restart strategy " ^ s ^ " does not exist; see -help"))
end
