open Kernel.Theories.Prop.APIplugin

module RestartStrategies (UASet: CollectExtra):
sig
  exception Restart of UASet.t

  class virtual strategy :
      bool ->
  object
    method virtual increment : unit -> unit
    method virtual next : int
    method virtual reset : unit -> unit
    method is_enabled : bool 
  end
  
  class constant :
    int ->
  object
    method increment : unit -> unit
    method next : int
    method reset : unit -> unit
    method is_enabled : bool 
  end
  
  class arithmetic :
    int * int ->
  object
    method increment : unit -> unit
    method next : int
    method reset : unit -> unit
    method is_enabled : bool 
  end
  
  class geometric :
    int * int * int ->
  object
    method increment : unit -> unit
    method next : int
    method reset : unit -> unit
    method is_enabled : bool 
  end
  
  class exponential :
    int * int ->
  object
    method increment : unit -> unit
    method next : int
    method reset : unit -> unit
    method is_enabled : bool 
  end
  
  class luby :
    int ->
  object
    method increment : unit -> unit
    method next : int
    method reset : unit -> unit
    method is_enabled : bool 
  end
  
  exception NotFound of string
  val getbyname : string -> int -> int -> strategy
end
