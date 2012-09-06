open Patricia
open Sums

module type FromHConsed = sig type t val id : t -> int end

module TypesFromHConsed(S : FromHConsed) 
  :Intern with type keys =S.t and type common  = int and type branching = int
  
module type FromCollect = sig
  type keys
  type t
  val tag : keys -> t
  type e
  val is_in : e -> t -> bool
  val inter : t -> t -> t
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val first_diff : t -> t -> e option * bool
end

module TypesFromCollect(S : FromCollect)
  :sig
    include Intern with type keys     = S.keys
		   and type common    = S.t
		   and type branching = S.e
    val pequals:common->common->bool
  end
  

module LexProduct  
  (I1:sig include Intern val pequals:common->common->bool end)
  (I2:Intern with type keys=I1.keys)
  :sig
    include Intern with type keys     = I1.keys
		   and type common    = I1.common*I2.common
		   and type branching = (I1.branching,I2.branching) sum
    val sub: 
      (bool-> I1.common -> I1.common -> I1.branching option -> (unit, I1.branching) almost) ->
      (bool-> I2.common -> I2.common -> I2.branching option -> (unit, I2.branching) almost) ->
      bool-> common -> common -> branching option -> (unit, branching) almost
    val pequals:(I2.common->I2.common->bool)->common->common->bool
  end


module Lift(I:sig include Intern
		  type newkeys 
		  val project :newkeys->keys option
	    end)
  :sig
    include Intern with type keys     = I.newkeys
		   and type common    = I.common option
		   and type branching = I.branching option
    val sub: 
      (bool-> I.common -> I.common -> I.branching option -> (unit, I.branching) almost) ->
      bool-> common -> common -> branching option -> (unit, branching) almost
    val pequals:(I.common->I.common->bool)->common->common->bool
  end
