open Patricia_interfaces
open Sums

(***************************************************)
(* Pre-defined infos types and build functions *)
       
(* Gives the standard info_build when info type is unit *)
module EmptyInfo : sig
  type infos
  val info_build : ('keys,'values,infos) info_build_type
end
                       
(* Info build function to record cardinal *)
module CardInfo  : sig
  type infos = int
  val info_build : ('keys,'values,infos) info_build_type
end

(* Info type to record maximum key *)
module MaxInfo(K: sig type t [@@deriving ord] end) : sig
  type infos = K.t option
  val info_build : (K.t,'values,infos) info_build_type
end

(***************************************************)
(* Automatic construction of a I:Intern from a HConsed type *)
                                                       
module type FromHConsed = sig
  type t 
  val id : t -> int 
end

module TypesFromHConsed(S : FromHConsed) 
  :sig include Intern with type keys      = S.t
	              and  type common    = int
	              and  type branching = int
       val pequals:common->common->bool
  end
  
(***************************************************)
(* Automatic construction of a I:Intern from a collection *)

module type FromCollect = sig
  type keys
  type t [@@deriving ord]
  val tag : keys -> t
  type e [@@deriving ord]
  val mem        : e -> t -> bool
  val inter      : t -> t -> t
    (* Computes the smallest element that is in one set 
       and not in the other, according to order compareE *)
  val first_diff : t -> t -> e option * bool
end

module TypesFromCollect(S : FromCollect)
  :sig
    include Intern with type keys     = S.keys
		   and type common    = S.t
		   and type branching = S.e
    val pequals:common->common->bool
  end
  
(***************************************************)
(* Automatic construction of a I:Intern for the product of two sets,
   given I1:Intern and I2:Intern *)

module LexProduct  
  (I1:sig
     include Intern 
     val pequals:common->common->bool 
   end)
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

(***************************************************)
(* Automatic construction of an Intern for a set extended with a top element,
   given the I:Intern for the original set *)

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
