open Patricia
open Collection

(* Automatic construction of a UT:UserTypes from a HConsed type *)

module type FromHConsed = sig
  type t
  val id : t->int
end

module TypesFromHConsed(S:FromHConsed) = 
  (struct
     type keys = S.t

     type common  = int
     let ccompare = Pervasives.compare
     let tag      = S.id

     type branching = int
     let bcompare   = Pervasives.compare
     let check k m  = (k land m) == 0

     let lowest_bit x        = x land (-x)
     let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
     let mask p m            = p land (m-1)

     let match_prefix q p m  = (mask q m) == p
     let disagree p0 p1      = 
       let m = branching_bit p0 p1 in (mask p0 m,m,check p0 m)

   end:Intern with type keys =S.t and type common  = int and type branching = int)


(* Automatic construction of a UT:UserTypes from a collection *)

module type FromCollect = sig
  type keys
  type t
  val tag: keys->t

  type e
  val is_in: e->t->bool
  val inter: t->t->t
    (* Comparison of collections *)
  val compare    : t->t->int
    (* Comparison of elements *)
  val compareE   : e->e->int
    (* Computes the smallest element that is in one set 
       and not in the other, according to order compareE *)
  val first_diff : t->t->(e option*bool)
end

module TypesFromCollect(S: FromCollect) = 
  (struct

     type keys    = S.keys

     type common  = S.t
     let ccompare = S.compare 
     let tag      = S.tag
       
     type branching = S.e
     let bcompare   = S.compareE
     let check p m  = S.is_in m p

     let match_prefix q p m = match S.first_diff q p with
       | (Some x,_)  when S.compareE x m<0 -> false
       | _ -> true

     let disagree p0 p1 = match S.first_diff p0 p1 with
       | (None,_)   -> failwith("disagree called with two arguments that are equal")
       | (Some b,c) -> (S.inter p0 p1,b,c)
	   
   end:Intern with type keys=S.keys and type common  = S.t and type branching = S.e)



(* Automatic construction of a I:Intern for the product of two sets,
given I1:Intern and I2:Intern *)

module LexProduct(I1:Intern)(I2:Intern with type keys=I1.keys) = 
  (struct

     type keys = I1.keys

     type common  = I1.common*I2.common
     let ccompare (a,b)(a',b')=
       let c = I1.ccompare a a' in
	 if c=0 then I2.ccompare b b' else c

     let tag s    = (I1.tag s,I2.tag s)
       
     type branching = (I1.branching,I2.branching) sum
     let bcompare b1 b2 = match b1,b2 with
       | A(a),A(a') -> I1.bcompare a a'
       | F(a),F(a') -> I2.bcompare a a'
       | A(a),F(a') -> -1 
       | F(a),A(a') -> 1

     let check (p1,p2) = function
       | A(a1)-> I1.check p1 a1
       | F(a2)-> I2.check p2 a2

     let match_prefix (q1,q2) (p1,p2) = function
       | A(a)-> I1.match_prefix q1 p1 a
       | F(a)-> (I1.ccompare q1 p1 ==0) && I2.match_prefix q2 p2 a

     let disagree (p1,p2) (p1',p2') = 
       if I1.ccompare p1 p1'==0
       then match I2.disagree p2 p2' with (p2'',d,c) -> ((p1,p2''),F(d),c)
       else match I1.disagree p1 p1' with (p1'',d,c) -> ((p1'',p2),A(d),c)

   end:Intern with type keys=I1.keys
	      and type common  = I1.common*I2.common
	      and type branching = (I1.branching,I2.branching) sum)

(* Automatic construction of an Intern for a set extended with a top element,
   given the I:Intern for the original set *)

module Lift(I:sig include Intern
		  type newkeys 
		  val project :newkeys->keys option
	    end) = 
  (struct

     type keys = I.newkeys

     type common  = I.common option
     let ccompare  a a'= match a,a' with
       | Some aa,Some aa' -> I.ccompare aa aa'
       | None, Some _     -> 1
       | Some _,None      -> -1
       | None, None       -> 0

     let tag s    = match I.project s with
       | None   -> None
       | Some k -> Some(I.tag k)
       
     type branching = I.branching option
     let bcompare a a'  =  match a,a' with
       | Some aa,Some aa' -> I.bcompare aa aa'
       | None, Some _     -> -1
       | Some _,None      -> 1
       | None, None       -> 0

     let check p m = match p,m with
       | Some pp,Some mm -> I.check pp mm
       | None,None       -> true
       | _ -> false

     let match_prefix q p = function
       | None    -> true
       | Some mm -> match q, p with
	   | Some qq,Some pp -> I.match_prefix qq pp mm
	   | None,None       -> true
	   | _               -> false
 
     let disagree p p' = match p, p' with
       | Some pp,Some pp' -> (match I.disagree pp pp' with (com,d,b) -> (Some com,Some d,b))
       | None , Some _    -> (None,None,true)
       | Some _, None     -> (None,None,false)
       | None, None       -> failwith("Disagree called on two equal arguments!")

   end:Intern with type keys=I.newkeys
	      and type common  =  I.common option
	      and type branching = I.branching option)
