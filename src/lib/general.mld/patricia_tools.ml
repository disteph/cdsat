open Sums
open Patricia_sig

module EmptyInfo = struct
  type infos = unit
  let info_build = {
      empty_info  = ();
      leaf_info   = (fun _ _ ->());
      branch_info = (fun _ _-> ())
    }
end
                       
module CardInfo = struct
  type infos = int
  let info_build = {
      empty_info  = 0;
      leaf_info   = (fun x _ -> 1);
      branch_info = (fun x1 x2 -> x1 + x2)
    }
end

module MaxInfo(K: sig type t [@@deriving ord] end) = struct
  type infos = K.t option
  let info_build = {
      empty_info  = None;
      leaf_info   = (fun x _ -> Some x);
      branch_info = (fun x1 x2 ->
        match x1,x2 with
        | None,_ -> failwith "Bad1"
        | _,None -> failwith "Bad2"
        | Some v1,Some v2-> if K.compare v1 v2<0 then x1 else x2
      )
    }
end


module type FromHConsed = sig
  type t
  val id : t->int
end

module TypesFromHConsed(S:FromHConsed) = struct
  type common  = int
  let tag      = S.id
  let compare  = Compare.id2compare tag

  type branching = int
  let bcompare   = Pervasives.compare
  let check k m  = (k land m) = 0

  let lowest_bit x        = x land (-x)
  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
  let mask p m            = p land (m-1)

  let match_prefix q p m  = mask q m = mask p m

  let disagree p0 p1      = let m = branching_bit p0 p1 in (mask p0 m,m,check p0 m)

  let pequals i1 i2 = (i1=i2)
end

module type FromCollect = sig
  type keys
  type t [@@deriving ord]
  val tag: keys->t
  type e [@@deriving ord]
  val mem  : e->t->bool
  val inter: t->t->t
  val first_diff : t->t->(e option*bool)
end

module TypesFromCollect(S: FromCollect) = struct

  type common       = S.t
  let tag           = S.tag
  let compare t1 t2 = S.compare(tag t1)(tag t2)
    
  type branching = S.e
  let bcompare   = S.compare_e
  let check p m  = S.mem m p

  let match_prefix q p m = (S.compare q p=0) ||
    match S.first_diff q p with
      | (Some x,_)  when S.compare_e x m<0 -> false
      | _ -> true

  let disagree p0 p1 = match S.first_diff p0 p1 with
    | (Some b,c) -> (S.inter p0 p1,b,c)
    | (None,_)   -> failwith "disagree called with two arguments that are equal"

  let pequals p1 p2 = (S.compare p1 p2=0)
end

module LexProduct
  (I1:sig
     include Key
     val pequals: common Equal.t
   end)
  (I2:Key with type t=I1.t) = struct

  let compare = I1.compare
  type common = I1.common*I2.common

  let tag s   = (I1.tag s,I2.tag s)
    
  type branching = (I1.branching,I2.branching) sum
  let bcompare b1 b2 = match b1,b2 with
    | Case1(a),Case1(a') -> I1.bcompare a a'
    | Case2(a),Case2(a') -> I2.bcompare a a'
    | Case1(a),Case2(a') -> -1 
    | Case2(a),Case1(a') -> 1

  let check (p1,p2) = function
    | Case1(a1)-> I1.check p1 a1
    | Case2(a2)-> I2.check p2 a2

  let match_prefix (q1,q2) (p1,p2) = function
    | Case1(a)-> I1.match_prefix q1 p1 a
    | Case2(a)-> (I1.pequals q1 p1) && I2.match_prefix q2 p2 a

  let disagree (p1,p2) (p1',p2') = 
    if I1.pequals p1 p1'
    then let (p2'',d,c) = I2.disagree p2 p2' in ((p1,p2''),Case2(d),c)
    else let (p1'',d,c) = I1.disagree p1 p1' in ((p1'',p2),Case1(d),c)

  let sub sub1 sub2 alm (k1,k2) (p1,p2) =
    let lift1 = function Almost a->Almost(Case1 a) | Yes _ -> Yes() | No->No in
    let lift2 = function Almost a->Almost(Case2 a) | Yes _ -> Yes() | No->No in
    let aux b = match sub1 alm k1 p1 None with
      | Yes _   -> lift2(sub2 alm k2 p2 b)
      | Almost f-> begin match sub2 false k2 p2 b with
	  | Yes _ -> Almost (Case1 f)
	  | _     -> No
	end
      | No       -> No
    in function
      | Some(Case1(a)) -> lift1(sub1 alm k1 p1 (Some a))
      | Some(Case2(a)) -> aux (Some a)
      | None       -> aux None

  let pequals pequals2 (p1,p2) (p1',p2')=I1.pequals p1 p1' && pequals2 p2 p2'
end


module Lift(I:sig include Key
		  type newkeys 
		  val project :newkeys-> t option
	    end) = struct

  type tmp    = I.t option [@@deriving ord] 
  let compare a b = compare_tmp (I.project a) (I.project b)
  type common = I.common option

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
    | Some mm -> begin match q, p with
	| Some qq,Some pp -> I.match_prefix qq pp mm
	| None,None       -> true
	| _               -> false
      end
	    
  let disagree p p' = match p, p' with
    | Some pp,Some pp' -> let (com,d,b) = I.disagree pp pp' in (Some com,Some d,b)
    | None , Some _    -> (None,None,true)
    | Some _, None     -> (None,None,false)
    | None, None       -> failwith "Disagree called on two equal arguments!"

  let sub sub1 alm a a' limit =
    let lift = function Almost b-> Almost (Some b) | Yes _ -> Yes() | No->No in
      match a,a',limit with
	| Some aa,Some aa',None         -> lift(sub1 alm aa aa' None)
	| Some aa,Some aa',Some(Some c) -> lift(sub1 alm aa aa' (Some c))
	| Some aa,Some aa',Some(None)   -> Yes()
	| None,None,_                   -> Yes()
	| _                             -> No

  let pequals pequals1 p1 p2 =match p1,p2 with
    | None,None     -> true
    | Some a, Some b-> pequals1 a b
    | _             -> false

end
