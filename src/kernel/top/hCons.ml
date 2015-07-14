(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module EmptyData = struct 
  type t = unit
  let build _ _ = ()
end

module type PolyS = sig
  type ('t,'a) initial
  type ('a,'data) generic
  type ('a,'data) g_revealed = (('a,'data) generic,'a) initial
  val reveal : ('a,'data) generic -> ('a,'data) g_revealed
  val id     : ('a,'data) generic -> int
  val data   : ('a,'data) generic -> 'data
  val compare: ('a,'data) generic -> ('a,'data) generic -> int
end

module MakePoly
  (M: sig 
    type ('t,'a) t
    val equal: ('t->'t->bool) -> ('a->'a->bool) -> (('t,'a) t -> ('t,'a) t -> bool) 
    val hash: ('t->int) -> ('a->int) -> (('t,'a) t -> int)    
  end) = struct

    type ('t,'a) initial = ('t,'a) M.t
    type ('a,'data) generic = {reveal: ('a,'data) g_revealed; id:int; data:'data option}
    and  ('a,'data) g_revealed = (('a,'data) generic,'a) M.t

    let reveal f = f.reveal
    let id f     = f.id
    let data f   = 
      match f.data with
      | Some d -> d
      | None -> failwith "HConsed value contains None!"

    let compare a1 a2 = Pervasives.compare a1.id a2.id

    module InitData
      (Par: Hashtbl.HashedType)
      (Data: sig
        type t
        val build : int -> (Par.t,t) g_revealed -> t
      end)
      = struct

        type t = (Par.t,Data.t) generic
        type revealed = (Par.t,Data.t) g_revealed

        module Arg = struct
          type t = (Par.t,Data.t) generic
          let equal a b = M.equal (fun x y -> x == y) Par.equal a.reveal b.reveal
          let hash a    = M.hash Hashtbl.hash Par.hash a.reveal
        end

        module H = Weak.Make(Arg)
        (* module H = Hashtbl.Make(Arg) *)

        let table   = H.create 5003
        let unique  = ref 0
        let build a =
          let f = {reveal =  a; id = !unique; data = None} in
          try H.find table f
          with Not_found -> 
            let newf = {reveal =  a; id = !unique; data = Some(Data.build !unique a)} in
            incr unique;
            (* H.add table newf newf; *)
            H.add table newf;
            newf

        let clear() = unique := 0; H.clear table

      end

    module Init(Par: Hashtbl.HashedType) = InitData(Par)(EmptyData)

  end

module type S = sig
  type 't initial
  type 'data generic
  type 'data g_revealed = 'data generic initial
  val reveal : 'data generic -> 'data g_revealed
  val id     : 'data generic -> int
  val data   : 'data generic -> 'data
  val compare: 'data generic -> 'data generic -> int
end

module type BuiltS = sig
  type 't initial
  type t
  type data
  val reveal : t -> t initial
  val id     : t -> int
  val data   : t -> data
  val compare: t -> t -> int
  val built  : t initial -> t
  val clear  : unit -> unit
end

module Make
  (M: sig 
    type 't t
    val equal: ('t->'t->bool) -> 't t -> 't t -> bool
    val hash: ('t->int) -> 't t -> int
  end) = struct

    module N = struct
      type ('t,'a) t = 't M.t
      let equal eq _ = M.equal eq
      let hash h _ = M.hash h
    end
    module TMP = MakePoly(N)

    type 't initial = 't M.t
    type 'data generic  = (unit,'data) TMP.generic
    type 'data g_revealed = (unit,'data) TMP.g_revealed

    let reveal f = f.TMP.reveal
    let id f     = f.TMP.id
    let data f   = TMP.data f
    let compare a1 a2 = Pervasives.compare a1.TMP.id a2.TMP.id

    module InitData
      (Data: sig
        type t
        val build : int -> t g_revealed -> t
      end)
      = TMP.InitData(struct
        type t = unit
        let equal _ _ = true
        let hash _ = 1
      end)
      (Data)
      
    module Init = InitData(EmptyData)

  end
