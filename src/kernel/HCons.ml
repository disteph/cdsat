(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module EmptyData = struct 
  type t = unit
  let build _ = ()
end

module MakePoly
  (M: sig 
    type ('t,'a) t
    val equal: ('a->'a->bool) -> ('t->'t->bool) -> (('t,'a) t -> ('t,'a) t -> bool) 
    val hash: ('a->int) -> ('t->int) -> (('t,'a) t -> int)    
  end) = struct

    type ('a,'data) t = {reveal: ('a,'data) revealt; id:int; data:'data}
    and  ('a,'data) revealt = (('a,'data) t,'a) M.t

    let reveal f = f.reveal
    let id f     = f.id
    let data f   = f.data

    module InitData
      (Par: sig
        type t
        val equal : t -> t -> bool
        val hash : t -> int
      end)
      (Data: sig
        type t
        val build : (Par.t,t) revealt -> t
      end)
      = struct

        module Arg = struct
          type t = (Par.t,Data.t) revealt
          let equal = M.equal Par.equal (fun x y -> x == y)
          let hash = M.hash Par.hash Hashtbl.hash
        end

        module H = Hashtbl.Make(Arg)

        include Arg
        
        let table = H.create 5003
        let unique =ref 0
        let build a =
          try H.find table a
          with Not_found -> 
            let f = {reveal =  a; id = !unique; data = Data.build a} in
            incr unique; H.add table a f; f

        let clear() = H.clear table

        let compare a1 a2 = Pervasives.compare a1.id a2.id

      end

    module Init
      (Par: sig
        type t
        val equal : t -> t -> bool
        val hash : t -> int
      end)
      = InitData(Par)(EmptyData)

  end


module Make
  (M: sig 
    type 't t
    val equal: ('t->'t->bool) -> 't t -> 't t -> bool
    val hash: ('t->int) -> 't t -> int
  end) = struct

    module N = struct
      type ('t,'a) t = 't M.t
      let equal _ = M.equal
      let hash _ = M.hash
    end
    module TMP = MakePoly(N)

    type 'data t = (unit,'data) TMP.t
    type 'data revealt = (unit,'data) TMP.revealt

    let reveal f = f.TMP.reveal
    let id f     = f.TMP.id
    let data f   = f.TMP.data

    module InitData
      (Data: sig
        type t
        val build : t revealt -> t
      end)
      = TMP.InitData(struct
        type t = unit
        let equal _ _ = true
        let hash _ = 1
      end)
      (Data)
      
    module Init = InitData(EmptyData)

  end
