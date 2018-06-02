(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

include HCons_sig

module EmptyData = struct 
  type t = unit
  let build _ = ()
end

module MakePoly(M: sig type ('recurs,'a) t end) = struct

  module G = struct
    type 'p t =
        G : { reveal : ('a*'data*'hcons) revealed;
              id     : int;
              data   : 'data Lazy.t } -> ('a*'data*'hcons) t
          constraint 'p=_*_*_

    and 'p revealed = ('p t,'a) M.t constraint 'p='a*_*_
    let reveal (G{reveal}) = reveal
    let data   (G{data})   = Lazy.force data
  end

  module NoHCons = struct
    type 'p t        = ('a*'data*[`NoHCons]) G.t constraint 'p='a*'data
    type 'p revealed = ('a*'data*[`NoHCons]) G.revealed constraint 'p='a*'data
    let build reveal data  = G.G{ reveal; id = 0; data }
  end

  let tableid = ref 0

  module InitData
      (M   : PolyArg with type ('recurs,'a) t := ('recurs,'a) M.t)
      (Par : sig type t [@@deriving eq,hash] end)
      (Data: sig
         type t
         val build : (Par.t*t*[`HCons]) G.t -> t
       end)
  = struct

    open G
        
    let () = incr tableid
    let tableid = !tableid
    let () = Print.print ["HCons",1] (fun p ->
        p "Creating hash-consing table %s%i" M.name tableid)

    type t        = (Par.t*Data.t*[`HCons]) G.t
    type revealed = (Par.t*Data.t*[`HCons]) G.revealed

    let id (G{id})  = id
    let hash_fold_t = Hash.hash2fold id
    let hash  = id
    let equal = (==)
    let compare a b = Compare.id2compare id a b

    module Arg = struct
      type t = revealed
      let equal = M.equal (==) Par.equal
      let hash  = Hash.fold2hash(M.hash_fold_t hash_fold_t Par.hash_fold_t)
    end

    module H = Hashtbl.Make(Arg)
    (* module H = Weak.Make(Arg) *)
    let table   = H.create 5003
    let unique  = ref 0

    (* module BackIndex = Hashtbl.Make(Int) *)

    (* let record, backindex =
     *   let aux : type a index. (a,index)Goption.t -> (int->t->unit)*((int->t,index)Goption.t)
     *     = function
     *       | Goption.Some _ ->
     *         let backtable = BackIndex.create 5003 in
     *         BackIndex.add backtable,
     *         Goption.Some(BackIndex.find backtable)
     *       | Goption.None -> (fun _ _ -> ()),Goption.None
     *   in aux M.backindex *)

    let build a =
      (* let f = {reveal =  a; id = !unique; data = None} in *)
      (* try H.find table f *)
      try H.find table a
      with Not_found ->
        let id = !unique in
        let rec newf = G { reveal =  a; id ; data = lazy(Data.build newf)} in
        let G{data} = newf in
        let _ = Lazy.force data in
        incr unique;
        H.add table a newf;
        (* H.add table newf; *)
        (* record id newf; *)
        newf

    let clear() =
      unique := 0;
      H.clear table

  end

  module Init
      (M   : PolyArg with type ('recurs,'a) t := ('recurs,'a) M.t)
      (Par : sig type t [@@deriving eq,hash] end)
    = InitData(M)(Par)(EmptyData)

end

module Make(M: sig type 'a t end) = struct

  module TMP = MakePoly(struct type ('t,'a) t = 't M.t end)

  module G = struct
    type 'dh t        = (unit*'d*'h) TMP.G.t constraint 'dh='d*'h
    type 'dh revealed = (unit*'d*'h) TMP.G.revealed constraint 'dh='d*'h
    let reveal = TMP.G.reveal
    let data   = TMP.G.data
  end

  module NoHCons = struct
    type 'd t        = ('d*[`NoHCons]) G.t
    type 'd revealed = ('d*[`NoHCons]) G.revealed
    let build  = TMP.NoHCons.build
  end

  module InitData
      (Mhash: Arg with type 'a t := 'a M.t)
      (Data : sig
         type t
         val build : (t*[`HCons]) G.t -> t
       end)
    = TMP.InitData(struct
      module Arg = struct
        include M
        include Mhash
      end
      type ('t,'a) t = 't Arg.t [@@deriving eq,hash]
      let name = Mhash.name
    end)
      (struct type t = unit [@@deriving eq, hash] end)
      (Data)

  module Init(M : Arg with type 'a t := 'a M.t) = InitData(M)(EmptyData)

end
