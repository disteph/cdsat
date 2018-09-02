(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

include HCons_sig

module EmptyData = struct 
  type t = unit
end

module MakePoly(M: sig type ('recurs,'a) t end) = struct

  module G = struct
    type 'p t =
        G : { reveal : ('a*'data*'hcons) revealed;
              id     : int;
              data   : 'data Lazy.t }
        -> ('a*'data*'hcons) t
          constraint 'p=_*_*_[@@ocaml.warning "-62"]

    and 'p revealed = ('p t,'a) M.t constraint 'p='a*_*_
  end

  let reveal (G.G{reveal}) = reveal
  let data   (G.G{data})   = Lazy.force data

  module NoHCons = struct
    type 'p t        = ('a*'data*[`NoHCons]) G.t constraint 'p='a*'data
    type 'p revealed = ('a*'data*[`NoHCons]) G.revealed constraint 'p='a*'data
    let build reveal data  = G.G{ reveal; id = 0; data }
  end

  let tableid = ref 0
      
  module InitData
      (Par : sig type t end)
      (M   : Arg with type 't t := ('t,Par.t) M.t)
      (Data: sig type t end)
  = struct

    open G
        
    let () = incr tableid
    let tableid = !tableid
    let () = Print.print ["HCons",1] (fun p ->
        p "Creating hash-consing table %s%i" M.name tableid)

    type t        = (Par.t*Data.t*[`HCons]) G.t
    type revealed = (Par.t*Data.t*[`HCons]) G.revealed

    let id (G{id}) = id
    let hash_fold_t = Hash.hash2fold id
    let hash  = id
    let equal a b = (=) (id a) (id b)
    let compare a b = Compare.id2compare id a b

    module Arg = struct
      type t = revealed
      let equal = M.equal (==)
      let hash  = Hash.fold2hash(M.hash_fold_t hash_fold_t)
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

    let build data_build reveal =
      (* let f = {reveal =  a; id = !unique; data = None} in *)
      (* try H.find table f *)
      try H.find table reveal
      with Not_found ->
        let rec newf = G { reveal; id = !unique; data = lazy(data_build newf)} in
        let G{data} = newf in
        let _ = Lazy.force data in
        incr unique;
        H.add table reveal newf;
        (* H.add table newf; *)
        (* record id newf; *)
        newf

    let clear() =
      unique := 0;
      H.clear table

  end

  module Init
      (Par : sig type t end)
      (M   : Arg with type 't t := ('t,Par.t) M.t)
  = struct
    include InitData(Par)(M)(EmptyData)
    let build = build (fun _->()) 
  end

end

module Make(M: sig type 'a t end) = struct

  module TMP = MakePoly(struct type ('t,'a) t = 't M.t end)

  module G = struct
    type 'dh t        = (unit*'d*'h) TMP.G.t constraint 'dh='d*'h
    type 'dh revealed = (unit*'d*'h) TMP.G.revealed constraint 'dh='d*'h
  end

  let reveal = TMP.reveal
  let data   = TMP.data

  module NoHCons = struct
    type 'd t        = ('d*[`NoHCons]) G.t
    type 'd revealed = ('d*[`NoHCons]) G.revealed
    let build  = TMP.NoHCons.build
  end

  module InitData
      (Mhash: Arg with type 'a t := 'a M.t)
      (Data : sig type t end)
    = TMP.InitData
      (struct type t = unit end)
      (struct
        include M
        include Mhash
        let name = Mhash.name
      end)
      (Data)

  module Init(M : Arg with type 'a t := 'a M.t) = struct
    include InitData(M)(EmptyData)
    let build = build (fun _->()) 
  end

end
