(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module type OptionValue = sig
  type t
  type index
  val value: (t,index) Opt.gadt
end

module EmptyData = struct 
  type t = unit
  let build _ = ()
end

module type PolyS = sig
  type ('t,'a) initial
  type ('a,'data) generic
  type ('a,'data) g_revealed = (('a,'data) generic,'a) initial
  val reveal : ('a,'data) generic -> ('a,'data) g_revealed
  val id     : ('a,'data) generic -> int
  val data   : ('a,'data) generic -> 'data
  (* val compare: ('a,'data) generic -> ('a,'data) generic -> int *)
end

module MakePoly(M: sig 
                    type ('t,'a) t [@@deriving eq, hash]
                    val name : string
                  end)
  = struct

  let () = Print.print ["HCons",1] (fun p -> p "Preparing hash-consing on %s" M.name)
  let tableid = ref 0
                       
  type ('t,'a) initial    = ('t,'a) M.t
  type ('a,'data) generic = {reveal: ('a,'data) g_revealed; id:int; data:'data Lazy.t }
  (* type ('a,'data) generic = {reveal: ('a,'data) g_revealed; id:int; data:'data option} *)
  and  ('a,'data) g_revealed = (('a,'data) generic,'a) M.t

  let reveal f = f.reveal
  let id f     = f.id
  let data f   = Lazy.force f.data
  (* match f.data with *)
  (* | Some d -> d *)
  (* | None -> failwith "HConsed value contains None!" *)


  module InitData
           (B: OptionValue)
           (Par: sig type t [@@deriving eq, hash] end)
           (Data: sig
                type t
                val build : (Par.t,t) generic -> t
              end)
    = struct

    let () = incr tableid
    let tableid = !tableid
    let () = Print.print ["HCons",1] (fun p ->
                 p "Creating hash-consing table %s%i" M.name tableid)

    type t        = (Par.t,Data.t) generic
    type revealed = (Par.t,Data.t) g_revealed

    let hash = id
    let hash_fold_t a = Hash.hash2fold hash a
    let equal = (==)
    let compare a b = Compare.id2compare id a b

    module Arg = struct
      type t = revealed
      let equal = M.equal (==) Par.equal
      let hash  = Hash.wrap2 M.hash_fold_t id Par.hash
    end
    (* module Arg = struct *)
    (*   type nonrec t = t *)
    (*   let equal a b = M.equal (==) Par.equal a.reveal b.reveal *)
    (*   let hash a    = M.hash id Par.hash a.reveal *)
    (* end *)

    module H = Hashtbl.Make(Arg)
    (* module H = Weak.Make(Arg) *)
    let table   = H.create 5003
    let unique  = ref 0

    module BackIndex = Hashtbl.Make(struct
                           type t = int
                           let equal = (=)
                           let hash = Hashtbl.hash 
                         end)
                                   
    let record, backindex =
      let aux : type a index. (a,index)Opt.gadt -> (int->t->unit)*((int->t,index)Opt.gadt)
      = function
      | Opt.Some _ ->
         let backtable = BackIndex.create 5003 in
         BackIndex.add backtable,
         Opt.Some(BackIndex.find backtable)
      | Opt.None -> (fun _ _ -> ()),Opt.None
    in aux B.value
                            
    let build a =
      (* let f = {reveal =  a; id = !unique; data = None} in *)
      (* try H.find table f *)
      try H.find table a
      with Not_found -> 
        let rec newf = { reveal =  a; id = !unique; data = lazy (Data.build newf) } in
        incr unique;
        H.add table a newf;
        (* H.add table newf; *)
        record newf.id newf;
        newf

    let clear() =
      Print.print ["HCons",1] (fun p ->
          p "Clearing hash-consing table %s%i" M.name tableid);
      unique := 0;
      H.clear table

  end

  module Init(B: OptionValue)(Par: sig type t [@@deriving eq, hash] end)
    = InitData(B)(Par)(EmptyData)

end

module type S = sig
  type 't initial
  type 'data generic
  type 'data g_revealed = 'data generic initial
  val reveal : 'data generic -> 'data g_revealed
  val id     : 'data generic -> int
  val data   : 'data generic -> 'data
  (* val compare: 'data generic -> 'data generic -> int *)
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

module Make(M: sig 
                type 't t [@@deriving eq, hash]
                val name : string
              end) = 
  struct

    module N = struct
      type ('t,'a) t = 't M.t [@@deriving eq, hash]
      let name = M.name
    end
    module TMP = MakePoly(N)

    type 't initial = 't M.t
    type 'data generic  = (unit,'data) TMP.generic
    type 'data g_revealed = (unit,'data) TMP.g_revealed

    let reveal f = f.TMP.reveal
    let id f     = f.TMP.id
    let data f   = TMP.data f
    let compare a b = Compare.id2compare id a b

    module InitData(B: OptionValue)
             (Data: sig
                  type t
                  val build : t generic -> t
                end)
      = TMP.InitData(B)(struct type t = unit [@@deriving eq,hash] end)(Data)
                    
    module Init(B: OptionValue) = InitData(B)(EmptyData)

  end

module BackIndex = struct
  type t = unit
  type index = Opt.some
  let value = Opt.Some ()
end

module NoBackIndex = struct
  type t = unit
  type index = Opt.none
  let value = Opt.None
end
