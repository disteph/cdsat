open General
open Patricia
open Patricia_tools

open Top
open Basic
open Specs

include Rationals_sig

module PreTS = struct

  type nonrec ('data,_) t = 'data t

  type ('data,'tset) api = (module API with type datatype = 'data
                                        and type tset = 'tset)

  module Make(Term : Term)(TSet : Collection with type e = Term.t) = struct

    type nonrec t = (Term.datatype,TSet.t) t

    module API = struct

      type datatype = Term.datatype
      type tset = TSet.t

      module VarMap = MapNH(struct
          include Term
          include EmptyInfo
          include TypesFromHConsed(Term)
          type values = Q.t
        end)

      let pp fmt t =
        let open Format in
        let pp_monome fmt (var,coeff) =
          let coeff = Q.(coeff * t.scaling) in
          if Q.equal coeff Q.one
          then fprintf fmt "(%a)" Term.pp var
          else if Q.equal coeff Q.minus_one
          then fprintf fmt "-(%a)" Term.pp var
          else fprintf fmt "%a·(%a)" Q.pp_print coeff Term.pp var
        in
        let cst = Q.(t.constant * t.scaling) in
        let pp_cst s fmt cst = fprintf fmt "%s%a" s Q.pp_print cst in
        let rec pp_map fmt = function
          | []             -> pp_cst "" fmt cst
          | [monome]       -> fprintf fmt "%a%a"
                                pp_monome monome
                                (if Q.sign cst>0 then pp_cst "+"
                                 else if Q.sign cst<0 then pp_cst ""
                                 else (fun _ _ -> ()))
                                cst
          | monome::(((_,coeff)::_) as coeffs) ->
            fprintf fmt "%a%s%a"
              pp_monome monome (if Q.sign coeff>0 then "+" else "") pp_map coeffs
        in
        let pp_expr s = fprintf fmt "%a%s" pp_map (VarMap.elements t.coeffs) s in
        match t.nature with
        | Other -> fprintf fmt "Not understandable"
        | Term -> pp_expr ""
        | Lt -> pp_expr " < 0"
        | Le -> pp_expr " ≤ 0"
        | Eq -> pp_expr " = 0"
        | NEq -> pp_expr " ≠ 0"

      let show = Print.stringOf pp

      let other = { scaling = Q.one; coeffs = VarMap.empty; constant = Q.zero; nature = Other }

    end

    let api : (Term.datatype,TSet.t) api = (module API)
    include API
        
    let mult factor t =
      match t.nature with
      | Lt | Le when Q.sign factor = -1
        -> failwith "Cannot multiply an inequality by a negative"
      | _ -> { t with scaling = Q.(factor * t.scaling) }

    let add_action scaling1 scaling2 =
      let sameleaf var coeff1 coeff2 =
        let open Q in
        let coeff = (scaling1 * coeff1) + (scaling2 * coeff2) in
        if equal coeff zero then VarMap.empty else VarMap.singleton var coeff
      in
      let emptyfull t = VarMap.map (fun _ -> Q.( * ) scaling2) t in
      let fullempty t = VarMap.map (fun _ -> Q.( * ) scaling1) t in
      let combine = VarMap.union (fun _ _ -> failwith "Should be disjoint") in
      VarMap.Merge.{ sameleaf; emptyfull; fullempty; combine }

    let add t1 t2 =
      let coeffs = VarMap.merge (add_action t1.scaling t2.scaling) t1.coeffs t2.coeffs in
      let constant = Q.((t1.constant * t1.scaling) + (t2.constant * t2.scaling)) in
      let nature =
        match t1.nature, t2.nature with
        | Term, Term -> Term
        | Eq, NEq | NEq, Eq -> NEq
        | NEq, _ | _, NEq -> failwith "Cannot add a disequality"
        | Lt, _ | _, Lt -> Lt
        | Le, _ | _, Le -> Le
        | Eq, _ | _, Eq -> Eq
        | _ -> failwith "Adding does not make sense"
      in
      { scaling = Q.one; coeffs; constant; nature }

    let minus t1 t2 = add t1 (mult Q.minus_one t2)

    let make_var t =
      let coeffs = VarMap.singleton t Q.one in
      { scaling = Q.one; coeffs; constant=Q.zero; nature = Term }


    let build ~proj (t:Term.t) : t =
      match Terms.reveal t with
      | Terms.C(symb,l)
        -> let l = List.map (Terms.data >> proj) l in
        begin
          match symb, l with
          | Symbols.CstRat n, []
            -> { scaling = Q.one; coeffs=VarMap.empty; constant=n; nature=Term }
          | Symbols.Eq Sorts.Rat, [a;b]           -> { (minus a b) with nature = Eq }
          | Symbols.NEq Sorts.Rat, [a;b]          -> { (minus a b) with nature = NEq }
          | Symbols.Le, [a;b] | Symbols.Ge, [b;a] -> { (minus a b) with nature = Le }
          | Symbols.Lt, [a;b] | Symbols.Gt, [b;a] -> { (minus a b) with nature = Lt }
          | Symbols.Plus, [a;b]                   -> add a b
          | Symbols.Minus, [a;b]                  -> minus a b
          | Symbols.Op, [a]                       -> mult Q.minus_one a 

          | Symbols.Times, [a;b] ->
            if VarMap.is_empty a.coeffs then mult Q.(a.scaling * a.constant) b
            else if VarMap.is_empty b.coeffs then mult Q.(b.scaling * b.constant) a
            else make_var t

          | Symbols.Divide, [a;b] ->
            if VarMap.is_empty b.coeffs then mult Q.(inv(b.scaling * b.constant)) a
            else make_var t

          | _,_ -> 
            match Symbols.arity symb with
            | Sorts.Rat, _ -> make_var t
            | _,_ -> other
        end

      | Terms.V fv ->
        begin
          match Variables.FreeVar.get_sort fv with
          | Sorts.Rat -> make_var t
          | _ -> other
        end

      | Terms.FB _ -> other

  end
end

module TS = Termstructure.Make(PreTS)
