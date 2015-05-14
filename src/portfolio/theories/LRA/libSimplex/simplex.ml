(*DEFINE DEBUG 1*)

open Core

type num = Num.num

let ( +/ ) = Num.( +/ )
let ( -/ ) = Num.( -/ )
let ( */ ) = Num.( */ )
let ( // ) = Num.( // )

type constraint_t  = [ (*`Eq |*) `Le | `Ge | `Lt | `Gt ]

type 'a simplex_t = 
       ?copy:bool
    -> num array array
    -> (num * constraint_t) array
    -> 'a

type base_flag_t = InBase of int | OutBase of int

type violation_t = LowViolation | HighViolation

let negv = function
  | LowViolation  -> HighViolation
  | HighViolation -> LowViolation

let signv = function
  | LowViolation  -> -1
  | HighViolation ->  1

type 'a core_var_t = {
  mutable v_low   : 'a option;   (* Optional lower bound *)
  mutable v_high  : 'a option;   (* Optional upper bound *)
  mutable v_value : 'a;          (* Assignment *)
  mutable v_base  : base_flag_t; (* [InBase  idx] => sp_vars.(sp_in_base.(idx))  == me *)
                                 (* [OutBase idx] => sp_vars.(sp_out_base.(idx)) == me *)
}

let var = fun ?low ?high ~base value ->
  { v_low = low; v_high = high; v_base = base; v_value = value; }

type 'a core_simplex_t = {
  sp_tableau   : num array array;     (* [sp_tableau i j] is the tableau coefficients for
                                       * [sp_vars.(sp_in_base.(i)), sp_vars.(sp_out_base.(j))] *)
  sp_vars      : 'a core_var_t array; (* Variables in order they have to be scanned *)
  sp_in_base   : int array;           (* [sp_vars] indexes of in base variables*)
  sp_out_base  : int array;           (* [sp_vars] indexes of out base variables *)
  sp_n_eqs     : int;                 (* Number of initial equations *)
  sp_n_vars    : int;                 (* Number of initial variables *)
}

exception InvalidProblem of string

module type CoreSimplexBaseI =
sig
  type value

  type var_t     = value core_var_t
  type simplex_t = value core_simplex_t

  val vzero : value

  val ( /+  ) : value -> value -> value
  val ( /-  ) : value -> value -> value
  val ( /*  ) : num -> value -> value
  val ( /// ) : value -> num -> value

  val compare : value -> value -> int

  val ( /<  ) : value -> value -> bool
  val ( /<= ) : value -> value -> bool
  val ( />  ) : value -> value -> bool
  val ( />= ) : value -> value -> bool
  val ( /=  ) : value -> value -> bool

  val string_of_value : value -> string
end

module CoreSimplexBase_Q : CoreSimplexBaseI
  with type value = Num.num
=
struct
  type value     = Num.num
  type var_t     = value core_var_t
  type simplex_t = value core_simplex_t

  let vzero = Num.num_0

  let ( /+ )  = Num.add_num
  let ( /- )  = Num.sub_num
  let ( /* )  = Num.mult_num
  let ( /// ) = Num.div_num

  let compare = Num.compare_num

  let ( /<  ) = Num.( </  )
  let ( /<= ) = Num.( <=/ )
  let ( />  ) = Num.( >/  )
  let ( />= ) = Num.( >=/ )
  let ( /=  ) = Num.( =/  )

  let string_of_value = Num.string_of_num
end

module Qd =
struct
  type qd_value = { qd_value : num; qd_delta : num; }

  let qd_zero = { qd_value = Num.num_0;
                  qd_delta = Num.num_0; }

  let qd_create = fun ?(delta = Num.num_0) value ->
    { qd_value = value; qd_delta = delta; }
end

module CoreSimplexBase_Qd : CoreSimplexBaseI
  with type value = Qd.qd_value
=
struct
  open Num
  open Qd

  type value = qd_value

  type var_t     = value core_var_t
  type simplex_t = value core_simplex_t

  let vzero = qd_zero

  let ( /+ ) = fun qd1 qd2 ->
    { qd_value = qd1.qd_value +/ qd2.qd_value;
      qd_delta = qd1.qd_delta +/ qd2.qd_delta; }

  let ( /- ) = fun qd1 qd2 ->
    { qd_value = qd1.qd_value -/ qd2.qd_value;
      qd_delta = qd1.qd_delta -/ qd2.qd_delta; }

  let ( /* ) = fun x qd ->
    { qd_value = x */ qd.qd_value;
      qd_delta = x */ qd.qd_delta; }

  let ( /// ) = fun qd x ->
    { qd_value = qd.qd_value // x;
      qd_delta = qd.qd_delta // x; }

  let compare = fun qd1 qd2 ->
    match compare_num qd1.qd_value qd2.qd_value with
      | n when n < 0 -> -1
      | n when n > 0 ->  1
      | _            -> compare_num qd1.qd_delta qd2.qd_delta

  let ( /<  ) = fun x y -> compare x y <  0
  let ( /<= ) = fun x y -> compare x y <= 0
  let ( />  ) = fun x y -> compare x y >  0
  let ( />= ) = fun x y -> compare x y >= 0
  let ( /=  ) = fun x y -> compare x y =  0

  let string_of_value = fun qd ->
    Printf.sprintf "%s[%s]"
      (string_of_num qd.qd_value)
      (string_of_num qd.qd_delta)
end

module CoreSimplex(CSB : CoreSimplexBaseI) =
struct
  open CSB
  open Num

  let fprint = fun simplex output ->
    let output_nl = fun () -> output "\n" in
    let output_hr = fun () -> output (String.make 72 '-'); output_nl ()
    in

    let doit = fun () ->
      let maxsize =
        Array.fold_left
          (Array.fold_left
             (fun x coeff -> max x (String.length (Num.string_of_num coeff))))
          0 simplex.sp_tableau
      in
        for irow = 0 to simplex.sp_n_eqs-1 do
          for icol = 0 to simplex.sp_n_vars-1 do
            let coeff = simplex.sp_tableau.(irow).(icol) in
              output (Printf.sprintf " %*s " maxsize (Num.string_of_num coeff))
          done;
          output_nl ()
        done;
        output_nl ();
        Array.iteri
          (fun i var ->
             output (Printf.sprintf "%-2d => %s [%s, %s]\n" i
                       (string_of_value var.v_value)
                       (unopt ~default:"None" (fopt string_of_value var.v_low))
                       (unopt ~default:"None" (fopt string_of_value var.v_high))))
          simplex.sp_vars;
        output_nl ();
        output "In  base:";
        Array.iter (fun i -> output (" " ^ (string_of_int i))) simplex.sp_in_base;
        output_nl ();
        output "Out base:";
        Array.iter (fun i -> output (" " ^ (string_of_int i))) simplex.sp_out_base;
        output_nl ();
    in
      output_hr (); doit (); output_hr ()

  let to_string = fun simplex ->
    let buffer = Buffer.create 0 in
      fprint simplex (Buffer.add_string buffer);
      Buffer.contents buffer

  let to_chan = fun simplex channel ->
    fprint simplex (output_string channel);
    flush channel

  let in_base = function
    | { v_base = InBase _ } -> true
    | _                     -> false

  let out_base = function
    | { v_base = OutBase _ } -> true
    | _                      -> false

  let inbase_idx = function
    | { v_base = InBase idx } -> idx
    | _                       -> failwith "[inbase_idx]"

  let outbase_idx = function
    | { v_base = OutBase idx } -> idx
    | _                        -> failwith "[outbase_idx]"

  let var_idx = fun simplex -> function
    | { v_base = OutBase idx } -> simplex.sp_out_base.(idx)
    | { v_base = InBase  idx } -> simplex.sp_in_base.(idx)

  let get_coeff = fun simplex vrow vcol ->
    match vrow, vcol with
    | { v_base = InBase irow }, { v_base = OutBase icol } ->
        simplex.sp_tableau.(irow).(icol)
    | _ ->
        failwith "[get_coeff] variables base status not correct"

  let violate_low_bound = function
    | { v_low = Some low; v_value = v }  -> v /< low
    | _                                  -> false

  let can_decrease = function
    | { v_low = Some low; v_value = v }  -> v /> low
    | _                                  -> true

  and violate_high_bound = function
    | { v_high = Some high; v_value = v } -> v /> high
    | _                                   -> false

  let can_increase = function
    | { v_high = Some high; v_value = v } -> v /< high
    | _                                   -> true

  let violate_bound = fun var bound ->
    match bound with
    | LowViolation  -> violate_low_bound  var
    | HighViolation -> violate_high_bound var

  let violating_bounds =
    fun (var : var_t) ->
      (**) if violate_low_bound  var then Some LowViolation
      else if violate_high_bound var then Some HighViolation
      else None

  let violating_base = fun (simplex : simplex_t) ->
    Array.efind
      (fun i var ->
         if in_base var then violating_bounds var else None)
      simplex.sp_vars

  exception Solved of [ `Success | `Failure of num array * num array ]

  let update_inbase_assignment = fun (simplex : simplex_t) ->
    Array.iteri
      (fun irow coeffs ->
         let value =
           Array.foldi_left
             (fun value icol coeff ->
                let outvar   = simplex.sp_out_base.(icol) in
                let outvalue = simplex.sp_vars.(outvar).v_value in
                  value /+ (coeff /* outvalue))
             vzero coeffs
         in let var = simplex.sp_vars.(simplex.sp_in_base.(irow)) in
           var.v_value <- value)
      simplex.sp_tableau

  let pivot = fun simplex ~vrow ~vcol ~delta ->
    let pirow = inbase_idx  vrow
    and picol = outbase_idx vcol in
    let prow  = simplex.sp_tableau.(pirow) in
    let pivot = prow.(picol) in

      (* Update assignment *)
      vcol.v_value <- vcol.v_value /+ delta;
      update_inbase_assignment simplex;

      (* Pivoting operation on tableau *)
      Array.iteri
        (fun idx coeff ->
           if   idx = picol
           then prow.(idx) <- num_1 // pivot
           else prow.(idx) <- minus_num (coeff // pivot))
        prow;
      Array.iteri
        (fun irow row ->
           let factor = row.(picol) in
             if irow <> pirow then
               Array.iteri
                 (fun icol coeff ->
                    if   icol = picol
                    then row.(icol) <- coeff // pivot
                    else row.(icol) <- coeff +/ (factor */ prow.(icol)))
                 row)
        simplex.sp_tableau;

      (* Pivoting operation on base *)
      vrow.v_base <- OutBase picol;
      vcol.v_base <- InBase  pirow;
      begin
        let picol_idx = simplex.sp_out_base.(picol)
        and pirow_idx = simplex.sp_in_base.(pirow)
        in
          simplex.sp_in_base.(pirow)  <- picol_idx;
          simplex.sp_out_base.(picol) <- pirow_idx;
      end

  let check = fun (simplex : simplex_t) ->
    let _check = fun b ->
      if not b then failwith "inconsistent simplex"
    in
      (* Sanity check of lengths *)
      _check (simplex.sp_n_vars >= 0);
      _check (simplex.sp_n_eqs  >  0);
      _check (Array.length simplex.sp_vars = (simplex.sp_n_eqs + simplex.sp_n_vars));
      _check (Array.length simplex.sp_in_base  = simplex.sp_n_eqs);
      _check (Array.length simplex.sp_out_base = simplex.sp_n_vars);
      _check (Array.length simplex.sp_tableau  = simplex.sp_n_eqs);
      Array.iter
        (fun coeffs ->
           _check (Array.length coeffs = simplex.sp_n_vars))
        simplex.sp_tableau;
      (* Check back-references *)
      Array.iteri
        (fun i ->
           function
           | { v_base = InBase idx } ->
               _check (idx >= 0 && idx < Array.length simplex.sp_in_base);
               _check (simplex.sp_in_base.(idx) = i)
           | { v_base = OutBase idx } ->
               _check (idx >= 0 && idx < Array.length simplex.sp_out_base);
               _check (simplex.sp_out_base.(idx) = i))
        simplex.sp_vars;
      (* Invariant 1 *)
      Array.iteri
        (fun irow coeffs ->
           let value =
             Array.foldi_left
               (fun value icol coeff ->
                  let outvar   = simplex.sp_out_base.(icol) in
                  let outvalue = simplex.sp_vars.(outvar).v_value in
                  value /+ (coeff /* outvalue))
               vzero coeffs
           in let var = simplex.sp_vars.(simplex.sp_in_base.(irow)) in
             _check (var.v_value /= value))
        simplex.sp_tableau;
      (* Invariant 2 *)
      Array.iter
        (fun idx ->
           _check ((violating_bounds simplex.sp_vars.(idx)) = None))
        simplex.sp_out_base

  let _solve = fun (simplex : simplex_t) ->
    while true do
      (* IFDEF DEBUG THEN *)
      (*   to_chan simplex stderr; *)
      (*   check simplex *)
      (* ENDIF; *)
      match violating_base simplex with
      | None ->
          raise (Solved `Success)

      | Some (_, vrow, vl) ->
          let pivot_test = fun _ vcol ->
            (out_base vcol) &&
              let coeff = get_coeff simplex vrow vcol in
                match - (sign_num coeff) * (signv vl) with
                |  1 -> can_increase vcol
                | -1 -> can_decrease vcol
                |  _ -> false
          in
            match Array.find pivot_test simplex.sp_vars with
            | None ->
                let coeffs_eqs  = Array.create simplex.sp_n_vars num_0
                and coeffs_vars = Array.create simplex.sp_n_eqs  num_0 in
                let set_cert_value = fun var value ->
                  let idx = var_idx simplex var in
                    if   idx < simplex.sp_n_vars
                    then coeffs_eqs.(idx) <- value
                    else coeffs_vars.(idx - simplex.sp_n_vars) <- value
                in let offending_coeffs =
                    simplex.sp_tableau.(inbase_idx vrow)
                in
                  set_cert_value vrow num_neg_1;
                  Array.iteri
                    (fun i value ->
                       let vcol = simplex.sp_vars.(simplex.sp_out_base.(i)) in
                         set_cert_value vcol value)
                    offending_coeffs;
                  raise (Solved (`Failure (coeffs_eqs, coeffs_vars)))

            | Some (_, vcol) ->
                let delta =
                  match vl with
                  | LowViolation  -> (unopt vrow.v_low ) /- vrow.v_value
                  | HighViolation -> (unopt vrow.v_high) /- vrow.v_value
                in let delta = delta /// (get_coeff simplex vrow vcol) in
                  pivot simplex ~vrow ~vcol ~delta
    done

  let solve = fun simplex ->
    try  _solve simplex; abort ()
    with Solved result -> result
end

module CoreSimplexQ  = CoreSimplex(CoreSimplexBase_Q)
module CoreSimplexQd = CoreSimplex(CoreSimplexBase_Qd)

type simplex_N_t = CoreSimplexBase_Q.simplex_t
type simplex_K_t = CoreSimplexBase_Qd.simplex_t

type ucert_K = num array

type ucert_N =
  [ `NC_Split of int * Big_int.big_int * ucert_N * ucert_N
  | `NC_Base  of num array * num array ]

type ('num, 'ucert) result =
  [ `Satisfiable   of 'num array
  | `Unsatisfiable of 'ucert ]

type simplex_check = {
  spc_nvars   : int;
  spc_neqs    : int;
  spc_problem : num array array;
}

let __check_problem =
  fun
    (data  : num array array)
    (cttrs : (num * _) array) ->

  if Array.length data = 0 then
    raise (InvalidProblem "no equations");
  if Array.length data <> Array.length cttrs then
    raise (InvalidProblem
             "tableau/constraint arrays have inconsistent lengths");
  let neqs  = Array.length data
  and nvars =
    let (vmin, vmax) =
      Array.fold_left
        (fun (vmin, vmax) eq ->
           let eqlength = Array.length eq in
             (min vmin eqlength, max vmax eqlength))
        (Array.length data.(0), Array.length data.(0))
        data
    in
      if vmin <> vmax then
        raise (InvalidProblem "# of variables is inconsistent");
      if vmax <= 0 then
        raise (InvalidProblem "problem contains empty equations");
      vmax
  in
    { spc_nvars   = nvars;
      spc_neqs    = neqs;
      spc_problem = data; }

let simplex_N = fun ?(copy = false) data cttrs ->
  let { spc_nvars = nvars;
        spc_neqs  = neqs; }
      = __check_problem data cttrs
  in
  let vars =
    Array.init (nvars+neqs)
      (fun i ->
         if   i < nvars
         then var ~base:(OutBase i) Num.num_0
         else
           let bound, cttr = cttrs.(i-nvars)
           and base        = InBase (i-nvars) in
             match cttr with
(*             | `Eq -> var ~base ~low:bound ~high:bound Num.num_0*)
             | `Le -> var ~base            ~high:bound Num.num_0
             | `Ge -> var ~base ~low:bound             Num.num_0
             | `Lt -> var ~base ~high:(bound -/ Num.num_1) Num.num_0
             | `Gt -> var ~base ~low:(bound  +/ Num.num_1)  Num.num_0)
  in
    { sp_tableau  = if copy then Array.map Array.copy data else data;
      sp_vars     = vars;
      sp_in_base  = Array.init neqs  (fun i -> nvars + i);
      sp_out_base = Array.init nvars (fun i -> i);
      sp_n_eqs    = neqs;
      sp_n_vars   = nvars; }

let simplex_K = fun ?(copy = true) data cttrs ->
  let { spc_nvars = nvars;
        spc_neqs  = neqs; }
      = __check_problem data cttrs
  in
  let vars =
    Array.init (nvars+neqs)
      (fun i ->
         if   i < nvars
         then var ~base:(OutBase i) Qd.qd_zero
         else
           let bound, cttr = cttrs.(i-nvars)
           and base        = InBase (i-nvars) in
         let bound =
           match cttr with
             | `Le | `Ge (*| `Eq*) -> Qd.qd_create                      bound
             | `Lt             -> Qd.qd_create ~delta:Num.num_neg_1 bound
             | `Gt             -> Qd.qd_create ~delta:Num.num_1     bound
         in
             match cttr with
(*             | `Eq       -> var ~base ~low:bound ~high:bound Qd.qd_zero*)
             | `Le | `Lt -> var ~base            ~high:bound Qd.qd_zero
             | `Ge | `Gt -> var ~base ~low:bound             Qd.qd_zero)
  in
    { sp_tableau  = if copy then Array.map Array.copy data else data;
      sp_vars     = vars;
      sp_in_base  = Array.init neqs  (fun i -> nvars + i);
      sp_out_base = Array.init nvars (fun i -> i);
      sp_n_eqs    = neqs;
      sp_n_vars   = nvars; }

exception KSolved of num array option
exception NSolved of Big_int.big_int array option

let solve_K = fun (simplex : simplex_K_t) ->
  match CoreSimplexQd.solve simplex with
  | `Failure (_, certificate) ->
      `Unsatisfiable certificate
  | `Success ->
      let min =
        let mindelta_var = fun var ->
          let mindelta = fun ~low ~high ->
            let num = high.Qd.qd_value -/ low.Qd.qd_value
            and div = low.Qd.qd_delta  -/ high.Qd.qd_delta in
              if   (Num.sign_num num > 0) && (Num.sign_num div > 0)
              then Some (num // div)
              else None
          in
          let md_low  = fopt_p (fun x -> mindelta x var.v_value) var.v_low
          and md_high = fopt_p (fun x -> mindelta var.v_value x) var.v_high in
            (fopt2 min md_low md_high : Num.num option)
        in
          unopt ~default:(Num.num_1)
            (Array.fold_left
               (fun acc var -> fopt2 min acc (mindelta_var var))
               None simplex.sp_vars)
      in
      let assignment =
        Array.init
          simplex.sp_n_vars
          (fun i ->
             let value = simplex.sp_vars.(i).v_value in
               value.Qd.qd_value +/ (value.Qd.qd_delta */ min))
      in
        `Satisfiable assignment

let rec solve_N = fun simplex ->
(*  print_endline("TRACE1");*)
  match CoreSimplexQ.solve simplex with
  | `Failure certificate ->
      `Unsatisfiable (`NC_Base certificate)

  | `Success ->
      let theta =
        Array.init
          simplex.sp_n_vars
          (fun i -> simplex.sp_vars.(i).v_value)
      in
        match Array.find (fun _ x -> not (Num.is_integer_num x)) theta with
        | None ->
            `Satisfiable (Array.map Num.big_int_of_num theta)

        | Some (i, x) ->
            let var    = simplex.sp_vars.(i) in
            let branch = fun vl ->
              let bget, bset, round =
                match vl with
                | `Low  -> (fun x -> x.v_low ), (fun x v -> x.v_low  <- v), Num.ceiling_num
                | `High -> (fun x -> x.v_high), (fun x v -> x.v_high <- v), Num.floor_num
              in
              let assign = Array.map (fun x -> x.v_value) simplex.sp_vars in
              let obound = bget var in
                bset var (Some (fopt2_left Num.max_num obound (round x)));
                if CoreSimplexQ.out_base var then
                  CoreSimplexQ.update_inbase_assignment simplex;
                (* IFDEF DEBUG THEN CoreSimplexQ.check simplex ENDIF; *)
                let rrun = solve_N simplex in
                  begin
                    match rrun with
                    | `Unsatisfiable certificate ->
                        bset var obound;    (* Restore old bounds *)
                        Array.iteri         (* Restore assignment *)
                          (fun i oldvalue ->
                             simplex.sp_vars.(i).v_value <- oldvalue)
                          assign;
                        (* Invariants are now OK, current simplex is
                           _equivalent_ to initial one (not equal) *)
                        (* IFDEF DEBUG THEN CoreSimplexQ.check simplex ENDIF *)
                    | _ -> ()
                  end; rrun
            in
              match branch `Low with
              | (`Satisfiable _) as rrun -> rrun
              | `Unsatisfiable cert1 -> begin
                  match branch `High with
                  | (`Satisfiable _) as rrun -> rrun
                  | `Unsatisfiable cert2 -> begin
                      let branch_v = Num.big_int_of_num (Num.floor_num x) in
                        `Unsatisfiable (`NC_Split (i, branch_v, cert1, cert2))
                    end
                end

let string_of_indent = fun indent ->
  let buffer = Buffer.create 0 in
    for i = 1 to indent do
      Buffer.add_string buffer "| "
    done;
    Buffer.add_string buffer "+ ";
    Buffer.contents buffer

let string_of_ucert_N = fun ucert ->
  let buffer = Buffer.create 0 in

  let print = fun indent format ->
    Printf.ksprintf
      (fun x ->
         Printf.bprintf buffer "%s%s\n" (string_of_indent indent) x)
      format

  in let rec string_of_ucert = fun indent -> function
    | `NC_Base (coeffs1, coeffs2) ->
        let coeffs1 = Array.to_list (Array.map Num.string_of_num coeffs1)
        and coeffs2 = Array.to_list (Array.map Num.string_of_num coeffs2)
        in
          print indent "C1: %s" (String.concat " ; " coeffs1);
          print indent "C2: %s" (String.concat " ; " coeffs2)
    | `NC_Split (i, x, cert1, cert2) ->
        print indent "(%d %s)" i (Big_int.string_of_big_int x);
        List.iter (string_of_ucert (1+indent)) [cert1; cert2]
  in
    string_of_ucert 0 ucert; Buffer.contents buffer
