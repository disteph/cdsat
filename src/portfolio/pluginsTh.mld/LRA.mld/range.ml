open Format
open General
open Kernel.Top
       
type 'a bound = (Q.t * bool * 'a) option

module Forbidden = Map.Make(Q)

type 'a t = { lower : 'a bound;
              upper : 'a bound;
              forbidden : 'a Forbidden.t;
              pick  : Q.t }

let pp fmt t =
  let pp_lower fmt = function
    | None            -> fprintf fmt "]-∞"
    | Some(q,true,_)  -> fprintf fmt "]%a" Qhashed.pp q
    | Some(q,false,_) -> fprintf fmt "[%a" Qhashed.pp q
  in
  let pp_upper fmt = function
    | None            -> fprintf fmt "∞["
    | Some(q,true,_)  -> fprintf fmt "%a[" Qhashed.pp q
    | Some(q,false,_) -> fprintf fmt "%a]" Qhashed.pp q
  in
  let pp_forbidden fmt forbidden =
    let rec aux fmt = function
      | []   -> fprintf fmt ""
      | [a,_]  -> fprintf fmt "%a" Qhashed.pp a
      | (a,_)::l -> fprintf fmt "%a, %a" Qhashed.pp a aux l
    in match Forbidden.bindings forbidden with
       | [] -> ()
       | list -> fprintf fmt"\\{%a}" aux list
  in
  fprintf fmt "%a;%a%a" pp_lower t.lower pp_upper t.upper pp_forbidden t.forbidden

let show r = Print.stringOf pp r

let init = { lower = None; upper = None; forbidden = Forbidden.empty; pick = Q.zero }

let is_valid q which = function
  | Some(q',b',_) when (Q.compare q q' = 0 && b')
                       || (Q.compare q' q < 0 && which)
                       || (Q.compare q q' < 0 && not which)
    -> false
  | _ -> true
           
type 'a pick_simpl =
  | Val of Q.t
  | FM of 'a*'a
  | DisEq of 'a*'a*'a
                                        
let pick_simpl = function
  | None,        None -> Val Q.zero
  | Some(l,_,_), None -> Val Q.(l+one)
  | None, Some(u,_,_) -> Val Q.(u-one)
  | Some(l,lb,lbassign), Some(u,ub,ubassign) ->
     if Q.compare u l <0 || (Q.compare u l = 0 && (lb || ub))
     then FM(lbassign,ubassign)
     else Val Q.((l+u)/(of_int 2))

let newbound q bassign b original =
  if is_valid q b original
  then Some(q,true,bassign)
  else original
              
let pick lower upper forbidden = match lower,upper with
  | None, None ->
     let lower =
       try
         let fmax,bassign = Forbidden.max_binding forbidden in
         newbound fmax bassign false lower
       with Not_found -> lower
     in
     pick_simpl (lower,upper)

  | Some(l,lb,lbassign),None ->
     let lower =
       try
         let fmax,bassign = Forbidden.max_binding forbidden in
         newbound fmax bassign false lower
       with Not_found -> lower
     in
     pick_simpl (lower,upper)

  | None,Some(u,ub,ubassign) ->
     let upper =
       try
         let fmin,bassign = Forbidden.min_binding forbidden in
         newbound fmin bassign true upper
       with Not_found -> upper
     in
     pick_simpl (lower,upper)

  | Some(l,lb,lbassign), Some(u,ub,ubassign) ->
     match pick_simpl (lower,upper) with
     | FM _ | DisEq _ as ans -> ans
     | Val middle as ans ->
        match Forbidden.split middle forbidden with
        | _,None,_ -> ans
        | left,Some bassign,right ->
           let right_lower = newbound middle bassign false lower in
           let right_upper =
             try
               let next_right,next_right_bassign = Forbidden.min_binding right in
               newbound next_right next_right_bassign true upper
             with Not_found -> upper
           in
           match pick_simpl (right_lower,right_upper) with
           | Val _ as ans -> ans
           | FM _ | DisEq _ ->
              let left_upper = newbound middle bassign true upper in
              let left_lower =
                try
                  let next_left,next_left_bassign = Forbidden.max_binding left in
                  newbound next_left next_left_bassign false lower
                with Not_found -> lower
              in
              match pick_simpl (left_lower,left_upper) with
              | Val _ as ans -> ans
              | FM _ | DisEq _ ->
                DisEq(lbassign,bassign,ubassign)

type 'a update =
  | Range of 'a t
  | FourierMotzkin of 'a*'a
  | DisEqual of 'a*'a*'a

let upper_update q ~is_strict bassign t =
  if is_valid q true t.upper
  then
    let upper = Some(q,is_strict,bassign) in
    if is_valid t.pick true upper
    then Range { t with upper }
    else
      match pick t.lower upper t.forbidden with
      | Val pick           -> Range { t with upper; pick }
      | FM(ba1,ba2)        -> FourierMotzkin(ba1,ba2)
      | DisEq(ba1,ba2,ba3) -> DisEqual(ba1,ba2,ba3)
  else Range t
         
let lower_update q ~is_strict bassign t =
  if is_valid q false t.lower
  then
    let lower = Some(q,is_strict,bassign) in
    if is_valid t.pick false lower
    then Range { t with lower }
    else
      match pick lower t.upper t.forbidden with
      | Val pick           -> Range { t with lower; pick }
      | FM(ba1,ba2)        -> FourierMotzkin(ba1,ba2)
      | DisEq(ba1,ba2,ba3) -> DisEqual(ba1,ba2,ba3)
  else Range t

let diseq_update q bassign t =
  if Forbidden.mem q t.forbidden
  then Range t
  else
    let forbidden = Forbidden.add q bassign t.forbidden in
    if Q.equal t.pick q
    then
      match pick t.lower t.upper forbidden with
      | Val pick           -> Range { t with forbidden; pick }
      | FM(ba1,ba2)        -> FourierMotzkin(ba1,ba2)
      | DisEq(ba1,ba2,ba3) -> DisEqual(ba1,ba2,ba3)
    else
      Range { t with forbidden }
  
let pick t = t.pick
               
let mem q t =
  is_valid q true t.upper && is_valid q true t.upper
  && not (Forbidden.mem q t.forbidden)

