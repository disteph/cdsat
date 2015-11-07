
include Num
open Num

type extNum = INFINITY | NUM of num
type interval = extNum * bool * extNum * bool

let real = INFINITY, false, INFINITY, false

let create inf isInfStrict sup isSupStrict =
    (inf,isInfStrict,sup,isSupStrict)

let getInf i =
    let inf, _, _, _ = i in inf

let getSup i =
    let _, _, sup, _ = i in sup

let isInfStrict i =
    let _, b, _, _ = i in b

let isSupStrict i =
    let _, _, _, b = i in b

let isEmpty i =
    let binf, iStrict, bsup, sStrict = i in match binf, bsup with
    | NUM(inf), NUM(sup) -> inf >/ sup || (inf =/ sup && (iStrict || sStrict))
    | _, _ -> false

let chooseValue i =
    let binf, iStrict, bsup, sStrict = i in match binf, bsup  with
    | NUM(inf), _ when not iStrict -> inf
    | _, NUM(sup) when not sStrict -> sup
    | NUM(inf), NUM(sup) -> (sup -/ inf) // (num_of_int 2)
    | INFINITY, INFINITY -> num_of_int 0
    | INFINITY, NUM(sup) -> sup -/ num_of_int 1
    | NUM(inf), INFINITY -> inf +/ num_of_int 1
