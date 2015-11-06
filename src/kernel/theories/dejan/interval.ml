
include Num
open Num

type interval = num * bool * num * bool

let infinity = num_of_int 1 // num_of_int 0
let real = infinity, false, infinity, false

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
    let inf, iStrict, sup, sStrict = i in
    inf >/ sup || (inf =/ sup && (iStrict || sStrict))

let chooseValue i =
    let inf, iStrict, sup, sStrict = i in
    if not iStrict && inf <>/ infinity then inf
    else if not sStrict && sup <>/ (infinity) then sup
    else if inf <>/ (infinity) && sup <>/ (infinity) then (sup -/ inf) // (num_of_int 2)
    else if inf =/ infinity && sup =/ infinity then num_of_int 0
    else if inf =/ infinity then sup -/ num_of_int 1
    else (*if sup =/ infinity then*) inf +/ num_of_int 1
