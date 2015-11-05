
type interval
val real : interval
val create : Num.num -> bool -> Num.num -> bool -> interval

val getInf : interval -> Num.num
val getSup : interval -> Num.num
val isInfStrict : interval -> bool
val isSupStrict : interval -> bool
val isEmpty : interval -> bool

val chooseValue : interval -> Num.num
