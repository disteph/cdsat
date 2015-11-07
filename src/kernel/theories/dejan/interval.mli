
type interval
type extNum = INFINITY | NUM of Num.num
val real : interval
val create : extNum -> bool -> extNum -> bool -> interval

val getInf : interval -> extNum
val getSup : interval -> extNum
val isInfStrict : interval -> bool
val isSupStrict : interval -> bool
val isEmpty : interval -> bool

val chooseValue : interval -> Num.num
