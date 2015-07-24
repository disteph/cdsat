open Kernel
open Top
open Hub

type 'tset allmsg =
| AllMsg: ('a,'tset,'c) Messages.thsays -> 'tset allmsg

module type ThPlugin = sig

  type sign
  type tset

  type slot_machine =
  | SM: (sign,tset,'msg) Messages.thsays option
    *(tset allmsg option -> slot_machine)
    -> slot_machine

  val search: tset -> slot_machine

end

type agglodata = unit
let datalist = NoData

module Strategy(WB: Interfaces.WhiteBoard)
  = struct
    open WB
    open DS
    let solve tset = check(PlNotProvable(tset,[]))
  end

