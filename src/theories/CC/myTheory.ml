open Cc

module Th = CCModulo (EmptyCC) (EmptyU)

module Sig = Th.Sig
module Atom = Th.Atom
let sugPlugin = Th.sugPlugin
module Consistency = Th.Consistency
module Structure = Th.Structure

