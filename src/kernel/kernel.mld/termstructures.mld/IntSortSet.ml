open General
open Patricia
open Patricia_tools
open Top
open Basic
       
module D = struct
  include IntSort
  include EmptyInfo
  let treeHCons = Some(IntSort.id)
end
             
include PatSet.Make(D)(TypesFromHConsed(IntSort))
let pp = print_in_fmt ~wrap:("{","}") IntSort.pp
