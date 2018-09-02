open General
open Patricia
open Patricia_tools
open Top
open Basic
             
include SetH(struct
    include IntSort
    include EmptyInfo
    include TypesFromHConsed(IntSort)
  end)
    
let pp = print_in_fmt ~wrap:("{","}") IntSort.pp
