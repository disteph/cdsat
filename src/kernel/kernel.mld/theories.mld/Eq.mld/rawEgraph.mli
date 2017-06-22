open Interfaces
                                 
module Make(P : Parameters) : (RawEgraph with type info := P.info
                                                       and type edge := P.edge
                                                                    and type node = P.Node.t)
