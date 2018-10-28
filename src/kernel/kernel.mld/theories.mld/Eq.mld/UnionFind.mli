include module type of UnionFind_sig
                                 
module Make(P : Parameters) : (S with type info := P.info
                                  and type edge := P.edge
                                  and type node := P.Node.t
                                  and type termmap := P.NodeMapCompress.t)
