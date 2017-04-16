open Top
open Specs

type sign
include Theory.Type with type ts = Termstructures.Clauses.TS.t
                     and type ('term,'tset) t = (module Api.Type with type term = 'term
                                                                      and type tset = 'tset
                                                                      and type sign = sign)
                                                                       
