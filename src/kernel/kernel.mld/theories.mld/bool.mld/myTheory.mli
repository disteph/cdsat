open Top.Specs

type sign

include Theory.Type with type ts = Termstructures.Clauses.TS.t
                     and type values = has_no_values
                     and type ('t,'v,'a) api
                              = (module API.Type with type term = 't termF
                                                  and type value  = 'v
                                                  and type assign = 'a
                                                  and type sign = sign)
