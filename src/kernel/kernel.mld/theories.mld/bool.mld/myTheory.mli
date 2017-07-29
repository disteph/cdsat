open Top
open Specs
open API
       
type sign

include Theory.Type
        with type ('t,'v,'a) api = (module API with type sign = sign
                                                and type assign  = 'a
                                                and type bassign = 't termF * bool
                                                and type sassign = 't termF * 'v Values.t)
                                     
