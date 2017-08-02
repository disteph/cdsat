open Top
open Specs
open Sassigns
open API
       
type sign

include Theory.Type
        with type ('t,'v,'a) api = (module API with type sign = sign
                                                and type termdata = 't
                                                and type value = 'v
                                                and type assign  = 'a )
                                     
