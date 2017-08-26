open Top.Specs
open API
       
type sign

include Theory.Type
        with type ('t,'v,'a,'s) api = (module API with type sign   = sign
                                                   and type assign = 'a
                                                   and type termdata = 't
                                                   and type value  = 'v
                                                   and type tset   = 's)
           
