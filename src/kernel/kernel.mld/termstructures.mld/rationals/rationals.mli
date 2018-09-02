include module type of Rationals_sig
            
module TS : Termstructure.Type with type ('data,_) t = 'data t
                                and type ('data,'tset) api = (module API with type datatype = 'data
                                                                          and type tset = 'tset)
