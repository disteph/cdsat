open Top.Terms

module type Type = sig
  val name : string (* name of theory *)
  type sign         (* secret type used by the theory module to sign its messages *)
  val ds : dsKey list (* list of keys of data structures (alternate implem of terms) *)
  type api          (* a theory exports an API *)
  val make : (module Writable) -> api (* the export *)
end

