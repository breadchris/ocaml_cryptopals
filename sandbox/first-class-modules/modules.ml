module type Bumpable = sig 
    type t
    val bump : t -> t
end

module Int_bumper = struct
    type t = int
    let bump n = n + 1
end

let int_bumper = (module Int_bumper : Bumpable with type t = int)

let (module Bumpable) = int_bumper in Bumpable.bump 3

(* exposing a polymorphic type. If we didn't do this then
    the fully abstract Bumpable module would hide what type
    t is from List.map which needs to know what goes into the
    resulting list. *)
let bump_list
        (type a)
        (module B : Bumpable with type t = a)
        (l: a list)
    =
    List.map ~f:B.bump l

module type Comparable = sig
    type t
    val compare : t -> t -> int
end

let create_comparable (type a) compare =
    (module struct
        type t = a
        let compare = compare
    end : Comparable with type t = a)
