#require "core.top";;
#require "core.syntax";;
open Core
open Base

module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module type Interval_intf = sig
    type t
    type endpoint
    val create : endpoint -> endpoint -> t
    val is_empty : t -> bool
    val contains : t -> endpoint -> bool
    val intersect : t -> t -> t
end

(* Create a module with a type that is used throughout
   the various other interfaces include in this
   
   TODO: Need to figure out how to get sexp working :/ *)
module type Interval_intf_with_sexp = sig
    type t
    include Interval_intf with type t := t
    include Core_kernel.Sexpable with type t := t
end

module Make_interval(Endpoint : Comparable)
    : Interval_intf with type endpoint := Endpoint.t
= struct

    type endpoint = Endpoint.t
    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty
    [@@deriving sexp]
    
    let create low high =
        if Endpoint.compare low high > 0 then Empty
        else Interval (low, high)

    let is_empty = function
        | Empty -> true
        | Interval _ -> false
    
    let contains t x =
        match t with
        | Empty -> false
        | Interval (l, h) ->
            Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    let intersect t1 t2 =
        let min x y = if Endpoint.compare x y <= 0 then x else y in
        let max x y = if Endpoint.compare x y >= 0 then x else y in 
        match t1, t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1, h1), Interval (l2, h2) ->
            create (max l1 l2) (min h1 h2)
end

module Int64_interval =
    Make_interval(struct
        type t = Int.t
        let compare = Int.compare
end)

module String_interval = Make_interval(String);;


module type S = sig 
    type 'a t 
    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end

module type Extension = sig 
    type 'a t 
    val iter    : 'a t -> f:('a -> unit) -> unit
    val length  : 'a t -> int
    val count   : 'a t -> f:('a -> bool) -> int
    val for_all : 'a t -> f:('a -> bool) -> bool
    val exists  : 'a t -> f:('a -> bool) -> bool
end

type 'a t = 'a list * 'a list

let empty = ([], [])

let enqueue (in_list, out_list) x =
    (x :: in_list, out_list)

let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] ->
        match List.rev in_list with 
        | [] -> None
        | hd :: tl -> Some (hd, ([], tl))
