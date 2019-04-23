open Sexplib.Std

module type Query_handler = sig

    (* Configuration for the query handler. Can be loaded
     * to and from a Sexp *)
    type config [@@deriving sexp]

    (* Name of the service *)
    val name : string

    (* The state of the query handler *)
    type t

    (* Creates a new query handler from a config *)
    val create : config -> t

    (* Evaluate a given query, where both input and output are
     * s-expressions *)
    val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

module Unique = struct
    type config = int [@@deriving sexp]
    type t = { mutable next_id: int }

    let name = "unique"
    let create start_at = { next_id = start_at }

    let eval t sexp =
        match Or_error.try_with (fun () -> unit_of_sexp sexp) with
        | Error _ as err -> err
        | Ok () ->
                let response = Ok (Int.sexp_of_t t.next_id) in
                t.next_id <- t.next_id + 1;
                response
end

module List_dir = struct
    type config = string [@@deriving sexp]
    type t = { cwd: string }

    let is_abs p =
        String.length p > 0 && Char.(=) p.[0] '/'

    let name = "ls"
    let create cwd = { cwd }

    let eval t sexp =
        match Or_error.try_with (fun () -> string_of_sexp sexp) with
        | Error _ as err -> err
        | Ok dir ->
                let dir =
                    if is_abs dir then dir
                    else Core.Filename.concat t.cwd dir
                in
                Ok (Array.sexp_of_t String.sexp_of_t (Core.Sys.readdir dir))
end
