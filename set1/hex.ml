#require "core.top";;
#require "core.syntax";;
open Core
open Base

module Hex = struct

let of_string str =
    let char_to_int c =
        let x = (Char.to_int c) - (Char.to_int 'a') in
        if x <= 0xf then x
        else failwith (sprintf "Non-hex character given: %c" c) in
    let rec parse_string chars =
        match chars with
        | [] -> []
        | [x] -> failwith "Odd-length string given"
        | a :: (b :: tl) ->
            let hex_num = ((char_to_int a) lsl 4) lor (char_to_int b) in
            hex_num :: parse_string tl in
    parse_string (String.to_list (String.lowercase str))

end