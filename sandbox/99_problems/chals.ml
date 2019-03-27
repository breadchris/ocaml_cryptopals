open OUnit2

(* Chal 1: Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last l = function
  | [] -> None
  | e -> Some e
  | e :: tl -> last tl

let chal1_tests = "Tests for chal1" >::: [
  "elements"  >:: (fun _ -> assert_equal Some "d" (last ["a";"b";"c";"d"]));
  "empty"    >:: (fun _ -> assert_equal None (last []));
]
