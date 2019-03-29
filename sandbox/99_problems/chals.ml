open OUnit2

(* Chal 1: Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last l = 
  match l with
  | [] -> None
  | [e] -> Some e
  | _ :: tl -> last tl

(* Chal 2: Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two l = 
  match l with
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: tl -> last_two tl

(* Chal 3: Find the k'th element of a list. (easy) *)
exception Nth of string

let first_elem l =
  match l with
  | [] -> None
  | [e] -> Some e
  | e :: _ -> Some e

let list_tail l =
  match l with
  | [] -> []
  | _ :: tl -> tl

let rec at n l =
  match n with
  | 0 -> first_elem l
  | _ -> at (n - 1) (list_tail l)

let () =
  run_test_tt_main (
    "chal_tests" >::: [
      "Tests for chal1" >::: [
        "elements"  >:: (fun _ -> assert_equal (Some "d") (last ["a";"b";"c";"d"]));
        "empty"    >:: (fun _ -> assert_equal None (last []));
      ];
      "Tests for chal2" >::: [
        "elements"  >:: (fun _ -> assert_equal (Some ("c","d")) (last_two ["a";"b";"c";"d"]));
        "one"    >:: (fun _ -> assert_equal None (last_two ["a"]));
      ];
      "Tests for chal3" >::: [
        "elements"  >:: (fun _ -> assert_equal (Some "c") (at 2 [ "a" ; "b"; "c"; "d"; "e" ]));
        "one"    >:: (fun _ -> assert_equal None (at 3 ["a"]));
      ]
    ])

