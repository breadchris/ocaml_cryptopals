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

(* Chal 4: Find the number of elements of a list. (easy) *)
let rec length l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl

(* Chal 5: OCaml standard library has List.rev but we ask that you reimplement it. *)
let rec rev l =
  match l with
  | [] -> []
  | e :: tl -> rev tl @ [e]

(* Chal 6: Find out whether a list is a palindrome. (easy) *)
let is_palindrome l =
  (* re-implement reverse here cause it is an interesting approach *)
  let rec rev acc = function
    | [] -> acc
    | h :: tl -> rev (h :: acc) tl in
  let rec equal l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | _, [] -> false
    | [], _ -> false
    | h1 :: tl1, h2 :: tl2 -> h1 = h2 && equal tl1 tl2 in
  let rev_l = rev [] l in
  equal l rev_l

type 'a node =
  | One of 'a
  | Many of 'a node list
  
(* Chal 7: Flatten a nested list structure. (medium) *)
let flatten tr = 

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
      ];
      "Tests for chal4" >::: [
        "elements"  >:: (fun _ -> assert_equal 3 (length [ "a" ; "b"; "c" ]));
        "empty"    >:: (fun _ -> assert_equal 0 (length []));
      ];
      "Tests for chal5" >::: [
        "elements"  >:: (fun _ -> assert_equal [ "c"; "b"; "a" ] (rev [ "a" ; "b"; "c" ]))
      ];
      "Tests for chal6" >::: [
        "is palindrome"  >:: (fun _ -> assert_equal true (is_palindrome [ "x" ; "a"; "m"; "a"; "x" ]));
        "is not palindrome" >:: (fun _ -> assert_equal false (is_palindrome [ "a"; "b" ]))
      ];
      "Tests for chal7" >::: [
        "tree"  >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten [ "x" ; "a"; "m"; "a"; "x" ]))
      ]
    ])

