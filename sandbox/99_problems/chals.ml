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
    | _, [] | [], _ -> false
    | h1 :: tl1, h2 :: tl2 -> h1 = h2 && equal tl1 tl2 in
  let rev_l = rev [] l in
  equal l rev_l
  
(* Chal 7: Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten tr = match tr with
  | [] -> []
  | One e :: tl -> e :: flatten tl
  | Many e :: tl -> flatten e @ flatten tl

(* Alternative solution to this challenge
let flatten_alt l =
  let rec aux acc = function
  | [] -> acc
  | One x :: t -> aux (x :: acc) t
  | Many x :: t -> aux (aux acc x) t in
  List.rev (aux [] l)
*)

(* Chal 8: Eliminate consecutive duplicates of list elements. *)
let compress l =
  let rec compress' prev l' =
    match l' with
    | [] -> []
    | e :: tl ->
      if e <> prev then e :: (compress' e tl)
      else compress' prev tl
  in
  match l with
  | [] -> []
  | e :: tl -> e :: compress' e tl

(* Alternative
let rec compress_alt = function
  | a :: (b :: t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller
*)

(* Chal 9: Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack list =
  let rec pack' prev l cur =
    match l with
    | [] -> [cur]
    | e :: tl -> 
      if e = prev then pack' prev tl (e :: cur)
      else cur :: pack' e tl [e]
  in
  match list with
  | [] -> []
  | e :: tl -> pack' e tl [e]

(*
let pack_alt list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: tl) ->
      if a = b then aux (a :: current) acc tl
      else aux [] (current :: acc) tl in
  List.rev (aux [] [] list)
*)

(* Chal 10: Run-length encoding of a list. (easy) *)
let encode list =
  let rec encode' n cur run l = 
    let pair = (n, cur) in
    match l with
    | [] -> pair :: run
    | e :: tl ->
      if e = cur then encode' (n + 1) cur run tl
      else encode' 1 e (pair :: run) tl in
  match list with
  | [] -> []
  | e :: tl -> List.rev (encode' 1 e [] tl)

(*
let encode_alt list =
  let rec encode' count acc = function
  | [] -> []
  | [x] -> (count + 1, x) :: acc
  | a :: (b :: _ as tl) ->
    if a = b then encode' (count + 1) acc tl
    else encode' 0 ((count + 1, x) :: acc) tl in
  List.rev (encode' 0 [] list)
*)
type 'a rle =
  | One of 'a
  | Many of int * 'a

(* Chal 11: Modified run-length encoding. (easy) *)
let mod_encode list =
  let get_type count e =
    match count with
    | 1 -> One e
    | _ -> Many (count, e) in
  let rec mod_encode' count acc = function
    | [] -> acc
    | [e] -> (One e) :: acc
    | a :: (b :: _ as tl) -> 
      if a = b then mod_encode' (count + 1) acc tl
      else mod_encode' 0 ((get_type (count + 1) a)::acc) tl in
    List.rev (mod_encode' 0 [] list)

(* Chal 12: Decode a run-length encoded list. (medium) *)
let decode list =
  let rec append_nchars n c acc =
    match n with
    | 0 -> acc
    | _ -> append_nchars (n-1) c (c::acc)
  in
  let rec decode' acc list =
    match list with
    | [] -> acc
    | Many(n, c)::tl -> decode' (append_nchars n c acc) tl
    | One(c)::tl -> decode' (c::acc) tl
  in 
  decode' [] (List.rev list)

let decode_forward list =
  let rec nchars n c acc =
    match n with
    | 0 -> acc
    | _ -> nchars (n-1) c (c::acc)
  in
  let rec decode' list =
    match list with
    | [] -> []
    | Many(n,c)::tl -> nchars n c (decode' tl)
    | One(c)::tl -> c::(decode' tl)
  in 
  decode' list

(* Chal 13: Run-length encoding of a list (direct solution). (medium) *)
let run_len_encode list =
  let get_type n x = if n = 0 then One x else Many (n + 1, x) in
  let rec run_len_encode' count acc = function
    | [] -> []
    | [x] ->(get_type count x)::acc
    | a :: (b :: _ as tl) ->
      if a = b then run_len_encode' (count + 1) acc tl
      else run_len_encode' 0 (get_type count a::acc) tl in
  List.rev (run_len_encode' 0 [] list)

let run = true

let () =
  if not run then () else
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
        "tree"  >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ]]))
      ];
      "Tests for chal8" >::: [
        "repeats"  >:: (fun _ -> assert_equal ["a";"b";"c";"a";"d";"e"] (compress  ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];))
      ];
      (* TODO: Implement a recursive compare
      "Tests for chal9" >::: [
        "some elements"  >:: (fun _ -> assert_equal [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] (pack  ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]))
      ]
      *)
      "Tests for chal10" >::: [
        "encode"  >:: (fun _ -> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
 )
      ];
      "Tests for chal11" >::: [
        "encode mod"  >:: (fun _ -> assert_equal [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] (mod_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]))
      ];
      "Tests for chal12" >::: [
        "encode mod"  >:: (fun _ -> assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]))
      ]
    ])
