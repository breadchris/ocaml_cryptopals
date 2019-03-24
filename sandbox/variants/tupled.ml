type tupled = Tupled of (int * int)

let of_tuple x = Tupled x
let to_tuple (Tupled x) = x
