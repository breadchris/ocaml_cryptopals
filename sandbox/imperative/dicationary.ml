open Base

type ('a,'b) t = { mutable length: int;
                   buckets: ('a * 'b) list array;
                 }

let num_buckets = 17

let hash_bucket key = (Hashtbl.hash key) % num_buckets

let create () =
  { length = 0;
    buckets = Array.create ~len:num_buckets [];
  }

let length t = t.length

let find t key =
  List.find_map t.buckets.(hash_bucket key)
    ~f:(fun (key', data) -> if key' = key then Some data else None)

let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done
