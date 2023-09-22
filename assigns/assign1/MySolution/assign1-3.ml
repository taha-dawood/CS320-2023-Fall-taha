#use "./../../../classlib/OCaml/MyOcaml.ml";; 

let string_avoid_132(cs) =
  let n = string_length(cs) in 

  let rec loop_k(i)(j)(k) =
    if k >= n then true
    else
      let a = string_get_at(cs)(i) in
      let b = string_get_at(cs)(j) in
      let c = string_get_at(cs)(k) in
      if a < c && c < b then false
      else loop_k(i)(j)(k+1)
  in

  let rec loop_j i j = 
    if j >= n then true 
    else
      if not (loop_k i j (j + 1)) then false
      else loop_j i (j + 1)
  in

  let rec loop_i i =
    if i >= n then true
    else 
      if not (loop_j i (i+1)) then false
      else loop_i (i +1)
  in
  loop_i 0
;;