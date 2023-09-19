#use "./../assign0.ml";;

let isPrime n0 =
  if n0 <= 1 then
    false
  else if n0 <= 3 then
    true
  else
    let rec is_divisible_by d =
      if d * d > n0 then
        true
      else if n0 mod d = 0 then
        false
      else
        is_divisible_by (d + 2)
    in
    n0 mod 2 <> 0 && is_divisible_by 3
;;