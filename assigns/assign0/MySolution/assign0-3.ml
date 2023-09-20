#use "./../assign0.ml";;

let count_digits d = 
  let rec count_helper n count =
    if n = 0 then
      if d = 0 then 1
      else count
    else
      count_helper (n / 10) (count + 1)
    in
    count_helper (abs d) 0 

(* ****** ****** *)

  let int2str i0 = 
    let num_digits = count_digits i0 in 
    let result = string_init num_digits (fun i ->
      let digit = (abs i0) / int_of_float (10. ** float_of_int (num_digits - i - 1)) mod 10 in
      chr (digit + ord '0')
    )
    in
      result
;;



