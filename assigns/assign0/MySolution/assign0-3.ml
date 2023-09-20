#use "./../assign0.ml";;

(*let count_digits d = 
  let rec count_helper n count =
    if n = 0 then
      if d = 0 then 1
      else count
    else
      count_helper (n / 10) (count + 1)
    in
    count_helper (abs d) 0 

  let int2str i0 = 
    let num_digits = count_digits i0 in 
    let result = string_init num_digits (fun i ->
      let digit = (abs i0) / int_of_float (10. ** float_of_int (num_digits - i - 1)) mod 10 in
      chr (digit + ord '0')
    )
    in
      result*)

let append (c0: char) (cs: string): string = 
  string_init (string_length cs + 1) (fun i ->
    if i = 0 then c0 else string_get(cs, i - 1)
  )

let rec num2str2 (i: int) (s: string): string = 
  if i < 10 then 
    append (chr (ord '0' + i)) s
  else
    let n0 = i mod 10 in
    let rest_int = i / 10 in
    num2str2 rest_int (append (chr (ord '0' + n0)) s)

let num2str (i: int): string = 
  if i = 0 then "0"
  else
    num2str2 i ""

let int2str (i0: int): string =
  if i0 < 0 then
    append '-' (num2str (-i0))
  else
    num2str i0



