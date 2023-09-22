#use "./../../../classlib/OCaml/MyOcaml.ml";; 

let intrep_add(ds1: string)(ds2: string): string =
  let len1 = String.length ds1 in
  let len2 = String.length ds2 in

  (* Helper function to convert a character to an integer *)
  let char_to_int c = int_of_char c - int_of_char '0' in

  let rec add_strings idx1 idx2 carry result =
    if idx1 < 0 && idx2 < 0 then
      if carry = 1 then "1" ^ result
      else result
    else
      let digit1 = if idx1 >= 0 then char_to_int ds1.[idx1] else 0 in
      let digit2 = if idx2 >= 0 then char_to_int ds2.[idx2] else 0 in
      let sum = digit1 + digit2 + carry in
      let new_carry = sum / 10 in
      let new_result_digit = sum mod 10 in
      let new_result = string_of_int new_result_digit ^ result in
      add_strings (idx1 - 1) (idx2 - 1) new_carry new_result
  in

  add_strings (len1 - 1) (len2 - 1) 0 ""
;;