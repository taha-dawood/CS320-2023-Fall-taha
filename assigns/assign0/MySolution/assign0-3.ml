#use "./../assign0.ml";;

let int2str i0 =
  let rec convert_to_string n acc len =
    if n = 0 then
      if len = 0 then "0" else acc
    else
      let remainder = n mod 10 in
      let digit_char = char_of_int (48 + remainder) in
      let new_acc = String.make 1 digit_char in
      let new_len = len + 1 in
      let new_acc_len = String.length new_acc in
      let combined_len = new_len + new_acc_len in
      let combined_str = String.init combined_len (fun i ->
        if i = 0 then '-'
        else if i = 1 then ' '
        else if i < new_acc_len + 2 then String.get new_acc (i - 2)
        else String.get acc (i - new_acc_len - 2)
      ) in
      convert_to_string (n / 10) combined_str combined_len
  in
  if i0 < 0 then
    (convert_to_string (-i0) "" 0)
  else
    convert_to_string i0 "" 0
;;



