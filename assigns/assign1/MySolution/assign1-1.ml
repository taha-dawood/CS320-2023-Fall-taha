let intrev10 n =
  let rec tail_recursive_helper n reversed_num =
    if n = 0 then
      reversed_num
    else
      let last_digit = n mod 10 in
      let new_reversed_num = reversed_num * 10 + last_digit in
      tail_recursive_helper (n / 10) new_reversed_num
  in
  tail_recursive_helper n 0
