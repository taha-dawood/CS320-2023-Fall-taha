#use "./../../../classlib/OCaml/MyOcaml.ml";; 

let string_avoid_132(cs: string): bool =
  let length = String.length cs in

  (* Helper function to check if a character is a digit *)
  let is_digit c = '0' <= c && c <= '9' in

  (* Helper function to check if a subsequence is 132-like *)
  let is_132_like a b c =
    is_digit a && is_digit b && is_digit c && a < c && c < b
  in

  let rec check_avoid index a b =
    if index >= length then true (* Reached the end of the string without finding a 132-like subsequence *)
    else
      let c = cs.[index] in
      if is_132_like a b c then false (* Found a 132-like subsequence, return false *)
      else
        (* Continue checking with the next character *)
        check_avoid (index + 1) b c
  in

  match length with
  | 0 | 1 | 2 -> true (* Empty or short strings are 132-avoid *)
  | _ -> check_avoid 2 cs.[0] cs.[1] (* Start checking from the third character *)

;;