#use "./../assign1-1.ml";;
#use "./../../../classlib/OCaml/MyOcaml.ml";; 

(*let string_longest_ascend xs =
  let len = String.length xs in
  if len = 0 then
    ""
  else
    let rec find_longest i current longest =
      if i = len then
        if String.length current > String.length longest then
          current
        else
          longest
      else if xs.[i] >= xs.[i - 1] then
        find_longest (i + 1) (current ^ Char.escaped xs.[i]) longest
      else
        let new_longest =
          if String.length current > String.length longest then
            current
          else
            longest
        in
        find_longest (i + 1) (Char.escaped xs.[i]) new_longest
    in
    find_longest 1 (Char.escaped xs.[0]) ""*)

let rec merge_sorted_strings cs1 cs2 =
   match cs1, cs2 with
   | "", "" -> ""
   | "", cs2' -> cs2'
   | cs1', "" -> cs1'
   | _ ->
    let char1 = string_head cs1 in 
    let char2 = string_head cs2 in
    if char1 <= char2 then
      string_cons char1 (merge_sorted_strings (string_tail cs1) cs2)
    else
      string_cons char2 (merge_sorted_strings cs1 (string_tail cs2))
;;

let string_merge cs1 cs2 =
  let merged = merge_sorted_strings cs1 cs2 in 
  let filtered = string_filter merged (fun c -> true) in 
  string_concat_list [filtered]
;;



