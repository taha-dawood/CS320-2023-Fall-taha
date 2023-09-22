#use "./../assign1-1.ml";;
#use "./../../../classlib/OCaml/MyOcaml.ml";; 

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



