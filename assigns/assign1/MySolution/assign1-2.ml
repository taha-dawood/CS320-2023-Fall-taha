#use "./../../../classlib/OCaml/MyOcaml.ml";; 

let string_merge(cs1: string) (cs2: string): string =
  let result = string_make_fwork (fun add_char ->
    let rec merge_strings s1 s2 =
      match (s1, s2) with
      | ("", "") -> ()
      | ("", s) -> String.iter add_char s
      | (s, "") -> String.iter add_char s
      | (c1::rest1, c2::rest2) ->
          if Char.code c1 <= Char.code c2 then (
            add_char c1;
            merge_strings rest1 s2
          ) else (
            add_char c2;
            merge_strings s1 rest2
          )
    in
    merge_strings (String.to_list cs1) (String.to_list cs2)
  )
  in
  result
;;
