#use "./../../assign3.ml";;
#use "./../../../../classib/OCaml/MyOCaml.ml";;

let list_of_buddies(word: string) string list =
  let n = string_length word in 
  let buddiesComb = ref [] in
  for i = 0 to n - 1 do
    let left = if i = 0 then "" else String.sub word 0 i in
    let right = if i = n - 1 then "" else String.sub word (i + 1) (n - i - 1) in
    for c = ord 'a' to ord 'z' do 
      let neighbor = left ^ Char.escaped (chr c) ^ right in
      if neighbor <> word then buddiesComb := neighbor:: !buddiesComb
    done
  done;
  !buddiesComb
