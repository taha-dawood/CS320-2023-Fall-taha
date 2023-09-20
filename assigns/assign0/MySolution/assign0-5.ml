#use "./../assign0.ml";;

let rec getChar (mystr: string) (index: int): char =
  string_get(mystr, index)

let rec stringrev(cs: string): string =
  let strLen = string_length cs in
  if strLen = 0 then "" 
  else
    let lastChar = strLen - 1 in
    let revstr = string_init strLen (fun i -> getChar cs (lastChar - i)) in revstr
