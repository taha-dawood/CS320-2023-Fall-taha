#use "./../assign0.ml";;

let rec getChar (myStr: string) (index: int): char =
  let myChar = myStr.[index] in myChar
;;
let rec stringrev(cs: string): string =
  let strLen = string_length cs in
  if strLen = 0 then "" 
  else
    let lastChar = string_length cs - 1 in
    let revStr = string_init strLen (fun i -> (getChar cs (lastChar - i))) in revStr
;;