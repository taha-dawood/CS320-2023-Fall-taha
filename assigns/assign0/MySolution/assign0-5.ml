#use "./../assign0.ml";;

let rec getchar (mystr: string) (index: int): char =
  let mychar = mystr. [index] in mychar
let rec stringrev(cs: string): string =
  let strLen = string length cs in
  if strLen = 0 then "" else
    let lastchar = string_ length cs - 1 in
    let revstr = string init strLen (fun i -› (getchar cs (lastChar - i))) in revstr