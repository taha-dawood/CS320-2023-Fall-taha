#use "./../assign0.ml";;

let isNeg (myStr: string): bool =
  if myStr.[0] = '-' then true else false
;;

(*************************************)

let rec str2int(cs: string): int =
  let strLen = string_length cs in
  if strLen = 0 then 0 else
    let lastCharStr = string_get(cs, strLen-1) in
    let ordVal = ord lastCharStr - 48 in
    let index = if isNeg(cs) = true then 1 else 0 in
      let addTostring = string_init (strLen-1) (fun i -> string_get(cs, index + i)) in
      let newString = str2int(addTostring) in
      if isNeg(cs) then -(((10*newString) + ordVal)) else (10*newString) + ordVal
  ;;