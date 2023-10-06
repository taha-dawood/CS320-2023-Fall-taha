#use "./../../assign3.ml";;
#use "./../../../../classib/OCaml/MyOCaml.ml";;

let list_nchoose (xs: 'a list) (n0: int) : 'a list list =
  let first = [[]] in 
  let append acc x = 
    List.concat_map (fun subseq ->
      if List.length subseq = n0 then [subseq] else [x :: subseq; subseq]
      ) acc
    in
    let res = List.fold_left append first xs in
    List.filter (fun subseq -> List.length subseq = n0) res