#use "./../../assign3.ml";;
#use "./../../../../classib/OCaml/MyOCaml.ml";;

let list_subsets (xs: 'a list): 'a list list =
  let initial_acc = [[]] in
  let subsets = 
    List.fold_left
      (fun acc x ->
        let new_subsets = List.map (fun subset -> x :: subset) acc in
        acc @ new_subsets)
      initial_acc
      xs
  in
  subsets