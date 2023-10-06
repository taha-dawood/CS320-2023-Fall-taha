#use "./../../assign3.ml";;
#use "./../../../../classib/OCaml/MyOCaml.ml";;

let rec transpose_matrix (matrix: 'a list list) : 'a list list =
  match matrix with 
  | [] -> []
  | []::_->[]
  | _ ->
    let first_column = List.map List.hd matrix in
    let rest_columns = transpose_matrix (List.map List.tl matrix) in
    first_column :: rest_columns

let matrix_transpose (xss: 'a list list) : 'a list list =
  transpose_matrix xss