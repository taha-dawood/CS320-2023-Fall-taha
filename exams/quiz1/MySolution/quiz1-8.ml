(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5 (a, b, c, d, e) =
  let input_list = [a; b; c; d; e] in
  let sorted_list = List.sort compare input_list in
  match sorted_list with
  | [x; y; z; w; v] -> (x, y, z, w, v)
  | _ -> failwith "Input list must contain exactly 5 integers"
  (* YOUR CODE *)


(* ************************************************ *)
