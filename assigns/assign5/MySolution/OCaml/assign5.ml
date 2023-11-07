#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

let is_digit (c : char) : bool =
    match Char.code c with
    | 48 (* ASCII code for '0' *) 
    | 49 (* ASCII code for '1' *)
    | 50 (* ASCII code for '2' *)
    | 51 (* ASCII code for '3' *)
    | 52 (* ASCII code for '4' *)
    | 53 (* ASCII code for '5' *)
    | 54 (* ASCII code for '6' *)
    | 55 (* ASCII code for '7' *)
    | 56 (* ASCII code for '8' *)
    | 57 (* ASCII code for '9' *) -> true
    | _ -> false
    
let parse (s : string) : expr option =
    let rec parse_num (cs : char list) : expr option * char list =
      match cs with
      | [] -> None, cs
      | ' ' :: rest -> parse_num (trim rest)
      | c :: rest when is_digit c ->
        let num, rest' = parse_num_helper cs in
        Some (Int num), rest'
      | _ -> None, cs
    and parse_num_helper (cs : char list) : int * char list =
      let rec aux (cs : char list) (acc : string) : int * char list =
        match cs with
        | [] -> int_of_string acc, []
        | c :: rest when is_digit c -> aux rest (acc ^ Char.escaped c)
        | _ -> int_of_string acc, cs
      in
      aux cs ""
    and parse_add (cs : char list) : expr option * char list =
      match cs with
      | [] -> None, cs
      | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: rest ->
        let exprs, rest' = parse_exprs rest in
        (match exprs with
         | Some args -> Some (Add args), rest'
         | None -> None, cs)
      | _ -> None, cs
    and parse_mul (cs : char list) : expr option * char list =
      match cs with
      | [] -> None, cs
      | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: rest ->
        let exprs, rest' = parse_exprs rest in
        (match exprs with
         | Some args -> Some (Mul args), rest'
         | None -> None, cs)
      | _ -> None, cs
    and parse_exprs (cs : char list) : expr list option * char list =
      let rec aux (cs : char list) (acc : expr list) : expr list option * char list =
        match cs with
        | [] -> Some (List.rev acc), cs
        | ')' :: rest -> Some (List.rev acc), rest
        | _ ->
          let expr, rest' = parse_expr cs in
          (match expr with
           | Some e -> aux rest' (e :: acc)
           | None -> None, cs)
      in
      aux cs []
    and parse_expr (cs : char list) : expr option * char list =
      match cs with
      | [] -> None, cs
      | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: _ -> parse_add cs
      | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: _ -> parse_mul cs
      | _ -> parse_num cs
    in
  
    let char_list = string_listize s in
    match parse_expr (trim char_list) with
    | Some e, [] -> Some e
    | _ -> None