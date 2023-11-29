#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)


type const = I of int | B of bool | U
type com = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt 
type prog = com list
type stack = const list
type trace = string list
let is_lower_case c =
  'a' <= c && c <= 'z'
let is_upper_case c =
  'A' <= c && c <= 'Z'
let is_alpha c =
  is_lower_case c || is_upper_case c
let is_digit c =
  '0' <= c && c <= '9'
let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c
let is_blank c =
  String.contains " \012\n\r\t" c
let explode s =
  List.of_seq (String.to_seq s)
let implode ls =
  String.of_seq (List.to_seq ls)
let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res
  (***To capitilize boolean outputs*)
  let boolToString (b : bool) : string =
    if b then "True" else "False"
  
  (***ToString for Trace command*)
  let toString (c : const) : string =
    match c with
    | I i -> string_of_int i
    | B b -> boolToString b
    | U -> "Unit"
  
(* end of util functions *)
(* parser combinators *)
type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)
let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)
let fail : 'a parser = fun ls -> None
let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None
let (>>=) = bind
let (let*) = bind
let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None
let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None
let char (c : char) : char parser =
  satisfy (fun x -> x = c)
let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None
let (>>) = seq
let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None
let (<<) = seq'
let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls
let (<|>) = disj
let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None
let (>|=) = map
let (>|) = fun p c -> map p (fun _ -> c)
let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)
let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None
let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)
let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None
let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None
let ws : unit parser =
  (many whitespace) >| ()
let ws1 : unit parser =
  (many1 whitespace) >| ()
let digit : char parser =
  satisfy is_digit
let option (p : 'a parser) : 'a option parser =
  fun ls ->
    match p ls with
    | Some (x, ls') -> Some (Some x, ls')
    | None -> Some (None, ls)
(***End of parser combinators*)
  
  let digit : int parser =
    let* d = satisfy is_digit in
    pure (int_of_char d - int_of_char '0')
  
  let nat : int parser =
    let* ds = many1 digit in
    pure (List.fold_left (fun n d -> 10 * n + d) 0 ds)
  
  let integer : int parser =
    let* sign = option (char '-') in
    let* n = nat in
    pure (match sign with Some _ -> -n | None -> n)
  
  let bool : bool parser =
    (keyword "True" >| true) <|> (keyword "False" >| false)
  
  let const : const parser =
    (integer >|= fun i -> I i) <|>
    (bool >|= fun b -> B b) <|>
    (keyword "Unit" >| U)
    (**Commands parser -> also eliminates trailing ws*)
    let com : com parser =
      (ws >> (let* _ = keyword "Push" in
               let* c = const in
               pure (Push c))) <|>
      (ws >> keyword "Pop" >| Pop) <|>
      (ws >> keyword "Trace" >| Trace) <|>
      (ws >> keyword "Add" >| Add) <|>
      (ws >> keyword "Sub" >| Sub) <|>
      (ws >> keyword "Mul" >| Mul) <|>
      (ws >> keyword "Div" >| Div) <|>
      (ws >> keyword "And" >| And) <|>
      (ws >> keyword "Or" >| Or) <|>
      (ws >> keyword "Not" >| Not) <|>
      (ws >> keyword "Lt" >| Lt) <|>
      (ws >> keyword "Gt" >| Gt)

      let rec parse_commands ls =
         match com ls with
         | Some (command, remaining_ls) ->
           (match char ';' remaining_ls with
            | Some (_, post_semicolon_ls) ->
              (match parse_commands post_semicolon_ls with
               | Some (commands, final_ls) -> Some (command :: commands, final_ls)
               | None -> Some ([command], remaining_ls))
            | None -> Some ([command], remaining_ls))
         | None -> None
           
       and parse_commands_with_ws ls =
         match parse_commands ls with
         | Some (commands, remaining_ls) ->
           (match ws remaining_ls with
            | Some (_, post_whitespace_ls) -> Some (commands, post_whitespace_ls)
            | None -> Some (commands, remaining_ls))
         | None -> None
       
    
         let interp (input_string : string) : string list option =
            match parse (ws >> parse_commands_with_ws) input_string with
            | Some (parsed_commands, remaining_input) ->
              let rec execute commands stack trace =
                match commands, stack with
                | [], _ -> Some trace
                | Push value :: remaining_commands, current_stack ->
                  execute remaining_commands (value :: current_stack) trace
                | Pop :: remaining_commands, _ :: updated_stack ->
                  execute remaining_commands updated_stack trace
                | Pop :: remaining_commands, [] ->
                  return_panic remaining_commands [] trace
                | Trace :: remaining_commands, top_value :: updated_stack ->
                  let updated_trace = (toString top_value) :: trace in
                  execute remaining_commands (U :: updated_stack) updated_trace
                | Trace :: remaining_commands, [] ->
                  return_panic remaining_commands [] trace
                | Add :: remaining_commands, I val1 :: I val2 :: updated_stack ->
                  execute remaining_commands (I (val1 + val2) :: updated_stack) trace
                | Sub :: remaining_commands, I val1 :: I val2 :: updated_stack ->
                  execute remaining_commands (I (val1 - val2) :: updated_stack) trace
                | Mul :: remaining_commands, I val1 :: I val2 :: updated_stack ->
                  execute remaining_commands (I (val1 * val2) :: updated_stack) trace
                | Div :: remaining_commands, I val1 :: I val2 :: updated_stack ->
                  if val2 != 0 then execute remaining_commands (I (val1 / val2) :: updated_stack) trace 
                  else return_panic remaining_commands stack trace
                (* ... other cases for And, Or, Not, Lt, Gt ... *)
                | _ -> return_panic commands stack trace
              and return_panic commands stack trace =
                Some ("Panic" :: trace)  (* Only returns panic if one of the panic cases are reached *)
              in
              execute parsed_commands [] []
            | _ -> None