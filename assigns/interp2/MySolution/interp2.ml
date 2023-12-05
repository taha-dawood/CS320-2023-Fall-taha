#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
 String.of_seq (List.to_seq ls)

let rec append_lists l1 l2 =
 match l1 with
 | [] -> l2
 | h :: t -> h :: append_lists t l2


type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of string
  | Closure of string * (string*const)list * com list
  
and com =
  | Push of const | Pop | Swap 
  | Trace | Add | Sub | Mul
  | Div | And | Or | Not
  | Lt | Gt | If of com list * com list
  | Bind | Lookup | Fun of com list
  | Call | Return

and coms = com list
let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let parse_sym =
  let* s = ((satisfy (char_isletter)) >>= fun c -> many (satisfy char_isalphanum) >|= fun cs -> c::cs) in pure (Sym (implode s))
 
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_sym


let rec parse_com ()= 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return) <|>
  parse_ifelse() <|>
  parse_fun()
and parse_coms ()= many' (fun x -> parse_com x << keyword ";")
and parse_ifelse ()=
  keyword "If" >> parse_coms () >>= fun c1 ->
  keyword "Else" >> parse_coms () >>= fun c2 -> 
  keyword "End" >>
  pure (If (c1, c2))
and parse_fun ()=
  keyword "Fun" >> parse_coms () >>= fun c1 -> keyword "End" >> pure (Fun c1)

type stack = const list
type trace = string list
type env = (string * const) list
type prog = coms

let rec str_of_nat (n : int) : string =
 let d = n mod 10 in 
 let n0 = n / 10 in
 let s = str (chr (d + ord '0')) in 
 if 0 < n0 then
   string_append (str_of_nat n0) s
 else s

let str_of_int (n : int) : string = 
 if n < 0 then
   string_append "-" (str_of_nat (-n))
 else str_of_nat n

let toString (c : const) : string =
 match c with
 | Int i -> str_of_int i
 | Bool true -> "True"
 | Bool false -> "False"
 | Unit -> "Unit"
 | Sym s -> s
 | Closure (name, env, coms) -> string_append (string_append ("Fun<") (name) ) ">"

 let rec evaluate (stack : stack) (trace : trace) (environment: env) (program : prog) : trace =
  match program with
  | [] -> trace
  | Push constant :: rest_program  -> evaluate (constant :: stack) trace environment rest_program
  | Pop :: rest_program ->
    (match stack with
     | _ :: stack_tail  -> evaluate stack_tail trace environment rest_program
     | []               -> evaluate [] ("Panic" :: trace) environment [])
  | Trace :: rest_program ->
    (match stack with
     | constant :: stack_tail  -> evaluate (Unit :: stack_tail) (toString constant :: trace) environment rest_program
     | []                      -> evaluate [] ("Panic" :: trace) environment [])
  | Add :: rest_program ->
    (match stack with
     | Int first :: Int second :: stack_tail -> evaluate (Int (first + second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | Sub :: rest_program ->
    (match stack with
     | Int first :: Int second :: stack_tail -> evaluate (Int (first - second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | Mul :: rest_program ->
    (match stack with
     | Int first :: Int second :: stack_tail -> evaluate (Int (first * second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | Div :: rest_program ->
    (match stack with
     | Int first :: Int 0 :: stack_tail      -> evaluate [] ("Panic" :: trace) environment []
     | Int first :: Int second :: stack_tail -> evaluate (Int (first / second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | And :: rest_program ->
    (match stack with
     | Bool first_bool :: Bool second_bool :: stack_tail -> evaluate (Bool (first_bool && second_bool) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                              -> evaluate [] ("Panic" :: trace) environment []
     | []                                                -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                                           -> evaluate [] ("Panic" :: trace) environment [])
  | Or :: rest_program ->
    (match stack with
     | Bool first_bool :: Bool second_bool :: stack_tail -> evaluate (Bool (first_bool || second_bool) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                              -> evaluate [] ("Panic" :: trace) environment []
     | []                                                -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                                           -> evaluate [] ("Panic" :: trace) environment [])
  | Not :: rest_program ->
    (match stack with
     | Bool boolean :: stack_tail -> evaluate (Bool (not boolean) :: stack_tail) trace environment rest_program
     | _ :: stack_tail            -> evaluate [] ("Panic" :: trace) environment []
     | []                         -> evaluate [] ("Panic" :: trace) environment [])
  | Lt :: rest_program ->
    (match stack with
     | Int first :: Int second :: stack_tail -> evaluate (Bool (first < second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | Gt :: rest_program ->
    (match stack with
     | Int first :: Int second :: stack_tail -> evaluate (Bool (first > second) :: stack_tail) trace environment rest_program
     | _ :: _ :: stack_tail                  -> evaluate [] ("Panic" :: trace) environment []
     | []                                    -> evaluate [] ("Panic" :: trace) environment []
     | _ :: []                               -> evaluate [] ("Panic" :: trace) environment [])
  | Swap :: rest_program ->
    (match stack with
     | first :: second :: stack_list -> evaluate (second :: first :: stack_list) trace environment rest_program
     | []                            -> evaluate [] ("Panic" :: trace) environment []
     | first :: []                   -> evaluate [] ("Panic" :: trace) environment [])
  | Bind :: rest_program -> 
    (match stack with
     | Sym symbol :: value :: stack_tail -> evaluate stack_tail trace ((symbol, value) :: environment) rest_program
     | _                                 -> evaluate [] ("Panic" :: trace) environment [])
  | Lookup :: rest_program -> 
    (match stack with
     | Sym symbol :: stack_tail -> 
       let rec find env key =
          match env with
          | [] -> None
          | (key, value) :: rest -> if key = symbol then Some value else find rest symbol
       in
       (match find environment symbol with
          | Some value -> evaluate (value :: stack_tail) trace environment rest_program
          | None -> evaluate [] ("Panic" :: trace) environment [])
      | _ -> evaluate [] ("Panic" :: trace) environment [])
  | If (condition1, condition2) :: rest_program -> 
    (match stack with
     | Bool condition :: stack_tail -> if condition then evaluate stack_tail trace environment (append_lists condition1 rest_program) else evaluate stack_tail trace environment (append_lists condition2 rest_program)
     | _                            -> evaluate [] ("Panic" :: trace) environment [])
  | Fun condition1 :: rest_program ->
    (match stack with
     | Sym symbol :: stack_tail -> evaluate (Closure(symbol, environment, condition1) :: stack_tail) trace environment rest_program
     | _                        -> evaluate [] ("Panic" :: trace) environment [])
  | Call :: rest_program ->
    (match stack with
     | Closure (function_name, var, code) :: argument :: stack_tail -> evaluate (argument :: Closure(toString(argument),environment,rest_program) :: stack_tail) trace ((function_name,Closure(function_name,var,code)) :: var) code
     | _                                                             -> evaluate [] ("Panic" :: trace) environment [])
  | Return :: rest_program ->
    (match stack with
     | Closure (function_name, var, code) :: argument :: stack_tail -> evaluate (argument :: stack_tail) trace var code
     | _                                                            -> evaluate [] ("Panic" :: trace) environment [])

let interp (s : string) : string list option =
 match string_parse (whitespaces >> parse_coms ()) s with
 | Some (p, []) -> Some (evaluate [] [] [] p)
 | _ -> None

let read_file (fname : string) : string =
 let fp = open_in fname in
 let s = string_make_fwork (fun work ->
     try
       while true do
         work (input_char fp)
       done
     with _ -> ())
 in
 close_in fp; s

let interp_file (fname : string) : string list option =
 let src = read_file fname in
 interp src