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
and parse_ifelse ()=| _ :: a :: s0 -> eval [] ("Panic" :: t) e []
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

let rec eval (s : stack) (t : trace) (e: env) (p : prog) : trace =
 match p with
 | [] -> t
 | Push c :: p0  -> eval (c :: s) t e p0
 | Pop :: p0 ->
   (match s with
    | _ :: s0  -> eval s0 t e p0
    | []       -> eval [] ("Panic" :: t) e [])
 | Trace :: p0 ->
   (match s with
    | c :: s0  -> eval (Unit :: s0) (toString c :: t) e p0
    | []       -> eval [] ("Panic" :: t) e [])
 | Add :: p0 ->
   (match s with
    | Int i :: Int j :: s0   -> eval (Int (i + j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | Sub :: p0 ->
   (match s with
    | Int i :: Int j :: s0   -> eval (Int (i - j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | Mul :: p0 ->
   (match s with
    | Int i :: Int j :: s0   -> eval (Int (i * j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | Div :: p0 ->
   (match s with
    | Int i :: Int 0 :: s0  -> eval [] ("Panic" :: t) e []
    | Int i :: Int j :: s0   -> eval (Int (i / j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | And :: p0 ->
   (match s with
    | Bool a :: Bool b :: s0   -> eval (Bool (a && b) :: s0) t e p0
    | _ :: _ :: s0            -> eval [] ("Panic" :: t) e []
    | []                      -> eval [] ("Panic" :: t) e []
    | _ :: []                 -> eval [] ("Panic" :: t) e [])
 | Or :: p0 ->
   (match s with
    | Bool a :: Bool b :: s0   -> eval (Bool (a || b) :: s0) t e p0
    | _ :: _ :: s0            -> eval [] ("Panic" :: t) e []
    | []                      -> eval [] ("Panic" :: t) e []
    | _ :: []                 -> eval [] ("Panic" :: t) e [])
 | Not :: p0 ->
   (match s with
    | Bool a :: s0  -> eval (Bool (not a) :: s0) t e p0
    | _ :: s0       -> eval [] ("Panic" :: t) e []
    | []            -> eval [] ("Panic" :: t) e [])
 | Lt :: p0 ->
   (match s with
    | Int i :: Int j :: s0   -> eval (Bool (i < j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | Gt :: p0 ->
   (match s with
    | Int i :: Int j :: s0   -> eval (Bool (i > j) :: s0) t e p0
    | _ :: _ :: s0          -> eval [] ("Panic" :: t) e []
    | []                    -> eval [] ("Panic" :: t) e []
    | _ :: []               -> eval [] ("Panic" :: t) e [])
 | Swap :: p0 ->
   (match s with
    | x :: y :: lst -> eval  (y :: x :: lst) t e p0
    | []  -> eval [] ("Panic" :: t) e []
    | x :: []  -> eval [] ("Panic" :: t) e [])
 | Bind :: p0 -> 
   (match s with
    | Sym x :: v :: s0 -> eval s0 t ((x, v) :: e) p0
    | _ -> eval [] ("Panic" :: t) e [])
 | Lookup :: p0 -> 
   (match s with
    | Sym x :: s0 -> 
     let rec find env (key) =
        match env with
        | [] -> None
        | (k, v) :: rest -> if k = key then Some v else find rest key
     in
     (match find e x with
        | Some v -> eval (v :: s0) t e p0
        | None -> eval [] ("Panic" :: t) e [])
    | _ -> eval [] ("Panic" :: t) e [])
 | If (c1, c2) :: p0 -> 
   (match s with
    | Bool b :: s0 ->  if b then eval s0 t e (append_lists c1 p0) else eval s0 t e (append_lists c2 p0)
    | _ -> eval [] ("Panic" :: t) e [])
 | Fun c1 :: p0 ->
   (match s with
    | Sym x :: s0 -> eval (Closure(x, e, c1) :: s0) t e p0
    | _ -> eval [] ("Panic" :: t) e [])
 | Call :: p0 ->
   (match s with
    | Closure (f, v, c) :: a :: s0 -> eval (a :: Closure(string_append (string_append "cc_foo(" (toString(a))) ")",e,p0) :: s0) t ((f,Closure(f,v,c)) :: v) c
    | _ -> eval [] ("Panic" :: t) e [])
 | Return :: p0 ->
   (match s with
    | Closure (f, v, c) :: a :: s0 -> eval (a :: s0) t v c
    | _ -> eval [] ("Panic" :: t) e [])

let interp (s : string) : string list option =
 match string_parse (whitespaces >> parse_coms ()) s with
 | Some (p, []) -> Some (eval [] [] [] p)
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