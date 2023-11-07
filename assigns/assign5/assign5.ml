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
type token =
  | TokInt of int
  | TokAdd
  | TokMul
  | TokLParen
  | TokRParen
  | TokEnd

let is_digit c = c >= '0' && c <= '9'

(* Convert a list of chars into a list of tokens *)
let rec tokenize = function
  | [] -> [TokEnd]
  | ' ' :: cs | '\n' :: cs | '\t' :: cs | '\r' :: cs -> tokenize (trim cs)
  | '(' :: cs -> TokLParen :: tokenize (trim cs)
  | ')' :: cs -> TokRParen :: tokenize (trim cs)
  | c :: cs when is_digit c ->
      let (num, rest) = span is_digit (c :: cs) in
      TokInt (int_of_string (implode num)) :: tokenize (trim rest)
  | 'a' :: 'd' :: 'd' :: cs -> TokAdd :: tokenize (trim cs)
  | 'm' :: 'u' :: 'l' :: cs -> TokMul :: tokenize (trim cs)
  | _ -> []  (* Invalid character found, so we return an empty list to indicate an error *)

and span p cs =
  let rec span' p (acc, cs) =
    match cs with
    | c :: cs when p c -> span' p (c :: acc, cs)
    | _ -> (acc, cs)
  in span' p ([], cs)

and implode cs = 
  let str = String.create (List.length cs) in
  let rec imp i = function
  | [] -> str
  | c :: cs -> str.[i] <- c; imp (i + 1) cs in
  imp 0 (List.rev cs)

  let rec parse_expr = function
  | TokInt n :: tokens -> (Int n, tokens)
  | TokLParen :: TokAdd :: tokens -> parse_add tokens
  | TokLParen :: TokMul :: tokens -> parse_mul tokens
  | _ -> raise (Failure "parse error")

and parse_add tokens =
  let rec parse_args acc tokens =
    match tokens with
    | TokRParen :: rest -> (Add (List.rev acc), rest)
    | _ ->
        let (expr, tokens') = parse_expr tokens in
        parse_args (expr :: acc) tokens'
  in
  parse_args [] tokens

and parse_mul tokens =
  let rec parse_args acc tokens =
    match tokens with
    | TokRParen :: rest -> (Mul (List.rev acc), rest)
    | _ ->
        let (expr, tokens') = parse_expr tokens in
        parse_args (expr :: acc) tokens'
  in
  parse_args [] tokens

let parse (s : string) : expr option = 
  let tokens = tokenize (string_listize s) in
  try Some (fst (parse_expr tokens))
  with Failure _ -> None