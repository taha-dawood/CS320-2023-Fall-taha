#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* Define sexpr type *)
type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* Define parsers *)
let rec parse_sexpr () : sexpr parser =
  parse_Sint () <|> parse_Add () <|> parse_Mul ()

and parse_Sint () : sexpr parser =
  let* n = digit >>= fun d -> pure (int_of_string (String.make 1 d)) in
  pure (SInt n) << whitespaces

and parse_Add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_Mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SMul es)

let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_sexpr ()) s with
  | Some (e, []) -> Some e
  | _ -> None

(* Define print function *)
let rec sexpr_to_string (c: sexpr) : string =
  match c with
  | SInt d -> string_of_int d
  | SAdd hs -> "(add " ^ (String.concat " " (List.map sexpr_to_string hs)) ^ ")"
  | SMul hs -> "(mul " ^ (String.concat " " (List.map sexpr_to_string hs)) ^ ")"