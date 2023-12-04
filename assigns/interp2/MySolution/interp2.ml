#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type value = 
  | Int of int
  | Bool of bool
  | Unit
  | Symbol of string

type command = 
  | Push of value
  | Pop
  | Add
  (* ... other commands ... *)

type state = {
  stack: value list;
  trace: string list;
  vars: (string * value) list;  (* For simplicity, a list is used for variable environment *)
}

let empty_state = {
  stack = [];
  trace = [];
  vars = [];
}

(* Example of a function to handle the Push command *)
let handle_push state value =
  {state with stack = value :: state.stack}

(* Example of a function to handle the Pop command *)
let handle_pop state =
  match state.stack with
  | _ :: tail -> {state with stack = tail}
  | [] -> {state with trace = "Panic" :: state.trace} (* Error handling *)

(* ... Implementations for other commands ... *)

(* Main function to execute a command *)
let exec_command state command =
  match command with
  | Push v -> handle_push state v
  | Pop -> handle_pop state
  | Add -> (* ... handle Add ... *)
  (* ... other commands ... *)

(* Assuming a parser function: parse : string -> command list *)
let parse (s: string): command list = (* ... *)

(* Main interpretation function *)
let interp (s: string) : string list option =
  let commands = parse s in
  let final_state = List.fold_left exec_command empty_state commands in
  if List.hd final_state.trace = "Panic"
  then None
  else Some final_state.trace