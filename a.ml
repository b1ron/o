open Base
open Stdio

(* TODO setup Dune for automatic formatting *)

let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)

let rec sum l =
  match l with
  | [] -> 0 (* base case *)
  | hd :: tl -> hd + sum tl (* inductive case *)

  (* Logically, you can think of the evaluation of a simple recursive function like 
  sum almost as if it were a mathematical equation whose meaning you were unfolding step by step
  sum [1;2;3]
  = 1 + sum [2;3]
  = 1 + (2 + sum [3])
  = 1 + (2 + (3 + sum []))
  = 1 + (2 + (3 + 0))
  = 1 + (2 + 3)
  = 1 + 5
  = 6 *)

(* a recursive function to compute the n-th fibonacci number *)
(* TODO tail recursion is more interesting *)
let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2);;

let rec remove_sequential_duplicates list =
  match list with
  | [] -> []
  | [x] -> [x]
  | first :: second :: tl ->
    if first = second then
      remove_sequential_duplicates (second :: tl)
    else
    first :: remove_sequential_duplicates (second :: tl);;

type point2d = { x : float; y : float };;

(* field punning *)
let magnitude { x; y } = Float.sqrt (x **. 2. +. y **. 2.);;

(* dot notation *)
let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;
