open Base
open Stdio

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
let rec fib n =
