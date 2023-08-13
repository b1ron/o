open Base

let first_if_true test x y =
  if test x then x else y

let big_number x = x > 3;;

first_if_true big_number 4 3 
