let rec rev_range = function
  | 0 -> []
  | x -> x::rev_range (x-1)

let rec sum_of_squares = function
  | 0 -> 0
  | n -> n * n + (sum_of_squares (n-1))

let rec square_of_sum n = 
  let xs = rev_range n in
  let sum = List.fold_left (+) 0 xs in
  sum * sum

let problem6 n =
  let a = sum_of_squares n in
  let b = square_of_sum n in
  b - a

let () =
  print_int @@ problem6 100
            
  
  
