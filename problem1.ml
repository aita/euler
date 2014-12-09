let is_multiply_of_3_or_5 n = 
  n mod 3 == 0 || n mod 5 == 0

let rec problem1 n = 
  let rec loop = function
    | 0 -> []
    | n ->
       if is_multiply_of_3_or_5 n then 
         n::loop (n-1)
       else
         loop(n-1)
  in
  List.fold_left (+) 0 @@ loop (n-1)

let () =
  print_int @@ problem1 10;
  print_newline ();
  print_int @@ problem1 1000
  
  
