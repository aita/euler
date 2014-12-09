let rec fibonacci_sequence limit = 
  let rec loop a b = 
    if a > limit then [a] else a::loop b (a+b)
  in
  loop 0 1

let is_even n = n mod 2 = 0

let probrem2 n = 
  let even_terms = List.filter is_even @@ fibonacci_sequence n in
  List.fold_left (+) 0 even_terms

let () =
  print_int @@ probrem2 4000000
    
