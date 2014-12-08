let is_prime n primes =
  List.for_all (fun x -> n mod x != 0) primes

let primes n =
  let rec loop x xs =
    if x > n then
      xs
    else
      loop (x+2) (if is_prime x xs then x::xs else xs)
  in
  loop 3 [2]

let sum = List.fold_left (+) 0

let problem10 n = sum @@ primes n

let () =
  print_int @@ problem10 2000000
  
