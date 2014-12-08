let is_prime n primes =
  List.for_all (fun x -> n mod x != 0) primes

let nth_prime nth =
  let rec loop n xs =
    if List.length xs = nth then
      List.hd xs
    else
      loop (n+2) (if is_prime n xs then n::xs else xs)
  in
  loop 3 [2]

let () =
  print_int @@ nth_prime 10001
