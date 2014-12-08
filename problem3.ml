let problem3 n =
  let limit = int_of_float @@ sqrt @@ float_of_int n in
  let rec loop m xs ys =
    if m > limit then
      ys
    else
      if not @@ List.exists (fun x -> m mod x = 0) xs then
        loop (m+2) (m::xs)
             (if n mod m = 0 then m::ys else ys)
      else
        loop (m+2) xs ys
  in
  let prime_factors =
    loop 3 [2] (if n mod 2 = 0 then [2] else [])
  in
  List.hd prime_factors

let () =
  (* print_int @@ problem3 13195; *)
  print_int @@ problem3 600851475143;
  print_newline ()
