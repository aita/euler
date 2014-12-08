let is_palindromic x =
  let s = string_of_int x in
  let len = String.length s in
  let rec loop m =
    let a, b = s.[m], s.[len-m-1] in
    if len / 2 < m then
      true
    else
      if a = b then loop (m+1) else false
  in
  loop 0

let power_of_10 x =
  int_of_float @@ 10. ** (float_of_int (x-1))

let products digits =
  let s = (power_of_10 (digits+1)) - 1 in
  let limit = power_of_10 digits in
  let rec loop m n xs =
    match m, n with
    | _, b when b < limit -> xs
    | a, b when a < limit -> loop s (b-1) xs
    | a, b -> loop (a-1) b ((a,b)::xs)
  in
  loop s s []

let problem4 digits =
  let xs = products digits in
  let ys = List.filter (fun (a,b) -> is_palindromic (a*b)) xs in
  let palindromics = List.map (fun (a,b) -> a*b) ys in
  List.fold_left max 0 palindromics

let () =
  print_int @@ problem4 3
