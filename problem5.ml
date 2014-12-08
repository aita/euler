let range n =
  let rec loop = function
    | 0 -> []
    | x -> x::loop (x-1)
  in
  List.rev @@ loop n  

let problem5 x =
  let xs = range x in
  let test n = List.for_all (fun a -> n mod a = 0) xs in
  let rec loop n =
    if test n then n else loop (n+x)
  in
  loop x

let () =
  print_int @@ problem5 20
