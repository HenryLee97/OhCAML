let rec lookup (x:'a) (l:('a * 'b) list) : 'b option =
  match l with
  | [] -> None
  | (y, v)::l if x = y then Some v else lookup x l