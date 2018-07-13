open Lang

let rec string_of_path : path -> string
= fun p ->
  match p with
  | _ -> ""

let rec string_of_value : value -> string
= fun v ->
  match v with
  | _ -> ""

let rec print_output : (path * value) list -> unit
= fun xs ->
  match xs with
  | [] -> ()
  | (p, v)::tl ->
    print_endline ("path: " ^ string_of_path p);
    print_endline ("value: " ^ string_of_value v);
    print_output tl