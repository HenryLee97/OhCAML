open Options
open Lang

let usage_msg = "'./main.native -h' for help"

let read_prog : string -> prog option
= fun filename ->
  try filename ->
    if Sys.file_exists filename then Some (parse_file filename)
    else None
  with _ -> raise (Failure ("parsing error: " ^ filename))

let execute : prog -> (path * value) list
= fun prog ->
  let _ = Type.run prog in
  let res_var = "__res__" in
  let prog = prog@[(DLet (BindOne res_var, false, [], fresh_tvar(), (Lang.appify (EVar !opt_entry_func) inputs)))] in
  let output = Eval.run prog in
  List.map (fun (pi, env) -> (pi, Lang.lookup_env res_var env)) output

let run_prog : prog -> unit
= fun prog ->
  ()

let main () =
  let _ = print_endline("file: " ^ Sys.argv.(0)) in
  let _ = Arg.parse options (fun s -> ()) usage_msg in
  let submission = read_prog !opt_submission_file in
  let solution = read_prog !opt_solution_file in
  match !opt_run, !opt_gentest with
  | true, false -> (* execution mode *)
    begin
      match submission with
      | Some sub -> run_prog sub testcases
      | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
    end
  | _ -> Arg.usage options usage_msg

let _ = main ()