open Util
open Lang
open Sym_lang
open Sym_eval
open Options

(* simple symbolic eval *)
let run : prog -> unit
= fun pgm ->
  let v = sym_eval pgm in
  print_endline (value_to_string v)

let usage_msg = "'main.native -h' for help"

let main () =
  let _ = Arg.parse options (fun s -> ()) usage_msg in
  if !opt_help then begin
    print_endline ("OhCAML: OhCAML is Checking Assistant for ML");
    print_endline ("Usage: main.native <options>");
    print_endline ("");
    print_endline ("options are:");
    print_endline ("    -h, --help              help");
    print_endline ("    -r, --run <file>        symbolic execution");
    print_endline ("    -s, --solution <file>   compare with 'target' file");
    print_endline ("    -t, --target <file>     compare with 'solution' file");
    print_endline ("    -c, --counter           make counter example")
    print_endlien ("                            that make the different output between 'solution' and 'target'")
  end else
  let pgm =
    if !opt_run = "" then None
    else Some (
      let file_channel = open_in !opt_run in
      let lexbuf = Lexing.from_channel file_channel in
      Parser.program Lexer.start lexbuf
    )
  in
  let solution =
    if !opt_sol_filename = "" then None
    else Some (
      let file_channel = open_in !opt_run in
      let lexbuf = Lexing.from_channel file_channel in
      Parser.program Lexer.start lexbuf  
    )
  in
  let target =
    if !opt_trg_filename = "" then None
    else Some (
      let file_channel = open_in !opt_trg_filename in
      let lexbuf = Lexing.from_channel file_channel in
      Parser.program Lexer.start lexbuf
    )
  in
  match pgm, target, solution with
  | Some e, None, None -> run e
  | _ -> print_endline ("Please check the arguments are correct"); print_endline (usage_msg)

let _ = main ()