let opt_trg_filename = ref ""
let opt_sol_filename = ref ""
let opt_run = ref ""
let opt_gen_counter_example = ref false
let opt_help = ref false

let options =
  [
    ("--target", Arg.String (fun fname -> opt_trg_filename := fname), " target file");
    ("-t", Arg.String (fun fname -> opt_trg_filename := fname), " target file");
    ("--solutoin", Arg.String (fun fname -> opt_sol_filename := fname), " solution file");
    ("-s", Arg.String (fun fname -> opt_sol_filename := fname), " solution file");
    ("--run", Arg.String (fun fname -> opt_run := fname), " run symbolic exectuion");
    ("-r", Arg.String (fun fname -> opt_run := fname), " run symbolic exectuion");
    ("--counter", Arg.Set opt_gen_counter_example, " counter example generation");
    ("-c", Arg.Set opt_gen_counter_example, " counter example generation");
    ("--help", Arg.Set opt_help, " help");
    ("-h", Arg.Set opt_help, " help")
  ]
  |> Arg.align

