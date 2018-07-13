let opt_solution_filename = ref ""
let opt_submission_filename = ref ""
let opt_run = ref false
let opt_gentest = ref false
let opt_entry_func = ref ""

let options =
  [
    ("-r", Arg.Set opt_run, " Symbolic execute submission");
    ("--run", Arg.Set opt_run, " Symbolic execute submission");
    ("--solution", Arg.String (fun fname -> opt_solution_filename := fname), " Solution file");
    ("--submission", Arg.String (fun fname -> opt_submission_filename := fname), " Submission file");
    ("-t", Arg.Set opt_gentest, " Generate a counter example testcase");
    ("--gentest", Arg.Set opt_gentest, " Generate a counter example testcase");
  ]