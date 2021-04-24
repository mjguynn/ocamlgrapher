open OUnit2

let default_bounds = (-5.0, 5.0)

let default_steps = 100

let default_out = "out.svg"

let dummy_stdin contents cxt =
  let name, out = bracket_tmpfile cxt in
  output_string out contents;
  close_out out;
  open_in name

let test_config_base
    x_bounds
    y_bounds
    steps
    output_file
    input_override
    name
    argv
    func
    expect =
  name >:: fun ctxt ->
  let chan =
    match input_override with
    | None -> stdin
    | Some contents -> dummy_stdin contents ctxt
  in
  assert_equal expect
    ( Array.append [| "./ocamlgrapher" |] argv
    |> Config.from_cmdline x_bounds y_bounds steps output_file chan
    |> func )

let test_config
    ?x_bounds:(xs = default_bounds)
    ?y_bounds:(ys = default_bounds)
    ?output_file:(o = default_out)
    ?steps:(s = default_steps)
    ?input_override:(io = None)
    name
    argv
    func
    expect =
  test_config_base xs ys s o io name argv
    (fun x -> Result.get_ok x |> func)
    expect

let test_config_error
    ?x_bounds:(xs = default_bounds)
    ?y_bounds:(ys = default_bounds)
    ?output_file:(o = default_out)
    ?steps:(s = default_steps)
    ?input_override:(io = None)
    name
    argv =
  test_config_base xs ys s o io name argv
    (function Ok _ -> true | Error _ -> false)
    false

let test_simple name =
  test_config ("Simple Valid Config - " ^ name) [| "y=12x+4" |]

(* I would write a helper function to transform a string into an argv
   array, but the logic for doing that is *really* complicated...*)

let cfg_xy cfg = (Config.x_bounds cfg, Config.y_bounds cfg)

let test_mode name flag =
  test_config
    ("Config w/ Command " ^ name)
    [| "y=x"; flag |] Config.command

let suite =
  "ocamlgrapher [Config] test suite"
  >:::
  let open Config in
  [
    (* Valid Cases *)
    test_simple "Equation" equations [ "y=12x+4" ];
    test_simple "Command" command Graph;
    test_simple "Default X Bounds" x_bounds default_bounds;
    test_simple "Default Y Bounds" y_bounds default_bounds;
    test_simple "Output File" output_file default_out;
    test_config "Config w/ Changed Default Output File"
      ~output_file:"test.txt" [| "y=12x" |] output_file "test.txt";
    test_config "Config w/ Output File 1"
      [| "-o"; "test.txt"; "y=x" |]
      output_file "test.txt";
    test_config "Config w/ Output File 2"
      [| "-otest.txt"; "y=x" |]
      output_file "test.txt";
    test_config "Config w/ Output File 3"
      [| "y=x"; "--output=test.txt" |]
      output_file "test.txt";
    test_config "Config w/ Output File 4"
      [| "y=x"; "-o"; "test.txt" |]
      output_file "test.txt";
    test_config "Config w/ Changed Default Bounds (program)" [| "y=x" |]
      ~x_bounds:(2.0, 2.5) ~y_bounds:(-12.0, 2.0) cfg_xy
      ((2.0, 2.5), (-12.0, 2.0));
    test_config "Config w/ Changed Bounds (cmdline)"
      [|
        "y=x"; "--x-min=7"; "--x-max=12"; "--y-min=-6.72"; "--y-max=12";
      |]
      cfg_xy
      ((7., 12.), (-6.72, 12.));
    test_config "Config w/ Changed Default Steps (program)" [| "y=x" |]
      ~steps:200 steps 200;
    test_config "Config w/ Changed Steps (cmdline)"
      [| "y=x"; "--quality=420" |]
      steps 420;
    test_mode "graph" "-g" Graph;
    test_mode "points" "-p" Points;
    test_mode "roots" "-r" Roots;
    test_mode "extrema" "-e" Extrema;
    test_config "Config w/ short option chaining"
      [| "-potest.txt"; "y=x" |]
      (fun cfg -> (command cfg, output_file cfg))
      (Points, "test.txt");
    (* Advanced Valid Cases *)
    test_config "Config w/ --"
      [| "--x-max=7"; "--y-max=12"; "--"; "-x-4=y" |]
      equations [ "-x-4=y" ];
    test_config "Multiple Equations (with --)"
      [| "--"; "y=x"; "y=2x" |]
      equations [ "y=x"; "y=2x" ];
    test_config "Multiple Equations" [| "y=x"; "y=2x" |] equations
      [ "y=x"; "y=2x" ];
    test_config "Config w/ equation (read from input channel)"
      ~input_override:(Some "y=12x^2\n") [| "--x-min=2"; "-" |]
      equations [ "y=12x^2" ];
    test_config "Config w/ multiple equations (read from input channel)"
      ~input_override:(Some "y=12x^2\ny=2x+4ln(x)\n")
      [| "--x-min=2"; "-" |] equations
      [ "y=12x^2"; "y=2x+4ln(x)" ];
    (*Intentionally Failing Cases*)
    test_config_error "Unknown Short Flag" [| "-t"; "y=x" |];
    test_config_error "Unknown Long Flag" [| "--test"; "y=x" |];
    test_config_error "Unknown Short Option" [| "-t2"; "y=x" |];
    test_config_error
      "Unknown Short Option, chained w/ valid short option"
      [| "-totest.txt"; "y=x" |];
    test_config_error "Unknown Long Option" [| "--test=2"; "y=x" |];
    test_config_error "Short Option w/ no param" [| "y=x"; "-o" |];
    test_config_error "Long Option w/ no param" [| "y=x"; "--x-min" |];
    test_config_error "Long Option w/ no param but an equals"
      [| "y=x"; "--x-min=" |];
    test_config_error "Invalid x Bounds"
      [| "y=x"; "--x-min=7"; "--x-max=6" |];
    test_config_error "Invalid x Min (string)"
      [| "y=x"; "--x-min=THE" |];
    test_config_error "Invalid x Min (-inf)" [| "y=x"; "--x-min=-inf" |];
    test_config_error "Invalid x Min (NaN)" [| "y=x"; "--x-min=NaN" |];
    test_config_error "Invalid x Max (string)"
      [| "y=x"; "--x-max=BUSTA" |];
    test_config_error "Multiply Defined X Bounds"
      [| "y=x"; "--x-min=7"; "--x-max=8"; "--x-min=6" |];
    test_config_error "Multiply Defined Y Bounds"
      [| "y=x"; "--y-min=7"; "--y-max=8"; "--y-min=6" |];
    test_config_error "Invalid Y Bounds"
      [| "y=x"; "--y-min=7"; "--y-max=6" |];
    test_config_error "Invalid X & Y Bounds"
      [| "y=x"; "--x-min=7"; "--x-max=6"; "--y-min=2"; "--y-max=1" |];
    test_config_error "Invalid Steps (string)"
      [| "y=x"; "--quality=gme" |];
    test_config_error "Invalid Steps (0)" [| "y=x"; "--quality=0" |];
    test_config_error "Invalid Steps (negative)"
      [| "y=x"; "--quality=-12" |];
    test_config_error "Multiple Output Files"
      [| "y=x"; "--output=test.txt"; "-otest2.txt" |];
    test_config_error "No Equation" [||];
    test_config_error "No Equation (with --)" [| "--" |];
    test_config_error "Duplicate Modes (chaining)" [| "-pe"; "y=x" |];
    test_config_error "Duplicate Modes (short)" [| "-p"; "-g"; "y=x" |];
    test_config_error "Duplicate Modes (long)"
      [| "--points"; "--extrema"; "y=x" |];
    test_config_error "No Equation (with -)" ~input_override:(Some "")
      [| "-" |];
  ]

let _ = run_test_tt_main suite
