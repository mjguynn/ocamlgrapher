open OUnit2

let default_domain = (-5.0, 5.0)

let default_range = (-5.0, 5.0)

let test_config
    ?domain:(d = default_domain)
    ?range:(r = default_range)
    ?channel:(ic = stdin)
    name
    argv
    func
    expected =
  name >:: fun _ ->
  assert_equal expected
    ( Config.from_cmdline
        (Array.append [| "./ocamlgrapher" |] argv)
        ic d r
    |> Result.get_ok |> func )

let test_config_error
    ?domain:(d = default_domain)
    ?range:(r = default_range)
    ?channel:(ic = stdin)
    name
    argv =
  name >:: fun _ ->
  assert_equal false
    ( match
        Config.from_cmdline
          (Array.append [| "./ocamlgrapher" |] argv)
          ic d r
      with
    | Ok _ -> true
    | Error _ -> false )

let test_simple name =
  test_config ("Simple Valid Config - " ^ name) [| "y=12x+4" |]

(* I would write a helper function to transform a string into an argv
   array, but the logic for doing that is *really* complicated...*)

let cfg_dr cfg = (Config.domain cfg, Config.range cfg)

let test_mode name flag =
  test_config
    ("Config w/ Command " ^ name)
    [| "y=x"; flag |] Config.command

let dummy_stdin contents cxt =
  let name, out = bracket_tmpfile cxt in
  output_string out contents;
  close_out out;
  open_in name

let test_config_error_stdin name contents =
  name >:: fun ctxt ->
  assert_equal false
    ( match
        Config.from_cmdline
          [| "./ocamlrunner"; "-" |]
          (dummy_stdin contents ctxt)
          default_domain default_range
      with
    | Error _ -> false
    | Ok _ -> true )

let suite =
  "ocamlgrapher [Config] test suite"
  >:::
  let open Config in
  [
    (* Valid Cases *)
    test_simple "Equation" equation "y=12x+4";
    test_simple "Command" command Graph;
    test_simple "Default Domain" domain default_domain;
    test_simple "Default Range" range default_range;
    test_simple "Output File" output_file None;
    test_config "Config w/ Output File 1"
      [| "-o"; "test.txt"; "y=x" |]
      output_file (Some "test.txt");
    test_config "Config w/ Output File 2"
      [| "-otest.txt"; "y=x" |]
      output_file (Some "test.txt");
    test_config "Config w/ Output File 3"
      [| "y=x"; "--output=test.txt" |]
      output_file (Some "test.txt");
    test_config "Config w/ Output File 4"
      [| "y=x"; "-o"; "test.txt" |]
      output_file (Some "test.txt");
    test_config "Config w/ Changed Default Bounds (program)" [| "y=x" |]
      ~domain:(2.0, 2.5) ~range:(-12.0, 2.0) cfg_dr
      ((2.0, 2.5), (-12.0, 2.0));
    test_config "Config w/ Changed Bounds (cmdline)"
      [|
        "y=x";
        "--domain-min=7";
        "--domain-max=12";
        "--range-min=-6.72";
        "--range-max=12";
      |]
      cfg_dr
      ((7., 12.), (-6.72, 12.));
    test_mode "graph" "-g" Graph;
    test_mode "points" "-p" Points;
    test_mode "roots" "-r" Roots;
    test_mode "extrema" "-e" Extrema;
    test_config "Config w/ short option chaining"
      [| "-potest.txt"; "y=x" |]
      (fun cfg -> (command cfg, output_file cfg))
      (Points, Some "test.txt");
    (* Advanced Valid Cases *)
    test_config "Config w/ --"
      [| "--domain-max=7"; "--range-max=12"; "--"; "-x-4=y" |]
      equation "-x-4=y";
    ( "Config w/ - (read from input channel)" >:: fun ctxt ->
      assert_equal "y=12x^2"
        ( Config.from_cmdline
            [| "./ocamlrunner"; "-" |]
            (dummy_stdin "y=12x^2\n" ctxt)
            default_domain default_range
        |> Result.get_ok |> equation ) );
    (*Intentionally Failing Cases*)
    test_config_error "Unknown Short Flag" [| "-t"; "y=x" |];
    test_config_error "Unknown Long Flag" [| "--test"; "y=x" |];
    test_config_error "Unknown Short Option" [| "-t2"; "y=x" |];
    test_config_error
      "Unknown Short Option, chained w/ valid short option"
      [| "-totest.txt"; "y=x" |];
    test_config_error "Unknown Long Option" [| "--test=2"; "y=x" |];
    test_config_error "Short Option w/ no param" [| "y=x"; "-o" |];
    test_config_error "Long Option w/ no param"
      [| "y=x"; "--default-min" |];
    test_config_error "Invalid Domain"
      [| "y=x"; "--domain-min=7"; "--domain-max=6" |];
    test_config_error "Multiply Defined Domain Bounds"
      [| "y=x"; "--domain-min=7"; "--domain-max=8"; "--domain-min=6" |];
    test_config_error "Multiply Defined Range Bounds"
      [| "y=x"; "--range-min=7"; "--range-max=8"; "--range-min=6" |];
    test_config_error "Invalid Range"
      [| "y=x"; "--range-min=7"; "--range-max=6" |];
    test_config_error "Invalid Domain & Range"
      [|
        "y=x";
        "--domain-min7";
        "--domain-max=6";
        "--range-min=2";
        "--range-max=1";
      |];
    test_config_error "Multiple Output Files"
      [| "y=x"; "--output=test.txt"; "-otest2.txt" |];
    test_config_error "No Equation" [||];
    test_config_error "Multiple Equations" [| "y=x"; "y=2x" |];
    test_config_error "No Equation (with --)" [| "--" |];
    test_config_error "Multiple Equations (with --)"
      [| "--"; "y=x"; "y=2x" |];
    test_config_error "Duplicate Modes (chaining)" [| "-pe"; "y=x" |];
    test_config_error "Duplicate Modes (short)" [| "-p"; "-g"; "y=x" |];
    test_config_error "Duplicate Modes (long)"
      [| "--points"; "--extrema"; "y=x" |];
    test_config_error_stdin "No Equation (with -)" "";
    test_config_error_stdin "Multiple Equations (with -)" "y=x^2\ny=2x";
  ]

let _ = run_test_tt_main suite
