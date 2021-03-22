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
      [| "y=x"; "-otest.txt" |]
      output_file (Some "test.txt");
    test_config "Config w/ Output File 4"
      [| "y=x"; "-o"; "test.txt" |]
      output_file (Some "test.txt");
    test_config "Config w/ Changed Default Bounds (program)" [| "y=x" |]
      ~domain:(2.0, 2.5) ~range:(-12.0, 2.0) cfg_dr
      ((2.0, 2.5), (-12.0, 2.0));
    test_config "Config w/ Changed Bounds (short names, cmdline)"
      [| "y=x"; "-x 7"; "-X12"; "-y-6.72"; "-Y12" |]
      cfg_dr
      ((7., 12.), (-6.72, 12.));
    test_config "Config w/ Changed Bounds (long names, cmdline)"
      [| "y=x"; "--xmin=8"; "--xmax=8.1"; "--ymin=111"; "--ymax=123" |]
      cfg_dr
      ((8., 8.1), (111., 123.));
    test_config "Config w/ Changed Bounds (long names, cmdline)"
      [| "y=x"; "--xmin=8"; "--xmax=8.1"; "--ymin=111"; "--ymax=123" |]
      cfg_dr
      ((8., 8.1), (111., 123.));
    test_mode "graph" "-g" Graph;
    test_mode "points" "-p" Points;
    test_mode "roots" "-r" Roots;
    test_mode "extrema" "-e" Extrema;
    (* Advanced Valid Cases *)
    test_config "Config w/ --"
      [| "-X7"; "--ymax=12"; "--"; "-x-4=y" |]
      equation "-x-4=y";
    ( "Config w/ - (read from input channel)" >:: fun ctxt ->
      assert_equal "y=12x^2"
        ( Config.from_cmdline
            [| "./ocamlrunner"; "-" |]
            (dummy_stdin "y=12x^2\n" ctxt)
            default_domain default_range
        |> Result.get_ok |> equation ) );
  ]

let _ = run_test_tt_main suite
