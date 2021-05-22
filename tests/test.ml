open OUnit2

(** Testing Methodology:

    We tested the modules [Cmdline], [Common], [Config],
    [Numericalmethods], [Parser], and [Tokenizer]. When testing, we
    aimed to achieve 100% code coverage with <code>make bisect</code>
    and cover all edge cases. Not every function was tested directly:
    for example, [Common.fpeq] was tested indirectly (largely by
    {{:../tests/test_numericalmethods.ml} test_numericalmethods.ml}),
    and most functions in [Cmdline] were tested indirectly by testing
    [Config]. Whenever something was indirectly tested, we ensured that
    we had 100% code coverage for it. We used a mixture of opaque-box
    and glass-box testing.

    Certain modules & functions are not tested by this test suite. These
    are:

    - [Author]: Only contains [hours_worked].
    - [Defs]: Only contains definitions which are shared between
      multiple modules, including tested ones.
    - [Grapher], [Graphstyles], [Svghelpers]: While it would be possible
      to property-based unit tests for these modules, writing standard
      "equality of output" unit tests would be infeasible. This is
      because their expected results are only required to look correct.
      By design, these two modules have free reign as to how the SVG
      file is written and structured. As an example, currently the
      grapher groups together all plot lines into a single
      <code><g></code> element. It could also emit each plot line
      individually. The visual result would be identical, but the SVG
      would be structured differently, causing the unit test to fail. If
      we had wrote equality based tests then we would have to spent a
      lot of time updating our tests to address changes like these. We
      don't believe that this is an efficient use of time compared to
      performing more manual testing.
    - [Io]: Most functions in [Io] create stylized program output. Their
      specifications are deliberately very vague in order to allow us
      the freedom to freely change how the text appears. For example,
      we're allowed to make [print_detail] use a darker shade of gray if
      we wanted to. This makes writing unit tests difficult/inefficient,
      as described above. Additionally, most functions in [Io] are
      designed to only output formatting if they are outputting to a
      TTY, so the formatting may be impossible to test in OUnit. All
      such functions were manually tested. [read_lines] could certainly
      have been tested with OUnit, however we manually tested it instead
      because we didn't feel creating an entire OUnit test suite for one
      (relatively simple) function was worth it.
    - [Ocamlgrapher]: The main module. It wasn't unit tested for the
      same reason as [Grapher]: we wanted to be able to spontaneously
      change the interface (such as, say, changing the color of
      outputted text). Solidifying these properties in a specification
      would have caused us to constantly rewrite tests, which would have
      been inefficient. We manually tested the program as a whole to
      remedy the lack of unit tests.
    - [Xmldom]: Code is trivial and was extensively manually verified.

    We're reasonably sure that our manual testing, combined with our
    test suite, demonstrates the correctness of our code for the above
    reasons. We have a manual test suite of program invocations,
    including:

    - <code>./ocamlgrapher.byte y=x</code> (basic)
    - <code>./ocamlgrapher.byte "y=1/(x-2)"</code> (infinite
      discontinuties)
    - <code>./ocamlgrapher.byte y="e^ln(x)" x=y y=x^3-2x
      y="(e^sin(x)-(e)/2)/abs(x)" "y=sqrt(81-x^2)"
      "y=-sqrt(81-x^2)"</code> (lots of functions, tests info box
      sizing, render order, color assignment, etc)
    - <code>./ocamlgrapher.byte "y=x^3-4x+2" X</code> where X is
      <code>-r, -e,</code> or <code>-p</code>(tests non-graphing
      functionality)

    We also occasionally try each of these commands with different
    bounds on the X and Y axes and different aspect ratios, as those
    have been sources of bugs in the past. Finally, we tested the
    resulting SVGs on Chrome, Firefox, Edge, and Safari to ensure that
    they were consistent. *)

let suite =
  "OCamlGrapher Test Suite"
  >::: List.flatten
         [
           Test_parser.all_tests;
           (* ^ also tests tokenizer *)
           Test_numericalmethods.all_tests;
           Test_config.all_tests;
           (* ^ also tests cmdline *)
           Test_common.all_tests;
         ]

let _ = run_test_tt_main suite
