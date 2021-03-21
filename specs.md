### List of Discontinuities

Here is a list of discontinuities that we should check for within the AST.
- If we have a division, we check that the denominator is not equal to zero.
- If we have a square root, we check that the number being rooted is non-negative.
- Trig functions: tan, csc, sec, cot
- Logarithm of zero

### CLI Interface Examples

Attempt to follow GNU Option Parsing Syntax.
`./ocamlgrapher.byte -g"y=x+4"`
`./ocamlgrapher.byte --graph "x=2y + x^2"`
`./ocamlgrapher.byte -g "y+2x = ln(x)*sin(e^y)"`
`./ocamlgrapher.byte --points "y=x+4"`
`./ocamlgrapher.byte --roots "y=x+4"`
`./ocamlgrapher.byte --extrema "y=(x-1)^2"`
`./ocamlgrapher.byte -g "y=7x" -x2 -X5 -y0 -Y 10"` (graphs on x in [2,5], y in [0, 10])

