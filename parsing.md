# Parsing Input Expressions
Input expressions will be parsed by a recursive-descent parser, which will generate an abstract syntax tree for the expression. Expressions will be parsed according to a specific grammar as outlined below.

## Input Conventions
Input for mathematical expressions and equations should follow the same conventions as [WeBWorK](https://webwork.maa.org/wiki/Available_Functions#Syntax_for_entering_expressions "WeBWorK Documentation")
 unless otherwise specified.

## Grammar
The context-free grammar used to parse expressions is defined according to the following productions.
```
E -> E + T | E - T
T -> T * F | T / F
...
```
