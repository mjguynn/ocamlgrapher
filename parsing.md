# Parsing Input Expressions
Input expressions will be parsed by a recursive-descent parser, which will generate an abstract syntax tree for the expression. Expressions will be parsed according to a specific grammar as outlined below.

## Input Conventions
Input for mathematical expressions and equations should follow the same conventions as [WeBWorK](https://webwork.maa.org/wiki/Available_Functions#Syntax_for_entering_expressions "WeBWorK Documentation")
 unless otherwise specified.

## Context-Free Grammar
The context-free grammar used to parse expressions is defined as follows. We include two context-free grammars, one containing left-recursive productions for the sake of readability, and one that has been reassociated to be right-recursive for the purpose of parsing.

### Left-Recursive Grammar
The left-recursive grammar is defined according to the following productions.
```
E -> y = expr | expr = y
expr -> expr + term | expr - term | term
term -> term * factor | term / factor | factor
factor -> - factor | group
group -> elem group | elem ^ elem | elem
elem -> const | x | func | ( expr )
func -> sqrt ( expr ) | abs ( expr ) | ln ( expr ) | log ( expr ) | sin ( expr ) | cos ( expr ) | tan ( expr ) | cot ( expr ) | sec ( expr ) | csc ( expr ) | arcsin ( expr ) | arccos ( expr ) | arctan ( expr ) | arccot ( expr ) | arcsec ( expr ) | arccsc ( expr )
const -> e | pi | num
```
Note that the current grammar only supports explicit functions in terms of a `var` token `x`.

### Reassociated Right-Recursive Grammar
The reassociated right-recursive grammar is defined according to the following productions.
```
E -> y = expr | expr = y
expr -> term { + term }* | expr { - term }*
term -> factor { * factor }* | factor { / factor }*
factor -> - factor | group
group -> elem group | elem ^ elem | elem
elem -> const | x | func | ( expr )
func -> sqrt ( expr ) | abs ( expr ) | ln ( expr ) | log ( expr ) | sin ( expr ) | cos ( expr ) | tan ( expr ) | cot ( expr ) | sec ( expr ) | csc ( expr ) | arcsin ( expr ) | arccos ( expr ) | arctan ( expr ) | arccot ( expr ) | arcsec ( expr ) | arccsc ( expr )
const -> e | pi | num
```
Note that the brackets `{ ... }` are metasyntax and the asterisk `*` represents a Kleene star.
