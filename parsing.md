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
E -> expr = expr
expr -> expr + term | expr - term | term
term -> term * factor | term / factor | factor
factor -> group | num group | - factor | num
group -> var subgroup | func subgroup | ( expr ) subgroup | subgroup
subgroup -> elem ^ subgroup | elem
elem -> num | var | func | ( expr )
func -> sqr [ expr ] | abs [ expr ] | ln [ expr ] | log [ expr ] | sin [ expr ] | cos [ expr ] | tan [ expr ] | cot [ expr ] | sec [ expr ] | csc [ expr ] | arcsin [ expr ] | arccos [ expr ] | arctan [ expr ] | arccot [ expr ] | arcsec [ expr ] | arccsc [ expr ]
var -> x | y
```
Note that for explicit functions, one of either of the nonterminal `expr`s in the first production must necessarily be a nonterminal `var`. For implicit functions, this restriction does not apply.

### Reassociated Right-Recursive Grammar
The reassociated right-recursive grammar is defined according to the following productions.
```
E -> expr = expr
expr -> term { + term }* | term { - term }*
...
```
Note that the brackets `{ ... }` are metasyntax and the asterisk `*` represents a Kleene star.
