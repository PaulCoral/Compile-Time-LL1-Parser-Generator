# Bachelor Project LARA

## Title : _Advancing Combinator Parsing in Scala_

### Description
A LL1 (mostly) compile time parser based on [Scall1on](https://github.com/epfl-lara/scallion). This parser allow the user to construct a syntax based on combinators, and construct a parsing table mostly at compile time. Some work is still done at runtime as some datatype are not well supported by macro, either the user as to handle them each new type, either it is not possible get them out of the macro context.

### Usage

An example is provided [here](https://github.com/PaulCoral/Compile-Time-LL1-Parser/example).

### Some Other work
- [Tests : Compile-time reduction of ASTs using Metaprogramming](https://gitlab.epfl.ch/pcoral/bachelor-project-lara-personal-information/-/tree/AST_macro_tests)

### Some Readings
- [Scall1on](https://github.com/epfl-lara/scallion)
- [Scala3 Metaprogramming](https://dotty.epfl.ch/docs/Metaprogramming/index.html)


