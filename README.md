# _Advancing Combinator Parsing in Scala_
## LL(1) Parser with Compile Time Capabilities

### Description
In my bachelor project at the **LARA** lab at EPFL, I built a LL(1) parser with compile time capabilities based on the [Scall1on](https://github.com/epfl-lara/scallion) parser. This parser allows the user to define a syntax based on combinators, and construct a parsing table mostly at compile time. Some work has to be done at runtime, as some datatype are not well supported by macro, so either the user has to handle each custom type, or it is not possible get them out of the macro context.

The advantages of this parser are:
- Shorter runtime setup time (only one traversal of the syntax definition)
- Efficient parsing algorithm
- An instant feedback of the possible LL(1) conflicts in the syntax definition.
- Possibly, a safer packaging of libraries as a flawed parser with LL(1) conflicts won't compile.

### Usage

An example is provided in [example/syntaxdef](https://github.com/PaulCoral/Compile-Time-LL1-Parser/tree/master/example/syntaxdef).
More modular examples are provided here [example/modular](https://github.com/PaulCoral/Compile-Time-LL1-Parser/tree/master/example/modular) or also in the [tests](https://github.com/PaulCoral/Compile-Time-LL1-Parser/tree/master/src/test/scala/ll1compiletime).

⚠️ The syntax definition and the macro call and implementation have to be in a single file to avoid compilation issues ⚠️.

An easy way to get started is by using SBT (here v1.5.3), `sbt new scala/scala3.g8` and adding the library jar file to a directory in the project `lib/`-


### Extras

- I used the [Silex](https://github.com/epfl-lara/silex) lexing library, which I had to upgrade to Scala 3 ([here](https://github.com/PaulCoral/silex/)) through some minor changes.

### Some Other work
- [Tests : Compile-time reduction of ASTs using Metaprogramming](https://github.com/PaulCoral/Compile-Time-LL1-Parser/tree/AST_macro_tests)

### Some Readings
- [Scall1on](https://github.com/epfl-lara/scallion)
- [Zippy LL(1) Parsing with Derivatives](https://github.com/epfl-lara/scallion/blob/master/paper/PLDI20_ZippyLL1PWD.pdf)
- [Scala3 Metaprogramming](https://dotty.epfl.ch/docs/Metaprogramming/index.html)


