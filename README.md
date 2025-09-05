# Lambda: lambda calculus with natural numbers

Simple lamdba calculus interpretator written in Haskell. I used [Fang](https://github.com/artempyanykh/Fang) by Artem Pianykh and [GHC.Core](https://downloads.haskell.org/ghc/9.0.1/docs/html/libraries/ghc-9.0.1/GHC-Core.html) for inspiration.

## Grammar
All of those are considered to be expressions. They can be wrapped in as many parentheses as you want, like `(((((x)))))`.
- Literal: a digit. Can be negative (but `+` is not allowed). Examples: `123`, `-999`, etc. Invalid: `+300`, `--1`, `1.23`
- Variable: an identifier. Can contain only letters (`isAlpha`) and must not use reserved keywords (see below). Examples: `var`, `x`, `y`, `MyVar`, etc. Invalid: `var1`, `my-var`, `else`
- Lambda: function definition. Has the following syntax: `\<variable name>.<expression>`. Usually the expression contains variable with the same name, like `\x.add 10 x`
- Application: application of a function. Has the following syntax: `<expression> <expression>`, f.e. `f 1`. As lambdas can accept only one argument, they may return closure (just like in Haskell). F.e. `add 10` returns a closure that adds 10 to an argument. Application body must be a function (lambda)
- Condition: `if <expression> then <expression> else <expression>`. First expression must be a number. If it is not zero it returns the second expression. Else it returns the third expression. F.e. `\x.if mod x 2 then 0 else 1` returns 0 if `x` is odd and 1 if `x` is even
- Let-expression: `let [rec] <variable name> = <expression> in <expression>`. Allows to create bindings, f.e. `let x = 3 in add x 1` is the same as `add 3 1`. It is more useful if `rec` modifier is used (as it allows to reference variable inside an expression, just like in ML languages): `let rec fib = \n.if less n 2 then 1 else add (fib (sub n 1)) (fib (sub n 2)) in fib 20`

Reserved keywords: `if`, `then`, `else`, `let`, `rec`, and `in`

## Repl
Repl has some useful options and provides 8 built-in functions:
- Arithmetic functions: `add`, `sub`, `mul`, `div`, `mod`
- Comparision functions (return 1 if true or 0 if false): `less`, `eq`, `greater`

When running repl all your input is interpreted as a program that needs to be evaluated except for these cases:
- `q` or `quit`: quit repl
- `h` or `help`: print help message
- `env`: show your current environment
- `set <variable name>`: add new value to the environment. It prompts you to write down your expression after you pressed Enter
- `unset <variable name>`: remove value from the environment

## Todos:
- Improve error messages
- Use memoiaztion for recursive functions and add other optimizations
