## Elm REPL [![Build Status](https://travis-ci.org/evancz/elm-repl.png?branch=dev)](https://travis-ci.org/evancz/elm-repl)

This tool lets you interact with values and functions directly.

### Install

Install [node.js](http://nodejs.org/download/) and then run:

```bash
cabal update
cabal install elm-repl
```

This package depends on the latest release of the Elm compiler, so
it will install that automatically.

### Use

You can type in expressions, definitions, ADTs, and module imports
using normal Elm syntax. 

```
> 1 + 1
2 : number

> "hello" ++ "world"
"helloworld" : String
```

The same can be done with definitions and ADTs:

```
> fortyTwo = 42
42 : number

> f n = n + 1
<function> : number -> number

> f 41
42 : number

> factorial n = \
|   if n < 1 then 1 \
|            else n * factorial (n-1)
<function> : number -> number

> factorial 5   
120 : number

> data Either a b = Left a | Right b

> case Left 32 of \
|   Left n -> 2 * n \
|   Right m -> m + 1
64 : number
```

You can import standard libraries and any library
reachable from the directory where `elm-repl` is running:

```
> import String

> String.length "hello"
5 : Int

> String.reverse "flow"
"wolf" : String
```
