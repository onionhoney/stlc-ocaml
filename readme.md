## Simply Typed Lambda Calculus

This repo aims to fully replicate the entire language presented in UCLA's CS 231, Types and Programming Languages.

Currently the function-parameter-annotated version is implemented, with primary support for letrec.  Feel free to fork and contribute! 

Todos:

- ~~Conditionals and Boolean~~
- Type Inference
- Printing of Type Derivation Tree
- Ref 
- Tuple 
- Variant Type

## Installation

Install Ocaml, Core, Jbuilder and Menhir. Then run the following:

```
jbuilder build stlc.exe
jbuilder exec -- ./stlc.exe yourcode.lambda

// Note that the prefix is always .exe regardless of OS.
```


Example code can be found in examples/*.lambda.
