## Simply Typed Lambda Calculus

This repo aims to fully replicate the entire language presented in UCLA's CS 231, Types and Programming Languages.

Currently the function-parameter-annotated version is implemented, with primary support for letrec.  Feel free to fork and contribute! 

Todos:

- Conditionals and Boolean
- Type Inference
- Printing of Type Derivation Tree
- Ref 
- Tuple 
- Variant Type

## Installation

Install Ocaml, Core and Jbuilder. Then run the following:

```
jbuilder build test.exe
jbuilder exec -- ./test.exe yourcode.lambda
```

Example code can be found in *.lambda.