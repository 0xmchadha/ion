# Ion

## Disclaimer
The language and this project are based on the awesome bitwise project (https://github.com/pervognsen/bitwise/tree/master/ion) by Per Vognsen.

## Overview

Ion is a system's programming language which is designed to be isomorphic to C. It does away with some obvious pitfalls while including some of the nice features from other programming languages. It stays as close to C as possible and only deviates in the following ways:

* Supports order independent declarations.
* Allows type inferencing using := syntax as used in other languages(like go).
* Import packages instead of header files.
* Follows a more intuitive left associative recursive type declaration, rather than the C style type declaration model. Rob Pike's blog on Go's declaration syntax explains these nuances very clearly (https://blog.golang.org/gos-declaration-syntax)

## Example

fib.ion

```
func fib(n :int) :int {
  if (n == 0) {
    return 0;
  }
  
  if (n == 1) {
    return 1;
  }
  
  return fib(n-1) + fib(n-2);
}
```

Running the above code through the compiler with ion fib.ion will generate a corresponding fib.c file.

## Status

The language and the compiler are designed to have a simple LL(1) hand coded parser. We currently are able to do type checking and generate a C backend from the orderd AST's. 

* currently working on a risc-v and x86-64 backend. 
* SSA based optimization.

## Motivation

[Ion motivation document](https://github.com/pervognsen/bitwise/blob/master/notes/ion_motivation.md)
