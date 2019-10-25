# Ion

## Disclaimer
The language and this project are based on the awesome bitwise project (https://github.com/pervognsen/bitwise/tree/master/ion) by Per Vognsen.

## Overview

Ion is a system's programming language which is designed to be isomorphic to C. It does away with some obvious pitfalls while including some of the nice features from other programming languages. It stays as close to C as possible and only deviates in the following ways:

1. Supports order independent declarations.
2. Allows type inferencing using := syntax as used in other languages(like go).
3. Import packages instead of header files.
4. Follows a more intuitive left associative recursive type construction, rather than the C style type declaration model.

## Status

The language and the compiler are designed to have a simple LL(1) hand coded parser. We currently are able to do type checking and generate a C backend from the orderd AST's. 

* currently working on a risc-v and x86-64 backend. 
* An ambitious goal is to also implement a rudimentary SSA optimizer between the AST and the final instruction generator.

## Motivation

[Ion motivation document](https://github.com/pervognsen/bitwise/blob/master/notes/ion_motivation.md)
