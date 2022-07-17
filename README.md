
<p align="center">
  <img width="500" height="100%" src="./docs/img/logo-text-colored-new-ts.png">
</p>

# The open GOAL type system for Racket

![CI](https://github.com/hww/open-goal-type-system-cl/workflows/CI/badge.svg?branch=master)

The implementation of the GOAL type system with Racket language. This explanation of the GOAL type system available [here](https://open-goal.github.io/docs/reference/type_system) Or you can see the source code of whole [Jack Project](https://github.com/open-goal/jak-project)

## This project motivations

I plan to write a GOAL compiler as an embedded solution in mobile applications or game engines. The compiler is incomplete at the moment, but the type system is ready.

## Why Racket

The plan was to use Racket because of its system of Syntax objects. The input .gc file can be parsed with read-syntax and the resulting Syntax object tree can be transformed to AST and then the code can be compiled.

Syntax tree advantage over s-expression is that Syntax-tree has information about position of Syntax-object in source file. Thus error information may contain necessary debugging information.

## Status

It is mostly works, the test code in deftype.rkt file parsing the file all-types.gc with 34k lines of code. As result it is defining 1539 types and 89 enums. 
For a futue development reuired only a bugfixing and making better error messaging with Racket exeptions handling and Syntax locations.

## Files

- interfaces.rkt -- Interface for all types
- type.rkt -- The base type for all types
- basic-types.rkt -- All avaibale types: field, null, struct, value, bitfield, enum
- type-spec.rkt -- The universal type reference aka '(function (int int int))'
- state.rkt -- The state type functions
- type-system.rkt -- The main type system 
- builting-types.rkt -- Initializer of type system by builting types
- defenum.rkt -- The enum expression parser
- deftype.rkt -- The deftype expression parser
- rt-type.rkt -- The runtime version of basic type. Not used by type system above
- goalc-all-types.gc -- The list of typespecs of the GOAL
- goalc-type-specs.txt -- The GOAL types file used for testing
