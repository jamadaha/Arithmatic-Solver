# Arithmatic Solver
A simple solver of arithmatic.

It does it in four steps:
- Scanner : Convert plain text to symbols
- Syntax Analysis : Convert symbols to syntax tree
- Semantic Analysis : Checks whether the syntax tree makes sense
- Calculation : Calculates the result from the tree

Sidenote:
First project which I have written in Haskell; expect bad code.

## Table of Contents
- [Arithmatic Solver](#arithmatic-solver)
  - [Table of Contents](#table-of-contents)
  - [Requirements](#requirements)
  - [Usage](#usage)
  - [Example](#example)

## Requirements
[GHC](https://www.haskell.org/downloads/) - A Haskell Compiler

## Usage
Simply run the following command in the folder where the Main.hs exists
```
ghc .\Main.hs; .\Main.exe
```

It will then process all lines in Input.txt which is in the same folder

## Example
Given input
```
2 * ( 3 + 4 )
```
It will produce the output
```
---- 2 * ( 3 + 4 ) ----
Symbols: [(Num,2),(Mul,0),(LPar,0),(Num,3),(Add,0),(Num,4),(RPar,0)]
Syntax Tree: Branch (Leaf (Num,2),Leaf (Mul,0),Branch (Leaf (LPar,0),Branch (Leaf (Num,3),Leaf (Add,0),Leaf (Num,4)),Leaf (RPar,0)))
Valid: True
Output: 14
```
