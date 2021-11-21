# TypeParser

## Installation

### Prerequisites
You will need a working innstallation of Stack to build this, link here: https://docs.haskellstack.org/en/stable/README/

### Setup
In a terminal, navigate to the intended folder, and type
```
git clone https://github.com/SteinarSi/TypeParser.git
cd TypeParser
stack build
```

### Running the program

After installing, type `stack run` in the terminal to start the program. 


## Usage

This is an interactive program that asks the user for lambda expressions and shows how to infer their types. It does *not* support expressions with constants; any reference to a variable, function or literal not bound by a lambda will result in an error.

The lambda expressions have to be typed in a Haskell-like syntax, that is `\x -> \y -> y x` and not `λx.λy.yx`.