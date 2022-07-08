This is my Haskell implementation of an interpreter of a C-like programming language with static type checking.
Examples of programs are in ./good and ./bad. Grammar is specified in Gramma.cf file.

How to run:
```
make
./interpreter <input_file>
``` 

You can also type in your your program from standard input.

Implemented:
- basic types: int, string, bool, void
- variables, expressions
- print, while (with break and continue), if
- functions, returning values and with recursion, nested functions
- passing arguments to function by value and reference
- variable shadowing and static binding
- 1D arrays

I had a lot of fun with implementing jumps (like break and returning from function) using *continuations*, which resulted in clean code structure.
