# brainfuck laboratory

This is my lab for experimentation with brainfuck. Currently I have:

- [bf](./bf.c), a very simple interpreter for brainfuck written in C.
- [bf-hs](./bf-hs), a more sophisticated interpreter written in Haskell, inspired in the one which appears in [this article](https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md).
- [b2bf](./b2bf), a very silly program which gets input from file or stdin and outputs a brainfuck program which when executed will output those bytes, comes with code size optimization phases.

## Research List

This is not a TODO list, it's just a reference so you know what are my thoughts during my lab sessions.

- [ ] Ways to handle character printing in the smallest footprint I can
    - [x] Computing diffs between the last ones (static)
    - [ ] Preparing memory on advance (static)

- [ ] Ways to initialize static memory in the smallest amount of time (during interpretation) and smallest footprint possible, assuming **no optimizations** in the interpreter.

## Goal

The goal of this lab is to explore lots of options regarding code generation for a small set of instructions and finally build the best compiler I can for
[this kata](https://www.codewars.com/kata/59f9cad032b8b91e12000035).

As well, I want to have fun with this, so don't expect anything too useful or too complex. I'm just a 16 year-old nerd who's playing around with brainfuck in his free time.
