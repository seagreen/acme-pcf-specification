# Acme PCF Specification

## The Problem

[RealWorld](https://github.com/gothinkster/realworld) is a fantastic tool for comparing programming languages, but I'm not very interested in HTTP APIs.

## The Solution

Make RealWorld-like specifications for more things than webapps.

In this case, a tiny programming language called PCF.

## Why PCF

PCF is "Programming Computable Functions", a language by [Gordon Plotkin](https://en.wikipedia.org/wiki/Gordon_Plotkin). He introduced it in a 1977 paper called [LCF Considered as a Programming Language](httsp://homepages.inf.ed.ac.uk/gdp/publications/LCF.pdf).

It's simply typed lambda calculus with a few extensions. These are a builtin `fix` function for recursion, primitive booleans and natural numbers with a handful of operations on them, and if/then/else statements.

This is perfect for our purposes. It's enough features to make implementations exert themselves, but still few enough that it stays a small project.

## Example

Plotkin doesn't give the langauge a concrete syntax, so we make one up. You can find details of this (as well as instructions for writing your own implementation) in [./spec](./spec).

Input:
```
let
  add =
    fix
      (\recurse : Nat -> Nat -> Nat.
        \x : Nat. \y : Nat.
          if is-zero x
            then
              y

            else
              recurse (pred x) (suc y))
in
  add 3 4
```

Output:
```
7
```

## Special thanks

This [delightful article](https://jozefg.bitbucket.io/posts/2015-03-24-pcf.html) by Daniel Gratzer ([GitHub repo link](https://github.com/jozefg/pcf)) introduced me to PCF, though he implements different builtins than Plotkin.
