# Acme PCF Specification

## The Problem

[RealWorld](https://github.com/gothinkster/realworld) is a fantastic tool for comparing programming languages, but I'm not interested in either frontend or backend webapps.

## The Solution

Make *RealWorld*-like specifications for more things than webapps.

## PCF

This [delightful article](https://jozefg.bitbucket.io/posts/2015-03-24-pcf.html) by Daniel Gratzer ([GitHub repo link](https://github.com/jozefg/pcf)) introduced me to PCF. He calls the variant he implements "partial computable functions".

As far as I can trace it PCF originated in a paper called [LCF Considered as a Programming Language](http://homepages.inf.ed.ac.uk/gdp/publications/LCF.pdf) by G.D. Plotkin in 1977. He actually calls it "Programming Computable Functions".

This is the variant we specify here. It's more complex than the one implemented by Gratzer, but I think that's a feature in this case. It will make the implementations exert themselves a little more.

However, this still won't be very much. PCF has few builtin functions, no user defined data types, and no type polymorphism-- ideal restrictions for a language we'd like people to be able to write in a weekend. It's basically simply typed lambda calculus with bools, natural numbers, and fix.

## Example

Input:
```
fix (\rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x))) 2
```

Output:
```
0
```

## Specification

Section 2 of Plotkin's [paper](http://homepages.inf.ed.ac.uk/gdp/publications/LCF.pdf), "2. The programming language, PCF"

Plotkin's paper doesn't provide a concrete syntax, so we make one up.

TODO: Informal description of syntax

We provide [test-cases](./misc/generated/test-cases.json) for parsing and evaluating it.

Additionally, we add two constants to the language, which is allowed by section 2.

+ `let`: for ease of writing code. It's sugar for abstracting and applying, and not polymorphic.

+ polymorphic `if... then... else`: because having only two monomorphic if/then/else constructs (one that returns `Bool` and one that returns `Nat`) would be sad.

## Implementation

![sloc](./misc/generated/sloc.svg)

There's a reference implementation in [./src](./src). It's still WIP, most notably missing a typechecker.

If you start on another implementation let me know. Once you're ready I'll mention it here. I definitely recommend hooking up the [test-cases](./misc/generated/test-cases.json) into your test suite. Right now it will probably also require looking at the reference source, but that will change as I write better docs.
