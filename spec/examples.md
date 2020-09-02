# Parsing examples

### var

Should parse:
```
x
```

### lam

Should parse:
```
\x : Nat. x
```

### app

Should parse:
```
f a
```

### app2

Should parse:
```
f a b
```

### app-parens

Should parse:
```
f (a b)
```

### app-extraneous-parens

Should parse:
```
(f a) b
```

### let

Should parse:
```
let a = 1 in a
```

### fix

Should parse:
```
fix (\x : Nat -> Nat. 1)
```

### bool-true

Should parse:
```
true
```

### bool-false

Should parse:
```
false
```

### if-then-else

Should parse:
```
if true then 1 else 0
```

### nat

Should parse:
```
1
```

### type-bool

Should parse:
```
\x : Bool. x
```

### type-bool-to-bool

Should parse:
```
\x : Bool -> Bool. x
```

### type-bool3

Should parse:
```
\x : Bool -> Bool -> Bool. x
```

### type-extraneous-parens

Should parse:
```
\x : Bool -> (Bool -> Bool). x
```

### type-function-as-argument

Should parse:
```
\x : (Bool -> Bool) -> Bool. x
```

### ignore-newlines

Should parse:
```
f a
b
  c

```

### invalid-symbol

Should not parse:
```
!
```

# Typechecking examples

### bool-literal

Should typecheck:
```
true
```

### nat-literal

Should typecheck:
```
1
```

### is-zero

Should typecheck:
```
is-zero 1
```

### is-zero-applied-to-bool

Should not typecheck:
```
is-zero true
```

### lambda

Should typecheck:
```
(\x : Nat. x)
```

### not-in-scope

Should not typecheck:
```
a
```

### app-not-function

Should not typecheck:
```
0 1
```

### applied-wrong-type

Should not typecheck:
```
(\x : Nat. x) true
```

### if-not-bool

Should not typecheck:
```
if 1 then true else false
```

### if-statement-mismatch

Should not typecheck:
```
if true then 1 else false
```

### fix

Should typecheck:
```
fix (\x : Nat. 1)
```

### fix-not-lambda

Should not typecheck:
```
fix 1
```

### fix-type-mismatch

Should not typecheck:
```
fix (\x : Nat -> Nat. 1)
```

# Evaluation examples

### lam

Expected: <success>
```
\x : Nat. x
```

### app

Expected: <success>
```
(\x : Nat. x) 1
```

### let

Expected: 1
```
let a = 1 in a
```

### if-then-else

Expected: 1
```
if true then 1 else 0
```

### suc

Expected: 1
```
suc 0
```

### pred

Expected: 0
```
pred 1
```

### pred-0

Expected: 0
```
pred 0
```

### is-zero

Expected: true
```
is-zero 0
```

### fix-simple

Expected: 1
```
fix (\x : Nat. 1)
```

### fix-realistic

Expected: 0
```
fix (\rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x)) 2
```

### detailed-example

Expected: 7
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

