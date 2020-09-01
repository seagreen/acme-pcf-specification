# Parsing examples

### var

Should succeed:
```
x
```

### lam

Should succeed:
```
\x : Nat. x
```

### app

Should succeed:
```
f a
```

### app2

Should succeed:
```
f a b
```

### app-parens

Should succeed:
```
f (a b)
```

### app-extraneous-parens

Should succeed:
```
(f a) b
```

### let

Should succeed:
```
let a = 1 in a
```

### fix

Should succeed:
```
fix (\x : Nat -> Nat. 1)
```

### bool-true

Should succeed:
```
true
```

### bool-false

Should succeed:
```
false
```

### if-then-else

Should succeed:
```
if true then 1 else 0
```

### nat

Should succeed:
```
1
```

### type-bool

Should succeed:
```
\x : Bool. x
```

### type-bool-to-bool

Should succeed:
```
\x : Bool -> Bool. x
```

### type-bool3

Should succeed:
```
\x : Bool -> Bool -> Bool. x
```

### type-extraneous-parens

Should succeed:
```
\x : Bool -> (Bool -> Bool). x
```

### type-function-as-argument

Should succeed:
```
\x : (Bool -> Bool) -> Bool. x
```

### ignore-newlines

Should succeed:
```
f a
b
  c

```

### invalid-symbol

Should fail:
```
!
```

# Typechecking examples

### bool-literal

Should succeed:
```
true
```

### is-zero

Should succeed:
```
is-zero 1
```

### app-not-function

Should fail:
```
0 1
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

