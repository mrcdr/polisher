# Polisher
Infix notation to S-expression (Polish notation) translator for Common Lisp

## Overview
Formulae inside reader macro #i{ ... } are interpreted as infix notation.

```common-lisp
#i{1+2*3}
;=> 7

#i{1 + 2*3} ; Spaces can be inserted anywhere
;=> 7

#i{2*3/4}
;=> 3/2

#i{2**2**3} ; identical to 2**(2**3), not (2**2)**3
;=> 256

#i{cos(1.0d0)**2 + sin(1.0d0)**2}
;=> 1.0d0

#i{atan(1.0d0, 1.0d0)}
;=> 0.7853981633974483d0

(flet ((add1 (x) (+ x 1)))
  #i{add1(2)+3})
;=> 6

#i{2*#c(1 2)+3}
;=> #C(5 4)

#i{#b101 +3} ; Space is required after #b101
;=> 8

(macroexpand-1 '#i{1+2*3})
;=> (+ 1 (* 2 3))
```

## Installation
1. Clone or download the latest version from [GitHub](https://github.com/mrcdr/polisher).
2. Place the directory where your ASDF system can find.
3. Run `(asdf:load-system :polisher)`.

## Requirements
- [cl-ppcre](https://edicl.github.io/cl-ppcre/)
- [1am](https://github.com/lmj/1am) (Only if you want to run tests)


## Default operators
Following operators are defined by default:

| symbol | function | priority | left associative |
|--------|----------|----------|------------------|
| +      | +        | 1        | t                |
| -      | -        | 1        | t                |
| \*     | \*       | 2        | t                |
| /      | /        | 2        | t                |
| \*\*   | expt     | 3        | nil              |

## Add your own operator

```common-lisp
(polisher:add-operator (make-instance 'polisher:operator
                                      :symbol '^
                                      :function 'expt
                                      :priority 3
                                      :left-associative nil))

#i{cos(1.0d0)^2 + sin(1.0d0)^2}
;=> 1.0d0
```

Note that if there are left-associative operators and right-associative operators
both having the same priority, formulae can't be evaluated correctly.
For example, when `op1` is left-associative and `op2` is right-associative,
`x op1 y op2 z` can be interpreted as either `(x op1 y) op2 z` and
`x op1 (y op2 z)`.

When you add your own operator, be careful of which package
its symbol is interned in.

## License
[MIT](https://github.com/mrcdr/polisher/blob/master/LICENSE)

## Author
[mrcdr](https://github.com/mrcdr)
