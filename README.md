[![CI](https://github.com/mrcdr/polisher/actions/workflows/ci.yml/badge.svg)](https://github.com/mrcdr/polisher/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/License-MIT-green.svg)]()
[![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/mrcdr/polisher)]()

# Polisher
Infix notation to S-expression (Polish notation) translator for Common Lisp

## Overview
Formulae inside the reader macro `#i{ ... }` are interpreted as infix notation.
If you don't want to use it, the macro `polish` is available instead.

```common-lisp
(polisher:activate-infix-syntax)  ; Activate #i{ ... } reader macro

#i{1+2*3}
;=> 7

(polisher:polish "1+2*3") ; Exactly the same as the above one
;=> 7

#i{1 + 2*3} ; Spaces can be inserted anywhere
;=> 7

#i{2*3/4}
;=> 3/2

#i{2**2**3} ; Identical to 2**(2**3), not (2**2)**3
;=> 256

#i{atan(1.0d0, 1.0d0)}
;=> 0.7853981633974483d0

(flet ((add1 (x) (+ x 1)))
  #i{add1(2)+3})
;=> 6

(defparameter *some-global-value* 1.5) ; The symbol containg operator charcters
#i{1 + 2 * "*some-global-value*"}      ; must be double-quoted
;=> 4.0

#i{2*#c(1 2)+3}
;=> #C(5 4)

#i{#b101 +3} ; Some spaces are needed after #b101
;=> 8
```

## Installation
### Quicklisp
If you already have [Quicklisp](https://www.quicklisp.org/beta/) client,
just run the following:

``` common-lisp
(ql:quickload :polisher)
```

It will resolve [dependencies](https://github.com/mrcdr/polisher#requirements) automatically.

### Github and Quicklisp
1. Clone or download the latest version from [GitHub](https://github.com/mrcdr/polisher).
2. In the cloned directory, run `(ql:register-local-projects)`.
3. Now you can use `(ql:quickload :polisher)` anywhere.

### ASDF
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

#i{2^2^3}
;=> 256
```

Note that if there are left-associative operators and right-associative operators
both having the same priority, formulae can't be evaluated correctly.
For example, when `op1` is left-associative and `op2` is right-associative,
`x op1 y op2 z` can be interpreted as either `(x op1 y) op2 z` and
`x op1 (y op2 z)`.

When you add your own operator, be careful of which package
its symbol is interned in.

### Usage example

By taking advantage of the ability to define custom operators, you can design a syntax for extremely concise expressions of those operations, while seamlessly integrating those expressions with Common Lisp code.

Setup:

``` common-lisp
(polisher:activate-infix-syntax)
;;; Create a filtering function that takes the sequence first
(defun reverse-filter (seq op)
  (if (sequencep seq)
    (remove-if-not op seq)
    (and (funcall op seq) seq)))
;;; Create a mapping function that takes the sequence first
(defun reverse-map (seq op)
  (if (sequencep seq)
    (cl:map (if (listp seq) 'list 'vector) op seq)
    (funcall op seq)))

(polisher:add-operator (make-instance 'polisher:operator :symbol '~ :function 'not :priority 10 :args 1)) ;;Note: this is a *postfix* `not` operator
(polisher:add-operator (make-instance 'polisher:operator :symbol '& :function 'and :priority 0))
(polisher:add-operator (make-instance 'polisher:operator :symbol '== :function 'equal :priority -2 :left-associative t))

(polisher:add-operator (make-instance 'polisher:operator :symbol '@ :function 'elt :priority 10))
(polisher:add-operator (make-instance 'polisher:operator :symbol '? :function 'reverse-filter :priority -1 :left-associative t))
(polisher:add-operator (make-instance 'polisher:operator :symbol '=> :function 'reverse-map :priority -1 :left-associative t))

```

Usage:

``` common-lisp
#i{(`(list 1 2 3 4 5)=>#'1+ ?#'evenp)}
;;; => (2 4 6)
#i{3&(`(list 1 2 3 4 5)=>#'1+ ?#'oddp)~==nil}
;;; => t
(unless #i{t&30&`(emptyp (polish "#(1 2 3 4 5)=>#'1+ ?#'oddp"))} 
  "See what I mean?")
;;; => "See what I mean?"
```


## Restrictions
### Symbols start with numbers
Symbols which start with numbers must be double-quoted.
The following example shows the reason:
``` common-lisp
(let ((1e 2))
  #i{1e+1+1})
;=> 11.0

(let ((1e 2))
  #i{"1e"+1+1})
;=> 4
```

No one defines such an odd symbol? _Remember the standard functions `1+` and `1-`!_

### Symbols with vertical bars
Symbols whose symbol-name sandwiched in vertical bars (e.g. `|ab de|`) can't be used.
This is because someone may want to use a vertical bar as the logical OR operator.

### Double-quoting is necessary?
The infix formula `1+*global-symbol*` can be uniquely interpreted as `(+ 1 *global-symbol*)`,
so double-quoting may be unnecessary.
However in my opinion, the formula seems very weird when it appears in ALGOL-like languages;
so I think double-quoting should be used.
In addition, many text editors highlight double-quoted things, helping us to distinguish
symbol-names from operators.



## License
[MIT](https://github.com/mrcdr/polisher/blob/master/LICENSE)

## Author
[mrcdr](https://github.com/mrcdr)
