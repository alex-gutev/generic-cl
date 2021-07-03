# GENERIC-CL

GENERIC-CL provides a generic function wrapper over various functions
in the Common Lisp standard, such as equality predicates and sequence
operations. The goal of this wrapper is to provide a standard
interface to common operations, such as testing for the equality of
two objects, which is extensible to user-defined types.

## Example

In Common lisp you have a number of different equality comparison
functions for different object types. Worse still you have to invent a
new name for the comparison functions for each of your own object
types.

**Standard Common Lisp:**

```lisp
(eq 'a x)           ;; Symbols
(= 1 2)             ;; Numbers
(equal '(1 2 3) x)  ;; Lists
(equal "hello" y)   ;; Strings
(equalp #(1 2 3) z) ;; Arrays

(foo-equal x y)     ;; Instances of class foo
```

In GENERIC-CL there is a single equality predicate `=` which can be
used on objects of any builtin type. Since it's implemented using
generic functions, it can be extended with methods for user-defined
classes and structures.

**Using GENERIC-CL:**

```lisp
(= 'a x)       ;; Symbols
(= 1 2)        ;; Numbers
(= '(1 2 3) x) ;; Lists
(= "hello" y)  ;; Strings
(= #(1 2 3) z) ;; Arrays

(= x y)        ;; Instances of class foo
```

GENERIC-CL also provides generic interfaces for comparison (`<`, `>`,
...) functions, copying objects, iteration, sequence operations,
hash-tables capable of storing keys of user defined classes, and many
more.

## Documentation

Full documentation is available online in HTML format at
<https://alex-gutev.github.io/generic-cl/>, and also in AsciiDoc
format in the `doc/` folder.

## Isn't this slow?

Yes generic functions do carry an additional overhead over ordinary
functions, however the author of this library believes this isn't an
issue for most applications.

Furthermore for those applications where the speed of
generic-functions is an issue, generic function calls can be
optimized, using
[static-dispatch](https://github.com/alex-gutev/static-dispatch) on
which this library is built, to be as efficient (and sometimes even
more efficient) as ordinary function calls, when the types of the
arguments are known at compile-time. Check out the
[https://alex-gutev.github.io/generic-cl/#optimization](Optimization)
section for more information.

