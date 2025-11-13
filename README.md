# ocamba

Ocamba is a library to replace expressions (+, -, ^, &, |) by their MBA equivalents. All of ocamba's work is done in compile time, via PPX.

# Installation

To use Ocamba you must add it as a dependency in your project's dune file and register it as a preprocessor for the relevant executables or libraries.

```Scheme
(executable
 (name main)
  (preprocess (pps ocamba))
)

; Optionally, to see the transformed code after invoking "dune build", use:
; (flags :standard -source)
```

# Usage

Usage is exemplified in this project's [main.ml](https://github.com/estr3llas/ocamba/blob/main/bin/main.ml).

The extension structure is the following

`%mba [{N}, {E}]`

Where `N` controls the recursion depth of the transformations, and `E` is the expression to be transformed.

# Example

An example of a transformed expression is:

```ocaml

```
