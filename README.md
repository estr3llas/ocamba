# ocamba

ocamba is a library to replace expressions (+, -, ^, &, |) by their MBA (Mixed-Boolean Arithmetics) equivalents. All of ocamba's work is done in compile time, via PPX.

ocamba's MBAs are Linear, not Polynomial.

# Installation

To use ocamba, you must add it as a dependency in your project's dune file and register it as a preprocessor for the relevant executables or libraries.

```Scheme
(executable
 (name main)
  (preprocess (pps ocamba))
)

; Optionally you can see the transformed code after invoking "dune build", use:
; (flags :standard -source)
```

# Usage

Usage is exemplified within this project's [main.ml](https://github.com/estr3llas/ocamba/blob/main/bin/main.ml).

The extension's structure follows:

`[%mba ({N}, {E})]`

Where `{N}` controls the recursion depth of the transformations, and `{E}` is the expression to be transformed.

# Example

An example of a transformed expression is:

```ocaml
let normal_xor = x lxor y
let obfuscated_xor =
  ((-1) -
     ((lnot
         ((((((-1) - ((lnot x) lor y)) + ((lnot x) land y)) lor
              (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))
             -
             ((lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y))) lor
                (((-1) - ((lnot x) lor y)) + ((lnot x) land y))))
            + (lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))))
        lor
        ((((((-1) - ((lnot x) lor y)) + ((lnot x) land y)) lor
             (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))
            -
            ((lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y))) lor
               (((-1) - ((lnot x) lor y)) + ((lnot x) land y))))
           + (lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y))))))
    +
    ((lnot
        ((((((-1) - ((lnot x) lor y)) + ((lnot x) land y)) lor
             (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))
            -
            ((lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y))) lor
               (((-1) - ((lnot x) lor y)) + ((lnot x) land y))))
           + (lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))))
       land
       ((((((-1) - ((lnot x) lor y)) + ((lnot x) land y)) lor
            (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))
           -
           ((lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y))) lor
              (((-1) - ((lnot x) lor y)) + ((lnot x) land y))))
          + (lnot (((-1) - ((lnot x) lor y)) + ((lnot x) land y)))))
```

If one tries to open the obfuscated program in a disassembler, they would see the following:

![Binja Example](/images/ocamba_binja.png)
