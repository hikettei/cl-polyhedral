
# cl-polyhedral

cl-polyhedral is an Abstract Polyhedral Compiler for Common Lisp. It can schedule the order of loops, changes the iteration spaces, etc...

which can supercharge the matrix operations in the Common Lisp. e.g.: optimizing einsum (WIP)

# What is a polyheldral compilation?

# Requirements

- [ISL](https://github.com/Meinersbur/isl)

# Usage

```lisp
(with-polyhedral
  (pfor (i 10)
    (pfor (k 10)
      ...)))
```

