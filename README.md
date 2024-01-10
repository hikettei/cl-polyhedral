
# cl-polyhedral

cl-polyhedral is a Common Lisp package which provides a **Polyhedral Compilation**.  It enables following powerful features on matrix operations:

- Automatically parallelized iterations

- SIMD Tiling, GPU Use

- JIT Compiler to Any devices

- etc...

- cl-polyhedral can be directly embedded in your Common Lisp Code!

cl-polyhedral is an Abstract Polyhedral Compiler for Common Lisp. It can schedule the order of loops, changes the iteration spaces, etc...

which can supercharge the matrix operations in the Common Lisp. e.g.: optimizing einsum (WIP)

# What is a polyheldral compilation?

# Requirements

- [ISL](https://github.com/Meinersbur/isl)

# Usage

```lisp

```

# TODO

- Detailed Installing Steps (ISL, GMP)


## Memo: ISL


# Getting Started

## Installing ISL

```sh
$ brew install isl
$ sudo apt install libisl-dev
```

## (Optional) Building from the source

```sh
$ cd ./isl
$ ./autogen.sh
$ CFLAGS="$(pkg-config --libs --cflags gmp)" ./configure --prefix $HOME/usr
$ make -j 8 && make install
```

Ensure CFFI can find out where libisl.dylib was installed.

```sh
# e.g.:
$ locate libisl.dylib
> /opt/homebrew/lib/libisl.dylib
```
