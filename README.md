# Calling External Functions from JIT-compiled LLVM Modules using llvm-hs

This repository contains sample code for calling functions in static
and dynamic libraries from a JIT-compiled module using `llvm-hs`.
Take a look at the corresponding [blog
post](https://purelyfunctional.org/posts/2018-04-02-llvm-hs-jit-external-function.html)
for a detailed explanation of the code in this repository.

You can build the static and the dynamic libraries by calling
`make`. The cabal file defines two executable called `static` and
`dynamic` which use the static and the dynamic library,
respectively. To build those executables run `stack build`.

You can run the examples using `stack exec`. For the `dynamic`
executable, you also need to set `LD_LIBRARY_PATH` to the current
directory:

```
stack exec static
LD_LIBRARY_PATH=. stack exec dynamic
```
