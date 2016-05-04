Pure OCaml emulation of {u,}int{8,16,32,etc} integers
=====================================================

I ripped this implementation out of one of my closed-source
projects. I'm actively using it, but I don't have time to
properly maintain it.

This library depends on Jane Street Core, because the project
that I'm ripping it from is Core-based, but this dependency
could be easily eliminated, pull-requests are welcome.

If you want to make this into a proper `opam` library, then
pull-requests are welcome as well.

Feel free to create issues to say "hi".

This library *emulates* fixed-width integer operations
by representing them as `int` under the hood and doing the
necessary math. It is not very efficient.

You might be interested in `ocaml-stdint` library, which
implements same integer types, but using C-bindings instead:

https://github.com/andrenth/ocaml-stdint
