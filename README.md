[![Build with Zig 0.7.1](https://github.com/marnix/zigmmverify/workflows/Build%20with%20zig%200.7.x/badge.svg?branch=zig-0.7.x)](https://github.com/marnix/zigmmverify/actions?query=branch%3Azig-0.7.x)

# A [Metamath](http://metamath.org) proof verifier in [Zig](https://ziglang.org/)

At least, the beginnings of one.

This is partly to learn Zig,
partly to have a fast verifier that I can hack
(Python is way too slow, Rust is too weird for me still, Nim seems less clean, C is out),
so that I can perhaps one day try to do experiments around
parsing (where parse tree of ` X... ` is proof of ` |- TOPLEVEL X... ` ),
abbreviations,
and modules.

# How to build and run

Build using Zig 0.7.1, then just run the resulting binary,
passing a single .mm file on the command line.

For example, use set.mm, which you can get from metamath.org,
or download the most recent version
directly from https://github.com/metamath/set.mm like so:
```
wget https://github.com/metamath/set.mm/raw/develop/set.mm -O set.mm
```

# Next tasks

- Verify that 'normal' tokens don't start with `$`.

- Clean-up / refactor RuleIterator + ScopeDiff:
  Add methods, move functionality between these structs.
  Also encapsulate some parts.
  Also try to avoid duplication in statement handling.

- Merge verify.zig and prove.zig into a single source file?

- Support `?` in proofs.

- Identify the location (line/column) of at least the first error.

- Don't use 'error union' for Metamath verification errors.

- Run the test suite from https://github.com/david-a-wheeler/metamath-test
  probably by checking it out as a Git submodule
  and having a binary that accepts a .mm file name
  and exits with 0 if (and only if) the file and all its proofs
  verify completly correctly.
