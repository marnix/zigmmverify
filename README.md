# A [Metamath](http://metamath.org) proof verifier in [Zig](https://ziglang.org/)

# Introduction

At least, the beginnings of one.

This is partly to learn Zig,
partly to have a fast verifier that I can hack
(Python is way too slow, Rust is too weird for me still, Nim seems less clean, C is out),
so that I can perhaps one day try to do experiments around
parsing (where parse tree of ` X... ` is proof of ` |- TOPLEVEL X... ` ),
abbreviations,
and modules.

# How to build and run

Get set.mm from metamath.org or GitHub, or download an older version like so:
```
curl --location https://github.com/metamath/set.mm/raw/b0925f0afd5963577ea76b252cb6613c885b393d/set.mm > set.mm
```

For now, set.mm is hardcoded and must be in the current directory.

Build using zig 0.6.0, then just run the resulting binary.

# Next tasks

- Create a StatementIterator.

   * How to represent statements?
     As a tagged union of the various statement types?

- Loop over statements keeping a stack of blocks (for $d/$e/$f and $c/$v)
  and a map (for $p/$a).

- For each statement, decompress the proof if necessary.

- Execute proof against state (stack+map).

- Verify all proofs by executing and comparing for all statements.

- Create a proper main, rather than one that hardcodes set.mm.
