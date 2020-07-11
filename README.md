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

- Clean-up / refactor VerifyState + ScopeDiff:
  Add methods, move functionality between these structs.

- Finish the iterator for 'mandatory hypotheses for given expression':

   * When going backwards over the active hypotheses list:

      - add the variables from that $e hypothesis.

- For each $a and $p statement, build the corresponding `InferenceRule`.

  This uses the above iterator.

- For each $p statement, run the proof.
  For compressed proofs, don't decompress-then-run:
  Decompression needs some context
  (viz. the mandatory hypotheses for the $p statement,
  and the number of hypotheses for each referenced statement),
  and that context is more readily available while running the proof.

  So execute the proof against the current state,
  keeping a stack of `Expression`s.

  (Initially ignore $d restrictions.)

- Verify all proofs by executing and comparing for all statements.

- Support $d.

- Support $[ ... $] includes.  (Presumably in the tokenizer.)

- Identify the location (line/column) of at least the first error.

- Don't use 'error union' for Metamath verification errors.

- Create a proper main, rather than one that hardcodes set.mm.
