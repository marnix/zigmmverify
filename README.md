_Note that this branch is built using nightly 'master' zig.
The more actively developed branch is
[zig-0.8.x](https://github.com/marnix/zigmmverify/tree/zig-0.8.x).
Changes are periodically merged from there to this branch._

[![Build with zig master](https://github.com/marnix/zigmmverify/workflows/Build%20with%20zig%20master/badge.svg?branch=zig-master)](https://github.com/marnix/zigmmverify/actions?query=branch%3Azig-master)

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

Build using Zig master, then just run the resulting binary,
passing a single .mm file on the command line.

For example, use set.mm, which you can get from metamath.org,
or download the most recent version directly from GitHub:
https://github.com/metamath/set.mm/raw/develop/set.mm.

The version of the Metamath specification that was implemented, is
the one from the 2nd edition Metamath book.
(Norman D. Megill, David A. Wheeler, 
"Metamath: A Computer Language for Mathematical Proofs".
Lulu Press, 2019.
http://us.metamath.org/downloads/metamath.pdf .)

# Next tasks

Verifier completeness:

- Verify that 'normal' tokens don't start with `$`,
  label tokens use only allowed characters, etc.

- Support `?` in proofs.

Clean up:

- Clean-up / refactor RuleIterator + ScopeDiff:
  Add methods, move functionality between these structs.
  Also encapsulate some parts.
  Also try to avoid duplication in statement handling.

- Work towards a library that can also be used for $a/$p/$e parse trees.

Functionality:

- Generate a parse tree for every $a/$p/$e statement.
  If possible/feasible, check that it is the only possible parse tree.
  Try to also support 'conditional syntax rules,
  so e.g. `${ $e -. B = 0 $. $a class ( A / B ) $. $}` which expresses that
  `A / B` is only a syntactically valid expression if `B` is not zero.

Verifier performance:

- Optimize performance by reducing memory use:
  A token is currently represented by a Zig slice (= pointer and a length),
  and this could be replaced by a single small(ish) integer
  by 'interning' all tokens.

- Optimize performance by parallelizing using Zig `async`.

- Optimize performance by reducing memory use:
  I suspect ArrayList is not the right data structure for some of the lists.

Verifier tests:

- Run the test suite from https://github.com/david-a-wheeler/metamath-test
  probably by checking it out as a Git submodule
  and using the zig-mm-verify binary as the approved-or-not judge.

- (Separate project.) Extend that test suite, to capture as many as possible
  deviations from the specification as possible.

Verifier usability:

- Identify the location (line/column) of at least the first error.

- Don't use 'error union' for Metamath verification errors.

Language dialects:

- Optional modes where $c/$v is allowed to be duplicated
  (useful to create set-parsed.mm which declares stuff before `$[ set.mm $]`);
  where $f implicitly does $c/$v;
  and perhaps more.
