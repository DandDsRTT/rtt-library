# The RTT Library

Regular Temperament Theory (RTT) utilities, implemented in [Wolfram Language](https://www.wolfram.com/language/) (
formerly Mathematica), a popular and capable programming language for working with math.

The `main.m` module here is the top-level module for the entire library.
It contains global variables and utilities for math, data structures, parsing/formatting, etc.

## how to use

If you have access to the Wolfram Language desktop application (a paid option), you can copy the code into that
environment and run it there.

Otherwise, the best option is to create a free account on [Wolfram Cloud](https://www.wolframcloud.com), where you can
use these functions for free right on the web without downloading or setting anything up on your computer. Just sign up
for an account, create a new computational notebook, paste any of these code snippets in to a cell, and Shift+Enter to
run them; you'll be computing temperaments and such in no time. FYI, any notebook you create has a lifespan of 60 days
before Wolfram will recycle it, so you'll have to copy and paste them to new notebooks or wherever if you don't want to
lose your work.

Any module nested within another relies on all `main.m` modules above it. Paste the entirety of each required `main.m`
into a different Wolfram Cloud notebook, run the whole thing (select "Evaluation" > "Evaluate all cells" from the menu
bar), and all the functions in the library will be globally available (context is shared between notebooks). You can
start another notebook in another browser tab and start working.

Similarly, any `tests.m` requires (potentially, anyway) every parent module's `tests.m`.

Some modules cross the requirement hierarchy. These ones' README's will say e.g. "This module also
requires `temperament/addition`." In these cases, both their `main.m` and `tests.m` will rely on that other module in
the same way they would a parent module.

## output

You can set the global variable `format` to one of three things:

* `display` (default): results displayed using Wolfram Language's `MatrixForm`, as numbers arranged in rows and columns
* `EBK`: results will be printed as EBK strings, in our preferred style `[⟨1 0 -4] ⟨0 1 4]⟩`
* `Wolfram`: results will be displayed in our underlying data structure, e.g. `{{{1, 0, -4}, {0, 1, 4}}, "row"}`

## roadmap

The following features are planned:

* IO
    * quotient sets
    * units
* new modules
    * scales & lattices
    * temperament complexity & badness
    * timbre
    * notation
    * temperament classification
    * chords

Please report any bugs you find, and we'll be happy to investigate ASAP. Pull requests are also welcome.

## credits

These implementations were developed by [Dave Keenan](https://en.xen.wiki/w/Dave_Keenan)
and [Douglas Blumeyer](https://en.xen.wiki/w/Douglas_Blumeyer) from 2021 - 2022. 
