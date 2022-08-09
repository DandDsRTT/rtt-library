# The RTT Library

Regular Temperament Theory (RTT) utilities, implemented in [Wolfram Language](https://www.wolfram.com/language/) (
formerly Mathematica), a popular and capable programming language for working with math.

This file contains functions related to temperaments, focusing on commas and mapping. It also contains global variables
and utilities relied upon by other files, as well as some basic user functions:

* `getD`
* `getR`
* `getN`
* `canonicalForm`
* `dual`
* `mapMerge`
* `commaMerge`
* `changeIntervalBasis`
* `sum`
* `diff`
* `getGeneratorsPreimageTransversal`

It is based on material from the following article series:

* [Dave Keenan & Douglas Blumeyer's guide to RTT](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: commas & mapping](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_commas_&_mapping)
* [Defactoring algorithms](https://en.xen.wiki/w/Defactoring_algorithms)
* [Temperament merging](https://en.xen.wiki/w/Temperament_merging)
* [Temperament addition](https://en.xen.wiki/w/Temperament_addition)

## how to use

If you have access to the Wolfram Language desktop application (a paid option), you can copy the code into that
environment and run it there.

Otherwise, the best option is to create a free account on [Wolfram Cloud](https://www.wolframcloud.com), where you can
use these functions for free right on the web without downloading or setting anything up on your computer. Just sign up
for an account, create a new computational notebook, paste any of these code snippets in to a cell, and Shift+Enter to
run them; you'll be computing temperaments and such in no time. FYI, any notebook you create has a lifespan of 60 days
before Wolfram will recycle it, so you'll have to copy and paste them to new notebooks or wherever if you don't want to
lose your work.

Most of the functions you need are available in `/main.nb`. The top half of that file has the public functions you would
use yourself, and the bottom half has the private functions that those rely on but you don't need to use directly. So
just paste the entirety of `/main.nb` into one Wolfram Cloud notebook, run the whole thing (select "Evaluation" > "
Evaluate all cells" from the menu bar), and all the functions in the library will be globally available (context is
shared between notebooks). You can start another notebook in another browser tab and start working.

## data structures

Temperament representations, such as mappings and comma bases, may be input like this:

* 12-ET's map: `⟨12 19 28]`
* meantone's mapping: `[⟨1 0 -4] ⟨0 1 4]⟩`
* meantone's comma: `[4 -4 1⟩`
* 12-ET's comma basis: `⟨[4 -4 1⟩ [-7 0 3⟩]`

Those are left `⟨` and right `⟩` angle braces there, but if these are not easy for you to type, less than `<` or
greater than `>` signs can be used instead.

Any amount of space is allowed, e.g. `[ ⟨ 1 0 -4] ⟨ 0 1 4 ] ⟩`.

Commas are also allowed, e.g. `⟨12, 19, 28]`.

You can use outer brackets on the (co)vectors if preferred, e.g. `[⟨12 19 28]⟩` or `⟨[4 -4 1⟩]`.

For outer brackets, it's acceptable to use square brackets on both sides, so long as variance is indicated by the
interior (co)vectors, e.g. `[⟨1 0 -4] ⟨0 1 4]]`.

It is also acceptable to input things directly into this library's internal data structure, which is based on how
Wolfram Language treats matrices as nested lists, e.g. `{{{1, 0, -4}, {0, 1, 4}}, "map"}`
or `{{{4, -4, 1}, {-7, 0, 3}}, "comma basis"}`. These structures open with three braces (`{`), which Wolfram Language
uses for lists. The outermost list is an ordered pair of a matrix and a variance. The matrix in turn is a list of lists,
so that accounts for the other two braces. The variance is a string which tells whether the inner lists of the matrix
are vectors or covectors. Recognized variance strings for covariant matrices:

* `"map"` (* TODO: update these *)
* `"covector"`
* `"covariant"`
* `"m"`
* `"map"`
* `"map"`
* `"et"`
* `"edo"`
* `"edomapping"`
* `"val"`
* `"with"`

Recognized variance strings for contravariant matrices:

* `"vector"`
* `"contravector"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"c"`
* `"comma"`
* `"comma basis"`
* `"commaBasis""`
* `"comma_basis""`
* `"i"`
* `"interval"`
* `"g"`
* `"generator"`
* `"pcv"`
* `"gcv"`
* `"monzo"`
* `"against"`

## edge cases

For 0-rank mappings or 0-nullity comma bases, the temperament's dimensionality `d` is encoded by a single row of `d`
zeros. For example, the mapping `{{{0, 0, 0, 0}}, "map"}` indicates the 7-limit because it is 4D.

## output

You can set the global variable `format` to one of three things:

* `display` (default): results displayed using Wolfram Language's `MatrixForm`, as numbers arranged in rows and columns
* `EBK`: results will be printed as EBK strings, in our preferred style `[⟨1 0 -4] ⟨0 1 4]⟩`
* `Wolfram`: results will be displayed in our underlying data structure, e.g. `{{{1, 0, -4}, {0, 1, 4}}, "map"}`

## conventional single-letter variable names

### basic data structures

* `l`: list (e.g. vector, covector)
* `a`: matrix

### temperaments

* `t = {a, variance}`: temperament, represented as a mapping or comma basis
* `m = {a, variance}`: temperament, represented as a mapping
* `c = {a, variance}`: temperament, represented as a comma basis

### properties of temperaments

* `d`: dimensionality
* `r`: rank
* `n`: nullity

This library is designed such that every public method returns its result
in [canonical form](https://en.xen.wiki/w/canonical_form). This is for convenience, and supported by the fact that in EA
the dual function was defined to automatically canonicalize.

## roadmap

The following features are planned:

* IO
  * quotient sets
  * units
* error handling
  * enfactored temperaments
  * temperament merging across different dimensionalities
  * impossible interval basis changes
  * \>3D tuning damage graph requests
* additional features
  * generator size manipulation (mingen form, etc.)
  * *simplest* generators preimage transversal
  * unreduce mappings to merged ETs
  * irrational interval bases
* new modules
  * scales & lattices
  * temperament complexity & badness
  * timbre
  * notation
  * temperament classification
  * chords

Please report any bugs you find and we'll be happy to investigate ASAP. Pull requests are also welcome.

## credits

These implementations were developed by [Dave Keenan](https://en.xen.wiki/w/Dave_Keenan)
and [Douglas Blumeyer](https://en.xen.wiki/w/Douglas_Blumeyer) in 2021. Several of them were adapted from or inspired by
algorithms described by [Gene Ward Smith](https://en.xen.wiki/w/Gene_Ward_Smith).
