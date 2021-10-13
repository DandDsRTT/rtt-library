# The RTT Library

Regular Temperament Theory (RTT) utilities, implemented in [Wolfram Language](https://www.wolfram.com/language/) (formerly Mathematica), a popular and capable programming language for working with math. 

## how to use

If you have access to the Wolfram Language desktop application (a paid option), you can copy the code into that environment and run it there.

Otherwise, the best option is to create a free account on [Wolfram Cloud](https://www.wolframcloud.com), where you can use these functions for free right on the web without downloading or setting anything up on your computer. Just sign up for an account, create a new computational notebook, paste any of these code snippets in to a cell, and Shift+Enter to run them; you'll be computing temperaments and such in no time. FYI, any notebook you create has a lifespan of 60 days before Wolfram will recycle it, so you'll have to copy and paste them to new notebooks or wherever if you don't want to lose your work.

Most of the functions you need are available in `/main.nb`. The top half of that file has the public functions you would use yourself, and the bottom half has the private functions that those rely on but you don't need to use directly. So just paste the entirety of `/main.nb` into one Wolfram Cloud notebook, run the whole thing, and all the functions in the library will be globally available (context is shared between notebooks). You can start another notebook in another browser tab and start working.

## data structures

Temperament representations, such as mappings and comma-bases, look like this in this library:

* meantone's mapping \[⟨1 0 -4] ⟨0 1 4]⟩ is input as `{{{1, 0, -4}, {0, 1, 4}}, "mapping"}`
* 12-ET's comma-basis ⟨\[4 -4 1⟩ \[-7 0 3⟩] is input as `{{{4, -4, 1}, {-7, 0, 3}}, "comma-basis"}`

These structures open with three braces (`{`), which Wolfram Language uses for lists. The outermost list is an ordered pair of a matrix and a variance. The matrix in turn is a list of lists, so that accounts for the other two braces. The variance is a string which tells whether the inner lists of the matrix are vectors or covectors.

Valid variance strings for covariant matrices:
* `"co"`
* `"covector"`
* `"covariant"`
* `"m"`
* `"map"`
* `"mapping"`
* `"et"`
* `"edo"`
* `"edomapping"`
* `"val"`
* `"with"`

Valid variance strings for contravariant matrices:
* `"contra"`
* `"contravector"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"c"`
* `"comma"`
* `"comma-basis"`
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

For 0-rank mappings or 0-nullity comma-bases, the temperament's dimensionality `d` is encoded by a single row of `d` zeros. For example, the mapping `{{{0, 0, 0, 0}}, "mapping"}` indicates the 7-limit because it is 4D. 

## conventional single-letter variable names

### basic data structures
* `l`: list (e.g. vector, covector)
* `a`: matrix

### temperament-specific matrices
* `m`: (temperament) mapping matrix
* `c`: comma-basis matrix
* `p`: projection mapping matrix
* `g`: generator matrix
* `j`: [JIP](https://en.xen.wiki/w/JIP) matrix

### temperaments
* `v`: variance
* `t = {a, v}`: temperament, represented as a mapping or comma-basis

### properties of temperaments
* `d`: dimensionality
* `r`: rank
* `n`: nullity

## canonical form

This library is designed such that every public method returns its result in [canonical form](https://en.xen.wiki/w/canonical_form). This is for convenience, and supported by the fact that in VEA the dual function was defined to automatically canonicalize.

## VEA

### data structures

If you are interested in [VEA](https://en.xen.wiki/w/VEA), multivectors are implemented in this library as ordered triplets:

1. the list of minor determinants
2. the variance (whether the brackets point to the left or the right)
3. the grade (the count of brackets)
4. the dimensionality

All multivectors in this library are varianced. So "multivector" refers to multivectors that may be of either variance, and "contravariant multivector" and "covariant multivector" are used for the specific variances.

Examples:

* meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as `{{1, 4, 4}, "co", 2, 3}`
* meantone's multicomma [4 -4 1⟩ is input as `{{4, -4, 1}, "contra", 1, 3}`

Valid variance strings for covariant multivectors:
* `"co"`
* `"covector"`
* `"multicovector"`
* `"covariant"`
* `"m"`
* `"map"`
* `"multimap"`
* `"val"`
* `"multival"`
* `"with"`

Valid variance strings for contravariant multivectors:
* `"contra"`
* `"contravector"`
* `"multicontravector"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"c"`
* `"comma"`
* `"multicomma"`
* `"i"`
* `"interval"`
* `"multinterval"`
* `"multiinterval"`
* `"monzo"`
* `"multimonzo"`
* `"against"`
* `"wedgie"`

### edge cases

Note that while nilovectors are essentially scalars, their first entry is still technically a minors *list*, albeit one with a single entry. So for example, the scalar `5` is input as `{{5}, v, 0, d}`. This indicates the number 5 nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has brackets.

### conventional variable names

#### basic data structures

* `tensor`: tensor

#### multivector-specific lists

* `minors`: minor( determinant)s list

#### multivectors

* `w = {minors, v, grade, d}`: temperament, represented as a multivector (`w` as a reference to "wedgie")

#### properties of multivectors

* `grade`: grade

## credits

These implementations were developed by [Dave Keenan](https://en.xen.wiki/w/Dave_Keenan) and [Douglas Blumeyer](https://en.xen.wiki/w/Douglas_Blumeyer) in 2021. Several of them were adapted from or inspired by algorithms described by [Gene Ward Smith](https://en.xen.wiki/w/Gene_Ward_Smith).
