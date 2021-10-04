# The RTT Library

Regular Temperament Theory (RTT) utilities, implemented in [Wolfram Language](https://www.wolfram.com/language/) (formerly Mathematica), a popular and capable programming language for working with math. 

## how to use

If you have access to the Wolfram Language desktop application (a paid option), you can copy the code into that environment and run it there.

Otherwise, the best option is to create a free account on [Wolfram Cloud](https://www.wolframcloud.com), where you can use these functions for free right on the web without downloading or setting anything up on your computer. Just sign up for an account, create a new computational notebook, paste any of these code snippets in to a cell, and Shift+Enter to run them; you'll be computing temperaments and such in no time. FYI, any notebook you create has a lifespan of 60 days before Wolfram will recycle it, so you'll have to copy and paste them to new notebooks or wherever if you don't want to lose your work.

Most of the functions you need are available in `/public.nb`, but they rely on helper functions defined in `/private.nb`. You can paste the entirety of `/private.nb` into one Wolfram Cloud notebook, run the whole thing, and those helpers will all be globally available (context is shared between notebooks). You can start another notebook in another browser tab where you paste in only what you need from `/public.nb` then.

## data structures

Matrices, such as RTT mappings and comma-bases, look like this in Wolfram Language:

* meantone's mapping \[⟨1 0 -4] ⟨0 1 4]⟩ is input as `{{1, 0, -4}, {0, 1, 4}}`
* 12-ET's comma-basis ⟨\[4 -4 1⟩ \[-7 0 3⟩] is input as `Transpose[{{4, -4, 1}, {-7, 0, 3}}]` or if you prefer `{{4, -7}, {-4, 0}, {1, 3}}`

## edge cases

When performing operations on objects with grade of zero, such as rank-0 mappings, nullity-0 comma-bases, it may be necessary to supply the dimensionality of the system somehow. With matrices, this can be done by using a matrix which consists of a single row of `d` zeros, where `d` is the dimensionality of the system, e.g. the mapping `{{0, 0, 0, 0}}` indicates the 7-limit because it is 4D (in the case of a comma-basis, you'd provide a single column of zeros, like `{{0}, {0}, {0}, {0}}`).

## conventional variable names

* `a`: matrix
* `m`: mapping
* `c`: comma-basis
* `r`: rank
* `n`: nullity
* `l`: list

## canonical form

This library is designed such that every method returns its result in [canonical form](https://en.xen.wiki/w/canonical_form). This is for convenience, and supported by the fact that in VEA the dual function was defined to automatically canonicalize (in the case of a multimap, extract GCD and normalize to a positive leading entry).

## VEA 

### data structures

If you are interested in [VEA](https://en.xen.wiki/w/VEA), vectorals are implemented in this library as ordered pairs, with the first element being the list of minor determinants, and the second element being the variance-signed grade (in other words, the count of brackets the list is nested within, where a negative sign indicates that the angle brackets point to the left and positive indicates that they point to the right):

* meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as `{{1, 4, 4}, -2}`
* meantone's multicomma [4 -4 1⟩ is input as `{{28, -19, 12}, 1}`

### edge cases

In the case of grade-0 vectorals, you will just need to provide the grade of the dual, or the dimensionality, as a second argument to the function.

Note that grade-0 vectorals, despite being essentially scalars, nonetheless require that you put the value inside a list. For example, the scalar `5` is input as `{{5}, 0}`. This indicates the number 5 nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has brackets.

### conventional variable names

* `v`: vectoral
* `s`: variance-signed grade
* `g`: grade
* `d`: dimensionality
* `w`: the minor determinants list part of a vectoral (the part other than the variance-signed grade) (`w` because of "wedgie")
* `t`: tensor

## credits

These implementations were developed by [Dave Keenan](https://en.xen.wiki/w/Dave_Keenan) and [Douglas Blumeyer](https://en.xen.wiki/w/Douglas_Blumeyer) in 2021. Several of them were adapted from or inspired by algorithms described by [Gene Ward Smith](https://en.xen.wiki/w/Gene_Ward_Smith).
