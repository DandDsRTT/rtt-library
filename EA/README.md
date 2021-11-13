# EA

[EA](https://en.xen.wiki/w/Intro_to_exterior_algebra_for_RTT) utilities, implemented in [Wolfram Language](https://www.wolfram.com/language/) (formerly Mathematica), a popular and capable programming language for working with math. 

This library relies on the modules at the top-level. You will need to add the basic RTT functions to scope and then you will be able to use these. Many concepts and conventions here build upon those in that library.

## data structures

Multivectors are implemented in this library as ordered triplets:

1. the list of minor determinants
2. the grade (the count of brackets)
3. the variance (whether the brackets point to the left or the right)

In the case of nilovectors, a fourth entry is required in order to fully specify the temperament: the dimensionality.

All multivectors in this library are varianced. So "multivector" refers to multivectors that may be of either variance, and "contravariant multivector" and "covariant multivector" are used for the specific variances.

Examples:

* meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as `{{1, 4, 4}, 2, "co"}`
* meantone's multicomma [4 -4 1⟩ is input as `{{4, -4, 1}, 1, "contra"}`

Recognized variance strings for covariant multivectors:
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

Recognized variance strings for contravariant multivectors:
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

## edge cases

Note that while nilovectors are essentially scalars, their first entry is still technically a minors *list*, albeit one with a single entry. So for example, the scalar `5` is input as `{{5}, 0, v, d}`. This indicates the number 5 nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has brackets.

## conventional variable names

### basic data structures

* `tensor`: tensor

### multivector-specific lists

* `minors`: minor( determinant)s list

### multivectors

* `w = {minors, v, grade, d}`: temperament, represented as a multivector (`w` as a reference to "wedgie")

### properties of multivectors

* `grade`: grade

## credits

These implementations were developed by [Dave Keenan](https://en.xen.wiki/w/Dave_Keenan) and [Douglas Blumeyer](https://en.xen.wiki/w/Douglas_Blumeyer) in 2021. Several of them were adapted from or inspired by algorithms described by [Gene Ward Smith](https://en.xen.wiki/w/Gene_Ward_Smith).
