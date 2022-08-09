# EA

[EA](https://en.xen.wiki/w/Intro_to_exterior_algebra_for_RTT) utilities, implemented
in [Wolfram Language](https://www.wolfram.com/language/) (formerly Mathematica), a popular and capable programming
language for working with math.

This file relies on the modules in `main.m`. You will need to add that file to scope, and then you will be able to use
this one.

This file contains the following functions:

* `eaGetD`
* `eaGetR`
* `eaGetN`
* `eaCanonicalForm`
* `eaDual`
* `multivectorToMatrix`
* `matrixToMultivector`
* `progressiveProduct`
* `regressiveProduct`
* `interiorProduct`
* `eaSum`
* `eaDiff`

It is based on material from the following article:

* [Dave Keenan & Douglas Blumeyer's guide to EA for RTT](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_EA_for_RTT)

## functions

### multivector utilities

#### get dimensionality 

`eaGetD[multivector]`

Given a representation of a temperament as a multivector,
returns the dimensionality.

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      eaGetD[meantoneMm]

Out   3
```

```
In    meantoneMc = {{4, -4, 1}, 1, "vector"};
      eaGetD[meantoneMc]

Out   3
```

#### get rank

`eaGetR[multivector]`

Given a representation of a temperament as a multivector,
returns the rank.

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      eaGetR[meantoneMm]

Out   2
```

```
In    meantoneMc = {{4, -4, 1}, 1, "vector"};
      eaGetR[meantoneMc]

Out   2
```

#### get nullity

`eaGetN[multivector]`

Given a representation of a temperament as a multivector,
returns the nullity.

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      eaGetN[meantoneMm]

Out   1
```

```
In    meantoneMc = {{4, -4, 1}, 1, "vector"};
      eaGetN[meantoneMc]

Out   1
```

### canonicalization

#### canonical form

`eaCanonicalForm[multivector]`
  
Returns the given multivector in canonical form.

If a multimap, the GCD is extracted,
and the leading entry is normalized to positive.
If a multicomma, the GCD is extracted,
and the trailing entry is normalized to positive.

```
In    enfactoredMeantoneMm = {{2, 8, 8}, 2, "map"};
      eaCanonicalForm[enfactoredMeantoneMm]
  
Out   {{1, 4, 4}, 2, "map"}
```

```
In    wrongSignMeantoneMc = {{-4, 4, -1}, 1, "vector"};
      eaCanonicalForm[wrongSignMeantoneMc]
  
Out   {{4, -4, 1}, 1, "vector"}
```

### dual

#### dual

`eaDual[multivector]`

Given a multivector, returns its dual in canonical form.

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      eaDual[meantoneMm]

Out   {{4, -4, 1}, 1, "vector"}
```

```
In    nilovector = {{1}, 0, "vector"};
      d = 3
      eaDual[nilovector, d]

Out   {{1}, 0, "map"}
```

### conversion to and from matrix

#### multivector to matrix

`multivectorToMatrix[multivector]`

Given a temperament represented as a multivector,
returns the corresponding mapping or comma basis
(given a multimap, returns the corresponding mapping, or
given a multicomma, returns the corresponding comma basis).
The matrix is returned in canonical form.

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      multivectorToMatrix[meantoneMm]

Out   {{{1, 0, -4}, {0, 1, 4}}, "map"}
```

#### matrix to multivector

`matrixToMultivector[m]`

Given a temperament represented as a mapping or comma basis,
returns the corresponding multivector
(for a mapping, returns a multimap, or
for a comma basis, returns a multicomma).
The multivector is returned in canonical form.

```
In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "map"};
      matrixToMultivector[meantoneM]

Out   {{1, 4, 4}, 2, "map"}
```

### merge

#### progressive product

`progressiveProduct[multivector1, multivector2]`

Given two multivectors, returns the multivector result for their progressive product.

Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.

Also known as the wedge product or the exterior product.

```
In    et5 = {{5, 8, 12}, 1, "map"};
      et7 = {{7, 11, 16}, 1, "map"};
      progressiveProduct[et5, et7]

Out   {{1, 4, 4}, 2, "map"}
```

#### regressive product

`regressiveProduct[multivector1, multivector2]`

Given two multivectors, returns the multivector result for their regressive product.

Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.

Also known as the vee product.

```
In    et5 = {{5, 8, 12}, 1, "map"};
      et7 = {{7, 11, 16}, 1, "map"};
      regressiveProduct[et5, et7]

Out   {{1, 4, 4}, 2, "map"}
```

#### interior product

`interiorProduct[multivector1, multivector2]`

Given two multivectors, returns the multivector result for their symmetric interior product.
By "symmetric", it is meant that it chooses either the right or left interior product
depending on the grades of the input multivectors.

Also known as the vee product.

```
In    et5 = {{5, 8, 12}, 1, "map"};
      et7 = {{7, 11, 16}, 1, "map"};
      interiorProduct[et5, et7]

Out   {{1, 4, 4}, 2, "map"}
```

### addition

#### sum

`eaSum[u1, u2]`

Sums the given multivectors: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be decomposed into a set of vectors
that are identical except for a single vector (or covector, if covariant)),
entry-wise sums the multivectors, then canonicalizes the result,
returning a single new multivector with the same dimensions as the inputs.

If the given multivectors are not the same dimensions and addable,
it will error.

Can accept multivectors of different variances,
but it will return a multivector with the same variance
as the first given multivector.

```
In    meantoneMc = {{4, -4, 1}, 1, "vector"};
      porcupineMc = {{1, -5, 3}, 1, "vector"};
      eaSum[meantoneMc, porcupineMc]

Out   {{{5, -9, 4}}, "vector"}
```

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      porcupineMm = {{3, 5, 1}, 2, "map"};
      eaSum[meantoneMm, porcupineMm]

Out   {{{1, 1, 1}, {0, 4, 9}}, "map"}
```

#### diff

`eaDiff[u1, u2]`

Diffs the given multivectors: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be decomposed into a set of vectors
that are identical except for a single vector (or covector, if covariant)),
entry-wise diffs the multivectors, then canonicalizes the result,
returning a single new multivector with the same dimensions as the inputs.

If the given multivectors are not the same dimensions and addable,
it will error.

Can accept multivectors of different variances,
but it will return a multivector with the same variance
as the first given multivector.

```
In    meantoneMc = {{4, -4, 1}, 1, "vector"};
      porcupineMc = {{1, -5, 3}, 1, "vector"};
      eaDiff[meantoneMc, porcupineMc]

Out   {{-3, -1, 2}, 1, "vector"}
```

```
In    meantoneMm = {{1, 4, 4}, 2, "map"};
      porcupineMm = {{3, 5, 1}, 2, "map"};
      eaDiff[meantoneMm, porcupineMm]

Out   {{2, 1, -3}, 2, "map"}
```

## data structures

Multivectors are implemented in this library as ordered triplets:

1. the list of largest-minors
2. the grade (the count of brackets)
3. the variance (whether the brackets point to the left or the right)

In the case of nilovectors, a fourth entry is required in order to fully specify the temperament: the dimensionality.

All multivectors in this library are varianced. So "multivector" refers to multivectors that may be of either variance,
and "contravariant multivector" and "covariant multivector" are used for the specific variances.

Examples:

* meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as `{{1, 4, 4}, 2, "co"}`
* meantone's multicomma [4 -4 1⟩ is input as `{{4, -4, 1}, 1, "contra"}`

Recognized variance strings for covariant multivectors:

* `"co"`
* `"covector"`
* `"covectors"`
* `"multicovector"`
* `"covariant"`
* `"m"`
* `"map"`
* `"maps"`
* `"multimap"`
* `"val"`
* `"vals"`
* `"multival"`
* `"with"`
* `"mm"`

Recognized variance strings for contravariant multivectors:

* `"contra"`
* `"contravector"`
* `"contravectors"`
* `"multicontravector"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"vectors"`
* `"c"`
* `"comma"`
* `"commas"`
* `"multicomma"`
* `"i"`
* `"interval"`
* `"intervals"`
* `"multinterval"`
* `"multiinterval"`
* `"monzo"`
* `"monzos"`
* `"multimonzo"`
* `"against"`
* `"wedgie"`
* `"mc"`

## edge cases

Note that while nilovectors are essentially scalars, their first entry is still technically a largestMinorsL *list*,
albeit one with a single entry. So for example, the scalar `5` is input as `{{5}, 0, v, d}`. This indicates the number 5
nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has
brackets.

## conventional single-character (or double-character) variable names

### multivectors

* `u = {largestMinorsL, variance, grade, d}`: temperament, represented as a multivector
* `mm`: multimap, a covariant `u`
* `mc`: multicomma, a contravariant `u`

## roadmap

The following features are planned:

* IO
    * EBK notation
    * matrix display
* error handling
    * progressive product across different dimensionalities
    * minors lists not matching grade
