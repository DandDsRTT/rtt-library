# Temperament addition with EA

This module contains the following functions:
* `eaSum`
* `eaDiff`

This module is based on material from:

* [Douglas Blumeyer and Dave Keenan's Intro to exterior algebra for RTT#Temperament addition](https://en.xen.wiki/w/Douglas_Blumeyer_and_Dave_Keenan%27s_Intro_to_exterior_algebra_for_RTT#Temperament_addition)

This module also requires `temperament/addition`.

## functions

### sum

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
In    meantoneMc = {{4, -4, 1}, 1, "col"};
      porcupineMc = {{1, -5, 3}, 1, "col"};
      eaSum[meantoneMc, porcupineMc]

Out   {{{5, -9, 4}}, "col"}
```

```
In    meantoneMm = {{1, 4, 4}, 2, "row"};
      porcupineMm = {{3, 5, 1}, 2, "row"};
      eaSum[meantoneMm, porcupineMm]

Out   {{{1, 1, 1}, {0, 4, 9}}, "row"}
```

### diff

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
In    meantoneMc = {{4, -4, 1}, 1, "col"};
      porcupineMc = {{1, -5, 3}, 1, "col"};
      eaDiff[meantoneMc, porcupineMc]

Out   {{-3, -1, 2}, 1, "col"}
```

```
In    meantoneMm = {{1, 4, 4}, 2, "row"};
      porcupineMm = {{3, 5, 1}, 2, "row"};
      eaDiff[meantoneMm, porcupineMm]

Out   {{2, 1, -3}, 2, "row"}
```
