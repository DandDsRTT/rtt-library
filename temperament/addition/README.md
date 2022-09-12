# Temperament addition

This module contains the following functions:
* `sum`
* `diff`

It is based on material from the following article:

* [Temperament addition](https://en.xen.wiki/w/Temperament_addition)

## functions

### sum

`sum[t1, t2]`

Sums the given temperaments: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be put into a form where
they are identical except for a single basis vector (or covector, if covariant)),
entry-wise sums this pair of linearly independent basis (co)vectors,
recombines them with identical vectors (their linear-dependence basis),
corrects for negativity, then canonicalizes the result,
returning a single new temperament with the same dimensions as the inputs.

If the given temperaments are not the same dimensions and addable,
it will error.

Can accept temperament representations of different variances,
but it will return a temperament with the same variance
as the first given temperament representation.

```
In    meantoneC = "[4 -4 1⟩";
      porcupineC = "[1 -5 3⟩";
      sum[meantoneC, porcupineC]

Out   "[5 -9 4⟩"
```

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
      porcupineM = "[⟨1 2 3] ⟨0 3 5]⟩";
      sum[meantoneM, porcupineM]

Out   "[⟨1 1 1] ⟨0 4 9]⟩"
```

### diff

`diff[t1, t2]`

Diffs the given temperaments: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be put into a form where
they are identical except for a single basis vector (or basis covector, if covariant)),
entry-wise diffs this pair of linearly independent basis (co)vectors,
recombines them with identical vectors (their linear-dependence basis),
corrects for negativity, then canonicalizes the result,
returning a single new temperament with the same dimensions as the inputs.

If the given temperaments are not the same dimensions and addable,
it will error.

Can accept temperament representations of different variances,
but it will return a temperament with the same variance
as the first given temperament representation.

```
In    meantoneC = "[4 -4 1⟩";
      porcupineC = "[1 -5 3⟩";
      diff[meantoneC, porcupineC]

Out   "[-3 -1 2⟩"
```

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
      porcupineM = "[⟨1 2 3] ⟨0 3 5]⟩";
      diff[meantoneM, porcupineM]

Out   "[⟨1 1 2] ⟨0 2 1]⟩"
```
