# Temperament

This module contains temperament exploration functions such as:

* `getD`
* `getR`
* `getN`
* `canonicalForm`
* `dual`
* `mapMerge`
* `commaMerge`
* `getGeneratorPreimageTransversal`

It is based on material from the following article series:

* [Dave Keenan & Douglas Blumeyer's guide to RTT: exploring temperaments](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_exploring_temperaments)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: mappings](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_mappings)
* [Defactoring algorithms](https://en.xen.wiki/w/Defactoring_algorithms)
* [Temperament merging](https://en.xen.wiki/w/Temperament_merging)

## functions

### dimensions

#### get dimensionality

`getD[t]`

Given a representation of a temperament as a mapping or comma basis,
returns the dimensionality.

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]}";
      getD[meantoneM]

Out   3
```

```
In    meantoneC = "[4 -4 1⟩";
      getD[meantoneC]

Out   3
```

#### get rank

`getR[t]`

Given a representation of a temperament as a mapping or comma basis,
returns the rank.

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]}";
      getR[meantoneM]

Out   2
```

```
In    meantoneC = "[4 -4 1⟩";
      getR[meantoneC]

Out   2
```

#### get nullity

`getN[t]`

Given a representation of a temperament as a mapping or comma basis,
returns the nullity.

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]}";
      getN[meantoneM]

Out   1
```

```
In    meantoneC = "[4 -4 1⟩";
      getN[meantoneC]

Out   1
```

### canonicalization

#### canonical form

`canonicalForm[t]`

Returns the given temperament representation — whether mapping or comma basis —
in canonical form: defactored, then put into Hermite Normal Form.

```
In    someMeantoneM = "[⟨5 8 12] ⟨7 11 16]}";
      canonicalForm[someMeantoneM]

Out   "[⟨1 0 -4] ⟨0 1 4]}"
```

```
In    someMeantoneC = "[-8 8 -2⟩";
      canonicalForm[someMeantoneC]

Out   "[4 -4 1⟩"
```

### dual

#### dual

`dual[t]`

Returns the dual for the given temperament representation
(if given a mapping, the comma basis, or vice-versa).

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]}";
      dual[meantoneM]

Out   "[4 -4 1⟩"
```

### merge

#### map merge

`mapMerge[t1, t2, t3...]`

Merges the given temperaments' maps:
concatenates their mappings
and puts the result into canonical form.

Can accept any number of temperaments representations,
as any combination of mappings or comma bases,
but returns the temperament as a mapping.

```
In    et5M = "⟨5 8 12]";
      et7M = "⟨7 11 16]";
      mapMerge[et5M, et7M]

Out   "[⟨1 0 -4] ⟨0 1 4]}"
```

```
In    et7dM = "⟨7 11 16 19]";
      et12M = "⟨12 19 28 34]";
      et22M = "⟨22 35 51 62]";
      mapMerge[et7dM, et12M, et22M]

Out   "[⟨1 0 0 -5] ⟨0 1 0 2] ⟨0 0 1 2]}"
```

#### comma merge

`commaMerge[t1, t2, t3...]`

Merges the given temperaments' comma bases:
concatenates their comma bases
and puts the result into canonical form.

Can accept any number of temperament representations,
as any combination of mappings or comma bases,
but returns the temperament as a comma basis.

```
In    meantoneC = "[4 -4 1⟩";
      porcupineC = "[1 -5 3⟩";
      commaMerge[meantoneC, porcupineC]

Out   "[[-11 7 0⟩ [-7 3 1⟩]"
```

```
In    mintC = "[2 2 -1 -1⟩";
      meantoneC = "[4 -4 1 0⟩";
      negriC = "[-14 3 4 0⟩";
      commaMerge[mintC, meantoneC, negriC]

Out   "[[30 19 0 0⟩ [-26 15 1 0⟩ [-6 2 0 1⟩]"
```

### generator preimage transversal

#### get generator preimage transversal

`getGeneratorPreimageTransversal[t]`

Given a representation of a temperament as a mapping or comma basis,
returns a generator preimage transversal
(for each generator, one JI interval that maps to it).

```
In    meantoneM = "[⟨1 1 0] ⟨0 1 4]}"
      getGeneratorPreimageTransversal[meantoneM]

Out   "[[1 0 0⟩ [-1 1 0⟩]"
```

## data structures

Temperament representations, such as mappings and comma bases, may be input like this:

* 12-ET's map: `⟨12 19 28]`
* meantone's mapping: `[⟨1 0 -4] ⟨0 1 4]}`
* meantone's comma: `[4 -4 1⟩`
* 12-ET's comma basis: `[[4 -4 1⟩ [-7 0 3⟩]`
* quarter-comma meantone's tuning map: `⟨1200.000 696.578]`

Those are left `⟨` and right `⟩` angle braces there, but if these are not easy for you to type, less than `<` or
greater than `>` signs can be used instead.

Any amount of space is allowed, e.g. `[ ⟨ 1 0 -4] ⟨ 0 1 4 ] ⟩`.

Commas are also allowed, e.g. `⟨12, 19, 28]`.

You can use outer brackets on the (co)vectors if preferred, e.g. `[⟨12 19 28]}` or `[[4 -4 1⟩]`.

For outer brackets, it's acceptable to use square brackets on both sides, so long as variance is indicated by the
interior (co)vectors, e.g. `[⟨1 0 -4] ⟨0 1 4]]`.

It is also acceptable to input things directly into this library's internal data structure, which is based on how
Wolfram Language treats matrices as nested lists, e.g. `{{{1, 0, -4}, {0, 1, 4}}, "row"}`
or `{{{4, -4, 1}, {-7, 0, 3}}, "comma basis"}`. These structures open with three braces (`{`), which Wolfram Language
uses for lists. The outermost list is an ordered pair of a matrix and a variance. The matrix in turn is a list of lists,
so that accounts for the other two braces. The variance is a string which tells whether the inner lists of the matrix
are vectors or covectors. Recognized variance strings for covariant matrices:

* `"co"`
* `"covector"`
* `"covectors"`
* `"covariant"`
* `"m"`
* `"map"`
* `"maps"`
* `"mapping"`
* `"et"`
* `"ets"`
* `"edo"`
* `"edos"`
* `"edomapping"`
* `"edomappings"`
* `"val"`
* `"vals"`
* `"with"`
* `"row"`
* `"rows"`

Recognized variance strings for contravariant matrices:

* `"contra"`
* `"contravector"`
* `"contravectors"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"vectors"`
* `"c"`
* `"comma"`
* `"commas"`
* `"comma basis"`
* `"commaBasis""`
* `"comma_basis""`
* `"i"`
* `"interval"`
* `"intervals"`
* `"g"`
* `"generator"`
* `"generators"`
* `"pcv"`
* `"gcv"`
* `"monzo"`
* `"monzos"`
* `"against"`
* `"col"`
* `"cols"`

## edge cases

For 0-rank mappings or 0-nullity comma bases, the temperament's dimensionality `d` is encoded by a single row of `d`
zeros. For example, the mapping `{{{0, 0, 0, 0}}, "row"}` indicates the 7-limit because it is 4D.

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

* error handling
    * enfactored temperaments
    * temperament merging across different dimensionalities
* additional features
    * generator size manipulation (mingen form, etc.)
    * *simplest* generator preimage transversal
    * unreduce mappings to merged ETs
