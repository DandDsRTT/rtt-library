# Target interval set schemes

This module contains functions which produce target interval sets according to predefined schemes. Like TILT, but
that's the default and is found in the main tuning module. This module includes:

* `getOld`
* `getOtonalChord`

This module is based on material from the following articles:

* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals#Target interval sets](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals#Target_interval_sets)

## functions

### odd limit diamond (OLD)

`getOld[maxOdd]`

Given a maximum odd number, returns a list of quotients following the pattern established for the historical tunings
called "Minimax" and "Least squares", per the Target tunings page of the xen wiki (essentially Partch's tonality
diamond, but with 2/1 instead of 1/1, which is not useful as a tuning target).

```
In      getOld[5]

Out     {2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5}
```

### otonal chord

`getOtonalChord[harmonics]`

Given a list of harmonics, returns a list of intervals between each of those harmonics.

```
In      getOtonalChord[{4, 5, 6, 7}]

Out     {5/4, 3/2, 7/4, 6/5, 7/5, 7/6}
```
