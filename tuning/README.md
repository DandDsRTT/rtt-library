# Tuning

This file contains functions related to temperament tunings, and in particular, schemes for optimizing generator sizes:

* `optimizeGeneratorsTuningMap`
* `getGeneratorsTuningMapMeanDamage`
* `getGeneratorsTuningMapDamages`
* `optimizeTuningMap`
* `getTuningMapMeanDamage`
* `getTuningMapDamages`
* `graphTuningDamage`
* `generatorsTuningMapFromTAndTuningMap`

This file relies on the modules in `main.m`. You will need to add that file to scope, and then you will be able to use
this one.

This article is based on material from the following articles:

* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning computation](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_computation)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: all-interval tuning schemes](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_all-interval_tuningg_schemes)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: advanced tuning concepts](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_advanced_tuning_concepts)

## functions

In all cases, tuning schemes may be specified by original name (e.g. `"TOP"`), systematic name (`"minimax-S"`), or by
individual parameters.

### optimization

#### generators tuning map

`optimizeGeneratorsTuningMap[t, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
and a tuning scheme, returns the optimum generator tuning map.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[
            meantoneM, 
            {
                "optimizationPower" -> \[Infinity], 
                "damageWeightingSlope" -> "simplicityWeighted"
            }
        ]

Out     "⟨1201.69 697.563]"
```

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[meantoneM, "TOP"]

Out     "⟨1201.70 697.563]"
```

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[meantoneM, "tid minisos-copfr-EC"]

Out     "⟨1200.522 1897.112]"
```

#### tuning map

`optimizeTuningMap[t, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
and a tuning scheme, returns the optimum tuning map.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[
            meantoneM, 
            {
                "optimizationPower" -> \[Infinity], 
                "damageWeightingSlope" -> "simplicityWeighted"
            }
        ]

Out     "⟨1201.69 1899.26 2790.25]"
```

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[meantoneM, "TOP"]

Out     "⟨1201.70 1899.26 2790.25]"
```

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[meantoneM, "tid minisos-copfr-EC"]

Out     "⟨1200.522 1897.112 2786.363]"
```

### mean damage

#### generators tuning map

`getGeneratorsTuningMapMeanDamage[t, generatorsTuningMap, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
plus a tuning map for that temperament, and a tuning scheme,
returns how much damage this tuning map causes this temperament using this tuning scheme.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapMeanDamage[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]

Out     3.39251
```

#### tuning map

`getTuningMapMeanDamage[t, tuningMap, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
plus a tuning map for that temperament, and a tuning scheme,
returns how much damage this tuning map causes this temperament using this tuning scheme.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapMeanDamage[meantoneM, quarterCommaTuningMap, "minimax-S"]

Out     3.39236
```

### damages

#### generators tuning map

`getGeneratorsTuningMapDamages[t, generatorsTuningMap, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
plus a tuning map for that temperament, and a tuning scheme,
returns the damages to each of the targeted intervals.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapDamages[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]

Out     {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
```

#### tuning map

`getTuningMapDamages[t, tuningMap, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis,
plus a tuning map for that temperament, and a tuning scheme,
returns the damages to each of the targeted intervals.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapDamages[meantoneM, quarterCommaTuningMap, "minimax-S"]

Out     {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
```

### target set schemes

#### Target tunings diamond

`getTtd[maxOdd]`

Given a maximum odd number, returns a list of quotients following the pattern established for the historical tunings
called "Minimax" and "Least squares", per the Target tunings page of the xen wiki (essentially Partch's tonality
diamond, but without the 1/1 which is not useful as a tuning target).

```
In      getTtd[5]

Out     {3/2, 4/3, 5/4, 8/5, 5/3, 6/5}
```

#### odd half-diamond

`getOhd[maxOdd]`

Given a maximum odd number, returns a list of quotients which are simliar to Partch's tonality diamond, except not
octave reduced, and then only those greater than 1 are included.

```
In      getTtd[5]

Out     {3/1, 5/1, 5/3}
```

#### truncated integer diamond

`getTid[maxInteger]`

Given a maximum integer, returns a list of quotients with numerator greater than the demoninator, numerator less than or
equal to the maximum integer, the size of the quotient between 15/13 and 13/4 (inclusive), and the numerator times the
denominator being less than the maximum integer times 13.

```
In      getTid[6]

Out     {2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}
```

#### otonal chord

`getOtonalChord[harmonics]`

Given a list of harmonics, returns a list of intervals between each of those harmonics.

```
In      getOtonalChord[{4, 5, 6, 7}]

Out     {5/4, 3/2, 7/4, 6/5, 7/5, 7/6}
```

### graphing

#### graph tuning damage

`graphTuningDamage[t, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis, and a tuning scheme,
graphs the damage to the targeted intervals within a close range around the optimum tuning.
Graphs in 2D for a rank-1 temperament, 3D for a rank-2 temperament, and errors otherwise.

```
In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
graphTuningDamage[meantoneM, "minisos-copfr-EC"]

Out   (3D graph)
```

```
In    12etM = "⟨12 19 28]";
graphTuningDamage[12etM, "minisos-copfr-EC"]

Out   (2D graph)
```

### conversion

### generators tuning map from temperament and tuning map

`generatorsTuningMapFromTAndTuningMap[t, tuningMap]`

Given a representation of a temperament as a mapping or comma basis,
plus a tuning map, returns the generators tuning map.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        generatorsTuningMapFromTAndTuningMap[meantoneM, quarterCommaTuningMap]

Out     "⟨1200.000 696.578]";
```

## roadmap

The following features are planned:

* tuning
    * diamond tradeoff and monotone tuning ranges
    * projection and generators matrices
    * "TOCTE" tuning and possibly other new tunings invented recently by Flora Canou
    * custom precision/accuracy
    * `getComplexity` should support original complexity names
    * exact results (not decimals)
    * `graphTuningDamage`
        * visualize the solution
        * user-controlled zoom
        * 3D graph checkerboard to translucent black continuum per optimization power
        * ability to specify which norms are included
        * titles and other info
        * contour-style topographic graphs for 3D
        * test failures automatically graph

