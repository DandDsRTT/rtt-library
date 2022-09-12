# Tuning

This module contains functions related to temperament tunings, and in particular, schemes for optimizing generator sizes:

* `optimizeGeneratorsTuningMap`
* `getGeneratorsTuningMapMeanDamage`
* `getGeneratorsTuningMapDamages`
* `optimizeTuningMap`
* `getTuningMapMeanDamage`
* `getTuningMapDamages`
* `graphTuningDamage`
* `generatorsTuningMapFromTAndTuningMap`

This article is based on material from the following articles:

* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning computation](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_computation)

## functions

In all cases, tuning schemes may be specified by original name (e.g. `"TOP"`), systematic name (`"minimax-S"`), or by
individual parameters.

Note that anywhere a mapping is called for, a comma basis representation of a temperament will also work, if you have also loaded `temperament/main`.

### optimization

#### generators tuning map

`optimizeGeneratorsTuningMap[m, tuningSchemeSpec]`

Given a mapping and tuning scheme, returns the optimum generator tuning map.

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
        optimizeGeneratorsTuningMap[meantoneM, "tid miniRMS-copfr-EC"]

Out     "⟨1200.522 1897.112]"
```

#### tuning map

`optimizeTuningMap[m, tuningSchemeSpec]`

Given a mapping and tuning scheme, returns the optimum tuning map.

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
        optimizeTuningMap[meantoneM, "tid miniRMS-copfr-EC"]

Out     "⟨1200.522 1897.112 2786.363]"
```

### mean damage

#### generators tuning map

`getGeneratorsTuningMapMeanDamage[m, generatorsTuningMap, tuningSchemeSpec]`

Given a mapping, tuning map, and tuning scheme,
returns how much damage this tuning map causes this temperament using this tuning scheme.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapMeanDamage[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]

Out     3.39251
```

#### tuning map

`getTuningMapMeanDamage[m, tuningMap, tuningSchemeSpec]`

Given a mapping, tuning map, and tuning scheme,
returns how much damage this tuning map causes this temperament using this tuning scheme.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapMeanDamage[meantoneM, quarterCommaTuningMap, "minimax-S"]

Out     3.39236
```

### damages

#### generators tuning map

`getGeneratorsTuningMapDamages[m, generatorsTuningMap, tuningSchemeSpec]`

Given a mapping, tuning map, and tuning scheme,
returns the damages to each of the targeted intervals.

Note: for all-interval tuning schemes, it is impossible to return
an infinitely-long list of damages to all intervals.
Instead, the damages to the primes will be returned.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapDamages[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]

Out     {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
```

#### tuning map

`getTuningMapDamages[m, tuningMap, tuningSchemeSpec]`

Given a mapping, tuning map, and tuning scheme,
returns the damages to each of the targeted intervals.

Note: for all-interval tuning schemes, it is impossible to return
an infinitely-long list of damages to all intervals.
Instead, the damages to the primes will be returned.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapDamages[meantoneM, quarterCommaTuningMap, "minimax-S"]

Out     {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
```

### targeted interval set schemes

#### truncated integer limit triangle (TILT)

`getTilt[maxInteger]`

Given a maximum integer, returns a list of quotients with numerator greater than the demoninator, numerator less than or
equal to the maximum integer, the size of the quotient between 15/13 and 13/4 (inclusive), and the numerator times the
denominator being less than the maximum integer times 13.

```
In      getTilt[6]

Out     {2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}
```

### conversion

#### generators tuning map from temperament and tuning map

`generatorsTuningMapFromTAndTuningMap[m, tuningMap]`

Given a mapping and tuning map, returns the generators tuning map.

```
In      meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        generatorsTuningMapFromTAndTuningMap[meantoneM, quarterCommaTuningMap]

Out     "⟨1200.000 696.578]";
```

## traits

You may notice that a numbered system of tuning scheme traits is used in the code. This is not necessarily advocated for
general use; it's just something we found helpful when organizing our thoughts around the problem ourselves.

## roadmap

The following features are planned:

* tradeoff and monotone tuning ranges
* projection and generators matrices
* "TOCTE" tuning and possibly other new tunings invented recently by Flora Canou
* custom precision/accuracy
* `getComplexity` should support original complexity names
* exact results (not decimals)
