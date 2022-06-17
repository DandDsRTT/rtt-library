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

It is based on material from the following articles:

* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: tuning computation](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_computation)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: infinite-target-set tuning schemes](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_infinite-target-set_tuningg_schemes)
* [Dave Keenan & Douglas Blumeyer's guide to RTT: advanced tuning concepts](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_advanced_tuning_concepts)

## roadmap

The following features are planned:

* tuning
    * diamond tradeoff and monotone tuning ranges
    * projection and generators matrices
    * "TOCTE" tuning
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

