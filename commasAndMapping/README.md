# Commas & mapping

This file contains functions related to temperaments, focusing on commas and mapping:

* `canonicalForm`
* `dual`
* `mapMerge`
* `commaMerge`
* `changeIntervalBasis`
* `sum`
* `diff`
* `getGeneratorsPreimageTransversal`

It is based on material from the following articles:
* [Dave Keenan & Douglas Blumeyer's guide to RTT: commas & mapping](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_commas_&_mapping)
* [Defactoring algorithms](https://en.xen.wiki/w/Defactoring_algorithms)
* [Temperament merging](https://en.xen.wiki/w/Temperament_merging)
* [Temperament addition](https://en.xen.wiki/w/Temperament_addition)

## roadmap

The following features are planned:

* error handling
    * enfactored temperaments
    * temperament merging across different dimensionalities
    * impossible interval basis changes
    * \>3D tuning damage graph requests
* generator size manipulation (mingen form, etc.)
* *simplest* generators preimage transversal
* unreduce mappings to merged ETs
* irrational interval bases
