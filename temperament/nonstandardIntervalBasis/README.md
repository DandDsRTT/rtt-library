# Nonstandard interval basis

This module contains the following functions:
* `changeIntervalBasis`

This module is based on material from:
* [Temperament merging across interval bases#Changing interval basis](https://en.xen.wiki/w/Temperament_merging_across_interval_bases#Changing_interval_basis)

## functions

### change interval basis

`changeIntervalBasis[t, targetIntervalBasis]`

Changes the interval basis for the given temperament.

If the target interval basis is not possible
(such as a *super*space for a mapping, or a *sub*space for
a comma basis), the function will error.

```
In    meantoneC = "[4 -4 1⟩";
      targetIntervalBasis = "2.3.5.7";
      changeIntervalBasis[meantoneC, targetIntervalBasis]

Out   "[4 -4 1 0⟩"
````

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
      targetIntervalBasis = "2.3";
      changeIntervalBasis[meantoneM, targetIntervalBasis]

Out   "[⟨1 0] ⟨0 1]⟩"
```

## roadmap

The following features are planned:

* error handling
    * impossible interval basis changes
* additional features
    * irrational interval bases
