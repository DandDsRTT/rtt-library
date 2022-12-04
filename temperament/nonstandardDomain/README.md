# Nonstandard domain basis

This module contains the following functions:
* `changeDomainBasis`

This module is based on material from:
* [Temperament merging across interval bases#Changing domain basis](https://en.xen.wiki/w/Temperament_merging_across_interval_bases#Changing_interval_basis)

## functions

### change domain basis

`changeDomainBasis[t, targetDomainBasis]`

Changes the domain basis for the given temperament.

If the target domain basis is not possible
(such as a *super*space for a mapping, or a *sub*space for
a comma basis), the function will error.

```
In    meantoneC = "[4 -4 1⟩";
      targetDomainBasis = "2.3.5.7";
      changeDomainBasis[meantoneC, targetDomainBasis]

Out   "[4 -4 1 0⟩"
````

```
In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]}";
      targetDomainBasis = "2.3";
      changeDomainBasis[meantoneM, targetDomainBasis]

Out   "[⟨1 0] ⟨0 1]}"
```

## roadmap

The following features are planned:

* error handling
    * impossible domain basis changes
* additional features
    * irrational interval bases
