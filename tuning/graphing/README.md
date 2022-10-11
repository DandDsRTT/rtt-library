# Tuning - graphing

This module contains functions related to graphing tuning damage, both 2D and 3D.

* `graphTuningDamage`

## graphing

### graph tuning damage

`graphTuningDamage[t, tuningSchemeSpec]`

Given a representation of a temperament as a mapping or comma basis, and a tuning scheme,
graphs the damage to the target intervals within a close range around the optimum tuning.
Graphs in 2D for a rank-1 temperament, 3D for a rank-2 temperament, and errors otherwise.

```
In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
graphTuningDamage[meantoneM, "miniRMS-copfr-EC"]

Out   (3D graph)
```

```
In    12etM = "⟨12 19 28]";
graphTuningDamage[12etM, "miniRMS-copfr-EC"]

Out   (2D graph)
```

## roadmap

The following features are planned:

* visualize the solution
* user-controlled zoom
* 3D graph checkerboard to translucent black continuum per optimization power
* ability to specify which norms are included
* titles and other info
* contour-style topographic graphs for 3D
* test failures automatically graph
* opacity configuration
* ability to explode out into a separate graph for each target interval
