projectionMapping[m_, g_] := g.m;

vProjectionMappingMethodCents[v_, m_, g_] := 1200.jip[Length[g]].projectionMapping[m, g].v;
vGeneratorUnitsMethodCents[v_, m_, g_] := cents[g].m.v;

isUnchangedInterval[{eigenvalue_, eigenvector_}] := eigenvalue == 1;
unchangedIntervalsMG[m_, g_] := unchangedIntervals[projectionMapping[m, g]];
unchangedIntervals[p_] := Module[{eigenvalues, eigenvectors, pairedEigens},
  eigenvalues = Eigenvalues[p];
  eigenvectors = Eigenvectors[p];

  pairedEigens = MapThread[List, {eigenvalues, eigenvectors}];

  Map[Transpose, Map[List, Map[Last, Select[pairedEigens, isUnchangedInterval]]]] (* perhaps I should have this return them as a matrix? *)
];

meantone = {{1, 2, 4}, {0, -1, -4}};
vector = {{-2}, {0}, {1}};


(* unweighted example *)

g = unweightedG[meantone]

octaves[g]
cents[g]
vCents1 = vGeneratorUnitsMethodCents[vector, meantone, g]

p = projectionMapping[meantone, g]
vCents2 = vProjectionMappingMethodCents[vector, meantone, g]

vCents1 == vCents2

unchangeds = unchangedIntervals[p]
unchangeds == Map[p.#&, unchangeds]

p.{{1}, {1}, {0}}


(* weighted example *)

g = weightedG[meantone]

octaves[g]
cents[g]
vCents1 = vGeneratorUnitsMethodCents[vector, meantone, g]

p = projectionMapping[meantone, g]
vCents2 = vProjectionMappingMethodCents[vector, meantone, g]

vCents1 == vCents2

unchangeds = unchangedIntervals[p]
unchangeds == Map[p.#&, unchangeds]


(* unchanged interval example *)

g = unchangedIntervalG[meantone, {{1, -2}, {0, 0}, {0, 1}}]
(* {{1,1},{0,0},{0,-1/4}} *)

octaves[g]
cents[g]
vCents1 = vGeneratorUnitsMethodCents[vector, meantone, g]

p = projectionMapping[meantone, g]
vCents2 = vProjectionMappingMethodCents[vector, meantone, g]

vCents1 == vCents2

unchangeds = unchangedIntervals[p]
unchangeds == Map[p.#&, unchangeds]
