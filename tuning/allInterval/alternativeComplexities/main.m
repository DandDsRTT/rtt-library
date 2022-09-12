augmentedTemperedSideGeneratorsPartArg[generatorsTuningMap_] := rowify[Join[
  getL[generatorsTuningMap],
  {Symbol["gAugmented"]}
]];

augmentedTemperedSideMappingPartArg[m_, intervalComplexityNormMultiplierSizeFactor_] := Module[
  {d, temperedSideMappingPartArg, mappingAugmentation},
  
  d = getDPrivate[m];
  temperedSideMappingPartArg = rowify[Map[Join[#, {0}]&, getA[m]]];
  mappingAugmentation = {Join[
    getL[multiplyToRows[
      rowify[Table[intervalComplexityNormMultiplierSizeFactor, d]],
      getLogPrimeMultiplier[m]
    ]],
    {-1}
  ]};
  
  rowify[Join[getA[temperedSideMappingPartArg], mappingAugmentation]]
];

augmentedJustSideGeneratorsPartArg[centsSummationMapAndLogPrimeMultiplier_] := rowify[Join[
  getL[centsSummationMapAndLogPrimeMultiplier],
  {0}
]];

augmentedJustSideMappingPartArg[primesI_] := Module[
  {a, augmentedA},
  
  a = getA[primesI];
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  rowify[augmentedA]
];

augmentedEitherSideIntervalsPartArg[transposedPrimesI_] := Module[
  {a, augmentedA},
  
  a = getA[transposedPrimesI];
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  colify[augmentedA]
];

augmentedEitherSideMultiplierPartArg[dualMultiplier_] := rowify[Join[
  getA[dualMultiplier],
  {Join[
    Table[
      0,
      Last[Dimensions[getA[dualMultiplier]]] - 1
    ],
    {1}
  ]}
]];

augmentedUnchangedIntervalsArg[unchangedIntervals_] := If[
  ToString[unchangedIntervals] == "Null",
  unchangedIntervals,
  colify[Map[
    Join[#, {0}]&,
    getA[unchangedIntervals]
  ]]
];
