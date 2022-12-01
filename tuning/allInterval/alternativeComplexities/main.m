augmentedTemperedSideGeneratorsPartArg[generatorTuningMap_] := rowify[Join[
  getL[generatorTuningMap],
  {Symbol["gAugmented"]}
]];

augmentedTemperedSideMappingPartArg[m_, intervalComplexityNormPreTransformerSizeFactor_] := Module[
  {d, temperedSideMappingPartArg, mappingAugmentation},
  
  d = getDPrivate[m];
  temperedSideMappingPartArg = rowify[Map[Join[#, {0}]&, getA[m]]];
  mappingAugmentation = {Join[
    getL[multiplyToRows[
      rowify[Table[intervalComplexityNormPreTransformerSizeFactor, d]],
      getLogPrimeA[m]
    ]],
    {-1}
  ]};
  
  rowify[Join[getA[temperedSideMappingPartArg], mappingAugmentation]]
];

augmentedJustSideGeneratorsPartArg[centsConversionAndSummationMapAndLogPrimeA_] := rowify[Join[
  getL[centsConversionAndSummationMapAndLogPrimeA],
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

augmentedEitherSideMultiplierPartArg[simplicityPreTransformer_] := rowify[Join[
  getA[simplicityPreTransformer],
  {Join[
    Table[
      0,
      Last[Dimensions[getA[simplicityPreTransformer]]] - 1
    ],
    {1}
  ]}
]];

augmentedUnchangedIntervalsArg[unchangedIntervals_] := If[
  ToString[unchangedIntervals] == "Null",
  unchangedIntervals,
  colify[Map[
    Join[#, {1}]&,
    getA[unchangedIntervals]
  ]]
];
