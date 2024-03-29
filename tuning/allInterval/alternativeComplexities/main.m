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

augmentedJustSideGeneratorsPartArg[justTuningMap_] := rowify[Join[
  getL[justTuningMap],
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

augmentedHeldIntervalsArg[heldIntervals_] := If[
  ToString[heldIntervals] == "Null",
  heldIntervals,
  colify[Map[
    Join[#, {1}]&,
    getA[heldIntervals]
  ]]
];
