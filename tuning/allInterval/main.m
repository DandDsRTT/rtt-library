(* ALL-INTERVAL *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getSimplicityPreTransformer[tuningSchemeProperties_] := Module[
  {
    t,
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    
    complexityPreTransformer
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  
  complexityPreTransformer = getComplexityPreTransformer[
    t,
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor (* trait 5c *)
  ];
  
  (* always essentially simplicity-weight *)
  tuningInverse[complexityPreTransformer]
];

(* compare with getTuningMethodArgs *)
getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    unchangedIntervals,
    intervalComplexityNormPower,
    intervalComplexityNormPreTransformerSizeFactor,
    logging,
    
    generatorTuningMap,
    m,
    centsConversionAndSummationMapAndLogPrimeA,
    primesI,
    transposedPrimesI,
    simplicityPreTransformer,
    retuningMagnitudeNormPower,
    
    temperedSideGeneratorsPartArg,
    temperedSideMappingPartArg,
    justSideGeneratorsPartArg,
    justSideMappingPartArg,
    eitherSideIntervalsPartArg,
    eitherSideMultiplierPartArg,
    powerArg,
    unchangedIntervalsArg
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA} = getTuningSchemeMappings[t];
  primesI = getPrimesI[t];
  transposedPrimesI = transpose[primesI];
  simplicityPreTransformer = getSimplicityPreTransformer[tuningSchemeProperties];
  retuningMagnitudeNormPower = getDualPower[intervalComplexityNormPower];
  
  If[
    (* handle tuning schemes like minimax-lil-S "Weil", minimax-E-lil-S "WE", unchanged-octave minimax-lil-S "Kees", unchanged-octave minimax-E-lil-S "KE" *)
    intervalComplexityNormPreTransformerSizeFactor != 0,
    
    (* augmentation of args *)
    temperedSideGeneratorsPartArg = augmentedTemperedSideGeneratorsPartArg[generatorTuningMap];
    temperedSideMappingPartArg = augmentedTemperedSideMappingPartArg[m, intervalComplexityNormPreTransformerSizeFactor];
    justSideGeneratorsPartArg = augmentedJustSideGeneratorsPartArg[centsConversionAndSummationMapAndLogPrimeA];
    justSideMappingPartArg = augmentedJustSideMappingPartArg[primesI];
    eitherSideIntervalsPartArg = augmentedEitherSideIntervalsPartArg[transposedPrimesI];
    eitherSideMultiplierPartArg = augmentedEitherSideMultiplierPartArg[simplicityPreTransformer];
    unchangedIntervalsArg = augmentedUnchangedIntervalsArg[unchangedIntervals];
    powerArg = retuningMagnitudeNormPower, (* doesn't make sense to augment a power *)
    
    (* same thing as above, but no need to augment them *)
    temperedSideGeneratorsPartArg = generatorTuningMap;
    temperedSideMappingPartArg = m;
    justSideGeneratorsPartArg = centsConversionAndSummationMapAndLogPrimeA;
    justSideMappingPartArg = primesI;
    eitherSideIntervalsPartArg = transposedPrimesI;
    eitherSideMultiplierPartArg = simplicityPreTransformer;
    unchangedIntervalsArg = unchangedIntervals;
    powerArg = retuningMagnitudeNormPower;
  ];
  
  If[
    logging == True,
    printWrapper["\n(ALL-INTERVAL TUNING SCHEME) TUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* 𝒈 *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* 𝑀 *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* 𝒋 *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* 𝑀ⱼ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* Tₚ *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* 𝑆ₚ *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* 𝒈 *)
    temperedSideMappingPartArg, (* 𝑀 *)
    justSideGeneratorsPartArg, (* 𝒋 *)
    justSideMappingPartArg, (* 𝑀ⱼ *)
    eitherSideIntervalsPartArg, (* Tₚ *)
    eitherSideMultiplierPartArg, (* 𝑆ₚ *)
    powerArg,
    unchangedIntervalsArg
  }
];
