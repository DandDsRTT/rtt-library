(* ALL-INTERVAL *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getSimplicityA[tuningSchemeProperties_] := Module[
  {
    t,
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor, (* trait 5c *)
    
    complexityA
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPrescalerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPrescalerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPrescalerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)
  
  complexityA = getComplexityA[
    t,
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor (* trait 5c *)
  ];
  
  (* always essentially simplicity-weight *)
  tuningInverse[complexityA]
];

(* compare with getTuningMethodArgs *)
getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    unchangedIntervals,
    intervalComplexityNormPower,
    intervalComplexityNormPrescalerSizeFactor,
    logging,
    
    generatorTuningMap,
    m,
    centsConversionAndSummationMapAndLogPrimeOctaveA,
    primesI,
    transposedPrimesI,
    simplicityA,
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
  intervalComplexityNormPrescalerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeOctaveA} = getTuningSchemeMappings[t];
  primesI = getPrimesI[t];
  transposedPrimesI = transpose[primesI];
  simplicityA = getSimplicityA[tuningSchemeProperties];
  retuningMagnitudeNormPower = getDualPower[intervalComplexityNormPower];
  
  If[
    (* handle tuning schemes like minimax-lil-S "Weil", minimax-E-lil-S "WE", unchanged-octave minimax-lil-S "Kees", unchanged-octave minimax-E-lil-S "KE" *)
    intervalComplexityNormPrescalerSizeFactor != 0,
    
    (* augmentation of args *)
    temperedSideGeneratorsPartArg = augmentedTemperedSideGeneratorsPartArg[generatorTuningMap];
    temperedSideMappingPartArg = augmentedTemperedSideMappingPartArg[m, intervalComplexityNormPrescalerSizeFactor];
    justSideGeneratorsPartArg = augmentedJustSideGeneratorsPartArg[centsConversionAndSummationMapAndLogPrimeOctaveA];
    justSideMappingPartArg = augmentedJustSideMappingPartArg[primesI];
    eitherSideIntervalsPartArg = augmentedEitherSideIntervalsPartArg[transposedPrimesI];
    eitherSideMultiplierPartArg = augmentedEitherSideMultiplierPartArg[simplicityA];
    unchangedIntervalsArg = augmentedUnchangedIntervalsArg[unchangedIntervals];
    powerArg = retuningMagnitudeNormPower, (* doesn't make sense to augment a power *)
    
    (* same thing as above, but no need to augment them *)
    temperedSideGeneratorsPartArg = generatorTuningMap;
    temperedSideMappingPartArg = m;
    justSideGeneratorsPartArg = centsConversionAndSummationMapAndLogPrimeOctaveA;
    justSideMappingPartArg = primesI;
    eitherSideIntervalsPartArg = transposedPrimesI;
    eitherSideMultiplierPartArg = simplicityA;
    unchangedIntervalsArg = unchangedIntervals;
    powerArg = retuningMagnitudeNormPower;
  ];
  
  If[
    logging == True,
    printWrapper["\n(ALL-INTERVAL TUNING SCHEME) TUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* Tₚ *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* S *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* g *)
    temperedSideMappingPartArg, (* M *)
    justSideGeneratorsPartArg, (* p *)
    justSideMappingPartArg, (* Mₚ *)
    eitherSideIntervalsPartArg, (* Tₚ *)
    eitherSideMultiplierPartArg, (* S *)
    powerArg,
    unchangedIntervalsArg
  }
];
