(* ALL-INTERVAL *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningSchemeProperties_] := Module[
  {
    t,
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormMultiplierLogPrimePower, (* trait 5a *)
    intervalComplexityNormMultiplierPrimePower, (* trait 5b *)
    intervalComplexityNormMultiplierSizeFactor, (* trait 5c *)
    
    complexityMultiplier
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormMultiplierLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormMultiplierPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierPrimePower"]; (* trait 5b *)
  intervalComplexityNormMultiplierSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierSizeFactor"]; (* trait 5c *)
  
  complexityMultiplier = getComplexityMultiplier[
    t,
    intervalComplexityNormMultiplierLogPrimePower, (* trait 5a *)
    intervalComplexityNormMultiplierPrimePower, (* trait 5b *)
    intervalComplexityNormMultiplierSizeFactor (* trait 5c *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplier]
];

(* compare with getTuningMethodArgs *)
getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    unchangedIntervals,
    intervalComplexityNormPower,
    intervalComplexityNormMultiplierSizeFactor,
    logging,
    
    generatorsTuningMap,
    m,
    centsSummationMapAndLogPrimeMultiplier,
    primesI,
    transposedPrimesI,
    dualMultiplier,
    primesErrorMagnitudeNormPower,
    
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
  intervalComplexityNormMultiplierSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierSizeFactor"]; (* trait 5c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, centsSummationMapAndLogPrimeMultiplier} = getTuningSchemeMappings[t];
  primesI = getPrimesI[t];
  transposedPrimesI = transpose[primesI];
  dualMultiplier = getDualMultiplier[tuningSchemeProperties];
  primesErrorMagnitudeNormPower = getDualPower[intervalComplexityNormPower];
  
  If[
    (* handle tuning schemes like minimax-lil-S "Weil", minimax-E-lil-S "WE", unchanged-octave minimax-lil-S "Kees", unchanged-octave minimax-E-lil-S "KE" *)
    intervalComplexityNormMultiplierSizeFactor != 0,
    
    (* augmentation of args *)
    temperedSideGeneratorsPartArg = augmentedTemperedSideGeneratorsPartArg[generatorsTuningMap];
    temperedSideMappingPartArg = augmentedTemperedSideMappingPartArg[m, intervalComplexityNormMultiplierSizeFactor];
    justSideGeneratorsPartArg = augmentedJustSideGeneratorsPartArg[centsSummationMapAndLogPrimeMultiplier];
    justSideMappingPartArg = augmentedJustSideMappingPartArg[primesI];
    eitherSideIntervalsPartArg = augmentedEitherSideIntervalsPartArg[transposedPrimesI];
    eitherSideMultiplierPartArg = augmentedEitherSideMultiplierPartArg[dualMultiplier];
    unchangedIntervalsArg = augmentedUnchangedIntervalsArg[unchangedIntervals];
    powerArg = primesErrorMagnitudeNormPower, (* doesn't make sense to augment a power *)
    
    (* same thing as above, but no need to augment them *)
    temperedSideGeneratorsPartArg = generatorsTuningMap;
    temperedSideMappingPartArg = m;
    justSideGeneratorsPartArg = centsSummationMapAndLogPrimeMultiplier;
    justSideMappingPartArg = primesI;
    eitherSideIntervalsPartArg = transposedPrimesI;
    eitherSideMultiplierPartArg = dualMultiplier;
    unchangedIntervalsArg = unchangedIntervals;
    powerArg = primesErrorMagnitudeNormPower;
  ];
  
  If[
    logging == True,
    printWrapper["\n(ALL-INTERVAL TUNING SCHEME) TUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* Tₚ *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* X⁻¹ *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* g *)
    temperedSideMappingPartArg, (* M *)
    justSideGeneratorsPartArg, (* p *)
    justSideMappingPartArg, (* Mₚ *)
    eitherSideIntervalsPartArg, (* Tₚ *)
    eitherSideMultiplierPartArg, (* X⁻¹ *)
    powerArg,
    unchangedIntervalsArg
  }
];
