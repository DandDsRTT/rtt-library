getPureStretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorTuningMap,
    m,
    centsConversionAndSummationMapAndLogPrimeOctaveA,
    justIntervalTuning,
    temperedIntervalTuning
  },
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeOctaveA} = getTuningSchemeMappings[t];
  
  justIntervalTuning = multiplyToCols[centsConversionAndSummationMapAndLogPrimeOctaveA, pureStretchedInterval];
  temperedIntervalTuning = multiplyToCols[optimumGeneratorTuningMap, m, pureStretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalTuning / temperedIntervalTuning) * getL[optimumGeneratorTuningMap]]
];
