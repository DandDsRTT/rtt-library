getPureStretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorTuningMap,
    m,
    centsSummationMapAndLogPrimeOctaveA,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorTuningMap, m, centsSummationMapAndLogPrimeOctaveA} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiplyToCols[centsSummationMapAndLogPrimeOctaveA, pureStretchedInterval];
  temperedIntervalSize = multiplyToCols[optimumGeneratorTuningMap, m, pureStretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalSize / temperedIntervalSize) * getL[optimumGeneratorTuningMap]]
];
