getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorsTuningMap,
    m,
    centsSummationMapAndLogPrimeOctaveA,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, centsSummationMapAndLogPrimeOctaveA} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiplyToCols[centsSummationMapAndLogPrimeOctaveA, pureStretchedInterval];
  temperedIntervalSize = multiplyToCols[optimumGeneratorsTuningMap, m, pureStretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalSize / temperedIntervalSize) * getL[optimumGeneratorsTuningMap]]
];
