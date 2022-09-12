getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorsTuningMap,
    m,
    centsSummationMapAndLogPrimeMultiplier,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, centsSummationMapAndLogPrimeMultiplier} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiplyToCols[centsSummationMapAndLogPrimeMultiplier, pureStretchedInterval];
  temperedIntervalSize = multiplyToCols[optimumGeneratorsTuningMap, m, pureStretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalSize / temperedIntervalSize) * getL[optimumGeneratorsTuningMap]]
];
