getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorsTuningMap,
    m,
    centsSummationMapAndLogPrimeCoordinator,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, centsSummationMapAndLogPrimeCoordinator} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiplyToCols[centsSummationMapAndLogPrimeCoordinator, pureStretchedInterval];
  temperedIntervalSize = multiplyToCols[optimumGeneratorsTuningMap, m, pureStretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalSize / temperedIntervalSize) * getL[optimumGeneratorsTuningMap]]
];
