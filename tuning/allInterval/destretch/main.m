getDestretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap_, t_, destretchedInterval_] := Module[
  {
    generatorTuningMap,
    m,
    justTuningMap,
    justIntervalTuning,
    temperedIntervalTuning
  },
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  justIntervalTuning = multiplyToCols[justTuningMap, destretchedInterval];
  temperedIntervalTuning = multiplyToCols[optimumGeneratorTuningMap, m, destretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalTuning / temperedIntervalTuning) * getL[optimumGeneratorTuningMap]]
];
