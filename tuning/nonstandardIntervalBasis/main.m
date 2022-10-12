retrievePrimeIntervalBasisGeneratorTuningMap[optimumGeneratorTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorPreimageTransversal, f},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorTuningMap, m];
  generatorPreimageTransversal = getGeneratorPreimageTransversalPrivate[originalT];
  f = getFormalPrimes[originalT];
  
  multiplyToRows[optimumTuningMap, f, generatorPreimageTransversal]
];

rebase[intervalRebase_, t_] := If[t == Null, t, multiplyToRows[intervalRebase, t]];
