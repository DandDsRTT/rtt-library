retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorsPreimageTransversal, f},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorsTuningMap, m];
  generatorsPreimageTransversal = getGeneratorsPreimageTransversalPrivate[originalT];
  f = getFormalPrimes[originalT];
  
  multiplyToRows[optimumTuningMap, f, generatorsPreimageTransversal]
];

rebase[intervalRebase_, t_] := If[t == Null, t, multiplyToRows[intervalRebase, t]];
