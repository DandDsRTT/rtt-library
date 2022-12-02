retrievePrimeIntervalBasisGeneratorTuningMap[optimumGeneratorTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorPreimageTransversal, basisChange},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorTuningMap, m];
  generatorPreimageTransversal = getGeneratorPreimageTransversalPrivate[originalT];
  basisChange = colify[getIntervalBasisChangeForM[getIntervalBasis[t], getIntervalBasis[originalT]]];
  
  multiplyToRows[optimumTuningMap, basisChange, generatorPreimageTransversal]
];

changeBasis[intervalBasisChange_, t_] := If[ToString[t] == "Null", t, multiplyToRows[intervalBasisChange, t]];

getMinimumStandardIntervalBasis[intervalBasis_] := Module[
  {unsorted},
  
  unsorted = {};
  Do[
    unsorted = Join[unsorted, FactorInteger[intervalBasisElement]],
    {intervalBasisElement, intervalBasis}
  ];
  
  DeleteDuplicates[Sort[Map[First, unsorted]]]
];
