retrievePrimeDomainBasisGeneratorTuningMap[optimumGeneratorTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorPreimageTransversal, basisChange},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorTuningMap, m];
  generatorPreimageTransversal = getGeneratorPreimageTransversalPrivate[originalT];
  basisChange = colify[getDomainBasisChangeForM[getDomainBasis[t], getDomainBasis[originalT]]];
  
  If[
    debug == True,
    printWrapper["optimumTuningMap: ", optimumTuningMap];
    printWrapper["basisChange: ", basisChange];
    printWrapper["generatorPreimageTransversal: ", generatorPreimageTransversal];
  ];
  
  multiplyToRows[optimumTuningMap, basisChange, generatorPreimageTransversal]
];

(* TODO: really, changeBasis only works one direction?!? that is, with vector-based stuff coming in from the right? *)
changeBasis[domainBasisChange_, t_] := If[ToString[t] == "Null", t, multiplyToRows[domainBasisChange, t]];

getSimplestPrimeOnlyBasis[domainBasis_] := Module[
  {unsorted},
  
  unsorted = {};
  Do[
    unsorted = Join[unsorted, FactorInteger[domainBasisElement]],
    {domainBasisElement, domainBasis}
  ];
  
  DeleteDuplicates[Sort[Map[First, unsorted]]]
];
