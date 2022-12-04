retrievePrimeDomainBasisGeneratorTuningMap[optimumGeneratorTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorPreimageTransversal, basisChange},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorTuningMap, m];
  generatorPreimageTransversal = getGeneratorPreimageTransversalPrivate[originalT];
  basisChange = colify[getDomainBasisChangeForM[getDomainBasis[t], getDomainBasis[originalT]]];
  
  If[
    debug == True,
    Print["optimumTuningMap: ", optimumTuningMap];
    Print["basisChange: ", basisChange];
    Print["generatorPreimageTransversal: ", generatorPreimageTransversal];
  ];
  
  multiplyToRows[optimumTuningMap, basisChange, generatorPreimageTransversal]
];

(* TODO: really, changeBasis only works one direction?!? *)
changeBasis[domainBasisChange_, t_] := If[ToString[t] == "Null", t, multiplyToRows[domainBasisChange, t]];

getMinimumStandardDomainBasis[domainBasis_] := Module[
  {unsorted},
  
  unsorted = {};
  Do[
    unsorted = Join[unsorted, FactorInteger[domainBasisElement]],
    {domainBasisElement, domainBasis}
  ];
  
  DeleteDuplicates[Sort[Map[First, unsorted]]]
];
