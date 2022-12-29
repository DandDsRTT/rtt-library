changeDomainBasis[unparsedT_] := formatOutput[changeDomainBasisPrivate[parseTemperamentData[unparsedT]]];
changeDomainBasisPrivate[t_, targetDomainBasis_] := If[
  isCols[t],
  changeDomainBasisForC[t, targetDomainBasis],
  changeDomainBasisForM[t, targetDomainBasis]
];




(* ___ PRIVATE ___ *)



nonstandardDomainBasisDual[t_] := If[
  isCols[t],
  {antiNullSpaceBasis[getA[t]], "row", getDomainBasis[t]},
  {nullspaceBasis[getA[t]], "col", getDomainBasis[t]}
];
