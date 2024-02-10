changeDomainBasis[unparsedT_] := formatOutput[changeDomainBasisPrivate[parseTemperamentData[unparsedT]]];
changeDomainBasisPrivate[t_, targetDomainBasis_] := If[
  isCols[t],
  changeDomainBasisForC[t, targetDomainBasis],
  changeDomainBasisForM[t, targetDomainBasis]
];
