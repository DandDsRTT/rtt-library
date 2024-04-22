changeDomainBasis[unparsedT_, unparsedTargetDomainBasis_] := formatOutput[changeDomainBasisPrivate[parseTemperamentData[unparsedT], parseDomainBasis[unparsedTargetDomainBasis]]];
changeDomainBasisPrivate[t_, targetDomainBasis_] := If[
  isCols[t],
  changeDomainBasisForC[t, targetDomainBasis],
  changeDomainBasisForM[t, targetDomainBasis]
];
