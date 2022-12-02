changeIntervalBasis[unparsedT_] := formatOutput[changeIntervalBasisPrivate[parseTemperamentData[unparsedT]]];
changeIntervalBasisPrivate[t_, targetIntervalBasis_] := If[
  isCols[t],
  changeIntervalBasisForC[t, targetIntervalBasis],
  changeIntervalBasisForM[t, targetIntervalBasis]
];




(* ___ PRIVATE ___ *)



nonstandardIntervalBasisDual[t_] := If[
  isCols[t],
  {antiNullSpaceBasis[getA[t]], "row", getIntervalBasis[t]},
  {nullSpaceBasis[getA[t]], "col", getIntervalBasis[t]}
];
