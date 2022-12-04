filterTargetIntervalsForNonstandardDomainBasis[targetIntervalL_, tWithNonstandardDomainBasis_] := Module[
  {pcvs, basis, maxPrimeD, possibleTargetIntervalL, basisWithPcv},
  
  pcvs = Map[quotientToPcv, targetIntervalL];
  basis = Map[quotientToPcv, getDomainBasis[tWithNonstandardDomainBasis]];
  maxPrimeD = Max[
    Join[
      Map[Length, pcvs],
      Map[Length, basis]
    ]
  ];
  pcvs = padVectorsWithZerosUpToD[pcvs, maxPrimeD];
  basis = padVectorsWithZerosUpToD[basis, maxPrimeD];

  possibleTargetIntervalL = {};
  Do[
    basisWithPcv = Join[basis, {pcv}];
    If[
      removeAllZeroRows[hnf[basisWithPcv]] == hnf[basis], (* the canonical forms of the bases must match *)
      possibleTargetIntervalL = Join[possibleTargetIntervalL, {pcv}]
    ],
    {pcv, pcvs}
  ];
  
  Map[pcvToQuotient, possibleTargetIntervalL]
];
