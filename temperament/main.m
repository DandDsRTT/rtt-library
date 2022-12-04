(* DIMENSIONS *)

getD[unparsedT_] := getDPrivate[parseTemperamentData[unparsedT]];

getR[unparsedT_] := getRPrivate[parseTemperamentData[unparsedT]];

getN[unparsedT_] := getNPrivate[parseTemperamentData[unparsedT]];
getNPrivate[t_] := If[
  isCols[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];


(* CANONICALIZATION *)

canonicalForm[unparsedT_] := formatOutput[canonicalFormPrivate[parseTemperamentData[unparsedT]]];


(* DUAL *)

dual[unparsedT_] := formatOutput[dualPrivate[parseTemperamentData[unparsedT]]];


(* MERGE *)

mapMerge[unparsedT_] := formatOutput[mapMergePrivate[parseTemperamentData[unparsedT]]];
mapMergePrivate[tl___] := Module[{ml, domainBasisL, intersectedDomainBasis, tlWithIntersectedDomainBasis},
  ml = Map[If[isCols[#], dualPrivate[#], #]&, {tl}];
  domainBasisL = Map[getDomainBasis, {tl}];
  intersectedDomainBasis = Apply[domainBasisIntersection, domainBasisL];
  tlWithIntersectedDomainBasis = Map[changeDomainBasisForM[#, intersectedDomainBasis]&, ml];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getM, tlWithIntersectedDomainBasis]]], "row", intersectedDomainBasis}]
];

commaMerge[unparsedT_] := formatOutput[commaMergePrivate[parseTemperamentData[unparsedT]]];
commaMergePrivate[tl___] := Module[{cl, domainBasisL, mergedDomainBasis, tlWithMergedDomainBasis},
  cl = Map[If[isCols[#], #, dualPrivate[#]]&, {tl}];
  domainBasisL = Map[getDomainBasis, {tl}];
  mergedDomainBasis = Apply[domainBasisMerge, domainBasisL];
  tlWithMergedDomainBasis = Map[changeDomainBasisForC[#, mergedDomainBasis]&, cl];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getC, tlWithMergedDomainBasis]]], "col", mergedDomainBasis}]
];




(* ___ PRIVATE ___ *)


(* MERGE *)

getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


(* DOMAIN BASIS *)

domainBasisMerge[domainBasisL___] := Module[{concattedDomainBasis, concattedDomainBasisA},
  concattedDomainBasis = Apply[Join, {domainBasisL}];
  concattedDomainBasisA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedDomainBasis], getDomainBasisDimension[concattedDomainBasis]];
  
  canonicalDomainBasis[Map[pcvToQuotient, concattedDomainBasisA]]
];

domainBasisIntersectionBinary[domainBasis1_, domainBasis2_] := Module[{domainBasisDimension, basisChangeA1, basisChangeA2, allZerosFillerBasisChangeA, blockA, intersectedBasisChangeA, blockLHalf1, blockLHalf2},
  domainBasisDimension = Max[getDomainBasisDimension[domainBasis1], getDomainBasisDimension[domainBasis2]];
  basisChangeA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis1], domainBasisDimension];
  basisChangeA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis2], domainBasisDimension];
  
  allZerosFillerBasisChangeA = Table[Table[0, Length[First[basisChangeA2]]], Length[basisChangeA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {basisChangeA1, basisChangeA1},
      {basisChangeA2, allZerosFillerBasisChangeA}
    }
  ]];
  
  intersectedBasisChangeA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedBasisChangeA = Join[intersectedBasisChangeA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedBasisChangeA = If[Length[intersectedBasisChangeA] == 0, {0}, intersectedBasisChangeA];
  
  canonicalDomainBasis[Map[pcvToQuotient, intersectedBasisChangeA]]
];

domainBasisIntersection[domainBasisL___] := Module[{intersectedDomainBasis},
  intersectedDomainBasis = First[{domainBasisL}];
  
  Do[
    intersectedDomainBasis = domainBasisIntersectionBinary[intersectedDomainBasis, domainBasis],
    {domainBasis, Drop[{domainBasisL}, 1]}
  ];
  
  canonicalDomainBasis[intersectedDomainBasis]
];

isSubspaceOf[candidateSubspaceDomainBasis_, candidateSuperspaceDomainBasis_] :=
    domainBasisMerge[candidateSubspaceDomainBasis, candidateSuperspaceDomainBasis] == candidateSuperspaceDomainBasis;

changeDomainBasisForM[m_, targetSubspaceDomainBasis_] := If[
  getDomainBasis[m] == targetSubspaceDomainBasis,
  m,
  If[
    isSubspaceOf[getDomainBasis[m], targetSubspaceDomainBasis],
    Error,
    canonicalFormPrivate[{getA[m].Transpose[getDomainBasisChangeForM[getDomainBasis[m], targetSubspaceDomainBasis]], "row", targetSubspaceDomainBasis}]
  ]
];

changeDomainBasisForC[c_, targetSuperspaceDomainBasis_] := If[
  getDomainBasis[c] == targetSuperspaceDomainBasis,
  c,
  If[
    isSubspaceOf[getDomainBasis[c], targetSuperspaceDomainBasis],
    canonicalFormPrivate[{Transpose[Transpose[getDomainBasisChangeForC[getDomainBasis[c], targetSuperspaceDomainBasis]].Transpose[getA[c]]], "col", targetSuperspaceDomainBasis}],
    Error
  ]
];

(* express the target domain basis elements in terms of the origin domain basis elements *)
getDomainBasisChangeForM[originalSuperspaceDomainBasis_, targetSubspaceDomainBasis_] := Module[
  {
    domainBasisDimension,
    targetSubspaceBasisChangeA,
    originalSuperspaceBasisChangeA,
    domainBasisChange,
    domainBasisChangeCol,
    domainBasisChangeColEntry,
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry
  },
  
  domainBasisDimension = getDomainBasisDimension[Join[originalSuperspaceDomainBasis, targetSubspaceDomainBasis]];
  targetSubspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceDomainBasis], domainBasisDimension];
  originalSuperspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceDomainBasis], domainBasisDimension];
  
  domainBasisChange = {};
  
  Do[
    domainBasisChangeCol = {};
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry = targetSubspaceBasisChangeAEntry;
    Do[
      domainBasisChangeColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        domainBasisChangeColEntry += 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry -= originalSuperspaceBasisChangeAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        domainBasisChangeColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry += originalSuperspaceBasisChangeAEntry
      ];
      
      domainBasisChangeCol = Join[domainBasisChangeCol, {domainBasisChangeColEntry}],
      {originalSuperspaceBasisChangeAEntry, originalSuperspaceBasisChangeA}
    ];
    domainBasisChange = Join[domainBasisChange, {domainBasisChangeCol}],
    {targetSubspaceBasisChangeAEntry, targetSubspaceBasisChangeA}
  ];
  
  domainBasisChange
];

(* yes, just swapping initial and target, that's all! *)
getDomainBasisChangeForC[originalSubspaceDomainBasis_, targetSuperspaceDomainBasis_] := getDomainBasisChangeForM[targetSuperspaceDomainBasis, originalSubspaceDomainBasis];

getPrimes[count_] := Map[Prime, Range[count]];

quotientToPcv[quotient_] := Module[{factorization, greatestPrime, count, primes, pcv, currentPrimeIndex},
  factorization = FactorInteger[quotient];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  pcv = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      pcv[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    pcv
  ]
];

pcvToQuotient[pcv_] := Module[{quotient, primeIndex},
  quotient = 1;
  primeIndex = 1;
  Do[
    quotient = quotient * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, pcv}
  ];
  
  quotient
];


signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

isNumeratorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry - superspaceFEntry}
], False];
isDenominatorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry + superspaceFEntry}
], False];
