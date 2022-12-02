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
mapMergePrivate[tl___] := Module[{ml, intervalBasisL, intersectedIntervalBasis, tlWithIntersectedIntervalBasis},
  ml = Map[If[isCols[#], dualPrivate[#], #]&, {tl}];
  intervalBasisL = Map[getIntervalBasis, {tl}];
  intersectedIntervalBasis = Apply[intervalBasisIntersection, intervalBasisL];
  tlWithIntersectedIntervalBasis = Map[changeIntervalBasisForM[#, intersectedIntervalBasis]&, ml];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getM, tlWithIntersectedIntervalBasis]]], "row", intersectedIntervalBasis}]
];

commaMerge[unparsedT_] := formatOutput[commaMergePrivate[parseTemperamentData[unparsedT]]];
commaMergePrivate[tl___] := Module[{cl, intervalBasisL, mergedIntervalBasis, tlWithMergedIntervalBasis},
  cl = Map[If[isCols[#], #, dualPrivate[#]]&, {tl}];
  intervalBasisL = Map[getIntervalBasis, {tl}];
  mergedIntervalBasis = Apply[intervalBasisMerge, intervalBasisL];
  tlWithMergedIntervalBasis = Map[changeIntervalBasisForC[#, mergedIntervalBasis]&, cl];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getC, tlWithMergedIntervalBasis]]], "col", mergedIntervalBasis}]
];




(* ___ PRIVATE ___ *)


(* MERGE *)

getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


(* INTERVAL BASIS *)

intervalBasisMerge[intervalBasisL___] := Module[{concattedIntervalBasis, concattedIntervalBasisA},
  concattedIntervalBasis = Apply[Join, {intervalBasisL}];
  concattedIntervalBasisA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedIntervalBasis], getIntervalBasisDimension[concattedIntervalBasis]];
  
  canonicalIntervalBasis[Map[pcvToQuotient, concattedIntervalBasisA]]
];

intervalBasisIntersectionBinary[intervalBasis1_, intervalBasis2_] := Module[{intervalBasisDimension, basisChangeA1, basisChangeA2, allZerosFillerBasisChangeA, blockA, intersectedBasisChangeA, blockLHalf1, blockLHalf2},
  intervalBasisDimension = Max[getIntervalBasisDimension[intervalBasis1], getIntervalBasisDimension[intervalBasis2]];
  basisChangeA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis1], intervalBasisDimension];
  basisChangeA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis2], intervalBasisDimension];
  
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
  
  canonicalIntervalBasis[Map[pcvToQuotient, intersectedBasisChangeA]]
];

intervalBasisIntersection[intervalBasisL___] := Module[{intersectedIntervalBasis},
  intersectedIntervalBasis = First[{intervalBasisL}];
  
  Do[
    intersectedIntervalBasis = intervalBasisIntersectionBinary[intersectedIntervalBasis, intervalBasis],
    {intervalBasis, Drop[{intervalBasisL}, 1]}
  ];
  
  canonicalIntervalBasis[intersectedIntervalBasis]
];

isSubspaceOf[candidateSubspaceIntervalBasis_, candidateSuperspaceIntervalBasis_] :=
    intervalBasisMerge[candidateSubspaceIntervalBasis, candidateSuperspaceIntervalBasis] == candidateSuperspaceIntervalBasis;

changeIntervalBasisForM[m_, targetSubspaceIntervalBasis_] := If[
  getIntervalBasis[m] == targetSubspaceIntervalBasis,
  m,
  If[
    isSubspaceOf[getIntervalBasis[m], targetSubspaceIntervalBasis],
    Error,
    canonicalFormPrivate[{getA[m].Transpose[getIntervalRebaseForM[getIntervalBasis[m], targetSubspaceIntervalBasis]], "row", targetSubspaceIntervalBasis}]
  ]
];

changeIntervalBasisForC[c_, targetSuperspaceIntervalBasis_] := If[
  getIntervalBasis[c] == targetSuperspaceIntervalBasis,
  c,
  If[
    isSubspaceOf[getIntervalBasis[c], targetSuperspaceIntervalBasis],
    canonicalFormPrivate[{Transpose[Transpose[getIntervalRebaseForC[getIntervalBasis[c], targetSuperspaceIntervalBasis]].Transpose[getA[c]]], "col", targetSuperspaceIntervalBasis}],
    Error
  ]
];

(* express the target primoids in terms of the origin primoids *)
getIntervalRebaseForM[originalSuperspaceIntervalBasis_, targetSubspaceIntervalBasis_] := Module[
  {
    intervalBasisDimension,
    targetSubspaceBasisChangeA,
    originalSuperspaceBasisChangeA,
    intervalRebase,
    intervalRebaseCol,
    intervalRebaseColEntry,
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry
  },
  
  intervalBasisDimension = getIntervalBasisDimension[Join[originalSuperspaceIntervalBasis, targetSubspaceIntervalBasis]];
  targetSubspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceIntervalBasis], intervalBasisDimension];
  originalSuperspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceIntervalBasis], intervalBasisDimension];
  
  intervalRebase = {};
  
  Do[
    intervalRebaseCol = {};
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry = targetSubspaceBasisChangeAEntry;
    Do[
      intervalRebaseColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        intervalRebaseColEntry += 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry -= originalSuperspaceBasisChangeAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        intervalRebaseColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry += originalSuperspaceBasisChangeAEntry
      ];
      
      intervalRebaseCol = Join[intervalRebaseCol, {intervalRebaseColEntry}],
      {originalSuperspaceBasisChangeAEntry, originalSuperspaceBasisChangeA}
    ];
    intervalRebase = Join[intervalRebase, {intervalRebaseCol}],
    {targetSubspaceBasisChangeAEntry, targetSubspaceBasisChangeA}
  ];
  
  intervalRebase
];

(* yes, just swapping initial and target, that's all! *)
getIntervalRebaseForC[originalSubspaceIntervalBasis_, targetSuperspaceIntervalBasis_] := getIntervalRebaseForM[targetSuperspaceIntervalBasis, originalSubspaceIntervalBasis];

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
