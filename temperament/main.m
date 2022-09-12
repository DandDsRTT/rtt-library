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
dualPrivate[t_] := If[
  isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]],
  If[
    isCols[t],
    rowify[antiNullSpaceBasis[getA[t]]],
    colify[nullSpaceBasis[getA[t]]]
  ],
  nonstandardIntervalBasisDual[t]
];


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


(* GENERATORS PREIMAGE TRANSVERSAL *)

getGeneratorsPreimageTransversal[unparsedT_] := formatOutput[getGeneratorsPreimageTransversalPrivate[parseTemperamentData[unparsedT]]];
getGeneratorsPreimageTransversalPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  colify[Transpose[generatorsPreimageTransversal]]
];




(* ___ PRIVATE ___ *)



(* DUAL *)

noncanonicalNullSpaceBasis[ma_] := reverseEachCol[NullSpace[ma]];
noncanonicalAntiNullSpaceBasis[ca_] := NullSpace[ca];

nullSpaceBasis[ma_] := Module[{ca},
  ca = canonicalCa[noncanonicalNullSpaceBasis[ma]];
  
  If[
    ca == {{}},
    {Table[0, getDPrivate[ma]]},
    ca
  ]
];
antiNullSpaceBasis[ca_] := Module[{ma},
  ma = canonicalMa[noncanonicalAntiNullSpaceBasis[ca]];
  
  If[
    ma == {{}},
    {Table[0, getDPrivate[ca]]},
    ma
  ]
];


(* MERGE *)

getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


(* INTERVAL BASIS *)

intervalBasisMerge[intervalBasisL___] := Module[{concattedIntervalBasis, concattedFormalPrimesA},
  concattedIntervalBasis = Apply[Join, {intervalBasisL}];
  concattedFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedIntervalBasis], getIntervalBasisDimension[concattedIntervalBasis]];
  
  canonicalIntervalBasis[Map[pcvToQuotient, concattedFormalPrimesA]]
];

intervalBasisIntersectionBinary[intervalBasis1_, intervalBasis2_] := Module[{intervalBasisDimension, formalPrimesA1, formalPrimesA2, allZerosFillerFormalPrimesA, blockA, intersectedFormalPrimesA, blockLHalf1, blockLHalf2},
  intervalBasisDimension = Max[getIntervalBasisDimension[intervalBasis1], getIntervalBasisDimension[intervalBasis2]];
  formalPrimesA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis1], intervalBasisDimension];
  formalPrimesA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis2], intervalBasisDimension];
  
  allZerosFillerFormalPrimesA = Table[Table[0, Length[First[formalPrimesA2]]], Length[formalPrimesA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {formalPrimesA1, formalPrimesA1},
      {formalPrimesA2, allZerosFillerFormalPrimesA}
    }
  ]];
  
  intersectedFormalPrimesA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedFormalPrimesA = Join[intersectedFormalPrimesA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedFormalPrimesA = If[Length[intersectedFormalPrimesA] == 0, {0}, intersectedFormalPrimesA];
  
  canonicalIntervalBasis[Map[pcvToQuotient, intersectedFormalPrimesA]]
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

(* express the target formal primes in terms of the initial formal primes*)
getIntervalRebaseForM[originalSuperspaceIntervalBasis_, targetSubspaceIntervalBasis_] := Module[
  {
    intervalBasisDimension,
    targetSubspaceFormalPrimesA,
    originalSuperspaceFormalPrimesA,
    intervalRebase,
    intervalRebaseCol,
    intervalRebaseColEntry,
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry
  },
  
  intervalBasisDimension = getIntervalBasisDimension[Join[originalSuperspaceIntervalBasis, targetSubspaceIntervalBasis]];
  targetSubspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceIntervalBasis], intervalBasisDimension];
  originalSuperspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceIntervalBasis], intervalBasisDimension];
  
  intervalRebase = {};
  
  Do[
    intervalRebaseCol = {};
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry = targetSubspaceFormalPrimesAEntry;
    Do[
      intervalRebaseColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry += 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry -= originalSuperspaceFormalPrimesAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry += originalSuperspaceFormalPrimesAEntry
      ];
      
      intervalRebaseCol = Join[intervalRebaseCol, {intervalRebaseColEntry}],
      {originalSuperspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesA}
    ];
    intervalRebase = Join[intervalRebase, {intervalRebaseCol}],
    {targetSubspaceFormalPrimesAEntry, targetSubspaceFormalPrimesA}
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
