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


(* GENERATOR PREIMAGE TRANSVERSAL *)

getGeneratorPreimageTransversal[unparsedT_] := formatOutput[getGeneratorPreimageTransversalPrivate[parseTemperamentData[unparsedT]]];
getGeneratorPreimageTransversalPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorPreimageTransversal = right.Transpose[snf].left;
  
  colify[Transpose[generatorPreimageTransversal]]
];




(* ___ PRIVATE ___ *)


(* MERGE *)

getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


(* INTERVAL BASIS *)

intervalBasisMerge[intervalBasisL___] := Module[{concattedIntervalBasis, concattedFormalPrimeA},
  concattedIntervalBasis = Apply[Join, {intervalBasisL}];
  concattedFormalPrimeA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedIntervalBasis], getIntervalBasisDimension[concattedIntervalBasis]];
  
  canonicalIntervalBasis[Map[pcvToQuotient, concattedFormalPrimeA]]
];

intervalBasisIntersectionBinary[intervalBasis1_, intervalBasis2_] := Module[{intervalBasisDimension, formalPrimeA1, formalPrimeA2, allZerosFillerFormalPrimeA, blockA, intersectedFormalPrimeA, blockLHalf1, blockLHalf2},
  intervalBasisDimension = Max[getIntervalBasisDimension[intervalBasis1], getIntervalBasisDimension[intervalBasis2]];
  formalPrimeA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis1], intervalBasisDimension];
  formalPrimeA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis2], intervalBasisDimension];
  
  allZerosFillerFormalPrimeA = Table[Table[0, Length[First[formalPrimeA2]]], Length[formalPrimeA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {formalPrimeA1, formalPrimeA1},
      {formalPrimeA2, allZerosFillerFormalPrimeA}
    }
  ]];
  
  intersectedFormalPrimeA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedFormalPrimeA = Join[intersectedFormalPrimeA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedFormalPrimeA = If[Length[intersectedFormalPrimeA] == 0, {0}, intersectedFormalPrimeA];
  
  canonicalIntervalBasis[Map[pcvToQuotient, intersectedFormalPrimeA]]
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
    targetSubspaceFormalPrimeA,
    originalSuperspaceFormalPrimeA,
    intervalRebase,
    intervalRebaseCol,
    intervalRebaseColEntry,
    remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry
  },
  
  intervalBasisDimension = getIntervalBasisDimension[Join[originalSuperspaceIntervalBasis, targetSubspaceIntervalBasis]];
  targetSubspaceFormalPrimeA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceIntervalBasis], intervalBasisDimension];
  originalSuperspaceFormalPrimeA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceIntervalBasis], intervalBasisDimension];
  
  intervalRebase = {};
  
  Do[
    intervalRebaseCol = {};
    remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry = targetSubspaceFormalPrimeAEntry;
    Do[
      intervalRebaseColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry, originalSuperspaceFormalPrimeAEntry],
        intervalRebaseColEntry += 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry -= originalSuperspaceFormalPrimeAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry, originalSuperspaceFormalPrimeAEntry],
        intervalRebaseColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimeAEntry += originalSuperspaceFormalPrimeAEntry
      ];
      
      intervalRebaseCol = Join[intervalRebaseCol, {intervalRebaseColEntry}],
      {originalSuperspaceFormalPrimeAEntry, originalSuperspaceFormalPrimeA}
    ];
    intervalRebase = Join[intervalRebase, {intervalRebaseCol}],
    {targetSubspaceFormalPrimeAEntry, targetSubspaceFormalPrimeA}
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
