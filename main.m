(* TEMPERAMENT UTILITIES *)

(*
  
  getD[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the dimensionality.
  
  Examples:
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        getD[meantoneM]
    
  Out   3
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        getD[meantoneC]
    
  Out   3
  
*)
getD[t_] := colCount[getA[t]];

(*
  
  getR[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the rank.
  
  Examples:
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        getR[meantoneM]
    
  Out   2
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        getR[meantoneC]
    
  Out   2
  
*)
getR[t_] := If[
  isCo[t],
  MatrixRank[getA[t]],
  getD[t] - MatrixRank[getA[t]]
];

(*
  
  getN[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the nullity.
  
  Examples:
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        getN[meantoneM]
    
  Out   1
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        getN[meantoneC]
    
  Out   1
  
*)
getN[t_] := If[
  isContra[t],
  MatrixRank[getA[t]],
  getD[t] - MatrixRank[getA[t]]
];


(* CANONICALIZATION *)

(*
  
  canonicalForm[t]
  
  Returns the given temperament representation (mapping or comma basis)
  in canonical form (defactored, then put into Hermite Normal Form).
  
  Examples:
  
  In    someMeantoneM = {{{5, 8, 12}, {7, 11, 16}}, "co"};
        canonicalForm[someMeantoneM]
    
  Out   {{{1, 0, -4}, {0, 1, 4}}, "co"}
  
  In    someMeantoneC = {{{-8, 8, -2}}, "contra"};
        canonicalForm[someMeantoneC]
    
  Out   {{{4, -4, 1}, "contra"}
  
*)
canonicalForm[t_] := Module[{b, canonicalT},
  canonicalT = If[
    isContra[t],
    {canonicalC[getA[t]], getV[t]},
    {canonicalM[getA[t]], getV[t]}
  ];
  b = getB[t];
  
  If[
    isStandardPrimeLimitB[b],
    canonicalT,
    Join[canonicalT, {b}]
  ]
];


(* DUAL *)

(*
  
  dual[t]
  
  Returns its dual for the given temperament representation
  (if given a mapping, the comma basis, or vice-versa).
  
  Examples:
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        dual[meantoneM]
    
  Out   {{{4, -4, 1}}, "contra"}
  
*)
dual[t_] := If[
  isStandardPrimeLimitB[getB[t]],
  If[
    isContra[t],
    {antiNullSpaceBasis[getA[t]], "co"},
    {nullSpaceBasis[getA[t]], "contra"}
  ],
  If[
    isContra[t],
    {antiNullSpaceBasis[getA[t]], "co", getB[t]},
    {nullSpaceBasis[getA[t]], "contra", getB[t]}
  ] (* TODO: break this down *)
];


(* MERGE *)

(*
  
  mapMerge[t1, t2, t3...]
  
  Merges the given temperaments' maps:
  concatenates their mappings
  and puts the result into canonical form.
  
  Can accept any number of temperaments representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a mapping.
  
  Examples:
  
  In    et5M = {{{5, 8, 12}}, "co"};
        et7M = {{{7, 11, 16}}, "co"};
        mapMerge[et5M, et7M]
    
  Out   {{{1, 0, -4}, {0, 1, 4}}, "co"};
  
  In    et7dM = {{{7, 11, 16, 19}}, "co"};
        et12M = {{{12, 19, 28, 34}}, "co"};
        et22M = {{{22, 35, 51, 62}}, "co"};
        mapMerge[et7dM, et12M, et22M]
    
  Out   {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
  
*)
mapMerge[tl___] := Module[{bl, intersectedB, tlWithIntersectedB},
  bl = Map[getB, {tl}];
  intersectedB = Apply[bIntersection, bl];
  tlWithIntersectedB = Map[changeBforM[#, intersectedB]&, {tl}];
  
  canonicalForm[{Apply[Join, Map[getM, tlWithIntersectedB]], "co", intersectedB}]
];

(*
  
  commaMerge[t1, t2, t3...]
  
  Merges the given temperaments' comma bases:
  concatenates their comma bases
  and puts the result into canonical form.
  
  Can accept any number of temperament representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a comma basis.
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        porcupineC = {{{1, -5, 3}}, "contra"};
        commaMerge[meantoneC, porcupineC]
    
  Out   {{{-11, 7, 0}, {-7, 3, 1}}, "contra"}
  
  In    mintC = {{{2, 2, -1, -1}}, "contra"};
        meantoneC = {{{4, -4, 1, 0}}, "contra"};
        negriC = {{{-14, 3, 4, 0}}, "contra"};
        commaMerge[mintC, meantoneC, negriC]
    
  Out   {{{30, 19, 0, 0}, {-26, 15, 1, 0}, {-6, 2, 0, 1}}, "contra"}
  
*)
commaMerge[tl___] := Module[{bl, mergedB, tlWithMergedB},
  bl = Map[getB, {tl}];
  mergedB = Apply[bMerge, bl];
  tlWithMergedB = Map[changeBforC[#, mergedB]&, {tl}];
  
  canonicalForm[{Apply[Join, Map[getC, tlWithMergedB]], "contra", mergedB}]
];



(* INTERVAL BASIS *)

(*
*)
changeB[t_, targetB_] := Module[{}, ""]; (* TODO: make a mix of changeBforM and changeBforC *)


(* ARITHMETIC *)

(*
  
  sum[t1, t2]
  
  Sums the given temperaments: if they have the same dimensions
  (same dimensionality, rank (and nullity)),
  and are addable (can be put into a form where
  they are identical except for a single basis vector (or covector, if covariant)),
  entry-wise sums this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
  If the given temperaments are not the same dimensions and addable,
  it will error.
  
  Can accept temperament representations of different variances,
  but it will return a temperament with the same variance
  as the first given temperament representation.
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        porcupineC = {{{1, -5, 3}}, "contra"};
        sum[meantoneC, porcupineC]
    
  Out   {{{5, -9, 4}}, "contra"}
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
        sum[meantoneM, porcupineM]
    
  Out   {{{1, 1, 1}, {0, 4, 9}}, "co"}
  
*)
sum[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = If[vMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];
  
  If[
    t1 == t2,
    t1,
    arithmetic[t1, t2, True]
  ]
];

(*
  
  diff[t1, t2]
  
  Diffs the given temperaments: if they have the same dimensions
  (same  dimensionality, rank (and nullity)),
  and are addable (can be put into a form where
  they are identical except for a single basis vector (or basis covector, if covariant)),
  entry-wise diffs this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
  If the given temperaments are not the same dimensions and addable,
  it will error.
  
  Can accept temperament representations of different variances,
  but it will return a temperament with the same variance
  as the first given temperament representation.
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        porcupineC = {{{1, -5, 3}}, "contra"};
        diff[meantoneC, porcupineC]
    
  Out   {{{-3, -1, 2}}, "contra"}
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
        diff[meantoneM, porcupineM]
    
  Out   {{{1, 1, 2}, {0, 2, 1}}, "co"}
  
*)
diff[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = If[vMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];
  
  If[
    t1 == t2,
    Error,
    arithmetic[t1, t2, False]
  ]
];




(* ___ PRIVATE ___ *)



(* LIST UTILITIES *)

getGcf[l_] := Apply[GCD, l];
divideOutGcf[l_] := Module[{gcf}, gcf = getGcf[l]; If[gcf == 0, l, l / gcf]];
multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

leadingEntry[l_] := First[Select[l, # != 0&, 1]];
trailingEntry[l_] := leadingEntry[Reverse[l]];

allZerosL[l_] := AllTrue[l, # == 0&];


(* MATRIX UTILITIES *)

allZeros[a_] := AllTrue[a, # == 0&, 2];

reverseEachRow[a_] := Reverse[a, 2];
reverseEachCol[a_] := Reverse[a];
antiTranspose[a_] := reverseEachRow[reverseEachCol[a]];

removeAllZeroRows[a_] := Select[a, FreeQ[#, {0 ..}] &];

removeUnneededZeroRows[a_] := If[
  allZeros[a],
  {Table[0, colCount[a]]},
  removeAllZeroRows[a]
];

colCount[a_] := Last[Dimensions[a]];


(* TEMPERAMENT UTILITIES *)

getA[t_] := Part[t, 1];
getV[t_] := Part[t, 2];

isContra[t_] := MemberQ[{
  "contra",
  "contravector",
  "contravariant",
  "v",
  "vector",
  "c",
  "comma",
  "comma basis",
  "comma-basis",
  "commaBasis",
  "comma_basis",
  "i",
  "interval",
  "g",
  "generator",
  "pcv",
  "gcv",
  "monzo",
  "against"
}, getV[t]];
isCo[t_] := MemberQ[{
  "co",
  "covector",
  "covariant",
  "m",
  "map",
  "mapping",
  "et",
  "edo",
  "edomapping",
  "val",
  "with"
}, getV[t]];


(* CANONICALIZATION *)

hnf[a_] := Last[HermiteDecomposition[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

canonicalM[m_] := If[
  allZeros[m],
  {Table[0, colCount[m]]},
  removeUnneededZeroRows[hnf[colHermiteDefactor[m]]]
];
canonicalC[c_] := antiTranspose[canonicalM[antiTranspose[c]]];


(* DUAL *)

noncanonicalNullSpaceBasis[m_] := reverseEachCol[NullSpace[m]];
noncanonicalAntiNullSpaceBasis[c_] := NullSpace[c];

nullSpaceBasis[m_] := Module[{c},
  c = canonicalC[noncanonicalNullSpaceBasis[m]];
  
  If[
    c == {{}},
    {Table[0, getD[m]]},
    c
  ]
];
antiNullSpaceBasis[c_] := Module[{m},
  m = canonicalM[noncanonicalAntiNullSpaceBasis[c]];
  
  If[
    m == {{}},
    {Table[0, getD[c]]},
    m
  ]
];


(* MERGE *)

getM[t_] := If[isCo[t] == True, getA[t], noncanonicalAntiNullSpaceBasis[getA[t]]];
getC[t_] := If[isContra[t] == True, getA[t], noncanonicalNullSpaceBasis[getA[t]]];


(* INTERVAL BASIS *)

bMerge[bl___] := Module[{concatedB, factorizedConcatedB},
  concatedB = Apply[Join, {bl}];
  factorizedConcatedB = padD[Map[rationalToI, concatedB], getDforB[concatedB]];
  
  canonicalB[Map[iToRational, factorizedConcatedB]]
];

bIntersectionBinary[b1_, b2_] := Module[{d, factorizedB1, factorizedB2, allZerosFillerB, blockA, intersectedB, blockLfirstHalf, blockLsecondHalf},
  d = Max[getDforB[b1], getDforB[b2]];
  (* TODO: need a helper for padD, I feel like I'm doing some repetitive stuff *)
  (* TODO: oh, note that this is a standardBasisI! that's pretty important here, may be nice to be explicit about that in general *)
  factorizedB1 = padD[Map[rationalToI, b1], d];
  factorizedB2 = padD[Map[rationalToI, b2], d];
  
  allZerosFillerB = Table[Table[0, Length[First[factorizedB2]]], Length[factorizedB2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {factorizedB1, factorizedB1},
      {factorizedB2, allZerosFillerB}
    }
  ]];
  
  intersectedB = {};
  Do[
    blockLfirstHalf = Take[blockL, Length[blockL] / 2];
    blockLsecondHalf = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLfirstHalf], intersectedB = Join[intersectedB, {blockLsecondHalf}]],
    {blockL, blockA}
  ];
  intersectedB = If[Length[intersectedB] == 0, {0}, intersectedB];
  
  canonicalB[Map[iToRational, intersectedB]]
];

bIntersection[bl___] := Module[{intersectedB},
  intersectedB = First[{bl}];
  
  Do[
    intersectedB = bIntersectionBinary[intersectedB, b],
    {b, Drop[{bl}, 1]}
  ];
  
  canonicalB[intersectedB]
];

isSubspaceOf[candidateSubspaceB_, candidateSuperspaceB_] := bMerge[candidateSubspaceB, candidateSuperspaceB] == candidateSuperspaceB;

canonicalB[b_] := Module[{factorizedB, canonicalizedFactorizedB},
  factorizedB = padD[Map[rationalToI, b], getDforB[b]];
  canonicalizedFactorizedB = antiTranspose[removeAllZeroRows[hnf[antiTranspose[factorizedB]]]];
  
  If[
    Length[canonicalizedFactorizedB] == 0,
    {1},
    Map[super, Map[iToRational, canonicalizedFactorizedB]]
  ]
];

changeBforM[m_, targetSubspaceB_] := If[
  getB[m] == targetSubspaceB,
  m,
  If[
    isSubspaceOf[getB[m], targetSubspaceB],
    Error,
    canonicalForm[{getA[m].getRforM[getB[m], targetSubspaceB], "co", targetSubspaceB}]
  ]
];

changeBforC[c_, targetSuperspaceB_] := If[
  getB[c] == targetSuperspaceB,
  c,
  If[
    isSubspaceOf[getB[c], targetSuperspaceB],
    canonicalForm[{Transpose[getRforC[getB[c], targetSuperspaceB].Transpose[getA[c]]], "contra", targetSuperspaceB}],
    Error
  ]
];

(* express the target formal primes in terms of the initial formal primes*)
getRforM[originalSuperspaceB_, targetSubspaceB_] := Module[
  {
    d,
    factorizedTargetSubspaceB,
    factorizedOriginalSuperspaceB,
    r,
    rCol,
    rColEntry,
    remainingToBeFactorizedTargetSubspaceF
  },
  
  d = getDforB[Join[originalSuperspaceB, targetSubspaceB]];
  factorizedTargetSubspaceB = padD[Map[rationalToI, targetSubspaceB], d];
  factorizedOriginalSuperspaceB = padD[Map[rationalToI, originalSuperspaceB], d];
  
  r = {};
  
  Do[
    rCol = {};
    remainingToBeFactorizedTargetSubspaceF = factorizedTargetSubspaceF;
    Do[
      rColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry += 1;
        remainingToBeFactorizedTargetSubspaceF -= factorizedOriginalSuperspaceF
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceF += factorizedOriginalSuperspaceF
      ];
      
      rCol = Join[rCol, {rColEntry}],
      {factorizedOriginalSuperspaceF, factorizedOriginalSuperspaceB}
    ];
    r = Join[r, {rCol}],
    {factorizedTargetSubspaceF, factorizedTargetSubspaceB}
  ];
  
  Transpose[r] (* TODO: I don't think this should be transposed, and dealt with accordingly in the few places where it's used *)
];

(* yes, just swapping initial and target, that's all! *)
getRforC[originalSubspaceB_, targetSuperspaceB_] := getRforM[targetSuperspaceB, originalSubspaceB];

getPrimes[count_] := Map[Prime, Range[count]];

rationalToI[rational_] := Module[{factorization, greatestPrime, count, primes, i, currentPrimeIndex},
  factorization = FactorInteger[rational];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  i = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      i[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    i
  ]
];

iToRational[i_] := Module[{rational, primeIndex},
  rational = 1;
  primeIndex = 1;
  Do[
    rational = rational * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, i}
  ];
  
  rational
];

getDforB[b_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, b]]]]]]; (* TODO: whoa this too is based on standard basis, like if you skip primes and only have 4 but top one is 11 it'll be 5 or whatever *)

padD[a_, d_] := Map[PadRight[#, d]&, a];

super[rational_] := If[rational < 1, Denominator[rational] / Numerator[rational], rational];

getStandardPrimeLimitB[t_] := getPrimes[getD[t]];

isStandardPrimeLimitB[b_] := canonicalB[b] == getPrimes[Length[b]];

getB[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitB[t]
];

signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

(*TODO: DRY this up with isDenominatorFactor *)
isNumeratorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF - factorizedSuperspaceF}], False];

isDenominatorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF + factorizedSuperspaceF}], False];


(* ARITHMETIC *)

arithmetic[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2],
  Error,
  Module[{ldb, tSumAndDiff},
    ldb = getLdb[t1, t2];
    
    If[
      ldb === Error, (* not addable *)
      Error,
      addableArithmetic[t1, t2, ldb, isSum]
    ]
  ]
];

addableArithmetic[t1_, t2_, ldb_, isSum_] := Module[
  {
    t1LibVector,
    t2LibVector,
    t1t2libVector
  },
  
  t1LibVector = getLibVector[t1, ldb];
  t2LibVector = getLibVector[t2, ldb];
  
  t1t2libVector = If[
    isSum,
    t1LibVector + t2LibVector,
    t1LibVector - t2LibVector
  ];
  
  canonicalForm[{Join[ldb, {t1t2libVector}], getV[t1]}]
];

getLibVector[t_, ldb_] := Module[{a, libVector},
  a = addabilizationDefactor[t, ldb];
  libVector = Last[a];
  If[isNegative[a, isContra[t]], libVector = -libVector];
  
  libVector
];

addabilizationDefactor[t_, ldb_] := Module[
  {
    grade,
    explicitLdbFormOfA
  },
  
  grade = getGrade[t];
  explicitLdbFormOfA = getInitialExplicitLdbFormOfA[t, ldb, grade];
  
  If[
    isLd[ldb],
    addabilizationDefactorWithNonemptyLdb[t, ldb, grade, explicitLdbFormOfA],
    explicitLdbFormOfA
  ]
];

addabilizationDefactorWithNonemptyLdb[t_, ldb_, grade_, explicitLdbFormOfAInput_] := Module[
  {
    explicitLdbFormOfA,
    d,
    ld,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  
  explicitLdbFormOfA = explicitLdbFormOfAInput;
  d = getD[t];
  ld = getLd[ldb];
  enfactoring = getGreatestFactor[explicitLdbFormOfA];
  
  multiples = Table[Subscript[x, i], {i, ld}];
  equations = Map[
    Function[
      dIndex,
      Mod[explicitLdbFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * ldb[[multiplesIndex]][[dIndex]]],
        Range[ld]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  explicitLdbFormOfA[[grade]] = divideOutGcf[explicitLdbFormOfA[[grade]] + getLdbLinearCombination[ldb, result]];
  
  explicitLdbFormOfA
];

vMatch[t1_, t2_] := getV[t1] == getV[t2];

getLdb[t1_, t2_] := Module[{ldb},
  ldb = removeAllZeroRows[getA[dual[
    If[
      isContra[t1],
      mapMerge[t1, t2],
      commaMerge[t1, t2]
    ]
  ]]];
  
  If[
    isAddable[ldb, t1],
    ldb,
    Error
  ]
];

isAddable[ldb_, t_] := getLd[ldb] === getGrade[t] - 1;

getLd[ldb_] := Length[ldb];

dimensionsDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

getGrade[t_] := If[isContra[t], getN[t], getR[t]];

isLd[ldb_] := getLd[ldb] > 0;

getInitialExplicitLdbFormOfA[t_, ldb_, grade_] := Module[
  {
    libSource,
    explicitLdbFormOfA
  },
  
  libSource = If[isContra[t], getC[t], getM[t]];
  explicitLdbFormOfA = ldb;
  
  Do[
    candidate = hnf[Join[ldb, {candidateLibVector}]];
    If[
      Length[explicitLdbFormOfA] < grade && MatrixRank[candidate] > Length[ldb],
      explicitLdbFormOfA = Join[explicitLdbFormOfA, {candidateLibVector}]
    ],
    {candidateLibVector, libSource}
  ];
  Take[explicitLdbFormOfA, grade]
];

getGreatestFactor[a_] := Det[getGreatestFactorA[a]];

getGreatestFactorA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getLdbLinearCombination[ldb_, ldbMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {ldb, ldbMultiplePermutation}
]];

computeMinors[a_] := divideOutGcf[First[Minors[a, MatrixRank[a]]]];

isNegative[a_, contra_] := Module[{minors, entryFn, normalizingEntry},
  minors = computeMinors[a];
  entryFn = If[contra, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[minors];
  
  normalizingEntry < 0
];
