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
canonicalForm[t_] := If[
  isContra[t],
  {canonicalC[getA[t]], getV[t]},
  {canonicalM[getA[t]], getV[t]}
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
  isContra[t],
  {antiNullSpaceBasis[getA[t]], "co"},
  {nullSpaceBasis[getA[t]], "contra"}
];


(* MEET AND JOIN *)

(*
  
join[t1, t2, t3...]
  
Joins the given temperaments: concatenates their mappings
and puts the result into canonical form.
  
Can accept any number of temperaments representations,
as any combination of mappings or comma bases,
but returns the temperament as a mapping.
  
Examples:
  
In    et5 = {{{5, 8, 12}}, "co"};
      et7 = {{{7, 11, 16}}, "co"};
      join[et5, et7]
  
Out   {{{1, 0, -4}, {0, 1, 4}}, "co"};
  
In    et7d = {{{7, 11, 16, 19}}, "co"};
      et12 = {{{12, 19, 28, 34}}, "co"};
      et22 = {{{22, 35, 51, 62}}, "co"};
      join[et7dLimit7, et12Limit7, et22Limit7]
  
Out   {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
  
*)
join[tSequence___] := canonicalForm[{Apply[Join, Map[getM, {tSequence}]], "co"}];

(*
  
meet[t1, t2, t3...]
  
Meets the given temperaments: concatenates their comma bases
and puts the result into canonical form.
  
Can accept any number of temperament representations,
as any combination of mappings or comma bases,
but returns the temperament as a comma basis.
  
In    meantone = {{{4, -4, 1}}, "contra"};
      porcupine = {{{1, -5, 3}}, "contra"};
      meet[meantone, porcupine]
  
Out   {{{-11, 7, 0}, {-7, 3, 1}}, "contra"}
  
In    mint = {{{2, 2, -1, -1}}, "contra"};
      meantone = {{{4, -4, 1, 0}}, "contra"};
      negri = {{{-14, 3, 4, 0}}, "contra"};
      meet[mint, meantone, negri]
  
Out   {{{30, 19, 0, 0}, {-26, 15, 1, 0}, {-6, 2, 0, 1}}, "contra"}
  
*)
meet[tSequence___] := canonicalForm[{Apply[Join, Map[getC, {tSequence}]], "contra"}];


(* ARITHMETIC *)

(*
  
sum[t1, t2]
  
Sums the given temperaments: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be put into a form where
they are identical except for a single basis vector (or covector, if covariant)),
entry-wise sums the pair of linearly independent basis (co)vectors,
recombines them with the basis for the linearly dependent vectors,
then canonicalizes the result, returning a single new temperament
with the same dimensions as the inputs.
  
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
  t2 = If[variancesMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];

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
entry-wise diffs the pair of linearly independent basis (co)vectors,
recombines them with the basis for the linearly dependent vectors,
then canonicalizes the result, returning a single new temperament
with the same dimensions as the inputs.
  
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
  t2 = If[variancesMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];

  If[
    t1 == t2,
    Error,
    arithmetic[t1, t2, False]
  ]
];




(* ___ PRIVATE ___ *)



(* LIST UTILITIES *)

divideOutGcd[l_] := Module[{gcd}, gcd = Apply[GCD, l]; If[gcd == 0, l, l / gcd]];
multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

leadingEntry[l_] := First[Select[l, # != 0&, 1]];
trailingEntry[l_] := leadingEntry[Reverse[l]];

allZerosL[a_] := AllTrue[a, # == 0&];


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

getA[t_] := First[t];
getV[t_] := Last[t];

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


(* MEET AND JOIN *)

getM[t_] := If[isCo[t] == True, getA[t], noncanonicalAntiNullSpaceBasis[getA[t]]];
getC[t_] := If[isContra[t] == True, getA[t], noncanonicalNullSpaceBasis[getA[t]]];


(* ARITHMETIC *)

arithmetic[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2],
  Error,
  Module[{linearDependenceBasis, tSumAndDiff},
    linearDependenceBasis = getLinearDependenceBasis[t1, t2];
    tSumAndDiff = If[
      linearDependenceBasis === Error,
      Error,
      getSumAndDiff[t1, t2, linearDependenceBasis]
    ];
    If[
      tSumAndDiff === Error,
      Error,
      chooseCorrectlyBetweenSumAndDiff[t1, t2, isSum, tSumAndDiff]
    ]
  ]
];

getSumAndDiff[t1_, t2_, inputLinearDependenceBasis_] := Module[
  {
    grade,
    linearDependenceBasis,
    a1,
    a2,
    tSum,
    tDiff,
    a1linearlyIndependentVector,
    a2linearlyIndependentVector,
    linearlyIndependentVectorSum,
    linearlyIndependentVectorDiff
  },
  grade = getGrade[t1];
  linearDependenceBasis = inputLinearDependenceBasis;
  a1 = defactorWhileLockingLinearDependenceBasis[t1, linearDependenceBasis];
  a2 = defactorWhileLockingLinearDependenceBasis[t2, linearDependenceBasis];
  a1linearlyIndependentVector = Last[a1];
  a2linearlyIndependentVector = Last[a2];
  linearlyIndependentVectorSum = a1linearlyIndependentVector + a2linearlyIndependentVector;
  linearlyIndependentVectorDiff = a1linearlyIndependentVector - a2linearlyIndependentVector;
  tSum = canonicalForm[{Join[linearDependenceBasis, {linearlyIndependentVectorSum}], getV[t1]}];
  tDiff = canonicalForm[{Join[linearDependenceBasis, {linearlyIndependentVectorDiff}], getV[t1]}];
  {tSum, tDiff}
];

defactorWhileLockingLinearDependenceBasis[t_, linearDependenceBasis_] := Module[
  {
    grade,
    lockedLinearDependenceBasisFormOfA
  },
  grade = getGrade[t];
  lockedLinearDependenceBasisFormOfA = getInitialLockedLinearDependenceBasisFormOfA[t, linearDependenceBasis, grade];
  If[
    isLinearlyDependent[linearDependenceBasis],
    defactorWhileLockingNonemptyLinearDependenceBasis[t, linearDependenceBasis, grade, lockedLinearDependenceBasisFormOfA],
    lockedLinearDependenceBasisFormOfA
  ]
];

defactorWhileLockingNonemptyLinearDependenceBasis[t_, linearDependenceBasis_, grade_, lockedLinearDependenceBasisFormOfAInput_] := Module[
  {
    lockedLinearDependenceBasisFormOfA,
    d,
    linearDependence,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  lockedLinearDependenceBasisFormOfA = lockedLinearDependenceBasisFormOfAInput;
  d = getD[t];
  linearDependence = getLinearDependence[linearDependenceBasis];
  enfactoring = getEnfactoring[lockedLinearDependenceBasisFormOfA];
  multiples = Table[Subscript[x, i], {i, linearDependence}];
  equations = Map[
    Function[
      dIndex,
      Mod[lockedLinearDependenceBasisFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * linearDependenceBasis[[multiplesIndex]][[dIndex]]],
        Range[linearDependence]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  lockedLinearDependenceBasisFormOfA[[grade]] = divideOutGcd[lockedLinearDependenceBasisFormOfA[[grade]] + getLinearDependenceBasisLinearCombination[linearDependenceBasis, result]];
  lockedLinearDependenceBasisFormOfA
];

variancesMatch[t1_, t2_] := getV[t1] == getV[t2];

getLinearDependenceBasis[t1_, t2_] := Module[{linearDependenceBasis},
  linearDependenceBasis = removeAllZeroRows[getA[dual[
    If[
      isContra[t1],
      join[t1, t2],
      meet[t1, t2]
    ]
  ]]];
  If[
    isAddable[linearDependenceBasis, t1],
    linearDependenceBasis,
    Error
  ]
];

isAddable[linearDependenceBasis_, t_] := getLinearDependence[linearDependenceBasis] === getGrade[t] - 1;

getLinearDependence[linearDependenceBasis_] := Length[linearDependenceBasis];

dimensionsDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

getGrade[t_] := If[isContra[t], getN[t], getR[t]];

isLinearlyDependent[linearDependenceBasis_] := getLinearDependence[linearDependenceBasis] > 0;

getInitialLockedLinearDependenceBasisFormOfA[t_, linearDependenceBasis_, grade_] := Module[
  {
    potentiallyLinearlyIndependentVectors,
    lockedLinearDependenceBasisFormOfA
  },
  potentiallyLinearlyIndependentVectors = If[isContra[t], getC[t], getM[t]];
  lockedLinearDependenceBasisFormOfA = linearDependenceBasis;
  Do[
    candidate = hnf[Join[linearDependenceBasis, {potentiallyLinearlyIndependentVector}]];
    If[
      Length[lockedLinearDependenceBasisFormOfA] < grade && MatrixRank[candidate] > Length[linearDependenceBasis],
      lockedLinearDependenceBasisFormOfA = Join[lockedLinearDependenceBasisFormOfA, {potentiallyLinearlyIndependentVector}]
    ],
    {potentiallyLinearlyIndependentVector, potentiallyLinearlyIndependentVectors}
  ];
  Take[lockedLinearDependenceBasisFormOfA, grade]
];

getEnfactoring[a_] := Det[getEnfactoredDetA[a]];

getEnfactoredDetA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getLinearDependenceBasisLinearCombination[linearDependenceBasis_, linearDependenceBasisMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {linearDependenceBasis, linearDependenceBasisMultiplePermutation}
]];

chooseCorrectlyBetweenSumAndDiff[t1_, t2_, isSum_, tSumAndDiff_] := Module[
  {
    tSum,
    tDiff,
    tSumMinors,
    tSumMinorsChecker,
    tSumMinorsMatch
  },
  tSum = First[tSumAndDiff];
  tDiff = Last[tSumAndDiff];
  tSumMinors = getMinors[tSum];
  tSumMinorsChecker = getSumMinorsChecker[t1, t2];
  tSumMinorsMatch = tSumMinors == tSumMinorsChecker;
  If[
    isSum,
    If[tSumMinorsMatch, tSum, tDiff],
    If[tSumMinorsMatch, tDiff, tSum]
  ]
];

getMinors[t_] := Module[{contra, grade, minors, entryFn, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = divideOutGcd[First[Minors[getA[t], grade]]];
  entryFn = If[contra, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[minors];
  If[normalizingEntry < 0, -minors, minors]
];

getSumMinorsChecker[t1_, t2_] := Module[{t2sameVariance},
  t2sameVariance = If[getV[t1] != getV[t2], dual[t2], t2];
  divideOutGcd[getMinors[t1] + getMinors[t2sameVariance]]
];
