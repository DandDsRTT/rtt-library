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

Sums the given temperaments: if they have the same shape
(same  dimensionality, rank (and nullity)),
and are monononcollinear (can be put into a form where
they are identical except for a single vector (or covector, if covariant)),
entry-wise sums the pair of noncollinear (co)vectors,
recombines them with the collinear vectors,
then canonicalizes the result, returning a single new temperament
with the same shape as the inputs.

If the given temperaments are not the same shape and monononcollinear,
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

Diffs the given temperaments: if they have the same shape
(same  dimensionality, rank (and nullity)),
and are monononcollinear (can be put into a form where
they are identical except for a single vector (or covector, if covariant)),
entry-wise diffs the pair of noncollinear (co)vectors,
recombines them with the collinear vectors,
then canonicalizes the result, returning a single new temperament
with the same shape as the inputs.

If the given temperaments are not the same shape and monononcollinear,
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

divideOutGcd[l_] := Module[{gcd}, gcd = Apply[GCD, l]; If[gcd==0, l, l/gcd]];
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
  tShapesDoNotMatch[t1, t2],
  Error,
  Module[{collinearityT, tSumAndDiff},
    collinearityT = getCollinearityT[t1, t2];
    tSumAndDiff = If[
      collinearityT === Error,
      Error,
      getSumAndDiff[t1, t2, collinearityT]
    ];

    If[
      tSumAndDiff === Error,
      Error,
      chooseCorrectlyBetweenSumAndDiff[t1, t2, isSum, tSumAndDiff]
    ]
  ]
];

getSumAndDiff[t1_, t2_, collinearityT_] := Module[
  {
    grade,
    collinearUnvariancedVectors,
    a1,
    a2,
    tSum,
    tDiff,
    a1noncollinearVector,
    a2noncollinearVector,
    noncollinearVectorSum,
    noncollinearVectorDiff
  },

  grade = getGradeMatchingCollinearity[t1, collinearityT];
  collinearUnvariancedVectors = getA[collinearityT];

  a1 = defactorWhileLockingCollinearUnvariancedVectors[t1, collinearityT];
  a2 = defactorWhileLockingCollinearUnvariancedVectors[t2, collinearityT];

  a1noncollinearVector = Last[a1];
  a2noncollinearVector = Last[a2];
  noncollinearVectorSum = a1noncollinearVector + a2noncollinearVector;
  noncollinearVectorDiff = a1noncollinearVector - a2noncollinearVector;

  tSum = {Join[collinearUnvariancedVectors, {noncollinearVectorSum}], getV[collinearityT]};
  tSum = If[variancesMatch[t1, collinearityT], canonicalForm[tSum], dual[tSum]];

  tDiff = {Join[collinearUnvariancedVectors, {noncollinearVectorDiff}], getV[collinearityT]};
  tDiff = If[variancesMatch[t1, collinearityT], canonicalForm[tDiff], dual[tDiff]];

  {tSum, tDiff}
];

defactorWhileLockingCollinearUnvariancedVectors[t_, collinearityT_] := Module[
  {
    grade,
    collinearUnvariancedVectors,
    lockedCollinearUnvariancedVectorsFormOfA
  },

  grade = getGradeMatchingCollinearity[t, collinearityT];
  collinearUnvariancedVectors = getA[collinearityT];
  lockedCollinearUnvariancedVectorsFormOfA = getInitialLockedCollinearUnvariancedVectorsFormOfA[t, collinearityT, grade, collinearUnvariancedVectors];

  If[
    isCollinear[collinearityT],
    defactorWhileLockingAtLeastOneCollinearUnvariancedVector[t, collinearityT, grade, collinearUnvariancedVectors, lockedCollinearUnvariancedVectorsFormOfA],
    lockedCollinearUnvariancedVectorsFormOfA
  ]
];

defactorWhileLockingAtLeastOneCollinearUnvariancedVector[t_, collinearityT_, grade_, collinearUnvariancedVectors_, lockedCollinearUnvariancedVectorsFormOfAInput_] := Module[
  {
    lockedCollinearUnvariancedVectorsFormOfA,
    d,
    collinearity,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },

  lockedCollinearUnvariancedVectorsFormOfA = lockedCollinearUnvariancedVectorsFormOfAInput;
  d = getD[t];
  collinearity = getCollinearity[collinearityT];
  enfactoring = getEnfactoring[lockedCollinearUnvariancedVectorsFormOfA];
  multiples = Table[Subscript[x, i], {i, collinearity}];
  equations = Map[
    Function[
      dIndex,
      Mod[lockedCollinearUnvariancedVectorsFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * collinearUnvariancedVectors[[multiplesIndex]][[dIndex]]],
        Range[collinearity]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];

  lockedCollinearUnvariancedVectorsFormOfA[[grade]] = divideOutGcd[lockedCollinearUnvariancedVectorsFormOfA[[grade]] + getCollinearUnvariancedVectorLinearCombination[collinearUnvariancedVectors, result]];

  lockedCollinearUnvariancedVectorsFormOfA
];

variancesMatch[t1_, t2_] := getV[t1] == getV[t2];

getCollinearityT[t1_, t2_] := Module[{collinearityM, collinearityC},
  collinearityM = dual[join[t1, t2]];
  collinearityC = dual[meet[t1, t2]];

  collinearityM[[1]] = removeAllZeroRows[collinearityM[[1]]];
  collinearityC[[1]] = removeAllZeroRows[collinearityC[[1]]];

  If[
    isMonononcollinear[collinearityC, t1] && isCollinear[collinearityC],
    collinearityC,
    If[
      isMonononcollinear[collinearityM, t1],
      collinearityM,
      Error
    ]
  ]
];

isMonononcollinear[collinearityT_, t_] := If[
  isContra[collinearityT],
  getCollinearity[collinearityT] === getN[t] - 1,
  getCollinearity[collinearityT] === getR[t] - 1
];

getCollinearity[collinearityT_] := Length[getA[collinearityT]];

tShapesDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

getGradeMatchingCollinearity[t_, collinearityT_] := If[
  variancesMatch[t, collinearityT],
  If[isContra[t], getN[t], getR[t]],
  If[isContra[t], getR[t], getN[t]]
];

isCollinear[collinearityT_] := getCollinearity[collinearityT] > 0;

getInitialLockedCollinearUnvariancedVectorsFormOfA[t_, collinearityT_, grade_, collinearUnvariancedVectors_] := Module[
  {
    potentiallyNoncollinearUnvariancedVectors,
    lockedCollinearUnvariancedVectorsFormOfA
  },

  potentiallyNoncollinearUnvariancedVectors = If[isContra[collinearityT], getC[t], getM[t]];
  lockedCollinearUnvariancedVectorsFormOfA = collinearUnvariancedVectors;

  Do[
    candidate = hnf[Join[collinearUnvariancedVectors, {potentiallyNoncollinearVector}]];
    If[
      Length[lockedCollinearUnvariancedVectorsFormOfA] < grade && MatrixRank[candidate] > Length[collinearUnvariancedVectors],
      lockedCollinearUnvariancedVectorsFormOfA = Join[lockedCollinearUnvariancedVectorsFormOfA, {potentiallyNoncollinearVector}]
    ],
    {potentiallyNoncollinearVector, potentiallyNoncollinearUnvariancedVectors}
  ];

  Take[lockedCollinearUnvariancedVectorsFormOfA, grade]
];

getEnfactoring[a_] := Det[getEnfactoredDetA[a]];

getEnfactoredDetA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getCollinearUnvariancedVectorLinearCombination[collinearUnvariancedVectors_, collinearVectorMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {collinearUnvariancedVectors, collinearVectorMultiplePermutation}
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
