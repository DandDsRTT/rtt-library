(* MULTIVECTOR UTILITIES *)

(*
  
eaGetD[multivector]
  
Given a representation of a temperament as a multivector,
returns the dimensionality.
  
Examples:
  
In    meantoneMultimap = {{1, 4, 4}, 2, "co"};
      eaGetD[meantoneMultimap]
  
Out   3
  
In    meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
      eaGetD[meantoneMulticomma]
  
Out   3
  
*)
eaGetD[w_] := If[
  isNondecomposable[w],
  Error,
  decomposableEaDimensionality[w]
];

(*
  
eaGetR[multivector]
  
Given a representation of a temperament as a multivector,
returns the rank.
  
Examples:
  
In    meantoneMultimap = {{1, 4, 4}, 2, "co"};
      eaGetR[meantoneMultimap]
  
Out   2
  
In    meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
      eaGetR[meantoneMulticomma]
  
Out   2
  
*)
eaGetR[w_] := If[
  isNondecomposable[w],
  Error,
  decomposableEaRank[w]
];

(*
  
eaGetN[multivector]
  
Given a representation of a temperament as a multivector,
returns the nullity.
  
Examples:
  
In    meantoneMultimap = {{1, 4, 4}, 2, "co"};
      eaGetN[meantoneMultimap]
  
Out   1
  
In    meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
      eaGetN[meantoneMulticomma]
  
Out   1
  
*)
eaGetN[w_] := If[
  isNondecomposable[w],
  Error,
  decomposableEaNullity[w]
];


(* MULTIVECTOR FORMS & DEFACTORING *)

(*
  
eaCanonicalForm[multivector]
  
Returns the given multivector in canonical form.
  
If a multimap, the GCD is extracted,
and the leading entry is normalized to positive.
If a multicomma, the GCD is extracted,
and the trailing entry is normalized to positive.
  
Examples:
  
In    enfactoredMeantoneMultimap = {{2, 8, 8}, 2, "co"};
      eaCanonicalForm[enfactoredMeantoneMultimap]
  
Out   {{1, 4, 4}, 2, "co"}
  
In    wrongSignMeantoneMulticomma = {{-4, 4, -1}, 1, "contra"};
      eaCanonicalForm[wrongSignMeantoneMulticomma]
  
Out   {{4, -4, 1}, 1, "contra"}
  
*)
eaCanonicalForm[w_] := If[
  allZerosL[eaGetMinors[w]],
  w,
  If[
    isNondecomposable[w],
    Error,
    decomposableEaCanonicalForm[w]
  ]
];


(* DUAL *)

(*
  
eaDual[multivector]
  
Given a multivector, returns its dual in canonical form.
  
Examples:
  
In    meantoneMultimap = {{1, 4, 4}, 2, "co"};
      eaDual[meantoneMultimap]
  
Out   {{4, -4, 1}, 1, "contra"}
  
In    nilovector = {{1}, 0, "contra"};
      d = 3
      eaDual[nilovector, d]
  
Out   {{1}, 0, "co"}
  
*)
eaDual[w_] := If[
  isNondecomposable[w],
  Error,
  decomposableEaDual[w]
];


(* CONVERSION TO AND FROM MATRIX *)

(*
  
multivectorToMatrix[multivector]
  
Given a temperament represented as a multivector,
returns the corresponding mapping or comma basis
(given a multimap, returns the corresponding mapping, or
given a multicomma, returns the corresponding comma basis).
The matrix is returned in canonical form.
  
In    meantoneMultimap = {{1, 4, 4}, 2, "co"};
      multivectorToMatrix[meantoneMultimap]
  
Out   {{{1, 0, -4}, {0, 1, 4}}, "mapping"}
  
*)
multivectorToMatrix[w_] := Module[{grade, t},
  grade = eaGetGrade[w];
  t = If[
    grade == 0,
    nilovectorToMatrix[w],
    If[
      grade == 1,
      monovectorToMatrix[w],
      If[
        eaIsContra[w],
        multicommaToC[w],
        multimapToM[w]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalForm[t]]
];

(*
  
matrixToMultivector[m]
  
Given a temperament represented as a mapping or comma basis,
returns the corresponding multivector
(for a mapping, returns a multimap, or
for a comma basis, returns a multicomma).
The multivector is returned in canonical form.
  
In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "mapping"};
      matrixToMultivector[meantoneM]
  
Out   {{1, 4, 4}, 2, "co"}
  
*)
matrixToMultivector[t_] := eaCanonicalForm[
  If[
    isContra[t],
    {minorsList[getA[t]], getN[t], getV[t], getD[t]},
    {minorsList[getA[t]], getR[t], getV[t], getD[t]}
  ]
];


(* MEET AND JOIN *)

(*
  
progressiveProduct[multivector1, multivector2]
  
Given two multivectors, returns the multivector result for their progressive product.
  
Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.
  
Also known as the wedge product or the exterior product.
  
In    et5 = {{5, 8, 12}, 1, "co"};
      et7 = {{7, 11, 16}, 1, "co"};
      progressiveProduct[et5, et7]
  
Out   {{1, 4, 4}, 2, "co"}
  
*)
progressiveProduct[w1_, w2_] := Module[{grade1, grade2,grade,d, v1, v2,v},
  grade1 = eaGetGrade[w1];
  grade2 = eaGetGrade[w2];
  grade =  grade1 +  grade2;
  d = eaGetD[w1];
  v1 = eaGetV[w1];
  v2 =  eaGetV[w2];
  v = If[v1 != v2, Error, v1];
  
  If[
    v === Error || grade > d ,
    Error,
    eaCanonicalForm[
      tensorToMultivector[
        TensorWedge[multivectorToTensor[w1], multivectorToTensor[w2]],
        grade,
        v1,
        d
      ]
    ]
  ]
];

(*
  
regressiveProduct[multivector1, multivector2]
  
Given two multivectors, returns the multivector result for their regressive product.
  
Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.
  
Also known as the vee product.
  
In    et5 = {{5, 8, 12}, 1, "co"};
      et7 = {{7, 11, 16}, 1, "co"};
      regressiveProduct[et5, et7]
  
Out   {{1, 4, 4}, 2, "co"}
  
*)
regressiveProduct[w1_, w2_] := Module[{dualW},
  dualW = progressiveProduct[eaDual[w1], eaDual[w2]];
  
  If[
    dualW === Error,
    Error,
    eaDual[dualW]
  ]
];

(*
  
interiorProduct[multivector1, multivector2]
  
Given two multivectors, returns the multivector result for their symmetric interior product.
By symmetric, it is meant that it chooses either the right or left interior product
depending on the grades of the input multivectors.
  
Also known as the vee product.
  
In    et5 = {{5, 8, 12}, 1, "co"};
      et7 = {{7, 11, 16}, 1, "co"};
      regressiveProduct[et5, et7]
  
Out   {{1, 4, 4}, 2, "co"}
  
*)
interiorProduct[w1_, w2_] := If[
  eaGetGrade[w1] >= eaGetGrade[w2],
  rightInteriorProduct[w1, w2],
  leftInteriorProduct[w1, w2]
];


(* ARITHMETIC *)

(*
  
eaSum[w1, w2]
  
Sums the given multivectors: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be decomposed into a set of vectors
that are identical except for a single vector (or covector, if covariant)),
entry-wise sums the multivectors, then canonicalizes the result,
returning a single new multivector with the same dimensions as the inputs.
  
If the given multivectors are not the same dimensions and addable,
it will error.
  
Can accept multivectors of different variances,
but it will return a multivector with the same variance
as the first given multivector.
  
In    meantoneC = {{{4, -4, 1}}, "contra"};
      porcupineC = {{{1, -5, 3}}, "contra"};
      sum[meantoneC, porcupineC]
  
Out   {{{5, -9, 4}}, "contra"}
  
In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
      sum[meantoneM, porcupineM]
  
Out   {{{1, 1, 1}, {0, 4, 9}}, "co"}
  
*)
eaSum[w1_, w2_] := eaArithmetic[w1, w2, True];

(*
  
eaDiff[w1, w2]
  
Diffs the given multivectors: if they have the same dimensions
(same dimensionality, rank (and nullity)),
and are addable (can be decomposed into a set of vectors
that are identical except for a single vector (or covector, if covariant)),
entry-wise diffs the multivectors, then canonicalizes the result,
returning a single new multivector with the same dimensions as the inputs.
  
If the given multivectors are not the same dimensions and addable,
it will error.
  
Can accept multivectors of different variances,
but it will return a multivector with the same variance
as the first given multivector.
  
*)
eaDiff[w1_, w2_] := eaArithmetic[w1, w2, False];


(* ___ PRIVATE ___ *)



(* MULTIVECTOR UTILITIES *)

eaIsContra[w_] := MemberQ[{
  "contra",
  "contravector",
  "multicontravector",
  "contravariant",
  "v",
  "vector",
  "c",
  "comma",
  "multicomma",
  "i",
  "interval",
  "multinterval",
  "multiinterval",
  "monzo",
  "multimonzo",
  "against"
}, eaGetV[w]];
eaIsCo[w_] := MemberQ[{
  "co",
  "covector",
  "multicovector",
  "covariant",
  "m",
  "map",
  "multimap",
  "val",
  "multival",
  "with",
  "wedgie"
}, eaGetV[w]];

decomposableEaDimensionality[w_] := If[
  Length[w] == 4,
  Part[w, 4],
  Module[{minors, grade, d},
    minors = eaGetMinors[w];
    grade = eaGetGrade[w];
  
    First[Association[Solve[
      Binomial[d, grade] == Length[minors] && d >= 0,
      d,
      Integers
    ]]]
  ]
];

decomposableEaRank[w_] := If[
  eaIsCo[w],
  eaGetGrade[w],
  decomposableEaDimensionality[w] - decomposableEaNullity[w]
];
decomposableEaNullity[w_] := If[
  eaIsContra[w],
  eaGetGrade[w],
  decomposableEaDimensionality[w] - decomposableEaRank[w]
];

eaIndices[d_, grade_] := Subsets[Range[d], {grade}];

isNondecomposable[v_] := multivectorToMatrix[v] === Error;

eaGetMinors[w_] := Part[w, 1];
eaGetGrade[w_] := Part[w, 2];
eaGetV[w_] := Part[w, 3];


(* MULTIVECTOR FORMS & DEFACTORING *)


decomposableEaCanonicalForm[w_] := Module[{minors, grade, v, normalizer},
  grade = eaGetGrade[w];
  v = eaGetV[w];
  minors = divideOutGcd[eaGetMinors[w]];
  normalizer = If[
    (eaIsCo[w] && leadingEntry[minors] < 0) || (eaIsContra[w] && trailingEntry[minors] < 0),
    -1,
    1
  ];
  
  If[
    grade == 0,
    {normalizer * minors, grade, v, eaGetD[w]},
    {normalizer * minors, grade, v}
  ]
];


(* DUAL *)

getDualV[w_] := If[
  eaIsCo[w],
  "contra",
  "co"
];

decomposableEaDual[w_] := Module[{dualV, d, grade},
  dualV = getDualV[w];
  d = decomposableEaDimensionality[w];
  grade = eaGetGrade[w];
  
  If[
    grade == 0,
    {{1}, d, dualV},
    If[
      grade == d,
      {{1}, 0, dualV, d},
      Module[{dualGrade, tensor, dualTensor, dualW},
        dualGrade = d - grade;
        tensor = multivectorToTensor[w];
        dualTensor = HodgeDual[tensor];
        dualW = tensorToMultivector[dualTensor, dualGrade, dualV, d];
  
        decomposableEaCanonicalForm[dualW]
      ]
    ]
  ]
];

multivectorToTensor[w_] := Module[{d, grade, minors},
  d = decomposableEaDimensionality[w];
  grade = eaGetGrade[w];
  minors = eaGetMinors[w];
  
  SymmetrizedArray[
    MapThread[Rule[#1, #2]&, {eaIndices[d, grade], minors}],
    ConstantArray[d, grade],
    Antisymmetric[All]
  ]
];

tensorToMultivector[tensor_, grade_, v_, d_] := Module[{rules , assoc, signTweak, minors},
  rules = SymmetrizedArrayRules[tensor];
  
  If[
    allZerosL[Map[Last, rules]],
    {Table[0, Binomial[d, grade]], grade, v},
    assoc = Association[rules];
    signTweak = If[eaIsCo[{{}, v, grade, d}] && Mod[grade(d - grade), 2] == 1, -1, 1];
    minors = signTweak * Map[If[KeyExistsQ[assoc, #], assoc[#], 0]&, eaIndices[d, grade]];
  
    {minors, grade, v}
  ]
];


(* CONVERSION TO AND FROM MATRIX *)

nilovectorToMatrix[{minors_, grade_, v_, d_}] := {{Table[0, d]}, v};

monovectorToMatrix[w_] := {{eaGetMinors[w]}, eaGetV[w]};

multimapToM[w_] := Module[{grade, flattenedTensorMatrix},
  grade = eaGetGrade[w];
  flattenedTensorMatrix = hnf[Flatten[multivectorToTensor[w], grade - 2]];
  
  If[
    MatrixRank[flattenedTensorMatrix] != grade,
    Error,
    {Take[flattenedTensorMatrix, grade], eaGetV[w]}
  ]
];

multicommaToC[w_] := Module[{grade, flattenedTensorMatrix},
  grade = eaGetGrade[w];
  flattenedTensorMatrix = hnf[reverseEachRow[Flatten[multivectorToTensor[w], grade - 2]]];
  
  If[
    MatrixRank[flattenedTensorMatrix] != grade,
    Error,
    {antiTranspose[Take[flattenedTensorMatrix, grade]], eaGetV[w]}
  ]
];

minorsList[a_] := divideOutGcd[First[Minors[a, MatrixRank[a]]]];


(* MEET AND JOIN *)

rightInteriorProduct[w1_, w2_] := Module[{dualW},
  dualW = progressiveProduct[eaDual[w1], w2];
  
  If[
    dualW === Error,
    Error,
    eaDual[dualW]
  ]
];
leftInteriorProduct[w1_, w2_] := Module[{dualW},
  dualW = progressiveProduct[w1, eaDual[w2]];
 
  If[
    dualW === Error,
    Error,
    eaDual[dualW]
  ]
];


(* ARITHMETIC *)

eaArithmetic[w1input_, w2input_, isSum_] := Module[{w1, w2},
  w1 = eaCanonicalForm[w1input];
  w2 = If[eaGetV[w2input] != eaGetV[w1], eaDual[w2input], eaCanonicalForm[w2input]];
  
  If[
    eaGetR[w1] != eaGetR[w2] || eaGetD[w1] != eaGetD[w2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetMinors[w1] + eaGetMinors[w2], eaGetGrade[w1], eaGetV[w1]}],
      eaCanonicalForm[{eaGetMinors[w1] - eaGetMinors[w2], eaGetGrade[w1], eaGetV[w1]}]
    ]
  ]
];
