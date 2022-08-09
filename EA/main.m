(*
  
  MULTIVECTOR UTILITIES
  
  
  eaGetD[multivector]
  
  Given a representation of a temperament as a multivector,
  returns the dimensionality.
  
  Examples:
  
  In    meantoneMm = {{1, 4, 4}, 2, "map"};
        eaGetD[meantoneMm]
    
  Out   3
  
  In    meantoneMc = {{4, -4, 1}, 1, "vector"};
        eaGetD[meantoneMc]
    
  Out   3
  
*)
eaGetD[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableD[u]
];

(*
  
  eaGetR[multivector]
  
  Given a representation of a temperament as a multivector,
  returns the rank.
  
  Examples:
  
  In    meantoneMm = {{1, 4, 4}, 2, "map"};
        eaGetR[meantoneMm]
    
  Out   2
  
  In    meantoneMc = {{4, -4, 1}, 1, "vector"};
        eaGetR[meantoneMc]
    
  Out   2
  
*)
eaGetR[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableR[u]
];

(*
  
  eaGetN[multivector]
  
  Given a representation of a temperament as a multivector,
  returns the nullity.
  
  Examples:
  
  In    meantoneMm = {{1, 4, 4}, 2, "map"};
        eaGetN[meantoneMm]
    
  Out   1
  
  In    meantoneMc = {{4, -4, 1}, 1, "vector"};
        eaGetN[meantoneMc]
    
  Out   1
  
*)
eaGetN[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableN[u]
];


(*
  
  MULTIVECTOR FORMS & DEFACTORING
  
  
  eaCanonicalForm[multivector]
  
  Returns the given multivector in canonical form.
  
  If a multimap, the GCD is extracted,
  and the leading entry is normalized to positive.
  If a multicomma, the GCD is extracted,
  and the trailing entry is normalized to positive.
  
  Examples:
  
  In    enfactoredMeantoneMm = {{2, 8, 8}, 2, "map"};
        eaCanonicalForm[enfactoredMeantoneMm]
    
  Out   {{1, 4, 4}, 2, "map"}
  
  In    wrongSignMeantoneMc = {{-4, 4, -1}, 1, "vector"};
        eaCanonicalForm[wrongSignMeantoneMc]
    
  Out   {{4, -4, 1}, 1, "vector"}
  
*)
eaCanonicalForm[u_] := If[
  allZerosL[eaGetLargestMinorsL[u]],
  u,
  If[
    isNondecomposable[u],
    Error,
    decomposableEaCanonicalForm[u]
  ]
];


(*
  
  DUAL
  
  
  eaDual[multivector]
  
  Given a multivector, returns its dual in canonical form.
  
  Examples:
  
  In    meantoneMm = {{1, 4, 4}, 2, "map"};
        eaDual[meantoneMm]
    
  Out   {{4, -4, 1}, 1, "vector"}
  
  In    nilovector = {{1}, 0, "vector"};
        d = 3
        eaDual[nilovector, d]
    
  Out   {{1}, 0, "map"}
  
*)
eaDual[u_] := If[
  isNondecomposable[u],
  Error,
  decomposableEaDual[u]
];


(*
  
  CONVERSION TO AND FROM MATRIX
  
  
  multivectorToMatrix[multivector]
  
  Given a temperament represented as a multivector,
  returns the corresponding mapping or comma basis
  (given a multimap, returns the corresponding mapping, or
  given a multicomma, returns the corresponding comma basis).
  The matrix is returned in canonical form.
  
  In    meantoneMm = {{1, 4, 4}, 2, "map"};
        multivectorToMatrix[meantoneMm]
    
  Out   {{{1, 0, -4}, {0, 1, 4}}, "map"}
  
*)
multivectorToMatrix[u_] := Module[{grade, t},
  grade = eaGetGrade[u];
  t = If[
    grade == 0,
    nilovectorToA[u],
    If[
      grade == 1,
      monovectorToA[u],
      If[
        eaIsVectors[u],
        mcToC[u],
        mmToM[u]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalFormPrivate[t]]
];

(*
  
  matrixToMultivector[m]
  
  Given a temperament represented as a mapping or comma basis,
  returns the corresponding multivector
  (for a mapping, returns a multimap, or
  for a comma basis, returns a multicomma).
  The multivector is returned in canonical form.
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "map"};
        matrixToMultivector[meantoneM]
    
  Out   {{1, 4, 4}, 2, "map"}
  
*)
matrixToMultivector[t_] := eaCanonicalForm[
  If[
    isVectors[t],
    {getLargestMinorsL[getA[t]], getNPrivate[t], getVariance[t], getDPrivate[t]},
    {getLargestMinorsL[getA[t]], getRPrivate[t], getVariance[t], getDPrivate[t]}
  ]
];


(*
  
  MERGE
  
  
  progressiveProduct[multivector1, multivector2]
  
  Given two multivectors, returns the multivector result for their progressive product.
  
  Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.
  
  Also known as the wedge product or the exterior product.
  
  In    et5 = {{5, 8, 12}, 1, "map"};
        et7 = {{7, 11, 16}, 1, "map"};
        progressiveProduct[et5, et7]
    
  Out   {{1, 4, 4}, 2, "map"}
  
*)
progressiveProduct[u1_, u2_] := Module[{grade1, grade2, grade, d, variance1, variance2, variance},
  grade1 = eaGetGrade[u1];
  grade2 = eaGetGrade[u2];
  grade = grade1 + grade2;
  d = eaGetD[u1];
  variance1 = eaGetVariance[u1];
  variance2 = eaGetVariance[u2];
  variance = If[variance1 != variance2, Error, variance1];
  
  If[
    variance === Error || grade > d,
    Error,
    eaCanonicalForm[
      tensorToU[
        TensorWedge[uToTensor[u1], uToTensor[u2]],
        grade,
        variance1,
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
  
  In    et5 = {{5, 8, 12}, 1, "map"};
        et7 = {{7, 11, 16}, 1, "map"};
        regressiveProduct[et5, et7]
    
  Out   {{1, 4, 4}, 2, "map"}
  
*)
regressiveProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[eaDual[u1], eaDual[u2]];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];

(*
  
  interiorProduct[multivector1, multivector2]
  
  Given two multivectors, returns the multivector result for their symmetric interior product.
  By symmetric, it is meant that it chooses either the right or left interior product
  depending on the grades of the input multivectors.
  
  Also known as the vee product.
  
  In    et5 = {{5, 8, 12}, 1, "map"};
        et7 = {{7, 11, 16}, 1, "map"};
        regressiveProduct[et5, et7]
    
  Out   {{1, 4, 4}, 2, "map"}
  
*)
interiorProduct[u1_, u2_] := If[
  eaGetGrade[u1] >= eaGetGrade[u2],
  rightInteriorProduct[u1, u2],
  leftInteriorProduct[u1, u2]
];


(*
  
  ADDITION
  
  
  eaSum[u1, u2]
  
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
  
  In    meantoneC = {{{4, -4, 1}}, "vector"}; (* TODO: uhhh... aren't these just non-EA things? and eaDiff doesn't even have anything, below *)
        porcupineC = {{{1, -5, 3}}, "vector"};
        sum[meantoneC, porcupineC]
    
  Out   {{{5, -9, 4}}, "vector"}
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "map"};
        porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "map"};
        sum[meantoneM, porcupineM]
    
  Out   {{{1, 1, 1}, {0, 4, 9}}, "map"}
  
*)
eaSum[u1_, u2_] := eaAddition[u1, u2, True];

(*
  
  eaDiff[u1, u2]
  
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
eaDiff[u1_, u2_] := eaAddition[u1, u2, False];




(* ___ PRIVATE ___ *)



(* MULTIVECTOR UTILITIES *)

eaIsVectors[u_] := MemberQ[{
  "contra",
  "contravector",
  "contravectors",
  "multicontravector",
  "contravariant",
  "v",
  "vector",
  "c",
  "comma",
  "commas",
  "multicomma",
  "i",
  "interval",
  "intervals",
  "multinterval",
  "multiinterval",
  "monzo",
  "monzos",
  "multimonzo",
  "against",
  "mc"
}, eaGetVariance[u]];
eaIsMaps[u_] := MemberQ[{
  "co",
  "covector",
  "covectors",
  "multicovector",
  "covariant",
  "m",
  "map",
  "maps",
  "multimap",
  "val",
  "vals",
  "multival",
  "with",
  "wedgie",
  "mm"
}, eaGetVariance[u]];

eaGetDecomposableD[u_] := If[
  Length[u] == 4,
  Part[u, 4],
  Module[{largestMinorsL, grade, d},
    largestMinorsL = eaGetLargestMinorsL[u];
    grade = eaGetGrade[u];
    
    First[Association[Solve[
      Binomial[d, grade] == Length[largestMinorsL] && d >= 0,
      d,
      Integers
    ]]]
  ]
];

eaGetDecomposableR[u_] := If[
  eaIsMaps[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableN[u]
];
eaGetDecomposableN[u_] := If[
  eaIsVectors[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableR[u]
];

eaIndices[d_, grade_] := Subsets[Range[d], {grade}];

isNondecomposable[variance_] := multivectorToMatrix[variance] === Error;

eaGetLargestMinorsL[u_] := Part[u, 1];
eaGetGrade[u_] := Part[u, 2];
eaGetVariance[u_] := Part[u, 3];


(* MULTIVECTOR FORMS & DEFACTORING *)


decomposableEaCanonicalForm[u_] := Module[{largestMinorsL, grade, variance, normalizer},
  grade = eaGetGrade[u];
  variance = eaGetVariance[u];
  largestMinorsL = divideOutGcd[eaGetLargestMinorsL[u]];
  normalizer = If[
    (eaIsMaps[u] && leadingEntry[largestMinorsL] < 0) || (eaIsVectors[u] && trailingEntry[largestMinorsL] < 0),
    -1,
    1
  ];
  
  If[
    grade == 0,
    {normalizer * largestMinorsL, grade, variance, eaGetD[u]},
    {normalizer * largestMinorsL, grade, variance}
  ]
];


(* DUAL *)

getDualV[u_] := If[
  eaIsMaps[u],
  "vector",
  "map"
];

decomposableEaDual[u_] := Module[{dualV, d, grade},
  dualV = getDualV[u];
  d = eaGetDecomposableD[u];
  grade = eaGetGrade[u];
  
  If[
    grade == 0,
    {{1}, d, dualV},
    If[
      grade == d,
      {{1}, 0, dualV, d},
      Module[{dualGrade, tensor, dualTensor, dualU},
        dualGrade = d - grade;
        tensor = uToTensor[u];
        dualTensor = HodgeDual[tensor];
        dualU = tensorToU[dualTensor, dualGrade, dualV, d];
        
        decomposableEaCanonicalForm[dualU]
      ]
    ]
  ]
];

uToTensor[u_] := Module[{d, grade, largestMinorsL},
  d = eaGetDecomposableD[u];
  grade = eaGetGrade[u];
  largestMinorsL = eaGetLargestMinorsL[u];
  
  SymmetrizedArray[
    MapThread[Rule[#1, #2]&, {eaIndices[d, grade], largestMinorsL}],
    ConstantArray[d, grade],
    Antisymmetric[All]
  ]
];

tensorToU[tensor_, grade_, variance_, d_] := Module[{rules, assoc, signTweak, largestMinorsL},
  rules = SymmetrizedArrayRules[tensor];
  
  If[
    allZerosL[Map[Last, rules]],
    {Table[0, Binomial[d, grade]], grade, variance},
    assoc = Association[rules];
    signTweak = If[eaIsMaps[{{}, variance, grade, d}] && Mod[grade(d - grade), 2] == 1, -1, 1];
    largestMinorsL = signTweak * Map[If[KeyExistsQ[assoc, #], assoc[#], 0]&, eaIndices[d, grade]];
    
    {largestMinorsL, grade, variance}
  ]
];


(* CONVERSION TO AND FROM MATRIX *)

nilovectorToA[{largestMinorsL_, grade_, variance_, d_}] := {{Table[0, d]}, variance};

monovectorToA[u_] := {{eaGetLargestMinorsL[u]}, eaGetVariance[u]};

mmToM[mm_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mm];
  flattenedTensorA = hnf[Flatten[uToTensor[mm], grade - 2]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {Take[flattenedTensorA, grade], eaGetVariance[mm]}
  ]
];

mcToC[mc_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mc];
  flattenedTensorA = hnf[reverseEachRow[Flatten[uToTensor[mc], grade - 2]]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {antiTranspose[Take[flattenedTensorA, grade]], eaGetVariance[mc]}
  ]
];


(* MERGE *)

rightInteriorProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[eaDual[u1], u2];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];
leftInteriorProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[u1, eaDual[u2]];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];


(* ADDITION *)

eaAddition[u1input_, u2input_, isSum_] := Module[{u1, u2},
  u1 = eaCanonicalForm[u1input];
  u2 = If[eaGetVariance[u2input] != eaGetVariance[u1], eaDual[u2input], eaCanonicalForm[u2input]];
  
  If[
    eaGetR[u1] != eaGetR[u2] || eaGetD[u1] != eaGetD[u2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetLargestMinorsL[u1] + eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}],
      eaCanonicalForm[{eaGetLargestMinorsL[u1] - eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}]
    ]
  ]
];
