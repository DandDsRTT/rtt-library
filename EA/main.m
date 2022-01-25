(*
  
  MULTIVECTOR UTILITIES
  
  
  eaGetD[multivector]
  
  Given a representation of a temperament as a multivector,
  returns the dimensionality.
  
  Examples:
  
  In    meantoneMm = {{1, 4, 4}, 2, "co"};
        eaGetD[meantoneMm]
    
  Out   3
  
  In    meantoneMc = {{4, -4, 1}, 1, "contra"};
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
  
  In    meantoneMm = {{1, 4, 4}, 2, "co"};
        eaGetR[meantoneMm]
    
  Out   2
  
  In    meantoneMc = {{4, -4, 1}, 1, "contra"};
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
  
  In    meantoneMm = {{1, 4, 4}, 2, "co"};
        eaGetN[meantoneMm]
    
  Out   1
  
  In    meantoneMc = {{4, -4, 1}, 1, "contra"};
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
  
  In    enfactoredMeantoneMm = {{2, 8, 8}, 2, "co"};
        eaCanonicalForm[enfactoredMeantoneMm]
    
  Out   {{1, 4, 4}, 2, "co"}
  
  In    wrongSignMeantoneMc = {{-4, 4, -1}, 1, "contra"};
        eaCanonicalForm[wrongSignMeantoneMc]
    
  Out   {{4, -4, 1}, 1, "contra"}
  
*)
eaCanonicalForm[u_] := If[
  allZerosL[eaGetLm[u]],
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
  
  In    meantoneMm = {{1, 4, 4}, 2, "co"};
        eaDual[meantoneMm]
    
  Out   {{4, -4, 1}, 1, "contra"}
  
  In    nilovector = {{1}, 0, "contra"};
        d = 3
        eaDual[nilovector, d]
    
  Out   {{1}, 0, "co"}
  
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
  
  In    meantoneMm = {{1, 4, 4}, 2, "co"};
        multivectorToMatrix[meantoneMm]
    
  Out   {{{1, 0, -4}, {0, 1, 4}}, "mapping"}
  
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
        eaIsContra[u],
        mcToC[u],
        mmToM[u]
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
    {getLm[getA[t]], getN[t], getV[t], getD[t]},
    {getLm[getA[t]], getR[t], getV[t], getD[t]}
  ]
];


(*
  
  MERGE
  
  
  progressiveProduct[multivector1, multivector2]
  
  Given two multivectors, returns the multivector result for their progressive product.
  
  Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.
  
  Also known as the wedge product or the exterior product.
  
  In    et5 = {{5, 8, 12}, 1, "co"};
        et7 = {{7, 11, 16}, 1, "co"};
        progressiveProduct[et5, et7]
    
  Out   {{1, 4, 4}, 2, "co"}
  
*)
progressiveProduct[u1_, u2_] := Module[{grade1, grade2, grade, d, v1, v2, v},
  grade1 = eaGetGrade[u1];
  grade2 = eaGetGrade[u2];
  grade = grade1 + grade2;
  d = eaGetD[u1];
  v1 = eaGetV[u1];
  v2 = eaGetV[u2];
  v = If[v1 != v2, Error, v1];
  
  If[
    v === Error || grade > d,
    Error,
    eaCanonicalForm[
      tensorToU[
        TensorWedge[uToTensor[u1], uToTensor[u2]],
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
  
  In    et5 = {{5, 8, 12}, 1, "co"};
        et7 = {{7, 11, 16}, 1, "co"};
        regressiveProduct[et5, et7]
    
  Out   {{1, 4, 4}, 2, "co"}
  
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
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        porcupineC = {{{1, -5, 3}}, "contra"};
        sum[meantoneC, porcupineC]
    
  Out   {{{5, -9, 4}}, "contra"}
  
  In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
        porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
        sum[meantoneM, porcupineM]
    
  Out   {{{1, 1, 1}, {0, 4, 9}}, "co"}
  
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

eaIsContra[u_] := MemberQ[{
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
  "against",
  "mc"
}, eaGetV[u]];
eaIsCo[u_] := MemberQ[{
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
  "wedgie",
  "mm"
}, eaGetV[u]];

eaGetDecomposableD[u_] := If[
  Length[u] == 4,
  Part[u, 4],
  Module[{lm, grade, d},
    lm = eaGetLm[u];
    grade = eaGetGrade[u];
    
    First[Association[Solve[
      Binomial[d, grade] == Length[lm] && d >= 0,
      d,
      Integers
    ]]]
  ]
];

eaGetDecomposableR[u_] := If[
  eaIsCo[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableN[u]
];
eaGetDecomposableN[u_] := If[
  eaIsContra[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableR[u]
];

eaIndices[d_, grade_] := Subsets[Range[d], {grade}];

isNondecomposable[v_] := multivectorToMatrix[v] === Error;

eaGetLm[u_] := Part[u, 1];
eaGetGrade[u_] := Part[u, 2];
eaGetV[u_] := Part[u, 3];


(* MULTIVECTOR FORMS & DEFACTORING *)


decomposableEaCanonicalForm[u_] := Module[{lm, grade, v, normalizer},
  grade = eaGetGrade[u];
  v = eaGetV[u];
  lm = divideOutGcd[eaGetLm[u]];
  normalizer = If[
    (eaIsCo[u] && leadingEntry[lm] < 0) || (eaIsContra[u] && trailingEntry[lm] < 0),
    -1,
    1
  ];
  
  If[
    grade == 0,
    {normalizer * lm, grade, v, eaGetD[u]},
    {normalizer * lm, grade, v}
  ]
];


(* DUAL *)

getDualV[u_] := If[
  eaIsCo[u],
  "contra",
  "co"
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

uToTensor[u_] := Module[{d, grade, lm},
  d = eaGetDecomposableD[u];
  grade = eaGetGrade[u];
  lm = eaGetLm[u];
  
  SymmetrizedArray[
    MapThread[Rule[#1, #2]&, {eaIndices[d, grade], lm}],
    ConstantArray[d, grade],
    Antisymmetric[All]
  ]
];

tensorToU[tensor_, grade_, v_, d_] := Module[{rules, assoc, signTweak, lm},
  rules = SymmetrizedArrayRules[tensor];
  
  If[
    allZerosL[Map[Last, rules]],
    {Table[0, Binomial[d, grade]], grade, v},
    assoc = Association[rules];
    signTweak = If[eaIsCo[{{}, v, grade, d}] && Mod[grade(d - grade), 2] == 1, -1, 1];
    lm = signTweak * Map[If[KeyExistsQ[assoc, #], assoc[#], 0]&, eaIndices[d, grade]];
    
    {lm, grade, v}
  ]
];


(* CONVERSION TO AND FROM MATRIX *)

nilovectorToA[{lm_, grade_, v_, d_}] := {{Table[0, d]}, v};

monovectorToA[u_] := {{eaGetLm[u]}, eaGetV[u]};

mmToM[mm_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mm];
  flattenedTensorA = hnf[Flatten[uToTensor[mm], grade - 2]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {Take[flattenedTensorA, grade], eaGetV[mm]}
  ]
];

mcToC[mc_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mc];
  flattenedTensorA = hnf[reverseEachRow[Flatten[uToTensor[mc], grade - 2]]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {antiTranspose[Take[flattenedTensorA, grade]], eaGetV[mc]}
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
  u2 = If[eaGetV[u2input] != eaGetV[u1], eaDual[u2input], eaCanonicalForm[u2input]];
  
  If[
    eaGetR[u1] != eaGetR[u2] || eaGetD[u1] != eaGetD[u2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetLm[u1] + eaGetLm[u2], eaGetGrade[u1], eaGetV[u1]}],
      eaCanonicalForm[{eaGetLm[u1] - eaGetLm[u2], eaGetGrade[u1], eaGetV[u1]}]
    ]
  ]
];
