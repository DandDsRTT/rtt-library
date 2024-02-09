(* MULTIVECTOR UTILITIES *)

eaGetD[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableD[u]
];

eaGetR[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableR[u]
];

eaGetN[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableN[u]
];


(* CANONICALIZATION *)

eaCanonicalForm[u_] := If[
  allZerosL[eaGetLargestMinorsL[u]],
  u,
  If[
    isNondecomposable[u],
    Error,
    decomposableEaCanonicalForm[u]
  ]
];


(* DUAL *)

eaDual[u_] := If[
  isNondecomposable[u],
  Error,
  decomposableEaDual[u]
];


(* CONVERSION TO AND FROM MATRIX *)

multivectorToMatrix[u_] := Module[{grade, t},
  grade = eaGetGrade[u];
  t = If[
    grade == 0,
    nilovectorToA[u],
    If[
      grade == 1,
      monovectorToA[u],
      If[
        eaIsCols[u],
        mcToC[u],
        mmToM[u]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalFormPrivate[t]]
];

matrixToMultivector[t_] := eaCanonicalForm[
  If[
    isCols[t],
    {getLargestMinorsL[getA[t]], getNPrivate[t], getVariance[t], getDPrivate[t]},
    {getLargestMinorsL[getA[t]], getRPrivate[t], getVariance[t], getDPrivate[t]}
  ]
];


(* MERGE *)

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

regressiveProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[eaDual[u1], eaDual[u2]];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];

interiorProduct[u1_, u2_] := If[
  eaGetGrade[u1] >= eaGetGrade[u2],
  rightInteriorProduct[u1, u2],
  leftInteriorProduct[u1, u2]
];




(* ___ PRIVATE ___ *)



(* MULTIVECTOR UTILITIES *)

eaIsCols[u_] := MemberQ[{
  "contra",
  "contravector",
  "contravectors",
  "multicontravector",
  "contravariant",
  "v",
  "vector",
  "vectors",
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
  "mc",
  "col",
  "cols"
}, eaGetVariance[u]];
eaIsRows[u_] := MemberQ[{
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
  "mm",
  "row",
  "rows"
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
  eaIsRows[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableN[u]
];
eaGetDecomposableN[u_] := If[
  eaIsCols[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableR[u]
];

eaIndices[d_, grade_] := Subsets[Range[d], {grade}];

isNondecomposable[variance_] := multivectorToMatrix[variance] === Error;

eaGetLargestMinorsL[u_] := Part[u, 1];
eaGetGrade[u_] := Part[u, 2];
eaGetVariance[u_] := Part[u, 3];


(* CANONICALIZATION *)

decomposableEaCanonicalForm[u_] := Module[{largestMinorsL, grade, variance, normalizer},
  grade = eaGetGrade[u];
  variance = eaGetVariance[u];
  largestMinorsL = divideOutGcd[eaGetLargestMinorsL[u]];
  normalizer = If[
    (eaIsRows[u] && leadingEntry[largestMinorsL] < 0) || (eaIsCols[u] && trailingEntry[largestMinorsL] < 0),
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
  eaIsRows[u],
  "col",
  "row"
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
    signTweak = If[eaIsRows[{{}, variance, grade, d}] && Mod[grade(d - grade), 2] == 1, -1, 1];
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
  flattenedTensorA = hnf[reverseInnerL[Flatten[uToTensor[mc], grade - 2]]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {rotate180[Take[flattenedTensorA, grade]], eaGetVariance[mc]}
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
