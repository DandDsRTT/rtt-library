(* ADDITION *)

sum[unparsedT_] := formatOutput[sumPrivate[parseTemperamentData[unparsedT]]];
sumPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    t1,
    addition[t1, t2, True]
  ]
];

diff[unparsedT_] := formatOutput[diffPrivate[parseTemperamentData[unparsedT]]];
diffPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    Error,
    addition[t1, t2, False]
  ]
];




(* ___ PRIVATE ___ *)



addition[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2] || intervalBasesDoNotMatch[t1, t2],
  Error,
  Module[{linearDependenceBasis},
    linearDependenceBasis = getLinearDependenceBasis[t1, t2];
    
    If[
      linearDependenceBasis === Error, (* not addable *)
      Error,
      addableAddition[t1, t2, linearDependenceBasis, isSum]
    ]
  ]
];

addableAddition[t1_, t2_, linearDependenceBasis_, isSum_] := Module[
  {
    t1LinearIndependenceBasisVector,
    t2LinearIndependenceBasisVector,
    t1t2LinearIndependenceBasisVector
  },
  
  t1LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t1, linearDependenceBasis];
  t2LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t2, linearDependenceBasis];
  
  t1t2LinearIndependenceBasisVector = If[
    isSum,
    t1LinearIndependenceBasisVector + t2LinearIndependenceBasisVector,
    t1LinearIndependenceBasisVector - t2LinearIndependenceBasisVector
  ];
  
  canonicalFormPrivate[{Join[linearDependenceBasis, {t1t2LinearIndependenceBasisVector}], getVariance[t1]}]
];

getLinearIndependenceBasisVector[t_, linearDependenceBasis_] := Module[{a, linearIndependenceBasisVector},
  a = addabilizationDefactor[t, linearDependenceBasis];
  linearIndependenceBasisVector = Last[a];
  If[isNegative[a, isCols[t]], linearIndependenceBasisVector = -linearIndependenceBasisVector];
  
  linearIndependenceBasisVector
];

addabilizationDefactor[t_, linearDependenceBasis_] := Module[
  {
    grade,
    explicitLinearDependenceBasisFormOfA
  },
  
  grade = getGrade[t];
  explicitLinearDependenceBasisFormOfA = getInitialExplicitLinearDependenceBasisFormOfA[t, linearDependenceBasis, grade];
  
  If[
    isLinearlyDependent[linearDependenceBasis],
    addabilizationDefactorWithNonemptyLinearDependenceBasis[t, linearDependenceBasis, grade, explicitLinearDependenceBasisFormOfA],
    explicitLinearDependenceBasisFormOfA
  ]
];

addabilizationDefactorWithNonemptyLinearDependenceBasis[t_, linearDependenceBasis_, grade_, explicitLdbFormOfAInput_] := Module[
  {
    explicitLinearDependenceBasisFormOfA,
    d,
    linearDependence,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  
  explicitLinearDependenceBasisFormOfA = explicitLdbFormOfAInput;
  d = getDPrivate[t];
  linearDependence = getLinearDependence[linearDependenceBasis];
  enfactoring = getGreatestFactor[explicitLinearDependenceBasisFormOfA];
  
  multiples = Table[Subscript[x, index], {index, linearDependence}];
  equations = Map[
    Function[
      dIndex,
      Mod[explicitLinearDependenceBasisFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * linearDependenceBasis[[multiplesIndex]][[dIndex]]],
        Range[linearDependence]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  explicitLinearDependenceBasisFormOfA[[grade]] = divideOutGcd[explicitLinearDependenceBasisFormOfA[[grade]] + getLinearDependenceBasisLinearCombination[linearDependenceBasis, result]];
  
  explicitLinearDependenceBasisFormOfA
];

variancesMatch[t1_, t2_] := getVariance[t1] == getVariance[t2];

getLinearDependenceBasis[t1_, t2_] := Module[{linearDependenceBasis},
  linearDependenceBasis = removeAllZeroRows[getA[dualPrivate[
    If[
      isCols[t1],
      mapMergePrivate[t1, t2],
      commaMergePrivate[t1, t2]
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

dimensionsDoNotMatch[t1_, t2_] := getRPrivate[t1] != getRPrivate[t2] || getDPrivate[t1] != getDPrivate[t2];

intervalBasesDoNotMatch[t1_, t2_] := getDomainBasis[t1] != getDomainBasis[t2];

getGrade[t_] := If[isCols[t], getNPrivate[t], getRPrivate[t]];

isLinearlyDependent[linearDependenceBasis_] := getLinearDependence[linearDependenceBasis] > 0;

getInitialExplicitLinearDependenceBasisFormOfA[t_, linearDependenceBasis_, grade_] := Module[
  {
    linearIndependenceBasisSource,
    explicitLinearDependenceBasisFormOfA
  },
  
  linearIndependenceBasisSource = getA[If[isCols[t], getC[t], getM[t]]];
  explicitLinearDependenceBasisFormOfA = linearDependenceBasis;
  
  Do[
    candidate = hnf[Join[linearDependenceBasis, {candidateLinearIndependenceBasisVector}]];
    If[
      Length[explicitLinearDependenceBasisFormOfA] < grade && MatrixRank[candidate] > Length[linearDependenceBasis],
      explicitLinearDependenceBasisFormOfA = Join[explicitLinearDependenceBasisFormOfA, {candidateLinearIndependenceBasisVector}]
    ],
    {candidateLinearIndependenceBasisVector, linearIndependenceBasisSource}
  ];
  Take[explicitLinearDependenceBasisFormOfA, grade]
];

getGreatestFactor[a_] := Det[getGreatestFactorA[a]];

getGreatestFactorA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getLinearDependenceBasisLinearCombination[linearDependenceBasis_, linearDependenceBasisMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {linearDependenceBasis, linearDependenceBasisMultiplePermutation}
]];

isNegative[a_, isContravariant_] := Module[{largestMinorsL, entryFn, normalizingEntry},
  largestMinorsL = getLargestMinorsL[a];
  entryFn = If[isContravariant, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[largestMinorsL];
  
  normalizingEntry < 0
];
