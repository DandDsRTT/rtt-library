smithMultivectorToMatrix[u_] := Module[{grade, t},
  grade = eaGetGrade[u];
  t = If[
    grade == 0,
    nilovectorToA[u],
    If[
      grade == 1,
      monovectorToA[u],
      If[
        eaIsContra[u],
        smithMulticommaToC[u],
        smithMultimapToM[u]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalForm[t]]
];

smithMultimapToM[mm_] := Module[{largestMinorsL, grade, d, genesC, genesB, indexedLargestMinorsL, colIndices, bigMatrix},
  largestMinorsL = eaGetLargestMinorsL[mm];
  grade = eaGetGrade[mm];
  d = eaGetD[mm];
  
  genesC = eaIndices[d, grade - 1];
  genesB = eaIndices[d, grade];
  
  indexedLargestMinorsL = Association[];
  MapThread[(indexedLargestMinorsL[#1] = #2)&, {genesB, largestMinorsL}];
  
  colIndices = Range[d];
  
  bigMatrix = hnf[Map[findRowForElOfC[#, indexedLargestMinorsL, colIndices]&, genesC]];
  
  If[
    MatrixRank[bigMatrix] != grade,
    Error,
    {Take[bigMatrix, grade], "co"}
  ]
];

smithMulticommaToC[mc_] := Module[{grade, dualMm, dualMmGrade, m, c},
  grade = eaGetGrade[mc];
  dualMm = eaDual[mc];
  dualMmGrade = eaGetGrade[dualMm];
  m = If[dualMmGrade == 0, {{Table[0, grade]}, "co"}, smithMultimapToM[dualMm]];
  c = dual[m];
  
  c
];


findRowForElOfC[genesCEl_, indexedLargestMinorsL_, colIndices_] := Module[{appendedUnsortedIndices, signsAndSortedIndices, signs, sortedIndices, largestMinorsL},
  appendedUnsortedIndices = Map[Join[genesCEl, {#}]&, colIndices];
  
  signsAndSortedIndices = Map[findSignsAndSortedIndices, appendedUnsortedIndices];
  signs = Map[First, signsAndSortedIndices];
  sortedIndices = Map[Last, signsAndSortedIndices];
  
  largestMinorsL = Map[indexedLargestMinorsL[#]&, sortedIndices];
  
  MapThread[Times, {largestMinorsL, signs}]
];

findSignsAndSortedIndices[unsortedIndices_] := Module[{sortedIndicesAndSwapCount, sortedIndices, swapCount},
  sortedIndicesAndSwapCount = sortIndicesAndCountSwaps[unsortedIndices];
  sortedIndices = First[sortedIndicesAndSwapCount];
  swapCount = Last[sortedIndicesAndSwapCount];
  
  If[
    DuplicateFreeQ[unsortedIndices],
    If[
      EvenQ[swapCount],
      {1, sortedIndices},
      {-1, sortedIndices}
    ],
    {0, sortedIndices}
  ]
];

sortIndicesAndCountSwaps[inputUnsortedIndices_] := Module[{swapCount, indices},
  swapCount = 0;
  indices = inputUnsortedIndices;
  
  While[
    indices != Sort[indices],
    For[i = 1, i < Length[indices], i++,
      el = indices[[i]];
      nextEl = indices[[i + 1]];
      If[el > nextEl,
        indices[[i]] = nextEl;
        indices[[i + 1]] = el;
        swapCount = swapCount + 1
      ]
    ]
  ];
  
  {indices, swapCount}
];
