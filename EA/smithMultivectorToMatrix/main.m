smithMultivectorToMatrix[u_] := Module[{grade, t},
  grade = eaGetGrade[u];
  t = If[
    grade == 0,
    nilovectorToMatrix[u],
    If[
      grade == 1,
      monovectorToMatrix[u],
      If[
        eaIsContra[u],
        smithMulticommaToC[u],
        smithMultimapToM[u]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalForm[t]]
];

smithMultimapToM[u_] := Module[{lm, grade, d, genesC, genesB, indexedLm, colIndices, bigMatrix},
  lm = eaGetLm[u];
  grade = eaGetGrade[u];
  d = eaGetD[u];
  
  genesC = eaIndices[d, grade - 1];
  genesB = eaIndices[d, grade];
  
  indexedLm = Association[];
  MapThread[(indexedLm[#1] = #2)&, {genesB, lm}];
  
  colIndices = Range[d];
  
  bigMatrix = hnf[Map[findRowForElOfC[#, indexedLm, colIndices]&, genesC]];
  
  If[
    MatrixRank[bigMatrix] != grade,
    Error,
    {Take[bigMatrix, grade], "co"}
  ]
];

smithMulticommaToC[u_] := Module[{grade, dualU, dualGrade, t},
  grade = eaGetGrade[u];
  dualU = eaDual[u];
  dualGrade = eaGetGrade[dualU];
  t = If[dualGrade == 0, {{Table[0, grade]}, "co"}, smithMultimapToM[dualU]];
  
  dual[t]
];


findRowForElOfC[genesCEl_, indexedLm_, colIndices_] := Module[{appendedUnsortedIndices, signsAndSortedIndices, signs, sortedIndices, lm},
  appendedUnsortedIndices = Map[Join[genesCEl, {#}]&, colIndices];
  
  signsAndSortedIndices = Map[findSignsAndSortedIndices, appendedUnsortedIndices];
  signs = Map[First, signsAndSortedIndices];
  sortedIndices = Map[Last, signsAndSortedIndices];
  
  lm = Map[indexedLm[#]&, sortedIndices];
  
  MapThread[Times, {lm, signs}]
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
