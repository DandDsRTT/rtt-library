smithMultivectorToMatrix[w_] := Module[{grade, t},
  grade = eaGetGrade[w];
  t = If[
    grade == 0,
    nilovectorToMatrix[w],
    If[
      grade == 1,
      monovectorToMatrix[w],
      If[
        eaIsContra[w],
        smithMulticommaToC[w],
        smithMultimapToM[w],
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalForm[t]]
];

smithMultimapToM[w_] := Module[{lm, grade, d, genesC, genesB, indexedLm, colIndices, bigMatrix},
  lm = eaGetLm[w];
  grade = eaGetGrade[w];
  d = eaGetD[w];
  
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

smithMulticommaToC[w_] := Module[{grade, dualW, dualGrade, t},
  grade = eaGetGrade[w];
  dualW = eaDual[w];
  dualGrade = eaGetGrade[dualW];
  t = If[dualGrade == 0, {{Table[0, grade]}, "co"}, smithMultimapToM[dualW]];
  
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
