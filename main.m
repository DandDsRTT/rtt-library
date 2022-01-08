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
canonicalForm[t_] := Module[{b, canonicalT},
  canonicalT = If[
    isContra[t],
    {canonicalC[getA[t]], getV[t]},
    {canonicalM[getA[t]], getV[t]}
  ];
  b = getB[t];
  
  If[
    isStandardPrimeLimitB[b],
    canonicalT,
    Join[canonicalT, {b}]
  ]
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
  isStandardPrimeLimitB[getB[t]],
  If[
    isContra[t],
    {antiNullSpaceBasis[getA[t]], "co"},
    {nullSpaceBasis[getA[t]], "contra"}
  ],
  If[
    isContra[t],
    {antiNullSpaceBasis[getA[t]], "co", getB[t]},
    {nullSpaceBasis[getA[t]], "contra", getB[t]}
  ] (* TODO: break this down *)
];


(* MERGE *)

(*
  
  mapMerge[t1, t2, t3...]
  
  Merges the given temperaments' maps:
  concatenates their mappings
  and puts the result into canonical form.
  
  Can accept any number of temperaments representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a mapping.
  
  Examples:
  
  In    et5M = {{{5, 8, 12}}, "co"};
        et7M = {{{7, 11, 16}}, "co"};
        mapMerge[et5M, et7M]
    
  Out   {{{1, 0, -4}, {0, 1, 4}}, "co"};
  
  In    et7dM = {{{7, 11, 16, 19}}, "co"};
        et12M = {{{12, 19, 28, 34}}, "co"};
        et22M = {{{22, 35, 51, 62}}, "co"};
        mapMerge[et7dM, et12M, et22M]
    
  Out   {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
  
*)
mapMerge[tl___] := Module[{bl, intersectedB, tlWithIntersectedB},
  bl = Map[getB, {tl}];
  intersectedB = Apply[bIntersection, bl];
  tlWithIntersectedB = Map[changeBforM[#, intersectedB]&, {tl}];
  
  canonicalForm[{Apply[Join, Map[getM, tlWithIntersectedB]], "co", intersectedB}]
];

(*
  
  commaMerge[t1, t2, t3...]
  
  Merges the given temperaments' comma bases:
  concatenates their comma bases
  and puts the result into canonical form.
  
  Can accept any number of temperament representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a comma basis.
  
  In    meantoneC = {{{4, -4, 1}}, "contra"};
        porcupineC = {{{1, -5, 3}}, "contra"};
        commaMerge[meantoneC, porcupineC]
    
  Out   {{{-11, 7, 0}, {-7, 3, 1}}, "contra"}
  
  In    mintC = {{{2, 2, -1, -1}}, "contra"};
        meantoneC = {{{4, -4, 1, 0}}, "contra"};
        negriC = {{{-14, 3, 4, 0}}, "contra"};
        commaMerge[mintC, meantoneC, negriC]
    
  Out   {{{30, 19, 0, 0}, {-26, 15, 1, 0}, {-6, 2, 0, 1}}, "contra"}
  
*)
commaMerge[tl___] := Module[{bl, mergedB, tlWithMergedB},
  bl = Map[getB, {tl}];
  mergedB = Apply[bMerge, bl];
  tlWithMergedB = Map[changeBforC[#, mergedB]&, {tl}];
  
  canonicalForm[{Apply[Join, Map[getC, tlWithMergedB]], "contra", mergedB}]
];



(* INTERVAL BASIS *)

(*
*)
changeB[t_, targetB_] := Module[{}, ""]; (* TODO: make a mix of changeBforM and changeBforC *)


(* ARITHMETIC *)

(*
  
  sum[t1, t2]
  
  Sums the given temperaments: if they have the same dimensions
  (same dimensionality, rank (and nullity)),
  and are addable (can be put into a form where
  they are identical except for a single basis vector (or covector, if covariant)),
  entry-wise sums this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
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
  t2 = If[vMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];
  
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
  entry-wise diffs this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
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
  t2 = If[vMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];
  
  If[
    t1 == t2,
    Error,
    arithmetic[t1, t2, False]
  ]
];




(* ___ PRIVATE ___ *)



(* LIST UTILITIES *)

getGcf[l_] := Apply[GCD, l];
divideOutGcf[l_] := Module[{gcf}, gcf = getGcf[l]; If[gcf == 0, l, l / gcf]];
multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

leadingEntry[l_] := First[Select[l, # != 0&, 1]];
trailingEntry[l_] := leadingEntry[Reverse[l]];

allZerosL[l_] := AllTrue[l, # == 0&];


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

getA[t_] := Part[t, 1];
getV[t_] := Part[t, 2];

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


(* MERGE *)

getM[t_] := If[isCo[t] == True, getA[t], noncanonicalAntiNullSpaceBasis[getA[t]]];
getC[t_] := If[isContra[t] == True, getA[t], noncanonicalNullSpaceBasis[getA[t]]];


(* INTERVAL BASIS *)

bMerge[bl___] := Module[{concatedB, factorizedConcatedB},
  concatedB = Apply[Join, {bl}];
  factorizedConcatedB = padD[Map[rationalToI, concatedB], getDforB[concatedB]];
  
  canonicalB[Map[iToRational, factorizedConcatedB]]
];

bIntersection[bl___] := Module[{intersectedB},
  intersectedB = First[{bl}];
  
  Do[
    intersectedB = crazytownBIntersection[intersectedB, b],
    {b, Drop[{bl}, 1]}
  ];
  
  canonicalB[intersectedB]
];

defactorB[b_] := Module[{thing, thing2},
  thing = padD[Map[rationalToI, b], getDforB[b]];
  thing2 = antiTranspose[removeAllZeroRows[hnf[colHermiteDefactor[antiTranspose[thing]]]]];
  (*Print["thing: ", thing, " thing2: ", thing2, "wtf", Map[rationalToI, b]];*)
  
  If[
    Length[thing2] == 0,
    {1},
    Map[super, Map[iToRational, thing2 ]]
  ]
];


bIntersectionBinary[b1_, b2_] := Module[{mergedB, b1InMergedB, b2InMergedB, dualOfB1, dualOfB2, actualMerge, d, factorizedActualMerge, dualOfMerged, dualOfMerged2, gretestFactorA1, greatestFactorA2, enfactoringsPerPrime, enfactoringPerPrime, primeIndex, currentGreatestFactorThing, basisElementIndex, dualOfMergedWithEnfactoringApplied, dualOfMergedWithEnfactoringAppliedEntry, appliedEnfactoring},
  
  (*If[
   b1 == b2, (* TODO: obviously this shoudl be handled sooner, and this whole thing is a mess *)
   b1,*)
  
  (*first take the duals, but in the d of their merge *)
  (* mergedB = getBasisElements[bMerge[b1, b2]]; (*now this is more like a standard space than a merged space, right?*)*)
  mergedB = getPrimes[getDforB[bMerge[b1, b2]]];
  
  (*mergedB = bMerge[b1, b2]; *)
  (*  Print["mergedB: ", mergedB, " for ", b1, " and " , b2];(*, " versus before defactor ", bMerge[b1, b2]];*)*)
  
  b1InMergedB = Transpose[getRforC[b1, mergedB]]; (* TODO: if this works, this is obviously not great, but maybe there's something both getRforC and this can share...? *)
  b2InMergedB = Transpose[getRforC[b2, mergedB]];
  (*  Print["b1 and b2 converted to the merged basis, or something: ", b1InMergedB,", ", b2InMergedB];*)
  
  dualOfB1 = Map[iToRational, antiNullSpaceBasis[b1InMergedB]];
  dualOfB2 = Map[iToRational, antiNullSpaceBasis[b2InMergedB]];
  (*  Print["their duals: ", dualOfB1, ", ", dualOfB2];*)
  
  (* then merge those *)
  actualMerge = bMerge[dualOfB1, dualOfB2];
  (*  Print["their actualMerge: ", actualMerge];*)
  
  If[
    actualMerge == {1},
    bIntersectionBinaryOld[b1, b2],
    
    (* then dual of result *)
    d = getDforB[mergedB]; (* used to be actualMerge *)
    factorizedActualMerge = padD[Map[rationalToI, actualMerge], d];
    (*Print["factorizedActualMerge: ", factorizedActualMerge, " and d: ", d];*)
    dualOfMerged = nullSpaceBasis[factorizedActualMerge];
    (*  If[
      allZeros[dualOfMerged],
      dualOfMerged = Table[1, Length[dualOfMerged[[1]]]]
      ];*)
    (*  Print["okay dualOfMerged: ", dualOfMerged,allZeros[dualOfMerged]];*)
    
    
    greatestFactorA1 = colMaxes[b1InMergedB];(*Diagonal[getGreatestFactorA[b1InMergedB]]; (* transpose?! no i don't think so*)*)
    greatestFactorA2 = colMaxes[b2InMergedB];(*Diagonal[getGreatestFactorA[b2InMergedB]]; (* note this sisnt a greatest factor... *)*)
    (*  Print["and the enfactoring matrices are: ", greatestFactorA1, ", ", greatestFactorA2];(*, " but would hav been ", getGreatestFactorA[b1InMergedB], " and ", getGreatestFactorA[b2InMergedB]];*)*)
    (*
    enfactoringsPerPrime = Table[1, Length[mergedB] ];
    (*Print["what, uh, ", mergedB];*)
    (*Print["it appares about to iterat over this thing and it has 3 elem?", b1InMergedB];*)
    Do[
      (*enfactoringPerPrime = 1;*)
      (*Print["outermost loop, doing mergedF ", mergedF, " of mergedB ", mergedB];*)
      
      primeIndex = 1;
      Do[
        currentGreatestFactorThing = greatestFactorA1[[primeIndex]];
        (*Print["or is it you",currentGreatestFactorThing];*)
        
        basisElementIndex = 1;
        Do[
          If[
            Abs[basisElement] != 0,
            enfactoringsPerPrime[[basisElementIndex]] = LCM[enfactoringsPerPrime[[basisElementIndex]], currentGreatestFactorThing]
          ];
          basisElementIndex += 1,
          {basisElement, b1f}
        ];
        
        primeIndex += 1,
        
        {b1f, b1InMergedB}
      ];
      
      primeIndex = 1;
      Do[
        currentGreatestFactorThing = greatestFactorA2[[primeIndex]];
       (* Print["or is it you2",currentGreatestFactorThing];*)
        
        basisElementIndex = 1;
        Do[
          If[
            Abs[basisElement] != 0,
            enfactoringsPerPrime[[basisElementIndex]] = LCM[enfactoringsPerPrime[[basisElementIndex]], currentGreatestFactorThing]
          ];
          basisElementIndex += 1,
          {basisElement, b2f}
        ];
        
        primeIndex += 1,
        
        {b2f, b2InMergedB}
      ],
      (*enfactoringsPerPrime = Join[enfactoringsPerPrime, {enfactoringPerPrime}],*)
      {mergedF, mergedB}
    ];
    *)
    enfactoringsPerPrime = colLcms[Join[{Map[Max[#, 1]&, greatestFactorA1]}, { Map[Max[#, 1]&, greatestFactorA2]}]];
    (*enfactoringsPerPrime = Map[Max[#,1]&, enfactoringsPerPrime];*)
    (*  Print["enfactoringsPerPrime: ", enfactoringsPerPrime];*)
    
    dualOfMergedWithEnfactoringApplied = {};
    Do[
      appliedEnfactoring = 1;
      primeIndex = 1;
      Do[
        If[
          Abs[dualOfMergedEntryEntry] != 0,
          appliedEnfactoring = LCM[appliedEnfactoring, enfactoringsPerPrime[[primeIndex]]] / dualOfMergedEntryEntry (* this is bad *)
        ];
        primeIndex += 1,
        {dualOfMergedEntryEntry, dualOfMergedEntry}
      ];
      dualOfMergedWithEnfactoringAppliedEntry = appliedEnfactoring * dualOfMergedEntry;
      dualOfMergedWithEnfactoringApplied = Join[dualOfMergedWithEnfactoringApplied, {dualOfMergedWithEnfactoringAppliedEntry}],
      {dualOfMergedEntry, dualOfMerged}
    ];
    
    (*  Print["dualOfMergedWithEnfactoringApplied: ", dualOfMergedWithEnfactoringApplied];*)
    
    canonicalB[Map[iToRational, dualOfMergedWithEnfactoringApplied]]
    (* ]*)
  ]
];

isSubspaceOf[candidateSubspaceB_, candidateSuperspaceB_] := bMerge[candidateSubspaceB, candidateSuperspaceB] == candidateSuperspaceB;

canonicalB[b_] := Module[{thing, thing2}, (*TODO: obviously DRY up with defactorB *)
  thing = padD[Map[rationalToI, b], getDforB[b]];
  thing2 = antiTranspose[removeAllZeroRows[hnf[antiTranspose[thing]]]];
  (*Print["thing: ", thing, " thing2: ", thing2, "wtf", Map[rationalToI, b]];*)
  
  If[
    Length[thing2] == 0,
    {1},
    Map[super, Map[iToRational, thing2 ]]
  ]
];

changeBforM[m_, targetSubspaceB_] := If[
  getB[m] == targetSubspaceB,
  m,
  If[
    isSubspaceOf[getB[m], targetSubspaceB],
    Error,
    canonicalForm[{getA[m].getRforM[getB[m], targetSubspaceB], "co", targetSubspaceB}]
  ]
];

changeBforC[c_, targetSuperspaceB_] := If[
  getB[c] == targetSuperspaceB,
  c,
  If[
    isSubspaceOf[getB[c], targetSuperspaceB],
    canonicalForm[{Transpose[getRforC[getB[c], targetSuperspaceB].Transpose[getA[c]]], "contra", targetSuperspaceB}],
    Error
  ]
];

(* express the target formal primes in terms of the initial formal primes*)
getRforM[originalSuperspaceB_, targetSubspaceB_] := Module[
  {
    d,
    factorizedTargetSubspaceB,
    factorizedOriginalSuperspaceB,
    r,
    rCol,
    rColEntry,
    remainingToBeFactorizedTargetSubspaceF
  },
  
  d = getDforB[Join[originalSuperspaceB, targetSubspaceB]];
  factorizedTargetSubspaceB = padD[Map[rationalToI, targetSubspaceB], d];
  factorizedOriginalSuperspaceB = padD[Map[rationalToI, originalSuperspaceB], d];
  
  r = {};
  
  Do[
    rCol = {};
    remainingToBeFactorizedTargetSubspaceF = factorizedTargetSubspaceF;
    Do[
      rColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry += 1;
        remainingToBeFactorizedTargetSubspaceF -= factorizedOriginalSuperspaceF
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceF += factorizedOriginalSuperspaceF
      ];
      
      rCol = Join[rCol, {rColEntry}],
      {factorizedOriginalSuperspaceF, factorizedOriginalSuperspaceB}
    ];
    r = Join[r, {rCol}],
    {factorizedTargetSubspaceF, factorizedTargetSubspaceB}
  ];
  
  Transpose[r] (* TODO: I don't think this should be transposed, and dealt with accordingly in the few places where it's used *)
];

(* yes, just swapping initial and target, that's all! *)
getRforC[originalSubspaceB_, targetSuperspaceB_] := getRforM[targetSuperspaceB, originalSubspaceB];

getPrimes[count_] := Map[Prime, Range[count]];

rationalToI[rational_] := Module[{factorization, greatestPrime, count, primes, i, currentPrimeIndex},
  factorization = FactorInteger[rational];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  i = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      i[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    i
  ]
];

iToRational[i_] := Module[{rational, primeIndex},
  rational = 1;
  primeIndex = 1;
  Do[
    rational = rational * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, i}
  ];
  
  rational
];

getDforB[b_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, b]]]]]]; (* TODO: whoa this too is based on standard basis, like if you skip primes and only have 4 but top one is 11 it'll be 5 or whatever *)

padD[a_, d_] := Map[PadRight[#, d]&, a];

super[rational_] := If[rational < 1, Denominator[rational] / Numerator[rational], rational];

getStandardPrimeLimitB[t_] := getPrimes[getD[t]];

isStandardPrimeLimitB[b_] := canonicalB[b] == getPrimes[Length[b]];

getB[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitB[t]
];

signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

(*TODO: DRY this up with isDenominatorFactor *)
isNumeratorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF - factorizedSuperspaceF}], False];

isDenominatorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF + factorizedSuperspaceF}], False];


(* ARITHMETIC *)

arithmetic[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2],
  Error,
  Module[{ldb, tSumAndDiff},
    ldb = getLdb[t1, t2];
    
    If[
      ldb === Error, (* not addable *)
      Error,
      addableArithmetic[t1, t2, ldb, isSum]
    ]
  ]
];

addableArithmetic[t1_, t2_, ldb_, isSum_] := Module[
  {
    t1LibVector,
    t2LibVector,
    t1t2libVector
  },
  
  t1LibVector = getLibVector[t1, ldb];
  t2LibVector = getLibVector[t2, ldb];
  
  t1t2libVector = If[
    isSum,
    t1LibVector + t2LibVector,
    t1LibVector - t2LibVector
  ];
  
  canonicalForm[{Join[ldb, {t1t2libVector}], getV[t1]}]
];

getLibVector[t_, ldb_] := Module[{a, libVector},
  a = addabilizationDefactor[t, ldb];
  libVector = Last[a];
  If[isNegative[a, isContra[t]], libVector = -libVector];
  
  libVector
];

addabilizationDefactor[t_, ldb_] := Module[
  {
    grade,
    explicitLdbFormOfA
  },
  
  grade = getGrade[t];
  explicitLdbFormOfA = getInitialExplicitLdbFormOfA[t, ldb, grade];
  
  If[
    isLd[ldb],
    addabilizationDefactorWithNonemptyLdb[t, ldb, grade, explicitLdbFormOfA],
    explicitLdbFormOfA
  ]
];

addabilizationDefactorWithNonemptyLdb[t_, ldb_, grade_, explicitLdbFormOfAInput_] := Module[
  {
    explicitLdbFormOfA,
    d,
    ld,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  
  explicitLdbFormOfA = explicitLdbFormOfAInput;
  d = getD[t];
  ld = getLd[ldb];
  enfactoring = getGreatestFactor[explicitLdbFormOfA];
  
  multiples = Table[Subscript[x, i], {i, ld}];
  equations = Map[
    Function[
      dIndex,
      Mod[explicitLdbFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * ldb[[multiplesIndex]][[dIndex]]],
        Range[ld]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  explicitLdbFormOfA[[grade]] = divideOutGcf[explicitLdbFormOfA[[grade]] + getLdbLinearCombination[ldb, result]];
  
  explicitLdbFormOfA
];

vMatch[t1_, t2_] := getV[t1] == getV[t2];

getLdb[t1_, t2_] := Module[{ldb},
  ldb = removeAllZeroRows[getA[dual[
    If[
      isContra[t1],
      mapMerge[t1, t2],
      commaMerge[t1, t2]
    ]
  ]]];
  
  If[
    isAddable[ldb, t1],
    ldb,
    Error
  ]
];

isAddable[ldb_, t_] := getLd[ldb] === getGrade[t] - 1;

getLd[ldb_] := Length[ldb];

dimensionsDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

getGrade[t_] := If[isContra[t], getN[t], getR[t]];

isLd[ldb_] := getLd[ldb] > 0;

getInitialExplicitLdbFormOfA[t_, ldb_, grade_] := Module[
  {
    libSource,
    explicitLdbFormOfA
  },
  
  libSource = If[isContra[t], getC[t], getM[t]];
  explicitLdbFormOfA = ldb;
  
  Do[
    candidate = hnf[Join[ldb, {candidateLibVector}]];
    If[
      Length[explicitLdbFormOfA] < grade && MatrixRank[candidate] > Length[ldb],
      explicitLdbFormOfA = Join[explicitLdbFormOfA, {candidateLibVector}]
    ],
    {candidateLibVector, libSource}
  ];
  Take[explicitLdbFormOfA, grade]
];

getGreatestFactor[a_] := Det[getGreatestFactorA[a]];

getGreatestFactorA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getLdbLinearCombination[ldb_, ldbMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {ldb, ldbMultiplePermutation}
]];

computeMinors[a_] := divideOutGcf[First[Minors[a, MatrixRank[a]]]];

isNegative[a_, contra_] := Module[{minors, entryFn, normalizingEntry},
  minors = computeMinors[a];
  entryFn = If[contra, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[minors];
  
  normalizingEntry < 0
];

getBasisElements[b_] := Module[{d, factorizedB, primes, result, index, primeIndex}, (*TODO: may not use this anymore *)
  d = getDforB[b];
  factorizedB = padD[Map[rationalToI, b], d]; (*TODO: need a helper for padD, I feel like i'm soidoing some repeptaive stuff*) (* TODO: oh, note that this is a standardBasisI! that's pretty important here, may be nice to be explicit about that in general *)
  primes = getPrimes[d];
  result = {};
  index = 1;
  (*Print["primes", primes, "d", d];*)
  
  Do[
    primeIndex = 1;
    Do[
      If[
        basisElement != 0,
        result = Join[result, {primes[[primeIndex]]}]
      ];
      primeIndex += 1,
      {basisElement, factorizedF}
    ],
    {factorizedF, factorizedB}
  ];
  
  DeleteDuplicates[result]
];

diagonalQ[mat_?MatrixQ] := With[
  {posns = Flatten[Map[Position[#, _?(# != 0&)]&, mat]]},
  Length[Union[posns]] == Length[posns]
];

d = 7;
pLimit = 17;

randomB[] := Module[{b, bCanonicalized, greatestFactorA},
  b = Table[
    RandomInteger[{1, pLimit}] / RandomInteger[{1, pLimit}],
    RandomInteger[{1, d}]
  ];
  
  bCanonicalized = canonicalB[b];
  greatestFactorA = getGreatestFactorA[padD[Map[rationalToI, bCanonicalized], getDforB[bCanonicalized]]];
  
  (*Print["b: ", b, " bCanonicalized: ", bCanonicalized, " greatestFactorA: ", greatestFactorA];*)
  
  If[
    !diagonalQ[greatestFactorA],
    Print[Style["BAAAAAAAAAD", 14, Red]];
    Print["b: ", b, " bCanonicalized: ", bCanonicalized, " greatestFactorA: ", greatestFactorA];
  ];
];

Do[
  randomB[],
  10000
];
Print["w000t"];

BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["2", "15"], ",", FractionBox["7", "4"], ",", FractionBox["13", "8"], ",", FractionBox["5", "3"], ",", FractionBox["9", "14"], ",", FractionBox["6", "11"], ",", FractionBox["17", "15"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "30", ",", "7", ",", "33", ",", "26", ",", "34"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[2, 15], Rational[7, 4], Rational[13, 8], Rational[5, 3], Rational[9, 14], Rational[6, 11], Rational[17, 15]}, " bCanonicalized: ", {4, 18, 30, 7, 33, 26, 34}, " greatestFactorA: ", {{2, 0, 0, 0, 0, 0, 0}, {1, 2, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["2", "9"], ",", "1", ",", FractionBox["9", "10"], ",", "4", ",", "1", ",", FractionBox["1", "17"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[2, 9], 1, Rational[9, 10], 4, 1, Rational[1, 17]}, " bCanonicalized: ", {4, 18, 5, 17}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["2", "11"], ",", FractionBox["15", "8"], ",", FractionBox["9", "10"], ",", "17", ",", FractionBox["11", "16"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "108", ",", "15", ",", "44", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"2", ",", "3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[2, 11], Rational[15, 8], Rational[9, 10], 17, Rational[11, 16]}, " bCanonicalized: ", {8, 108, 15, 44, 17}, " greatestFactorA: ", {{3, 0, 0, 0, 0}, {2, 3, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["10", "3"], ",", FractionBox["7", "15"], ",", FractionBox["9", "4"], ",", FractionBox["5", "12"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "60", ",", "28"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[10, 3], Rational[7, 15], Rational[9, 4], Rational[5, 12]}, " bCanonicalized: ", {8, 18, 60, 28}, " greatestFactorA: ", {{3, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["7", "8"], ",", "1", ",", FractionBox["9", "10"], ",", FractionBox["11", "5"], ",", FractionBox["1", "7"], ",", FractionBox["4", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "20", ",", "7", ",", "44"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[7, 8], 1, Rational[9, 10], Rational[11, 5], Rational[1, 7], Rational[4, 9]}, " bCanonicalized: ", {8, 18, 20, 7, 44}, " greatestFactorA: ", {{3, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["15", "13"], ",", FractionBox["8", "5"], ",", FractionBox["2", "5"], ",", FractionBox["5", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "10", ",", "39"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[15, 13], Rational[8, 5], Rational[2, 5], Rational[5, 9]}, " bCanonicalized: ", {4, 18, 10, 39}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["11", "7"], ",", FractionBox["9", "7"], ",", FractionBox["7", "2"], ",", FractionBox["11", "8"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "14", ",", "22"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[11, 7], Rational[9, 7], Rational[7, 2], Rational[11, 8]}, " bCanonicalized: ", {4, 18, 14, 22}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["11", "15"], ",", FractionBox["9", "2"], ",", FractionBox["17", "2"], ",", FractionBox["10", "17"], ",", FractionBox["8", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "66", ",", "34"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[11, 15], Rational[9, 2], Rational[17, 2], Rational[10, 17], Rational[8, 9]}, " bCanonicalized: ", {4, 18, 5, 66, 34}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["9", "11"], ",", FractionBox["11", "8"], ",", FractionBox["1", "4"], ",", FractionBox["8", "7"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "14", ",", "22"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[9, 11], Rational[11, 8], Rational[1, 4], Rational[8, 7]}, " bCanonicalized: ", {4, 18, 14, 22}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["1", "4"], ",", FractionBox["8", "9"], ",", FractionBox["3", "13"], ",", FractionBox["5", "12"], ",", FractionBox["8", "7"], ",", FractionBox["8", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "30", ",", "14", ",", "78"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[1, 4], Rational[8, 9], Rational[3, 13], Rational[5, 12], Rational[8, 7], Rational[8, 9]}, " bCanonicalized: ", {4, 18, 30, 14, 78}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["3", "11"], ",", FractionBox["9", "2"], ",", "16", ",", FractionBox["12", "7"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"16", ",", "72", ",", "42", ",", "264"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"4", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[3, 11], Rational[9, 2], 16, Rational[12, 7]}, " bCanonicalized: ", {16, 72, 42, 264}, " greatestFactorA: ", {{4, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["9", "4"], ",", FractionBox["1", "8"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[9, 4], Rational[1, 8]}, " bCanonicalized: ", {8, 18}, " greatestFactorA: ", {{3, 0}, {1, 2}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["9", "4"], ",", FractionBox["1", "8"], ",", "1", ",", FractionBox["5", "16"], ",", FractionBox["17", "3"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "20", ",", "102"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[9, 4], Rational[1, 8], 1, Rational[5, 16], Rational[17, 3]}, " bCanonicalized: ", {8, 18, 20, 102}, " greatestFactorA: ", {{3, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["5", "3"], ",", FractionBox["13", "5"], ",", FractionBox["8", "9"], ",", "4"}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "30", ",", "78"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[5, 3], Rational[13, 5], Rational[8, 9], 4}, " bCanonicalized: ", {4, 18, 30, 78}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["4", "5"], ",", "1", ",", FractionBox["7", "16"], ",", FractionBox["10", "9"], ",", FractionBox["7", "4"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "7"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[4, 5], 1, Rational[7, 16], Rational[10, 9], Rational[7, 4]}, " bCanonicalized: ", {4, 18, 5, 7}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["8", "5"], ",", FractionBox["9", "8"], ",", FractionBox["2", "5"], ",", FractionBox["1", "17"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "10", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[8, 5], Rational[9, 8], Rational[2, 5], Rational[1, 17]}, " bCanonicalized: ", {4, 18, 10, 17}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["13", "16"], ",", FractionBox["2", "9"], ",", FractionBox["6", "11"], ",", FractionBox["13", "4"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "33", ",", "13"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[13, 16], Rational[2, 9], Rational[6, 11], Rational[13, 4]}, " bCanonicalized: ", {4, 18, 33, 13}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["2", "9"], ",", FractionBox["8", "5"], ",", FractionBox["5", "8"], ",", FractionBox["5", "2"], ",", FractionBox["2", "5"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "10"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[2, 9], Rational[8, 5], Rational[5, 8], Rational[5, 2], Rational[2, 5]}, " bCanonicalized: ", {4, 18, 10}, " greatestFactorA: ", {{2, 0, 0}, {1, 2, 0}, {0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"13", ",", FractionBox["7", "11"], ",", FractionBox["15", "2"], ",", FractionBox["9", "10"], ",", FractionBox["13", "16"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"16", ",", "108", ",", "120", ",", FractionBox["11", "7"], ",", "13"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"4", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"2", ",", "3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {13, Rational[7, 11], Rational[15, 2], Rational[9, 10], Rational[13, 16]}, " bCanonicalized: ", {16, 108, 120, Rational[11, 7], 13}, " greatestFactorA: ", {{4, 0, 0, 0, 0}, {2, 3, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["16", "5"], ",", FractionBox["1", "13"], ",", FractionBox["9", "4"], ",", FractionBox["10", "13"], ",", FractionBox["10", "17"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"32", ",", "72", ",", "10", ",", "13", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"5", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[16, 5], Rational[1, 13], Rational[9, 4], Rational[10, 13], Rational[10, 17]}, " bCanonicalized: ", {32, 72, 10, 13, 17}, " greatestFactorA: ", {{5, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["17", "13"], ",", FractionBox["7", "5"], ",", FractionBox["1", "4"], ",", FractionBox["11", "10"], ",", "14", ",", FractionBox["3", "17"], ",", FractionBox["2", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "10", ",", "14", ",", "11", ",", "78", ",", "102"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[17, 13], Rational[7, 5], Rational[1, 4], Rational[11, 10], 14, Rational[3, 17], Rational[2, 9]}, " bCanonicalized: ", {4, 18, 10, 14, 11, 78, 102}, " greatestFactorA: ", {{2, 0, 0, 0, 0, 0, 0}, {1, 2, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["17", "5"], ",", FractionBox["8", "15"], ",", FractionBox["5", "6"], ",", "1", ",", "7", ",", FractionBox["8", "7"], ",", FractionBox["13", "12"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "15", ",", "7", ",", "156", ",", "51"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[17, 5], Rational[8, 15], Rational[5, 6], 1, 7, Rational[8, 7], Rational[13, 12]}, " bCanonicalized: ", {8, 18, 15, 7, 156, 51}, " greatestFactorA: ", {{3, 0, 0, 0, 0, 0}, {1, 2, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["13", "11"], ",", "1", ",", FractionBox["17", "16"], ",", FractionBox["9", "2"], ",", "16"}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"16", ",", "72", ",", FractionBox["13", "11"], ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"4", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[13, 11], 1, Rational[17, 16], Rational[9, 2], 16}, " bCanonicalized: ", {16, 72, Rational[13, 11], 17}, " greatestFactorA: ", {{4, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["7", "8"], ",", FractionBox["2", "7"], ",", FractionBox["8", "9"], ",", FractionBox["7", "9"], ",", FractionBox["15", "11"], ",", FractionBox["16", "13"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "14", ",", FractionBox["66", "5"], ",", "13"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[7, 8], Rational[2, 7], Rational[8, 9], Rational[7, 9], Rational[15, 11], Rational[16, 13]}, " bCanonicalized: ", {4, 18, 14, Rational[66, 5], 13}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", FractionBox["9", "8"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2"}], "}"}]}], "}"}]}], SequenceForm["b: ", {4, Rational[9, 8]}, " bCanonicalized: ", {4, 18}, " greatestFactorA: ", {{2, 0}, {1, 2}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["1", "10"], ",", FractionBox["13", "11"], ",", FractionBox["9", "11"], ",", FractionBox["11", "8"], ",", FractionBox["2", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "10", ",", "22", ",", "26"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[1, 10], Rational[13, 11], Rational[9, 11], Rational[11, 8], Rational[2, 9]}, " bCanonicalized: ", {4, 18, 10, 22, 26}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"1", ",", FractionBox["13", "9"], ",", FractionBox["9", "2"], ",", FractionBox["12", "7"], ",", "1", ",", FractionBox["8", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "42", ",", "26"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {1, Rational[13, 9], Rational[9, 2], Rational[12, 7], 1, Rational[8, 9]}, " bCanonicalized: ", {4, 18, 42, 26}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"1", ",", FractionBox["13", "15"], ",", FractionBox["9", "2"], ",", "4", ",", "1", ",", FractionBox["8", "15"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "30", ",", "26"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {1, Rational[13, 15], Rational[9, 2], 4, 1, Rational[8, 15]}, " bCanonicalized: ", {4, 18, 30, 26}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["4", "5"], ",", FractionBox["4", "7"], ",", "4", ",", "4", ",", FractionBox["17", "8"], ",", FractionBox["10", "13"], ",", FractionBox["9", "14"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "7", ",", "26", ",", "34"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[4, 5], Rational[4, 7], 4, 4, Rational[17, 8], Rational[10, 13], Rational[9, 14]}, " bCanonicalized: ", {4, 18, 5, 7, 26, 34}, " greatestFactorA: ", {{2, 0, 0, 0, 0, 0}, {1, 2, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["10", "9"], ",", FractionBox["15", "11"], ",", FractionBox["15", "16"], ",", FractionBox["11", "14"], ",", FractionBox["2", "7"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "54", ",", "15", ",", "14", ",", "11"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[10, 9], Rational[15, 11], Rational[15, 16], Rational[11, 14], Rational[2, 7]}, " bCanonicalized: ", {4, 54, 15, 14, 11}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 3, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["2", "15"], ",", FractionBox["12", "5"], ",", FractionBox["8", "9"], ",", FractionBox["7", "8"], ",", FractionBox["12", "5"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"16", ",", "18", ",", "120", ",", "14"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"4", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[2, 15], Rational[12, 5], Rational[8, 9], Rational[7, 8], Rational[12, 5]}, " bCanonicalized: ", {16, 18, 120, 14}, " greatestFactorA: ", {{4, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["13", "8"], ",", FractionBox["5", "16"], ",", FractionBox["10", "9"], ",", FractionBox["2", "13"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "26"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[13, 8], Rational[5, 16], Rational[10, 9], Rational[2, 13]}, " bCanonicalized: ", {4, 18, 5, 26}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"1", ",", FractionBox["6", "5"], ",", FractionBox["7", "4"], ",", FractionBox["9", "13"], ",", FractionBox["9", "2"], ",", FractionBox["15", "16"], ",", FractionBox["17", "9"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "15", ",", "7", ",", "26", ",", "34"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {1, Rational[6, 5], Rational[7, 4], Rational[9, 13], Rational[9, 2], Rational[15, 16], Rational[17, 9]}, " bCanonicalized: ", {4, 18, 15, 7, 26, 34}, " greatestFactorA: ", {{2, 0, 0, 0, 0, 0}, {1, 2, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["1", "7"], ",", FractionBox["7", "4"], ",", FractionBox["9", "14"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "7"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[1, 7], Rational[7, 4], Rational[9, 14]}, " bCanonicalized: ", {4, 18, 7}, " greatestFactorA: ", {{2, 0, 0}, {1, 2, 0}, {0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"1", ",", "4", ",", FractionBox["8", "9"], ",", "5"}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {1, 4, Rational[8, 9], 5}, " bCanonicalized: ", {4, 18, 5}, " greatestFactorA: ", {{2, 0, 0}, {1, 2, 0}, {0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["3", "10"], ",", FractionBox["7", "15"], ",", FractionBox["4", "7"], ",", FractionBox["1", "4"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "15", ",", "7"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[3, 10], Rational[7, 15], Rational[4, 7], Rational[1, 4]}, " bCanonicalized: ", {4, 18, 15, 7}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", FractionBox["9", "4"], ",", FractionBox["1", "5"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "5"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {8, Rational[9, 4], Rational[1, 5]}, " bCanonicalized: ", {8, 18, 5}, " greatestFactorA: ", {{3, 0, 0}, {1, 2, 0}, {0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["1", "4"], ",", FractionBox["15", "2"], ",", FractionBox["6", "13"], ",", "1", ",", FractionBox["7", "2"], ",", FractionBox["3", "5"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "30", ",", "14", ",", "39"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[1, 4], Rational[15, 2], Rational[6, 13], 1, Rational[7, 2], Rational[3, 5]}, " bCanonicalized: ", {4, 18, 30, 14, 39}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["14", "5"], ",", "5", ",", FractionBox["10", "9"], ",", FractionBox["9", "7"], ",", FractionBox["4", "5"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "14"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[14, 5], 5, Rational[10, 9], Rational[9, 7], Rational[4, 5]}, " bCanonicalized: ", {4, 18, 5, 14}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["9", "2"], ",", FractionBox["6", "7"], ",", "17", ",", FractionBox["1", "4"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "21", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[9, 2], Rational[6, 7], 17, Rational[1, 4]}, " bCanonicalized: ", {4, 18, 21, 17}, " greatestFactorA: ", {{2, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{FractionBox["8", "13"], ",", FractionBox["13", "5"], ",", FractionBox["9", "4"], ",", "5"}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"8", ",", "18", ",", "5", ",", "13"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {Rational[8, 13], Rational[13, 5], Rational[9, 4], 5}, " bCanonicalized: ", {8, 18, 5, 13}, " greatestFactorA: ", {{3, 0, 0, 0}, {1, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}], Editable -> False]
BAAAAAAAAAD
InterpretationBox[RowBox[{"\"b: \"", "\:f360", RowBox[{"{", RowBox[{"1", ",", FractionBox["17", "5"], ",", FractionBox["8", "9"], ",", FractionBox["16", "17"], ",", FractionBox["1", "4"], ",", FractionBox["11", "15"]}], "}"}], "\:f360", "\" bCanonicalized: \"", "\:f360", RowBox[{"{", RowBox[{"4", ",", "18", ",", "5", ",", "66", ",", "17"}], "}"}], "\:f360", "\" greatestFactorA: \"", "\:f360", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "2", ",", "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "1"}], "}"}]}], "}"}]}], SequenceForm["b: ", {1, Rational[17, 5], Rational[8, 9], Rational[16, 17], Rational[1, 4], Rational[11, 15]}, " bCanonicalized: ", {4, 18, 5, 66, 17}, " greatestFactorA: ", {{2, 0, 0, 0, 0}, {1, 2, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}], Editable -> False]
w000t

getGreatestFactorAforB[b_] := getGreatestFactorA[padD[Map[rationalToI, canonicalB[b]], getDforB[b]]];

canonicalB[{4, 18}]
getGreatestFactorAforB[{4, 18}]
bIntersection[{4}, {18}]
LCM[4, 18]

colMaxes[a_] := Map[Max, Transpose[a]];
colMaxes[{{1, 12, 3}, {14, 5, 6}, {7, 8, 9}}]

colLcms[a_] := Map[Apply[LCM, #]&, Transpose[a]];
colLcms[{{1, 12, 3}, {14, 5, 6}, {7, 8, 9}}]

leastCommonPower[rational1_, rational2_] := Module[{i1, i2, i1Max, i2Max, i1Normal, i2Normal, },
  (*{i1, i2, d, kicker},*) (*TODO: this is no longer rationalsShareRoot *)
  i1 = rationalToI[rational1];
  i2 = rationalToI[rational2];
  
  i1Max = Max[i1];
  i2Max = Max[i2];
  
  i1Normal = i1 / i1Max;
  i2Normal = i2 / i2Max;
  
  If[
    i1Normal == i2Normal,
    (*Print["leadingEntry[i1]: ", leadingEntry[i1], " leadingEntry[i2]: ", leadingEntry[i2], " (LCM[leadingEntry[i1], leadingEntry[i2]]/leadingEntry[i1]): ", (LCM[leadingEntry[i1], leadingEntry[i2]]/leadingEntry[i1]), " i1: ", i1];*)
    iToRational[Abs[LCM[leadingEntry[i1], leadingEntry[i2]] / leadingEntry[i1]] * i1],
    False
  ]
  
  
  (*d = Max[Length[i1],Length[ i2]];
  i1 =PadRight[i1,d];
  i2 = PadRight[i2,d];
  kicker =  MapThread[LCM[#1,#2]&,{i1, i2}];
  Print["hm whats the kicker ", kicker, " and that's made up from ", i1, " and ", i2];
  
  iToRational[kicker]*)
  
  (*
      gcf = getGcf[{rational1, rational2}];
      
      If[
        gcf == 1,
        False,
        IntegerQ[Log[gcf, rational1]] && IntegerQ[Log[gcf, rational2]]
      ]*)
];
findFIfAnyInOtherIntervalBasisThatSharesRoot[b1f_, b2_] := Module[{fSharingRoot, lcp},
  fSharingRoot = Null;
  Do[
    lcp = leastCommonPower[b1f, b2f];
    If[
      lcp > 1,
      (*Print["doing it for ",b1f," and ", b2f, " with ",lcp];*)
      fSharingRoot = lcp
    ],
    {b2f, b2}
  ];
  
  fSharingRoot
];
bIntersectionBinaryOld[b1_, b2_] := Module[{intersectedB},
  intersectedB = {};
  
  Do[
    fSharingRoot = findFIfAnyInOtherIntervalBasisThatSharesRoot[b1f, b2];
    If[
      fSharingRoot === Null,
      "",
      intersectedB = Join[intersectedB, {fSharingRoot}]
    ],
    {b1f, b1}
  ];
  
  (* If[
   Length[intersectedB] == 0 && Length[b1]==1 && Length[b2]==1,
   Print["holy shit", b1, b1[[1]], rationalToI[b1[[1]]], "and the kcier", MapThread[LCM[#1,#2]&,{rationalToI[b1[[1]]], rationalToI[b2[[1]]]}]];
   {iToRational[MapThread[LCM[#1,#2]&,{rationalToI[b1[[1]]], rationalToI[b2[[1]]]}]]},*)
  canonicalB[intersectedB]
  (* ]*)
];

bIntersectionBinaryOld[{4}, {8}]
bIntersectionBinaryOld[{4, 18}, {8, 18}]
(*test[bIntersectionBinaryOld, {2, 3}, {10, 15}, {3 / 2}]; just wondering... but no*)

leastCommonPower[4, 8]
leastCommonPower[2, 3]
leastCommonPower[4, 18]
leastCommonPower[5 / 3, 25 / 9]

bMerge[{4, 18, 5}, {8, 18, 7}]

test[bIntersectionBinaryOld, {4, 18, 5}, {8, 18, 7}, {64, 18}];

canonicalB[{18, 3^12}]

crazytownBIntersection[b1_, b2_] := Module[{d, factorizedB1, factorizedB2, allZerosB, crazytown, hc, josh, firstHalf, secondHalf},
  d = Max[getDforB[b1], getDforB[b2]];
  factorizedB1 = padD[Map[rationalToI, b1], d ];
  factorizedB2 = padD[Map[rationalToI, b2], d ];
  
  allZerosB = Table[
    Table[
      0,
      Length[First[factorizedB2]] (* TODO: save these *)
    ],
    Length[factorizedB2]
  ];
  
  (*Print[allZerosB];*)
  
  crazytown = ArrayFlatten[
    {
      {factorizedB1, factorizedB1},
      {factorizedB2, allZerosB}
    }
  ];
  
  
  
  hc = hnf[crazytown];
  josh = {};
  Do[
    firstHalf = Take[hcRow, Length[hcRow] / 2];
    secondHalf = Take[hcRow, {Length[hcRow] / 2 + 1, Length[hcRow]}];
    If[allZerosL[firstHalf], josh = Join[josh, {secondHalf}]],
    {hcRow, hc}
  ];
  (*hc[[Length[hc]-Length[factorizedB2] +1;; Length[hc], Length[First[hc]]-Length[First[factorizedB2]]+1;; Length[First[hc]]]];*)
  
  (*Print[
  "factorizedB1: ", factorizedB1// MatrixForm, 
   "factorizedB2: ", factorizedB2// MatrixForm, 
   "block: ", crazytown // MatrixForm,
   "hnf'd: ", hc // MatrixForm,
   "bot rght desire: ", josh // MatrixForm
  ];*)
  
  canonicalB[Map[iToRational, If[Length[josh] == 0, {0}, josh]]]
];
crazytownBIntersection[{4, 18, 5}, {8, 18, 7}]

Table[Table[0, 3], 4]

lyst = {1, 2, 3, 4, 5, 6};
Take[lyst, {3, 6}]
Take[lyst, 3]
