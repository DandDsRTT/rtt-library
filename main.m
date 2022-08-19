debug = False;


(* TEMPERAMENT UTILITIES *)

getD[unparsedT_] := getDPrivate[parseTemperamentData[unparsedT]];
getDPrivate[t_] := colCount[getA[t]];

getR[unparsedT_] := getRPrivate[parseTemperamentData[unparsedT]];
getRPrivate[t_] := If[
  isRows[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];

getN[unparsedT_] := getNPrivate[parseTemperamentData[unparsedT]];
getNPrivate[t_] := If[
  isCols[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];


(* CANONICALIZATION *)

canonicalForm[unparsedT_] := formatOutput[canonicalFormPrivate[parseTemperamentData[unparsedT]]];
canonicalFormPrivate[t_] := Module[{intervalBasis, canonicalT},
  canonicalT = If[
    isCols[t],
    {canonicalCa[getA[t]], getVariance[t]},
    {canonicalMa[getA[t]], getVariance[t]}
  ];
  intervalBasis = getIntervalBasis[t];
  
  If[
    isStandardPrimeLimitIntervalBasis[intervalBasis],
    canonicalT,
    Join[canonicalT, {intervalBasis}]
  ]
];


(* DUAL *)

dual[unparsedT_] := formatOutput[dualPrivate[parseTemperamentData[unparsedT]]];
dualPrivate[t_] := If[
  isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]],
  If[
    isCols[t],
    rowify[antiNullSpaceBasis[getA[t]]],
    colify[nullSpaceBasis[getA[t]]]
  ],
  nonstandardIntervalBasisDual[t]
];


(* MERGE *)

mapMerge[unparsedT_] := formatOutput[mapMergePrivate[parseTemperamentData[unparsedT]]];
mapMergePrivate[tl___] := Module[{ml, intervalBasisList, intersectedIntervalBasis, tlWithIntersectedIntervalBasis},
  ml = Map[If[isCols[#], dualPrivate[#], #]&, {tl}];
  intervalBasisList = Map[getIntervalBasis, {tl}];
  intersectedIntervalBasis = Apply[intervalBasisIntersection, intervalBasisList];
  tlWithIntersectedIntervalBasis = Map[changeIntervalBasisForM[#, intersectedIntervalBasis]&, ml];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getM, tlWithIntersectedIntervalBasis]]], "row", intersectedIntervalBasis}]
];

commaMerge[unparsedT_] := formatOutput[commaMergePrivate[parseTemperamentData[unparsedT]]];
commaMergePrivate[tl___] := Module[{cl, intervalBasisList, mergedIntervalBasis, tlWithMergedIntervalBasis},
  cl = Map[If[isCols[#], #, dualPrivate[#]]&, {tl}];
  intervalBasisList = Map[getIntervalBasis, {tl}];
  mergedIntervalBasis = Apply[intervalBasisMerge, intervalBasisList];
  tlWithMergedIntervalBasis = Map[changeIntervalBasisForC[#, mergedIntervalBasis]&, cl];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getC, tlWithMergedIntervalBasis]]], "col", mergedIntervalBasis}]
];


(* INTERVAL BASIS *)

changeIntervalBasis[unparsedT_] := formatOutput[changeIntervalBasisPrivate[parseTemperamentData[unparsedT]]];
changeIntervalBasisPrivate[t_, targetIntervalBasis_] := If[
  isCols[t],
  changeIntervalBasisForC[t, targetIntervalBasis],
  changeIntervalBasisForM[t, targetIntervalBasis]
];


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


(* GENERATORS PREIMAGE TRANSVERSAL *)

getGeneratorsPreimageTransversal[unparsedT_] := formatOutput[getGeneratorsPreimageTransversalPrivate[parseTemperamentData[unparsedT]]];
getGeneratorsPreimageTransversalPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  colify[Transpose[generatorsPreimageTransversal]]
];




(* ___ PRIVATE ___ *)



(* PARSING *)

parseTemperamentData[temperamentData_] := Module[
  {ebk, intervalBasis, variance, ebkVectors, aOrL},
  
  If[
    StringMatchQ[ToString[temperamentData], RegularExpression[".*[\\[\\]⟨⟩<>]+.*"]],
    
    ebk = supportMathInEntries[temperamentData];
    
    If[
      StringMatchQ[ebk, RegularExpression["^[\\d\\.\\/]+\\s.*"]],
      
      intervalBasis = First[StringCases[ebk, RegularExpression["^([\\d\\.\\/]+)\\s.*"] -> "$1"]];
      ebk = First[StringCases[ebk, RegularExpression["^[\\d\\.\\/]+\\s(.*)"] -> "$1"]],
      
      intervalBasis = Null;
      ebk = ebk;
    ];
    
    variance = If[isCovariantEBK[ebk], "row", "col"];
    
    ebkVectors = If[
      isRows[{{}, variance}], (* use a dummy t *)
      StringCases[ebk, RegularExpression["[⟨<]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[\\]\\|]\\s*"] -> "$1"],
      StringCases[ebk, RegularExpression["[\\[\\|]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[⟩>]\\s*"] -> "$1"]
    ];
    
    aOrL = Map[parseEBKVector, ebkVectors];
    aOrL = If[Length[aOrL] == 1, First[aOrL], aOrL]; (* reduce from {{x}} to {x} if possible *)
    
    If[
      ToString[intervalBasis] == "Null",
      {aOrL, variance},
      {aOrL, variance, parseIntervalBasis[intervalBasis]}
    ],
    
    temperamentData
  ]
];

supportMathInEntries[ebk_] := StringReplace[
  StringReplace[
    StringReplace[
      StringReplace[
        ebk,
        RegularExpression["\\s*\\*\\s*"] -> "*"
      ],
      RegularExpression["\\s*\\/\\s*"] -> "/"
    ],
    RegularExpression["\\s*\\+\\s+"] -> "+"
  ],
  RegularExpression["\\s*\\-\\s+"] -> "-"
];

isCovariantEBK[ebk_] := StringMatchQ[ebk, RegularExpression["^[\\[]?\\s*[<⟨][^\\[]*"]];
parseEBKVector[ebkVector_] := Map[ToExpression, StringSplit[ebkVector, RegularExpression["(?:\\s*\\,\\s*)|\\s+"]]];

parseIntervalBasis[intervalBasisString_] := Map[ToExpression, StringSplit[intervalBasisString, "."]];

toEBK[t_] := If[
  hasAOrL[t],
  If[
    isCols[t],
    If[
      Length[getA[t]] == 1,
      vectorToEBK[getL[t]],
      ToString[StringForm["⟨``]", StringRiffle[Map[vectorToEBK, getA[t]]]]]
    ],
    If[
      Length[getA[t]] == 1,
      covectorToEBK[getL[t]],
      ToString[StringForm["[``⟩", StringRiffle[Map[covectorToEBK, getA[t]]]]]
    ]
  ],
  t
];

hasAOrL[maybeT_] := ListQ[maybeT] && Length[maybeT] > 1 && (isRows[{{}, getVariance[maybeT]}] || isCols[{{}, getVariance[maybeT]}]);

outputPrecision = 4;
vectorToEBK[vector_] := ToString[StringForm["[``⟩", StringRiffle[Map[formatNumber, vector]]]];
covectorToEBK[covector_] := ToString[StringForm["⟨``]", StringRiffle[Map[formatNumber, covector]]]];

formatNumber[entry_] := ToString[If[IntegerQ[entry], entry, SetAccuracy[N[entry], outputPrecision]]];
formatNumberList[l_] := Map[formatNumber, l];

toDisplay[t_] := If[
  hasAOrL[t],
  MatrixForm[Map[
    formatNumberList,
    If[isCols[t], Transpose[getA[t]], getA[t]]
  ]],
  t
];

formatOutput[output_] := If[
  format == "EBK",
  toEBK[output],
  If[
    format == "display",
    toDisplay[output],
    output
  ]
];

printWrapper[string___] := Apply[Print, {string}];

parseQuotientL[quotientLMaybeString_, t_] := Module[
  {quotientLString, quotientL},
  
  quotientLString = If[
    StringQ[quotientLMaybeString],
    quotientLMaybeString,
    quotientLToString[quotientLMaybeString]
  ];
  quotientLString = If[
    StringMatchQ[quotientLString, RegularExpression["^\\{.*\\}$"]],
    quotientLString,
    "{" <> quotientLString <> "}"
  ];
  
  quotientL = Map[ToExpression, StringCases[quotientLString, RegularExpression["([\\d\\/]+)[\\,\\s\\}]+"] -> "$1"]];
  
  colify[padVectorsWithZerosUpToD[
    Map[quotientToPcv, quotientL],
    getDPrivate[t]
  ]]
];

quotientLToString[quotientL_] := ToString[Map[
  ToString[Numerator[#]] <> "/" <> ToString[Denominator[#]]&,
  quotientL
]];

(* format = "EBK"; *)
format = "display";
(* format = "Wolfram"; *)


(* LIST UTILITIES *)

getGcd[l_] := Apply[GCD, l];
divideOutGcd[l_] := Module[{gcd}, gcd = getGcd[l]; If[gcd == 0, l, l / gcd]];
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

getAOrLOrS[t_] := If[
  ListQ[t],
  Part[t, 1],
  t
];

hasA[t_] := If[
  ListQ[t],
  ListQ[First[getAOrLOrS[t]]],
  False
];

hasL[t_] := If[
  ListQ[t],
  !hasA[t],
  False
];

getA[t_] := If[
  hasA[t],
  getAOrLOrS[t],
  If[
    hasL[t],
    {getAOrLOrS[t]},
    {{t}}
  ]
];

getL[t_] := If[
  hasL[t],
  getAOrLOrS[t],
  If[
    hasA[t],
    Error, (* you probably didn't mean to ask for the first (co)vector of a list *)
    {t}
  ]
];

breakByRowsOrCols[t_] := If[
  hasA[t],
  Map[
    {#, getVariance[t]}&,
    getA[t]
  ],
  If[
    hasL[t],
    {t},
    Error
  ]
];

scale[t_, scalar_] := If[
  hasA[t] || hasL[t],
  {scalar * getAOrLOrS[t], getVariance[t]},
  scalar * t
];

(* currently assumes matching variance and data type *)
addT[t1_, t2_] := If[
  hasA[t1],
  {getA[t1] + getA[t2], getVariance[t1]},
  If[
    hasL[t1],
    {getL[t1] + getL[t2], getVariance[t1]},
    t1 + t2
  ]
];

(* currently assumes matching variance and data type *)
subtractT[t1_, t2_] := If[
  hasA[t1],
  {getA[t1] - getA[t2], getVariance[t1]},
  If[
    hasL[t1],
    {getL[t1] - getL[t2], getVariance[t1]},
    t1 - t2
  ]
];

rowify[aOrL_] := {aOrL, "row"};
colify[aOrL_] := {aOrL, "col"};

maybeRowify[t_] := If[hasL[t], t, rowify[{t}]];

getVariance[t_] := Part[t, 2];

isCols[t_] := MemberQ[{
  "vector",
  "vectors",
  "contravector",
  "contravariant",
  "v",
  "c",
  "comma",
  "commas",
  "comma basis",
  "comma-basis",
  "commaBasis",
  "comma_basis",
  "i",
  "interval",
  "intervals",
  "g",
  "generator",
  "generators",
  "pcv",
  "gcv",
  "monzo",
  "monzos",
  "against",
  "col",
  "cols"
}, getVariance[t]];
isRows[t_] := MemberQ[{
  "map",
  "maps",
  "covector",
  "covectors",
  "covariant",
  "m",
  "mapping",
  "et",
  "ets",
  "edo",
  "edos",
  "edomapping",
  "edomappings",
  "val",
  "vals",
  "with",
  "row",
  "rows"
}, getVariance[t]];

multiply[tl_, variance_] := Module[
  {a, aOrL},
  
  a = Apply[Dot, Map[If[hasAOrL[#] && isCols[#], Transpose[getA[#]], getA[#]]&, tl]];
  
  aOrL = If[Length[a] == 1, First[a], a]; (* reduce from {{x}} to {x} if possible *)
  
  If[
    Length[aOrL] == 1, (* it's a scalar! *)
    
    First[aOrL], (* return without any variance; it's irrelevant *)
    
    If[
      isRows[{{}, variance}], (* create dummy t to check variance *)
      {aOrL, variance},
      {
        If[
          Length[Transpose[aOrL]] == 1, (* post transposing, again, reduce from {{x}} to {x} if possible *)
          First[Transpose[aOrL]],
          Transpose[aOrL]
        ],
        variance
      }
    ]
  ]
];
multiplyToRows[tl___] := multiply[{tl}, "row"];
multiplyToCols[tl___] := multiply[{tl}, "col"];

inverse[t_] := If[
  hasA[t],
  {Inverse[getA[t]], getVariance[t]},
  If[
    hasL[t],
    {1 / getL[t], getVariance[t]},
    1 / t
  ]
];

transpose[t_] := If[
  hasA[t],
  {getA[t], If[isRows[t], "col", "row"]},
  If[
    hasL[t],
    {getL[t], If[isRows[t], "col", "row"]},
    Error (* you probably don't mean to be transposing a scalar *)
  ]
];


(* CANONICALIZATION *)

hnf[a_] := Last[HermiteDecomposition[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

canonicalMa[ma_] := If[
  allZeros[ma],
  {Table[0, colCount[ma]]},
  removeUnneededZeroRows[hnf[colHermiteDefactor[ma]]]
];
canonicalCa[ca_] := antiTranspose[canonicalMa[antiTranspose[ca]]];


(* DUAL *)

noncanonicalNullSpaceBasis[ma_] := reverseEachCol[NullSpace[ma]];
noncanonicalAntiNullSpaceBasis[ca_] := NullSpace[ca];

nullSpaceBasis[ma_] := Module[{ca},
  ca = canonicalCa[noncanonicalNullSpaceBasis[ma]];
  
  If[
    ca == {{}},
    {Table[0, getDPrivate[ma]]},
    ca
  ]
];
antiNullSpaceBasis[ca_] := Module[{ma},
  ma = canonicalMa[noncanonicalAntiNullSpaceBasis[ca]];
  
  If[
    ma == {{}},
    {Table[0, getDPrivate[ca]]},
    ma
  ]
];

nonstandardIntervalBasisDual[t_] := If[
  isCols[t],
  {antiNullSpaceBasis[getA[t]], "row", getIntervalBasis[t]},
  {nullSpaceBasis[getA[t]], "col", getIntervalBasis[t]}
];


(* MERGE *)

getM[t_] := If[isRows[t] == True, t, dualPrivate[t]];
getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


(* INTERVAL BASIS *)

intervalBasisMerge[intervalBasisList___] := Module[{concattedIntervalBasis, concattedFormalPrimesA},
  concattedIntervalBasis = Apply[Join, {intervalBasisList}];
  concattedFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedIntervalBasis], getIntervalBasisDimension[concattedIntervalBasis]];
  
  canonicalIntervalBasis[Map[pcvToQuotient, concattedFormalPrimesA]]
];

intervalBasisIntersectionBinary[intervalBasis1_, intervalBasis2_] := Module[{intervalBasisDimension, formalPrimesA1, formalPrimesA2, allZerosFillerFormalPrimesA, blockA, intersectedFormalPrimesA, blockLHalf1, blockLHalf2},
  intervalBasisDimension = Max[getIntervalBasisDimension[intervalBasis1], getIntervalBasisDimension[intervalBasis2]];
  formalPrimesA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis1], intervalBasisDimension];
  formalPrimesA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis2], intervalBasisDimension];
  
  allZerosFillerFormalPrimesA = Table[Table[0, Length[First[formalPrimesA2]]], Length[formalPrimesA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {formalPrimesA1, formalPrimesA1},
      {formalPrimesA2, allZerosFillerFormalPrimesA}
    }
  ]];
  
  intersectedFormalPrimesA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedFormalPrimesA = Join[intersectedFormalPrimesA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedFormalPrimesA = If[Length[intersectedFormalPrimesA] == 0, {0}, intersectedFormalPrimesA];
  
  canonicalIntervalBasis[Map[pcvToQuotient, intersectedFormalPrimesA]]
];

intervalBasisIntersection[intervalBasisList___] := Module[{intersectedIntervalBasis},
  intersectedIntervalBasis = First[{intervalBasisList}];
  
  Do[
    intersectedIntervalBasis = intervalBasisIntersectionBinary[intersectedIntervalBasis, intervalBasis],
    {intervalBasis, Drop[{intervalBasisList}, 1]}
  ];
  
  canonicalIntervalBasis[intersectedIntervalBasis]
];

isSubspaceOf[candidateSubspaceIntervalBasis_, candidateSuperspaceIntervalBasis_] :=
    intervalBasisMerge[candidateSubspaceIntervalBasis, candidateSuperspaceIntervalBasis] == candidateSuperspaceIntervalBasis;

canonicalIntervalBasis[intervalBasis_] := Module[{formalPrimesA, canonicalFormalPrimesA},
  formalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis], getIntervalBasisDimension[intervalBasis]];
  canonicalFormalPrimesA = antiTranspose[removeAllZeroRows[hnf[antiTranspose[formalPrimesA]]]];
  
  If[
    Length[canonicalFormalPrimesA] == 0,
    {1},
    Map[super, Map[pcvToQuotient, canonicalFormalPrimesA]]
  ]
];

changeIntervalBasisForM[m_, targetSubspaceIntervalBasis_] := If[
  getIntervalBasis[m] == targetSubspaceIntervalBasis,
  m,
  If[
    isSubspaceOf[getIntervalBasis[m], targetSubspaceIntervalBasis],
    Error,
    canonicalFormPrivate[{getA[m].Transpose[getIntervalRebaseForM[getIntervalBasis[m], targetSubspaceIntervalBasis]], "row", targetSubspaceIntervalBasis}]
  ]
];

changeIntervalBasisForC[c_, targetSuperspaceIntervalBasis_] := If[
  getIntervalBasis[c] == targetSuperspaceIntervalBasis,
  c,
  If[
    isSubspaceOf[getIntervalBasis[c], targetSuperspaceIntervalBasis],
    canonicalFormPrivate[{Transpose[Transpose[getIntervalRebaseForC[getIntervalBasis[c], targetSuperspaceIntervalBasis]].Transpose[getA[c]]], "col", targetSuperspaceIntervalBasis}],
    Error
  ]
];

(* express the target formal primes in terms of the initial formal primes*)
getIntervalRebaseForM[originalSuperspaceIntervalBasis_, targetSubspaceIntervalBasis_] := Module[
  {
    intervalBasisDimension,
    targetSubspaceFormalPrimesA,
    originalSuperspaceFormalPrimesA,
    intervalRebase,
    intervalRebaseCol,
    intervalRebaseColEntry,
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry
  },
  
  intervalBasisDimension = getIntervalBasisDimension[Join[originalSuperspaceIntervalBasis, targetSubspaceIntervalBasis]];
  targetSubspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceIntervalBasis], intervalBasisDimension];
  originalSuperspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceIntervalBasis], intervalBasisDimension];
  
  intervalRebase = {};
  
  Do[
    intervalRebaseCol = {};
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry = targetSubspaceFormalPrimesAEntry;
    Do[
      intervalRebaseColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry += 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry -= originalSuperspaceFormalPrimesAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry += originalSuperspaceFormalPrimesAEntry
      ];
      
      intervalRebaseCol = Join[intervalRebaseCol, {intervalRebaseColEntry}],
      {originalSuperspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesA}
    ];
    intervalRebase = Join[intervalRebase, {intervalRebaseCol}],
    {targetSubspaceFormalPrimesAEntry, targetSubspaceFormalPrimesA}
  ];
  
  intervalRebase
];

(* yes, just swapping initial and target, that's all! *)
getIntervalRebaseForC[originalSubspaceIntervalBasis_, targetSuperspaceIntervalBasis_] := getIntervalRebaseForM[targetSuperspaceIntervalBasis, originalSubspaceIntervalBasis];

getPrimes[count_] := Map[Prime, Range[count]];

quotientToPcv[quotient_] := Module[{factorization, greatestPrime, count, primes, pcv, currentPrimeIndex},
  factorization = FactorInteger[quotient];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  pcv = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      pcv[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    pcv
  ]
];

pcvToQuotient[pcv_] := Module[{quotient, primeIndex},
  quotient = 1;
  primeIndex = 1;
  Do[
    quotient = quotient * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, pcv}
  ];
  
  quotient
];

getIntervalBasisDimension[intervalBasis_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, intervalBasis]]]]]];

padVectorsWithZerosUpToD[a_, d_] := Map[PadRight[#, d]&, a];

super[quotient_] := If[quotient < 1, Denominator[quotient] / Numerator[quotient], quotient];

getStandardPrimeLimitIntervalBasis[t_] := getPrimes[getDPrivate[t]];

isStandardPrimeLimitIntervalBasis[intervalBasis_] := canonicalIntervalBasis[intervalBasis] == getPrimes[Length[intervalBasis]];

getIntervalBasis[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitIntervalBasis[t]
];

signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

isNumeratorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry - superspaceFEntry}
], False];
isDenominatorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry + superspaceFEntry}
], False];

getFormalPrimes[t_] := Module[{intervalBasis},
  intervalBasis = getIntervalBasis[t];
  
  colify[padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis], getIntervalBasisDimension[intervalBasis]]]
];


(* ADDITION *)

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

intervalBasesDoNotMatch[t1_, t2_] := getIntervalBasis[t1] != getIntervalBasis[t2];

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

getLargestMinorsL[a_] := divideOutGcd[First[Minors[a, MatrixRank[a]]]];

isNegative[a_, isContravariant_] := Module[{largestMinorsL, entryFn, normalizingEntry},
  largestMinorsL = getLargestMinorsL[a];
  entryFn = If[isContravariant, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[largestMinorsL];
  
  normalizingEntry < 0
];
