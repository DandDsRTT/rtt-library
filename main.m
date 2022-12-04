debug = False;

(* MATH UTILITIES *)

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

padVectorsWithZerosUpToD[a_, d_] := Map[PadRight[#, d]&, a];

super[quotient_] := If[quotient < 1, Denominator[quotient] / Numerator[quotient], quotient];

getLargestMinorsL[a_] := divideOutGcd[First[Minors[a, MatrixRank[a]]]];



(* PARSING *)

parseTemperamentData[temperamentData_] := Module[
  {ebk, domainBasis, variance, ebkVectors, aOrL},
  
  If[
    StringMatchQ[ToString[temperamentData], RegularExpression[".*[\\[\\]⟨⟩<>]+.*"]], (* {} not included because those ID Wolfram-format stuff too! *)
    
    ebk = supportMathInEntries[temperamentData];
    
    If[
      StringMatchQ[ebk, RegularExpression["^[\\d\\.\\/]+\\s.*"]],
      
      domainBasis = First[StringCases[ebk, RegularExpression["^([\\d\\.\\/]+)\\s.*"] -> "$1"]];
      ebk = First[StringCases[ebk, RegularExpression["^[\\d\\.\\/]+\\s(.*)"] -> "$1"]],
      
      domainBasis = Null;
      ebk = ebk;
    ];
    
    variance = If[isCovariantEBK[ebk], "row", "col"];
    
    ebkVectors = If[
      isRows[{{}, variance}], (* use a dummy t *)
      StringCases[ebk, RegularExpression["[⟨{<]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[\\]\\|]\\s*"] -> "$1"],
      StringCases[ebk, RegularExpression["[\\[\\|]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[}⟩>]\\s*"] -> "$1"]
    ];
    
    aOrL = Map[parseEBKVector, ebkVectors];
    aOrL = If[Length[aOrL] == 1, First[aOrL], aOrL]; (* reduce from {{x}} to {x} if possible *)
    
    If[
      ToString[domainBasis] == "Null",
      {aOrL, variance},
      {aOrL, variance, parseDomainBasis[domainBasis]}
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

isCovariantEBK[ebk_] := StringMatchQ[ebk, RegularExpression["^[\\[]?\\s*[<⟨\\{][^\\[]*"]];
parseEBKVector[ebkVector_] := Map[ToExpression, StringSplit[ebkVector, RegularExpression["(?:\\s*\\,\\s*)|\\s+"]]];

parseDomainBasis[domainBasisString_] := Map[ToExpression, StringSplit[domainBasisString, "."]];

toEBK[t_] := If[
  hasAOrL[t],
  If[
    isCols[t],
    If[
      Length[getA[t]] == 1,
      vectorToEBK[First[getA[t]], t],
      If[
        Length[getA[t]] == getDPrivate[t],
        ToString[StringForm["⟨``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]],
        If[
          Length[getA[t]] == getRPrivate[t],
          ToString[StringForm["{``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]],
          ToString[StringForm["[``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]]
        ]
      ]
    ],
    If[
      Length[getA[t]] == 1,
      covectorToEBK[First[getA[t]], t],
      If[
        Length[getA[t]] == getDPrivate[t],
        ToString[StringForm["[``⟩", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]],
        If[
          Length[getA[t]] == getRPrivate[t],
          ToString[StringForm["[``}", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]],
          ToString[StringForm["[``]", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]]
        ]
      ]
    ]
  ],
  t
];

hasAOrL[maybeT_] := ListQ[maybeT] && Length[maybeT] > 1 && (isRows[{{}, getVariance[maybeT]}] || isCols[{{}, getVariance[maybeT]}]);

vectorToEBK[vector_, t_] := If[
  Length[vector] == getDPrivate[t],
  ToString[StringForm["[``⟩", StringRiffle[Map[formatNumber, vector]]]],
  If[
    Length[vector] == getRPrivate[t],
    ToString[StringForm["[``}", StringRiffle[Map[formatNumber, vector]]]],
    ToString[StringForm["[``]", StringRiffle[Map[formatNumber, vector]]]]
  ]
];
covectorToEBK[covector_, t_] := If[
  Length[covector] == getDPrivate[t],
  ToString[StringForm["⟨``]", StringRiffle[Map[formatNumber, covector]]]],
  If[
    Length[covector] == getRPrivate[t],
    ToString[StringForm["{``]", StringRiffle[Map[formatNumber, covector]]]],
    ToString[StringForm["[``]", StringRiffle[Map[formatNumber, covector]]]]
  ]
];

outputAccuracy = 3;
formatNumber[entry_] := If[
  IntegerQ[entry],
  entry,
  NumberForm[
    N[entry],
    {\[Infinity], outputAccuracy}, (* as many digits as needed to the left of the decimal point, 3 to the right *)
    ScientificNotationThreshold -> {-Infinity, Infinity} (* never lapse into scientific notation, e.g. 1\[Times]10⁻⁴ *)
  ]
];
formatNumberL[l_] := Map[formatNumber, l];

toDisplay[t_] := If[
  hasAOrL[t],
  MatrixForm[Map[
    formatNumberL,
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

isTemperamentData[maybeString_] := StringQ[maybeString] && StringMatchQ[maybeString, RegularExpression[".*[⟨⟩\\[\\]].*"]];

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
    getDomainBasisDimension[getDomainBasis[t]]
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

hnf[a_] := Last[HermiteDecomposition[a]];


(* VARIANCE UTILITIES *)

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


(* GENERATOR PREIMAGE TRANSVERSAL *)

getGeneratorPreimageTransversal[unparsedT_] := formatOutput[getGeneratorPreimageTransversalPrivate[parseTemperamentData[unparsedT]]];
getGeneratorPreimageTransversalPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorPreimageTransversal = right.Transpose[snf].left;
  
  colify[Transpose[generatorPreimageTransversal]]
];


(* TEMPERAMENT UTILITIES *)

getDPrivate[t_] := colCount[getA[t]];

getRPrivate[t_] := If[
  isRows[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];

getStandardPrimeLimitDomainBasis[t_] := getPrimes[getDPrivate[t]];

isStandardPrimeLimitDomainBasis[domainBasis_] := canonicalDomainBasis[domainBasis] == getPrimes[Length[domainBasis]];

getDomainBasis[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitDomainBasis[t]
];

(* TODO: wait does this actually do the superunison-ification *)
canonicalDomainBasis[domainBasis_] := Module[{basisChangeA, canonicalBasisChangeA},
  basisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis], getDomainBasisDimension[domainBasis]];
  canonicalBasisChangeA = antiTranspose[removeAllZeroRows[hnf[antiTranspose[basisChangeA]]]];
  
  If[
    Length[canonicalBasisChangeA] == 0,
    {1},
    Map[super, Map[pcvToQuotient, canonicalBasisChangeA]]
  ]
];

getDomainBasisDimension[domainBasis_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, domainBasis]]]]]];

getM[t_] := If[isRows[t] == True, t, dualPrivate[t]];

dualPrivate[t_] := If[
  isStandardPrimeLimitDomainBasis[getDomainBasis[t]],
  If[
    isCols[t],
    rowify[antiNullSpaceBasis[getA[t]]],
    colify[nullSpaceBasis[getA[t]]]
  ],
  nonstandardDomainBasisDual[t]
];

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

canonicalFormPrivate[t_] := Module[{domainBasis, canonicalT},
  canonicalT = If[
    isCols[t],
    {canonicalCa[getA[t]], getVariance[t]},
    {canonicalMa[getA[t]], getVariance[t]}
  ];
  domainBasis = getDomainBasis[t];
  
  If[
    isStandardPrimeLimitDomainBasis[domainBasis],
    canonicalT,
    Join[canonicalT, {domainBasis}]
  ]
];
canonicalMa[ma_] := If[
  allZeros[ma],
  {Table[0, colCount[ma]]},
  removeUnneededZeroRows[hnf[colHermiteDefactor[ma]]]
];
canonicalCa[ca_] := antiTranspose[canonicalMa[antiTranspose[ca]]];
hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

getBasisA[t_] := Module[{domainBasis},
  domainBasis = getDomainBasis[t];
  
  colify[padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis], getDomainBasisDimension[domainBasis]]]
];
