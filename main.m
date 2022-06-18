debug = False;

(*
  
  TEMPERAMENT UTILITIES
  
  
  getD[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the dimensionality.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getD[meantoneM]
    
  Out   3
  
  In    meantoneC = "[4 -4 1⟩";
        getD[meantoneC]
    
  Out   3
  
*)
getD[unparsedT_] := getDPrivate[parseInput[unparsedT]];
getDPrivate[t_] := colCount[getA[t]];

(*
  
  getR[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the rank.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getR[meantoneM]
    
  Out   2
  
  In    meantoneC = "[4 -4 1⟩";
        getR[meantoneC]
    
  Out   2
  
*)
getR[unparsedT_] := getRPrivate[parseInput[unparsedT]];
getRPrivate[t_] := If[
  isCo[t],
  MatrixRank[getA[t]],
  getDPrivate[t] - MatrixRank[getA[t]]
];

(*
  
  getN[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the nullity.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getN[meantoneM]
    
  Out   1
  
  In    meantoneC = "[4 -4 1⟩";
        getN[meantoneC]
    
  Out   1
  
*)
getN[unparsedT_] := getNPrivate[parseInput[unparsedT]];
getNPrivate[t_] := If[
  isContra[t],
  MatrixRank[getA[t]],
  getDPrivate[t] - MatrixRank[getA[t]]
];




(* ___ PRIVATE ___ *)



(* PARSING *)

parseInput[tMaybeEbk_] := If[
  StringMatchQ[ToString[tMaybeEbk], RegularExpression[".*[\\[\\]⟨⟩<>]+.*"]],
  parseEBK[tMaybeEbk],
  tMaybeEbk
];

parseEBK[inputEbk_] := Module[
  {ebk, intervalBasis, variance, ebkVectors},
  
  ebk = supportMathInEntries[inputEbk];
  
  If[
    StringMatchQ[ebk, RegularExpression["^[\\d\\.\\/]+\\s.*"]],
    
    intervalBasis = First[StringCases[ebk, RegularExpression["^([\\d\\.\\/]+)\\s.*"] -> "$1"]];
    ebk = First[StringCases[ebk, RegularExpression["^[\\d\\.\\/]+\\s(.*)"] -> "$1"]],
    
    intervalBasis = Null;
    ebk = ebk;
  ];
  
  variance = If[isCovariantEBK[ebk], "co", "contra"];
  
  ebkVectors = If[
    variance == "co",
    StringCases[ebk, RegularExpression["[⟨<]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[\\]\\|]\\s*"] -> "$1"],
    StringCases[ebk, RegularExpression["[\\[\\|]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[⟩>]\\s*"] -> "$1"]
  ];
  
  If[
    ToString[intervalBasis] == "Null",
    {Map[parseEBKVector, ebkVectors], variance},
    {Map[parseEBKVector, ebkVectors], variance, parseIntervalBasis[intervalBasis]}
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
  isContra[t],
  If[
    getN[t] == 1,
    vectorToEBK[First[getA[t]]],
    ToString[StringForm["⟨``]", StringRiffle[ Map[vectorToEBK, getA[t]]]]]
  ],
  If[
    getR[t] == 1,
    covectorToEBK[First[getA[t]]],
    ToString[StringForm["[``⟩", StringRiffle[ Map[covectorToEBK, getA[t]]]]]
  ]
];

outputPrecision = 4;
vectorToEBK[vector_] := ToString[StringForm["[``⟩", StringRiffle[Map[formatNumber, vector]]]];
covectorToEBK[covector_] := ToString[StringForm["⟨``]", StringRiffle[Map[formatNumber, covector]]]];

formatNumber[entry_] := ToString[If[IntegerQ[entry], entry, SetAccuracy[N[entry], outputPrecision]]];
formatNumberList[l_] := Map[formatNumber, l];

toDisplay[t_] := MatrixForm[Map[formatNumberList, If[isContra[t], Transpose[getA[t]], getA[t]]]];

formatOutput[input_] := If[
  format == "EBK",
  toEBK[input],
  If[
    format == "display",
    toDisplay[input],
    input
  ]
];

parseQuotientSet[inputQuotientSetString_, t_] := Module[
  {quotientSetString, quotients},
  
  quotientSetString = If[
    StringMatchQ[inputQuotientSetString, RegularExpression["^\\{.*\\}$"]],
    inputQuotientSetString,
    "{" <> inputQuotientSetString <> "}"
  ];
  
  quotients = Map[ToExpression, StringCases[quotientSetString, RegularExpression["([\\d\\/]+)[\\,\\s\\}]+"] -> "$1"]];
  
  {
    padVectorsWithZerosUpToD[
      Map[quotientToPcv, quotients],
      getDPrivate[t]
    ],
    "contra"
  }
];

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

getA[t_] := Part[t, 1];
getVariance[t_] := Part[t, 2];

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
}, getVariance[t]];
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
}, getVariance[t]];
