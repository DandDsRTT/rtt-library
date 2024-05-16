(* ::Package:: *)

(* ::Title:: *)
(*RTT Library in Wolfram Language*)


(* ::Subtitle:: *)
(*by Douglas Blumeyer and Dave Keenan*)


SetOptions[
  EvaluationNotebook[],
  StyleDefinitions -> Notebook[{
    Cell[StyleData[StyleDefinitions -> "Default.nb"]],
    Cell[StyleData["Chapter"], ShowGroupOpener -> "OutsideFrame", Selectable -> False, Editable -> False, CellMargins -> {{0, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Section"], ShowGroupOpener -> "OutsideFrame", Selectable -> False, Editable -> False, CellMargins -> {{40, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Subsection"], ShowGroupOpener -> "OutsideFrame", Selectable -> False, Editable -> False, CellMargins -> {{80, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Subsubsection"], ShowGroupOpener -> True, Selectable -> False, Editable -> False, Evaluatable -> False, CellMargins -> {{120, Inherited}, {Inherited, Inherited}}]
  }]
]
SetOptions[EvaluationCell[], CellOpen -> False]


(* ::Chapter::Closed:: *)
(*tests*)


failures = 0;
passes = 0;


test[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    TrueQ[actual == expectation],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was: ``", fn, {args}, expectation, actual], 14, Red]]
  ]
];


format = "Wolfram";


(* ::Text:: *)
(*some temperaments to check against*)


meantone = "[\:27e81 1 0] \:27e80 1 4]}";
blackwood = "[\:27e85 8 0] \:27e80 0 1]}";
dicot = "[\:27e81 1 2] \:27e80 2 1]}";
augmented = "[\:27e83 0 7] \:27e80 1 0]}";
mavila = "[\:27e81 0 7] \:27e80 1 -3]}";
porcupine = "[\:27e81 2 3] \:27e80 3 5]}";
srutal = "[\:27e82 0 11] \:27e80 1 -2]}";
hanson = "[\:27e81 0 1] \:27e80 6 5]}";
magic = "[\:27e81 0 2] \:27e80 5 1]}";
negri = "[\:27e81 2 2] \:27e80 -4 3]}";
tetracot = "[\:27e81 1 1] \:27e80 4 9]}";
meantone7 = "[\:27e81 0 -4 -13] \:27e80 1 4 10]}";
magic7 = "[\:27e81 0 2 -1] \:27e80 5 1 12]}";
pajara = "[\:27e82 3 5 6] \:27e80 1 -2 -2]}";
augene = "[\:27e83 0 7 18] \:27e80 1 0 -2]}";
sensi = "[\:27e81 -1 -1 -2] \:27e80 7 9 13]}";
sensamagic = "[\:27e81 0 0 0] \:27e80 1 1 2] \:27e80 0 2 -1]}";


(* ::Section::Closed:: *)
(*main*)


(* ::Subsection::Closed:: *)
(*music utilities*)


test[octaveReduce, 3, 3 / 2];
test[octaveReduce, 5, 5 / 4];
test[octaveReduce, 2 / 3, 4 / 3];


(* ::Subsection::Closed:: *)
(*math utilities*)


test[getPrimes, 5, {2, 3, 5, 7, 11}];


test[quotientToPcv, 22 / 5, {1, 0, -1, 0, 1}];
test[quotientToPcv, 1, {0}];


test[pcvToQuotient, {1, 0, -1, 0, 1}, 22 / 5];
test[pcvToQuotient, {0}, 1];


test[super, 5 / 3, 5 / 3];
test[super, 3 / 5, 5 / 3];


test[padVectorsWithZerosUpToD, {{1, 2, 3}, {4, 5, 6}}, 5, {{1, 2, 3, 0, 0}, {4, 5, 6, 0, 0}}];


(* ::Subsection::Closed:: *)
(*parsing utilities*)


map = "\:27e81200.000 1901.955 2786.314]";
mapping = "[\:27e81 0 -4] \:27e80 1 4]}";
comma = "[1 -5 3\:27e9";
commaBasis = "[[-4 4 -1\:27e9 [7 0 -3\:27e9]";


withOuterBrackets = "[\:27e81200.000 1901.955 2786.314]]";
withGtLtSigns = "[<1 0 -4] <0 1 4]>";
withPunctuationCommas = "[1, -5, 3\:27e9";
withLotsOfSpaces = " \:27e8 [ -4 4 -1 \:27e9 [ 7 0 -3 \:27e9 ] ";


mapInWolfram = {{1200.000, 1901.955, 2786.314}, "row"};
mappingInWolfram = {{{1, 0, -4}, {0, 1, 4}}, "row"};
commaInWolfram = {{1, -5, 3}, "col"};
commaBasisInWolfram = {{{-4, 4, -1}, {7, 0, -3}}, "col"};


test[parseTemperamentData, map, mapInWolfram];
test[parseTemperamentData, mapping, mappingInWolfram];
test[parseTemperamentData, comma, commaInWolfram];
test[parseTemperamentData, commaBasis, commaBasisInWolfram];
test[parseTemperamentData, withOuterBrackets, mapInWolfram];
test[parseTemperamentData, withGtLtSigns, mappingInWolfram];
test[parseTemperamentData, withPunctuationCommas, commaInWolfram];
test[parseTemperamentData, withLotsOfSpaces, commaBasisInWolfram];
test[parseTemperamentData, mapInWolfram, mapInWolfram];
test[parseTemperamentData, mappingInWolfram, mappingInWolfram];
test[parseTemperamentData, commaInWolfram, commaInWolfram];
test[parseTemperamentData, commaBasisInWolfram, commaBasisInWolfram];
test[parseTemperamentData, "2.3.7 [6 -2 -1\:27e9", {{6, -2, -1}, "col", {2, 3, 7}}];


test[isCovariantEBK, map, True];
test[isCovariantEBK, mapping, True];
test[isCovariantEBK, comma, False];
test[isCovariantEBK, commaBasis, False];
test[isCovariantEBK, withOuterBrackets, True];
test[isCovariantEBK, withGtLtSigns, True];
test[isCovariantEBK, withPunctuationCommas, False];
test[isCovariantEBK, withLotsOfSpaces, False];


test[parseEBKVector, "1, 3, 4", {1, 3, 4}];
test[parseEBKVector, "1,3,4", {1, 3, 4}];
test[parseEBKVector, "1 3 4", {1, 3, 4}];
test[parseEBKVector, "1  3  4", {1, 3, 4}];
test[parseEBKVector, "1 ,3 ,4", {1, 3, 4}];
test[parseEBKVector, "1 , 3 , 4", {1, 3, 4}];
test[parseEBKVector, "1 ,, 3 , 4", {1, Null, 3, 4}];


test[parseDomainBasis, "2.3.7", {2, 3, 7}];


test[toEBK, mapInWolfram, "\:27e81200.000 1901.955 2786.314]" ];
test[toEBK, mappingInWolfram, "[\:27e81 0 -4] \:27e80 1 4]}" ];
test[toEBK, commaInWolfram, "[1 -5 3\:27e9"];
test[toEBK, commaBasisInWolfram, "[[-4 4 -1\:27e9 [7 0 -3\:27e9]"];
test[toEBK, {{{4}, {5}}, "row"}, "[\:27e84] \:27e85]]"];
test[toEBK, {{{4}, {5}}, "col"}, "[[4\:27e9 [5\:27e9]"];


format = "EBK";


dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "row"};


test[vectorToEBK, {-4, 4, -1}, dummy5limitTemp, "[-4 4 -1\:27e9"];
test[vectorToEBK, {-3, 2}, dummy5limitTemp, "[-3 2}"];
test[vectorToEBK, {-3, 2, 0, 0}, dummy5limitTemp, "[-3 2 0 0]"];


test[covectorToEBK, {1, 0, -4}, dummy5limitTemp, "\:27e81 0 -4]"];
test[covectorToEBK, {7, 7}, dummy5limitTemp, "{7 7]"];
test[covectorToEBK, {7, 7, 7, 7}, dummy5limitTemp, "[7 7 7 7]"];


format = "Wolfram";


test[formatOutput, mappingInWolfram, mappingInWolfram];


format = "EBK";
test[formatOutput, mappingInWolfram, "[\:27e81 0 -4] \:27e80 1 4]}"];
format = "Wolfram";


test[parseQuotientL, "2", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "2/1", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2}", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2/1}", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2/1, 3/2}", dummy5limitTemp, {{{1, 0, 0}, {-1, 1, 0}}, "col"}];
test[parseQuotientL, "{11/7}", {{}, "row", {2, 9, 7, 11}}, {{{0, 0, 0, -1, 1}}, "col"}];


(* ::Subsection::Closed:: *)
(*list utilities*)


test[divideOutGcd, {0, -6, 9}, {0, -2, 3}];
test[divideOutGcd, {-1, -2, -3}, {-1, -2, -3}];
test[divideOutGcd, {0, 0, 0}, {0, 0, 0}];


test[multByLcd, {1 / 3, 1, 2 / 5}, {5, 15, 6}];


test[leadingEntry, {0, -6, 9, 0}, -6];


test[trailingEntry, {0, -6, 9, 0}, 9];


test[allZerosL, {0, -6, 9}, False];
test[allZerosL, {0, 0, 0}, True];


(* ::Subsection::Closed:: *)
(*matrix utilities*)


test[reverseInnerL, {{1, 0, -4}, {0, 1, 4}}, {{-4, 0, 1}, {4, 1, 0}}];


test[reverseOuterL, {{1, 0, -4}, {0, 1, 4}}, {{0, 1, 4}, {1, 0, -4}}];


test[rotate180, {{1, 0, -4}, {0, 1, 4}}, {{4, 1, 0}, {-4, 0, 1}}];


test[innerLLength, {{0, 0}, {0, 0}}, 2];
test[innerLLength, {{0}, {0}}, 1];
test[innerLLength, {{0, 0}}, 2];


test[hnf, {{5, 8, 12}, {7, 11, 16}}, {{1, 0, -4}, {0, 1, 4}}];
test[hnf, {{3, 0, -1}, {0, 3, 5}}, {{3, 0, -1}, {0, 3, 5}}];


test[getLargestMinorsL, {{17, 16, -4}, {4, -4, 1}}, {-4, 1, 0}];


test[allZeros, {{1, 0, -4}, {0, 1, 4}}, False];
test[allZeros, {{0, 0, 0}, {0, 0, 0}}, True];


test[removeAllZeroLists, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeAllZeroLists, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeAllZeroLists, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeAllZeroLists, {{0, 0}, {0, 0}}, {}];


test[removeUnneededZeroLists, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeUnneededZeroLists, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeUnneededZeroLists, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeUnneededZeroLists, {{0, 0}, {0, 0}}, {{0, 0}}];


(* ::Subsection::Closed:: *)
(*variance utilities*)


test[getAOrLOrS, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{1, 0, -4}, {0, 1, 4}}];
test[getAOrLOrS, {{12, 19, 28}, "row"}, {12, 19, 28}];
test[getAOrLOrS, 1200, 1200];


test[hasA, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[hasA, {{12, 19, 28}, "row"}, False];
test[hasA, 1200, False];


test[hasL, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[hasL, {{12, 19, 28}, "row"}, True];
test[hasL, 1200, False];


test[getA, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{1, 0, -4}, {0, 1, 4}}];
test[getA, {{12, 19, 28}, "row"}, {{12, 19, 28}}];
test[getA, 1200, {{1200}}];


test[getL, {{{1, 0, -4}, {0, 1, 4}}, "row"}, Error];
test[getL, {{12, 19, 28}, "row"}, {12, 19, 28}];
test[getL, 1200, {1200}];


test[breakByRowsOrCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 0, -4}, "row"}, {{0, 1, 4}, "row"}}];
test[breakByRowsOrCols, {{12, 19, 28}, "row"}, {{{12, 19, 28}, "row"}}];
test[breakByRowsOrCols, 1200, Error];


test[scale, {{{1, 0, -4}, {0, 1, 4}}, "row"}, 2, {{{2, 0, -8}, {0, 2, 8}}, "row"}];
test[scale, {{12, 19, 28}, "row"}, 2, {{24, 38, 56}, "row"}];
test[scale, 1200, 2, 2400];


test[addT, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 1}, {1, 1, 1}}, "row"}, {{{2, 1, -3}, {1, 2, 5}}, "row"}];
test[addT, {{12, 19, 28}, "row"}, {{1, 1, 1}, "row"}, {{13, 20, 29}, "row"}];
test[addT, 1200, 1, 1201];


test[subtractT, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 1}, {1, 1, 1}}, "row"}, {{{0, -1, -5}, {-1, 0, 3}}, "row"}];
test[subtractT, {{12, 19, 28}, "row"}, {{1, 1, 1}, "row"}, {{11, 18, 27}, "row"}];
test[subtractT, 1200, 1, 1199];


test[getVariance, {{{1, 0, -4}, {0, 1, 4}}, "row"}, "row"];


test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[isCols, {{{1, 2}, {3, 4}, {5, 6}}, "col"}, True];
test[isCols, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "row"}, False];
test[isCols, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "col"}, True];
test[isCols, {{{1, 2}, {0, 0}, {0, 0}}, "col"}, True];
test[isCols, {{{1, 0, 0}, {2, 0, 0}}, "row"}, False];
test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, True];


test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[isRows, {{{1, 2}, {3, 4}, {5, 6}}, "col"}, False];
test[isRows, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "row"}, True];
test[isRows, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "col"}, False];
test[isRows, {{{1, 2}, {0, 0}, {0, 0}}, "col"}, False];
test[isRows, {{{1, 0, 0}, {2, 0, 0}}, "row"}, True];
test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, False];


oneByThreeM = {{{1, 1, 1}}, "row"};
oneByThreeMap = {{1, 1, 1}, "row"};
twoByThreeM = {{{1, 1, 1}, {1, 1, 1}}, "row"};
threeByOneC = {{{1, 1, 1}}, "col"};
threeByOneComma = {{1, 1, 1}, "col"};
threeByTwoC = {{{1, 1, 1}, {1, 1, 1}}, "col"};


test[multiply, {oneByThreeM, threeByOneC}, "row", 3];
test[multiply, {oneByThreeM, threeByOneComma}, "row", 3];
test[multiply, {oneByThreeM, threeByTwoC}, "row", {{3, 3}, "row"}];
test[multiply, {oneByThreeMap, threeByOneC}, "row", 3];
test[multiply, {oneByThreeMap, threeByOneComma}, "row", 3];
test[multiply, {oneByThreeMap, threeByTwoC}, "row", {{3, 3}, "row"}];
test[multiply, {twoByThreeM, threeByOneC}, "row", {{{3}, {3}}, "row"}];
test[multiply, {twoByThreeM, threeByOneComma}, "row", {{{3}, {3}}, "row"}];
test[multiply, {twoByThreeM, threeByTwoC}, "row", {{{3, 3}, {3, 3}}, "row"}];


test[multiply, {oneByThreeM, threeByOneC}, "col", 3];
test[multiply, {oneByThreeM, threeByOneComma}, "col", 3];
test[multiply, {oneByThreeM, threeByTwoC}, "col", {{3, 3}, "col"}];
test[multiply, {oneByThreeMap, threeByOneC}, "col", 3];
test[multiply, {oneByThreeMap, threeByOneComma}, "col", 3];
test[multiply, {oneByThreeMap, threeByTwoC}, "col", {{3, 3}, "col"}];
test[multiply, {twoByThreeM, threeByOneC}, "col", {{3, 3}, "col"}];
test[multiply, {twoByThreeM, threeByOneComma}, "col", {{3, 3}, "col"}];
test[multiply, {twoByThreeM, threeByTwoC}, "col", {{{3, 3}, {3, 3}}, "col"}];


test[inverse, {{{1, 2, 3}, {4, 5, 0}, {0, 0, 9}}, "row"}, {{{-5 / 3, 2 / 3, 5 / 9}, {4 / 3, -1 / 3, -4 / 9}, {0, 0, 1 / 9}}, "row"}];
test[inverse, {{1, 2, 3}, "row"}, {{1, 1 / 2, 1 / 3}, "row"}];
test[inverse, 3, 1 / 3];


test[transpose, {{{1, 2, 3}, {4, 5, 6}}, "row"}, {{{1, 2, 3}, {4, 5, 6}}, "col"}];
test[transpose, {{{1, 2, 3}, {4, 5, 6}}, "col"}, {{{1, 2, 3}, {4, 5, 6}}, "row"}];
test[transpose, {{1, 2, 3}, "row"}, {{1, 2, 3}, "col"}];
test[transpose, {{1, 2, 3}, "col"}, {{1, 2, 3}, "row"}];
test[transpose, 1, Error];


(* ::Section::Closed:: *)
(*temperament*)


(* ::Subsection::Closed:: *)
(*utilities*)


test[getM, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];
test[getM, {{{4, -4, 1}}, "col"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];


test[getC, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{4, -4, 1}}, "col"}];
test[getC, {{{4, -4, 1}}, "col"}, {{{4, -4, 1}}, "col"}];


(* ::Subsection::Closed:: *)
(*dimensions*)


meantoneM = "[\:27e81 0 -4] \:27e80 1 4]}";
meantoneC = "[4 -4 1\:27e9";


test[getD, meantoneM, 3];
test[getD, meantoneC, 3];


test[getR, meantoneM, 2];
test[getR, meantoneC, 2];

test[getN, meantoneM, 1];
test[getN, meantoneC, 1];


test[getDPrivate, {{{0}}, "row"}, 1];
test[getDPrivate, {{{0}}, "col"}, 1];
test[getDPrivate, {{{0, 0}}, "row"}, 2];
test[getDPrivate, {{{0, 0}}, "col"}, 2];
test[getDPrivate, {{{0}, {0}}, "row"}, 1];
test[getDPrivate, {{{0}, {0}}, "col"}, 1];
test[getDPrivate, {IdentityMatrix[2], "row"}, 2];
test[getDPrivate, {IdentityMatrix[2], "col"}, 2];
test[getDPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, 3];
test[getDPrivate, {{{4, -4, 1}}, "col"}, 3];
test[getDPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "row"}, 4];
test[getDPrivate, {{{4, -4, 1, 0}}, "col"}, 4];
test[getDPrivate, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, 3];
test[getDPrivate, {{{1200.000, 1901.955, 2386.314}}, "row"}, 3];
test[getDPrivate, {{12, 19, 28}, "row"}, 3];
test[getDPrivate, {{-4, 4, -1}, "col"}, 3];


test[getRPrivate, {{{0}}, "row"}, 0];
test[getRPrivate, {{{0}}, "col"}, 1];
test[getRPrivate, {{{0, 0}}, "row"}, 0];
test[getRPrivate, {{{0, 0}}, "col"}, 2];
test[getRPrivate, {{{0}, {0}}, "row"}, 0];
test[getRPrivate, {{{0}, {0}}, "col"}, 1];
test[getRPrivate, {IdentityMatrix[2], "row"}, 2];
test[getRPrivate, {IdentityMatrix[2], "col"}, 0];
test[getRPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, 2];
test[getRPrivate, {{{4, -4, 1}}, "col"}, 2];
test[getRPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "row"}, 2];
test[getRPrivate, {{{4, -4, 1, 0}}, "col"}, 3];
test[getRPrivate, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, 2];
test[getRPrivate, {{{1200.000, 1901.955, 2386.314}}, "row"}, 1];
test[getRPrivate, {{12, 19, 28}, "row"}, 1];
test[getRPrivate, {{-4, 4, -1}, "col"}, 2];


test[getNPrivate, {{{0}}, "row"}, 1];
test[getNPrivate, {{{0}}, "col"}, 0];
test[getNPrivate, {{{0, 0}}, "row"}, 2];
test[getNPrivate, {{{0, 0}}, "col"}, 0];
test[getNPrivate, {{{0}, {0}}, "row"}, 1];
test[getNPrivate, {{{0}, {0}}, "col"}, 0];
test[getNPrivate, {IdentityMatrix[2], "row"}, 0];
test[getNPrivate, {IdentityMatrix[2], "col"}, 2];
test[getNPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, 1];
test[getNPrivate, {{{4, -4, 1}}, "col"}, 1];
test[getNPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "row"}, 2];
test[getNPrivate, {{{4, -4, 1, 0}}, "col"}, 1];
test[getNPrivate, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, 1];
test[getNPrivate, {{{1200.000, 1901.955, 2386.314}}, "row"}, 2];
test[getNPrivate, {{12, 19, 28}, "row"}, 2];
test[getNPrivate, {{-4, 4, -1}, "col"}, 1];


(* ::Subsection::Closed:: *)
(*canonicalization*)


format = "EBK";


someMeantoneM = "[\:27e85 8 12] \:27e87 11 16]}";
test[canonicalForm, someMeantoneM, "[\:27e81 0 -4] \:27e80 1 4]}"];


someMeantoneC = "[-8 8 -2\:27e9";
canonicalForm[canonicalForm, someMeantoneC, "[4 -4 1\:27e9"];


format = "Wolfram";


test[canonicalFormPrivate, {{{12, 0, 0}, {19, 0, 0}}, "a"}, {{{1, 0, 0}}, "a"}];
test[canonicalFormPrivate, {{{1, 1, 0}, {0, 1, 4}}, "a"}, {{{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalFormPrivate, {{{12, 19, 28}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalFormPrivate, {{{7, 11, 16}, {22, 35, 51}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalFormPrivate, {{{3, 0, -1}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalFormPrivate, {{{1, 2, 3}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalFormPrivate, {{{0, 1, 4, 10}, {1, 0, -4, -13}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalFormPrivate, {{{10, 13, 12, 0}, {-1, -1, 0, 3}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalFormPrivate, {{{5, 8, 0}, {0, 0, 1}}, "a"}, {{{5, 8, 0}, {0, 0, 1}}, "a"}];
test[canonicalFormPrivate, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}];
test[canonicalFormPrivate, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}];
test[canonicalFormPrivate, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}];
test[canonicalFormPrivate, {{{12, 19, 28}, {26, 43, 60}}, "a"}, {{{1, 8, 0}, {0, 11, -4}}, "a"}];
test[canonicalFormPrivate, {{{17, 16, -4}, {4, -4, 1}}, "a"}, {{{1, 0, 0}, {0, 4, -1}}, "a"}];
test[canonicalFormPrivate, {{{6, 5, -4}, {4, -4, 1}}, "a"}, {{{2, 1, -1}, {0, 2, -1}}, "a"}];
test[canonicalFormPrivate, {{{12, 19, 28}, {0, 0, 0}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalFormPrivate, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {1, 1, 0, -3}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}}, "a"}];
test[canonicalFormPrivate, {{{0, 0}}, "a"}, {{{0, 0}}, "a"}];
test[canonicalFormPrivate, {IdentityMatrix[3], "a"}, {IdentityMatrix[3], "a"}];
test[canonicalFormPrivate, {{{1, 0, -4}, {0, 1, 4}, {0, 0, 0}}, "a"}, {{{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalFormPrivate, {{{12, 19, 28, 0}}, "a"}, {{{12, 19, 28, 0}}, "a"}];
test[canonicalFormPrivate, {{{0, 0, 0}, {0, 0, 0}}, "a"}, {{{0, 0, 0}}, "a"}];
test[canonicalFormPrivate, {{{24, 38, 56}}, "row", {2, 3, 5}}, {{{12, 19, 28}}, "row"}];
test[canonicalFormPrivate, {{{22, 70, 62}}, "row", {2, 9, 7}}, {{{11, 35, 31}}, "row", {2, 9, 7}}];


test[canonicalMa, {{1, 1, 0}, {0, 1, 4}}, {{1, 0, -4}, {0, 1, 4}}];


test[canonicalCa, {{-4, 4, -1}}, {{4, -4, 1}}];
test[canonicalCa, {{8, 2, 9, 8}, {2, 9, -4, -8}, {3, 1, -9, -2}}, {{370, 327, 0, 0}, {150, 133, 1, 0}, {127, 110, 0, 2}}]; (* should put zeroes in the top-right, for larger primes and the first commas in the list *)


test[hermiteRightUnimodular, {{6, 5, -4}, {4, -4, 1}}, {{1, 2, 1}, {-1, 0, 2}, {0, 3, 4}}];


test[colHermiteDefactor, {{6, 5, -4}, {4, -4, 1}}, {{6, 5, -4}, {-4, -4, 3}}];


(* ::Subsection::Closed:: *)
(*dual*)


format = "EBK";
test[dual, meantoneM, meantoneC];
format = "Wolfram";


test[dualPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{4, -4, 1}}, "col"}];
test[dualPrivate, {{{0, 9, 4}}, "row"}, {{{1, 0, 0}, {0, -4, 9}}, "col"}];
test[dualPrivate, {{{0}}, "row"}, {IdentityMatrix[1], "col"}];
test[dualPrivate, {{{0, 0}}, "row"}, {IdentityMatrix[2], "col"}];
test[dualPrivate, {{{0, 0, 0}}, "row"}, {IdentityMatrix[3], "col"}];
test[dualPrivate, {IdentityMatrix[1], "row"}, {{{0}}, "col"}];
test[dualPrivate, {IdentityMatrix[2], "row"}, {{{0, 0}}, "col"}];
test[dualPrivate, {IdentityMatrix[3], "row"}, {{{0, 0, 0}}, "col"}];
test[dualPrivate, {{{12, 19}}, "row"}, {{{-19, 12}}, "col"}];
test[dualPrivate, {{{4, -4, 1}}, "col"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];
test[dualPrivate, {{{1, 0, 0}, {0, -4, 9}}, "col"}, {{{0, 9, 4}}, "row"}];
test[dualPrivate, {{{0}}, "col"}, {IdentityMatrix[1], "row"}];
test[dualPrivate, {{{0, 0}}, "col"}, {IdentityMatrix[2], "row"}];
test[dualPrivate, {{{0, 0, 0}}, "col"}, {IdentityMatrix[3], "row"}];
test[dualPrivate, {IdentityMatrix[1], "col"}, {{{0}}, "row"}];
test[dualPrivate, {IdentityMatrix[2], "col"}, {{{0, 0}}, "row"}];
test[dualPrivate, {IdentityMatrix[3], "col"}, {{{0, 0, 0}}, "row"}];
test[dualPrivate, {{{-19, 12}}, "col"}, {{{12, 19}}, "row"}];


verifyDuals[m_, c_] := Module[{dualM, dualC},
  dualC = dualPrivate[m];
  dualM = dualPrivate[c];
  
  If[
    TrueQ[dualC == canonicalFormPrivate[c]] && TrueQ[dualM == canonicalFormPrivate[m]],
    passes += 1,
    failures += 1;
    printWrapper["verifyDuals[", m, ", ", c, "]; dualC: ", dualC, " canonicalFormPrivate[c]: ", canonicalFormPrivate[c], " dualM: ", dualM, " canonicalFormPrivate[m]: ", canonicalFormPrivate[m]]
  ];
];


verifyDuals[{{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{4, -4, 1}}, "col"}];
verifyDuals[{{{1, 0, 0}, {0, -4, 9}}, "row"}, {{{0, 9, 4}}, "col"}];
verifyDuals[{{{0}}, "row"}, {IdentityMatrix[1], "col"}];
verifyDuals[{{{0, 0}}, "row"}, {IdentityMatrix[2], "col"}];
verifyDuals[{{{0, 0, 0}}, "row"}, {IdentityMatrix[3], "col"}];
verifyDuals[{IdentityMatrix[1], "row"}, {{{0}}, "col"}];
verifyDuals[{IdentityMatrix[2], "row"}, {{{0, 0}}, "col"}];
verifyDuals[{IdentityMatrix[3], "row"}, {{{0, 0, 0}}, "col"}];
verifyDuals[{{{12, 19}}, "row"}, {{{-19, 12}}, "col"}];


(* ::Subsection::Closed:: *)
(*generator detempering*)


format = "EBK";
meantoneM = "[\:27e81 1 0] \:27e80 1 4]}";
test[getGeneratorDetempering, meantoneM, "[[1 0 0\:27e9 [-1 1 0\:27e9]"];
format = "Wolfram";


test[getGeneratorDetemperingPrivate, {{{1, 1, 0}, {0, 1, 4}}, "row"}, {{{1, 0, 0}, {-1, 1, 0}}, "col"}];
test[getGeneratorDetemperingPrivate, {{{4, -4, 1}}, "col"}, {{{1, 0, 0}, {0, 1, 0}}, "col"}];


(* ::Subsection::Closed:: *)
(*merging*)


format = "EBK";


et5M = "\:27e85 8 12]";
et7M = "\:27e87 11 16]";
test[mapMerge, et5M, et7M, "[\:27e81 0 -4] \:27e80 1 4]}"];


et7dM = "\:27e87 11 16 19]";
et12M = "\:27e812 19 28 34]";
et22M = "\:27e822 35 51 62]";
test[mapMerge, et7dM, et12M, et22M, "[\:27e81 0 0 -5] \:27e80 1 0 2] \:27e80 0 1 2]}"];


meantoneC = "[4 -4 1\:27e9";
porcupineC = "[1 -5 3\:27e9";
test[commaMerge, meantoneC, porcupineC, "[[-11 7 0\:27e9 [-7 3 1\:27e9]"];


mintC = "[2 2 -1 -1\:27e9";
meantoneC = "[4 -4 1 0\:27e9";
negriC = "[-14 3 4 0\:27e9";
test[commaMerge, mintC, meantoneC, negriC, "[[-30 19 0 0\:27e9 [-26 15 1 0\:27e9 [-6 2 0 1\:27e9]"];


format = "Wolfram";


(* ::Subsubsection::Closed:: *)
(*basic examples*)


et5M5 = {{{5, 8, 12}}, "row"};
et5C5 = {{{-8, 5, 0}, {-4, 1, 1}}, "col"};
et7M5 = {{{7, 11, 16}}, "row"};
et7C5 = {{{-11, 7, 0}, {-7, 3, 1}}, "col"};
meantoneM5 = {{{1, 0, -4}, {0, 1, 4}}, "row"};
meantoneC5 = {{{4, -4, 1}}, "col"};
porcupineM5 = {{{1, 2, 3}, {0, 3, 5}}, "row"};
porcupineC5 = {{{1, -5, 3}}, "col"};


(* ::Subsubsection::Closed:: *)
(*just demonstrating that these are equivalent temperaments*)


test[dualPrivate, et5C5, et5M5];
test[dualPrivate, et7C5, et7M5];
test[dualPrivate, meantoneC5, meantoneM5];
test[dualPrivate, porcupineC5, porcupineM5];


(* ::Subsubsection::Closed:: *)
(*okay, now some actual tests*)


test[mapMergePrivate, et5M5, et7M5, meantoneM5];
test[commaMergePrivate, meantoneC5, porcupineC5, et7C5];


(* ::Subsubsection::Closed:: *)
(*prove out that you can specify temperaments by either their mappings or their comma bases*)


test[mapMergePrivate, et5M5, et7C5, meantoneM5];
test[commaMergePrivate, meantoneM5, porcupineC5, et7C5];
test[mapMergePrivate, et5C5, et7M5, meantoneM5];
test[commaMergePrivate, meantoneC5, porcupineM5, et7C5];
test[mapMergePrivate, et5C5, et7C5, meantoneM5];
test[commaMergePrivate, meantoneM5, porcupineM5, et7C5];


(* ::Subsubsection::Closed:: *)
(*prove out that you can comma-merge or map-merge more than 2 temperaments at a time*)


et7dM7 = {{{7, 11, 16, 19}}, "row"};
et12M7 = {{{12, 19, 28, 34}}, "row"};
et22M7 = {{{22, 35, 51, 62}}, "row"};
marvel = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "row"};
test[mapMergePrivate, et7dM7, et12M7, et22M7, marvel];


mintC7 = {{{2, 2, -1, -1}}, "col"};
meantoneC7 = {{{4, -4, 1, 0}}, "col"};
negriC7 = {{{-14, 3, 4, 0}}, "col"};
et19dC7 = dualPrivate[{{{19, 30, 44, 54}}, "row"}];
test[commaMergePrivate, mintC7, meantoneC7, negriC7, et19dC7];


(* ::Subsubsection::Closed:: *)
(*examples from Meet and Join page*)


meantoneComma7 = {-4, 4, -1, 0};
starlingComma7 = {1, 2, -3, 1};
septimalComma7 = {6, -2, 0, -1};
porcupineComma7 = {1, -5, 3, 0};
marvelComma7 = {-5, 2, 2, -1};
gamelisma7 = {-10, 1, 0, 3};
sensamagicComma7 = {0, -5, 1, 2};


meantoneComma11 = {-4, 4, -1, 0, 0};
starlingComma11 = {1, 2, -3, 1, 0};
keenanisma11 = {-7, -1, 1, 1, 1};
marvelComma11 = {-5, 2, 2, -1, 0};
septimalComma11 = {6, -2, 0, -1, 0};
ptolemisma11 = {2, -2, 2, 0, -1};
telepathma11 = {-1, -3, 1, 0, 1};
mothwellsma11 = {-1, 2, 0, -2, 1};
rastma11 = {-1, 5, 0, 0, -2};
sensamagicComma11 = {0, -5, 1, 2, 0};
werckisma11 = {-3, 2, -1, 2, -1};
valinorsma11 = {4, 0, -2, -1, 1};


meantoneM11 = {{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "row"};
meantoneC11 = {{meantoneComma11, starlingComma11, mothwellsma11}, "col"};
meanpopM11 = {{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "row"};
meanpopC11 = {{meantoneComma11, starlingComma11, keenanisma11}, "col"};
marvelM11 = {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "row"};
marvelC11 = {{marvelComma11, keenanisma11}, "col"};
porcupineM11 = {{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "row"};
porcupineC11 = {{telepathma11, septimalComma11, ptolemisma11}, "col"};
et31M11 = {{{31, 49, 72, 87, 107}}, "row"};
et31C11 = {{{-49, 31, 0, 0, 0}, {-45, 27, 1, 0, 0}, {-36, 21, 0, 1, 0}, {-24, 13, 0, 0, 1}}, "col"};
meantoneM7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
meantoneC7 = {{meantoneComma7, starlingComma7}, "col"};
porcupineM7 = {{{1, 2, 3, 2}, {0, 3, 5, -6}}, "row"};
porcupineC7 = {{septimalComma7, porcupineComma7}, "col"};
miracleM11 = {{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "row"};
miracleC11 = {{marvelComma11, rastma11, keenanisma11}, "col"};
magicM11 = {{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "row"};
magicC11 = {{marvelComma11, sensamagicComma11, ptolemisma11}, "col"};
et41M11 = {{{41, 65, 95, 115, 142}}, "row"};
et41C11 = {{{-65, 41, 0, 0, 0}, {-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {-32, 18, 0, 0, 1}}, "col"};
miracleM7 = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "row"};
miracleC7 = {{marvelComma7, gamelisma7}, "col"};
magicM7 = {{{1, 0, 2, -1}, {0, 5, 1, 12}}, "row"};
magicC7 = {{marvelComma7, sensamagicComma7}, "col"};
et41M7 = {{{41, 65, 95, 115}}, "row"};
et41C7 = {{{-65, 41, 0, 0}, {-15, 8, 1, 0}, {-25, 14, 0, 1}}, "col"};
mothraM11 = {{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "row"};
mothraC11 = {{meantoneComma11, mothwellsma11, keenanisma11}, "col"};
mothraM7 = {{{1, 1, 0, 3}, {0, 3, 12, -1}}, "row"};
mothraC7 = {{meantoneComma7, gamelisma7}, "col"};
portentM11 = {{{1, 1, 0, 3, 5}, {0, 3, 0, -1, 4}, {0, 0, 1, 0, -1}}, "row"};
portentC11 = {{keenanisma11, werckisma11}, "col"};
gamelanM7 = {{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "row"};
gamelanC7 = {{gamelisma7}, "col"};
marvelM7 = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "row"};
marvelC7 = {{marvelComma7}, "col"};


test[dualPrivate, meantoneC11, meantoneM11];
test[dualPrivate, meanpopC11, meanpopM11];
test[dualPrivate, marvelC11, marvelM11];
test[dualPrivate, porcupineC11, porcupineM11];
test[dualPrivate, et31C11, et31M11];
test[dualPrivate, meantoneC7, meantoneM7];
test[dualPrivate, porcupineC7, porcupineM7];
test[dualPrivate, miracleC11, miracleM11];
test[dualPrivate, magicC11, magicM11];
test[dualPrivate, et41C11, et41M11];
test[dualPrivate, miracleC7, miracleM7];
test[dualPrivate, magicC7, magicM7];
test[dualPrivate, et41C7, et41M7];
test[dualPrivate, mothraC11, mothraM11];
test[dualPrivate, mothraC7, mothraM7];
test[dualPrivate, portentC11, portentM11];
test[dualPrivate, gamelanC7, gamelanM7];
test[dualPrivate, marvelC7, marvelM7];



(* ::Subsubsection::Closed:: *)
(*\:22ce = COMMA MERGE, \:22cf = MAP MERGE*)


(* ::Text:: *)
(*Meantone\:22ceMeanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val*)


test[commaMergePrivate, meantoneC11, meanpopC11, et31C11];


(* ::Text:: *)
(*Meantone\:22cfMeanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>*)


test[mapMergePrivate, meantoneM11, meanpopM11, {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {0, 0, 0, 0, 1}}, "row"}];


(* ::Text:: *)
(*Meantone\:22ceMarvel = 31*)


test[commaMergePrivate, meantoneC11, marvelC11, et31C11];


(* ::Text:: *)
(*Meantone\:22cfMarvel = <225/224>*)


test[mapMergePrivate, meantoneM11, marvelM11, dualPrivate[{{marvelComma11}, "col"}]];


(* ::Text:: *)
(*Meantone\:22cePorcupine = G = <JI>*)


test[commaMergePrivate, meantoneC11, porcupineC11, {IdentityMatrix[5], "col"}];


(* ::Text:: *)
(*Meantone\:22cfPorcupine = <176/175>*)


test[mapMergePrivate, meantoneM11, porcupineM11, dualPrivate[{{valinorsma11}, "col"}]];


(* ::Text:: *)
(*In the 7-limit, that become Meantone\:22cePorcupine = <JI>, Meantone\:22cfPorcupine = <1>*)


test[commaMergePrivate, meantoneC7, porcupineC7, {IdentityMatrix[4], "col"}];
test[mapMergePrivate, meantoneM7, porcupineM7, {IdentityMatrix[4], "row"}];


(* ::Text:: *)
(*Miracle\:22ceMagic = 41*)


test[commaMergePrivate, miracleC11, magicC11, et41C11];


(* ::Text:: *)
(*Miracle\:22cfMagic = Marvel*)


test[mapMergePrivate, miracleM11, magicM11, marvelM11];


(* ::Text:: *)
(*In the 7-limit, again Miracle\:22ceMagic = 41, Miracle\:22cfMagic = Marvel*)


test[commaMergePrivate, miracleC7, magicC7, et41C7];
test[mapMergePrivate, miracleM7, magicM7, marvelM7];


(* ::Text:: *)
(*Miracle\:22ceMothra = 31*)


test[commaMergePrivate, miracleC11, mothraC11, et31C11];


(* ::Text:: *)
(*Miracle\:22cfMothra = Portent*)


test[mapMergePrivate, miracleM11, mothraM11, portentM11];


(* ::Text:: *)
(*In the 7-limit, Miracle\:22cfMothra = Gamelan*)


test[mapMergePrivate, miracleM7, mothraM7, gamelanM7];


(* ::Text:: *)
(*Meantone\:22ceMagic = <JI>*)


test[commaMergePrivate, meantoneC11, magicC11, {IdentityMatrix[5], "col"}];


(* ::Text:: *)
(*Meantone\:22cfMagic = <225/224>*)


test[mapMergePrivate, meantoneM11, magicM11, dualPrivate[{{marvelComma11}, "col"}]];


(* ::Subsubsection::Closed:: *)
(*nonstandard domain basis*)


t1 = {{{22, 35, 51, 76}}, "row", {2, 3, 5, 11}};
t2 = {{{17, 54, 48, 59}}, "row", {2, 9, 7, 11}};
expectedT = {{{1, 0, 13}, {0, 1, -3}}, "row", {2, 9, 11}};(* {{{22,70,76},{17,54,59}},"row",{2,9,11}}; before canonicalization *)
test[mapMergePrivate, t1, t2, expectedT];


t1 = {{{4, -4, 1}}, "col"};
t2 = {{{4, -2, 1, 0}, {6, -3, 0, 1}}, "col", {2, 9, 5, 11}};
expectedT = {{{1, 0, -4}, {0, 1, 2}}, "row", {2, 9, 5}};
test[mapMergePrivate, t1, t2, expectedT];


t1 = {{{4, -4, 1}}, "col"};
t2 = {{{6, -1, -1}}, "col", {2, 9, 7}};
expectedT = {{{4, -4, 1, 0}, {-6, 2, 0, 1}}, "col"};
test[commaMergePrivate, t1, t2, expectedT];


t1 = {{{5, 8, 12}, {7, 11, 16}}, "row"};
t2 = {{{7, 22, 16, 24}, {6, 19, 14, 21}}, "row", {2, 9, 5, 11}};
expectedT = {{{4, -4, 1, 0}, {6, -6, 0, 1}}, "col", {2, 3, 5, 11}};
test[commaMergePrivate, t1, t2, expectedT];


(* ::Subsection::Closed:: *)
(*domain basis*)


format = "EBK";


meantoneC = "[4 -4 1\:27e9";
targetDomainBasis = "2.3.5.7";
test[changeDomainBasis, meantoneC, targetDomainBasis, "[4 -4 1 0\:27e9"];


meantoneM = "[\:27e81 0 -4] \:27e80 1 4]}";
targetDomainBasis = "2.3";
test[changeDomainBasis, meantoneM, targetDomainBasis, "[\:27e81 0] \:27e80 1]\:27e9"];


format = "Wolfram";


test[canonicalDomainBasis, "2.7.9", {2, 9, 7}];


(* ::Text:: *)
(*order by prime limit*)


test[canonicalDomainBasisPrivate, {2, 7, 9}, {2, 9, 7}];
test[canonicalDomainBasisPrivate, {2, 9 / 7, 5}, {2, 5, 9 / 7}];
test[canonicalDomainBasisPrivate, {2, 9 / 7, 5 / 3}, {2, 5 / 3, 9 / 7}];


(* ::Text:: *)
(*consolidate redundancies*)


test[canonicalDomainBasisPrivate, {2, 3, 9}, {2, 3}];
test[canonicalDomainBasisPrivate, {2, 3, 15}, {2, 3, 5}];
test[canonicalDomainBasisPrivate, {2, 3, 5 / 3}, {2, 3, 5}];


(* ::Text:: *)
(*tricky stuff*)


test[canonicalDomainBasisPrivate, {2, 5 / 3, 7 / 5}, {2, 5 / 3, 7 / 3}];
test[canonicalDomainBasisPrivate, {1, 1}, {1}];


(* ::Text:: *)
(*all the subgroups on the wiki page, if they are canonical according to this*)


test[canonicalDomainBasisPrivate, {2, 3, 7}, {2, 3, 7}];
test[canonicalDomainBasisPrivate, {2, 5, 7}, {2, 5, 7}];
test[canonicalDomainBasisPrivate, {2, 3, 7 / 5}, {2, 3, 7 / 5}];
test[canonicalDomainBasisPrivate, {2, 5 / 3, 7}, {2, 5 / 3, 7}];
test[canonicalDomainBasisPrivate, {2, 5, 7 / 3}, {2, 5, 7 / 3}];
test[canonicalDomainBasisPrivate, {2, 5 / 3, 7 / 3}, {2, 5 / 3, 7 / 3}];
test[canonicalDomainBasisPrivate, {2, 27 / 25, 7 / 3}, {2, 27 / 25, 7 / 3}];
test[canonicalDomainBasisPrivate, {2, 9 / 5, 9 / 7}, {2, 9 / 5, 9 / 7}];
test[canonicalDomainBasisPrivate, {2, 3, 11}, {2, 3, 11}];
test[canonicalDomainBasisPrivate, {2, 5, 11}, {2, 5, 11}];
test[canonicalDomainBasisPrivate, {2, 7, 11}, {2, 7, 11}];
test[canonicalDomainBasisPrivate, {2, 3, 5, 11}, {2, 3, 5, 11}];
test[canonicalDomainBasisPrivate, {2, 3, 7, 11}, {2, 3, 7, 11}];
test[canonicalDomainBasisPrivate, {2, 5, 7, 11}, {2, 5, 7, 11}];
test[canonicalDomainBasisPrivate, {2, 5 / 3, 7 / 3, 11 / 3}, {2, 5 / 3, 7 / 3, 11 / 3}];
test[canonicalDomainBasisPrivate, {2, 3, 13}, {2, 3, 13}];
test[canonicalDomainBasisPrivate, {2, 3, 5, 13}, {2, 3, 5, 13}];
test[canonicalDomainBasisPrivate, {2, 3, 7, 13}, {2, 3, 7, 13}];
test[canonicalDomainBasisPrivate, {2, 5, 7, 13}, {2, 5, 7, 13}];
test[canonicalDomainBasisPrivate, {2, 5, 7, 11, 13}, {2, 5, 7, 11, 13}];
test[canonicalDomainBasisPrivate, {2, 3, 13 / 5}, {2, 3, 13 / 5}];
test[canonicalDomainBasisPrivate, {2, 3, 11 / 5, 13 / 5}, {2, 3, 11 / 5, 13 / 5}];
test[canonicalDomainBasisPrivate, {2, 3, 11 / 7, 13 / 7}, {2, 3, 11 / 7, 13 / 7}];
test[canonicalDomainBasisPrivate, {2, 7 / 5, 11 / 5, 13 / 5}, {2, 7 / 5, 11 / 5, 13 / 5}];
test[canonicalDomainBasisPrivate, {1}, {1}];
test[canonicalDomainBasisPrivate, {0}, {1}];


(* ::Subsubsection::Closed:: *)
(*private*)


test[getBasisA, {{{11, 35, 31}}, "row", {2, 9, 7}}, {{{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 0, 1}}, "col"}];


test[getStandardPrimeLimitDomainBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];


test[isStandardPrimeLimitDomainBasis, {2, 3, 5, 7, 11}, True];
test[isStandardPrimeLimitDomainBasis, {2, 3, 7, 5, 11}, True];
test[isStandardPrimeLimitDomainBasis, {2, 3, 5, 9, 11}, False];


test[getDomainBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];
test[getDomainBasis, {{{11, 35, 31}}, "row", {2, 9, 7}}, {2, 9, 7}];


test[getDomainBasisDimension, {2, 9, 7}, 4];
test[getDomainBasisDimension, {1}, 1];


(* returns the supergroup, when one is a subgroup of the other *)
test[domainBasisMerge, {2, 3, 5}, {2, 9, 5}, {2, 3, 5}];

(* basically works *)
test[domainBasisMerge, {2, 3, 5}, {2, 9, 7}, {2, 3, 5, 7}];

(* can handle more than two interval bases at once *)
test[domainBasisMerge, {2, 3, 5}, {2, 9, 7}, {2, 5 / 7, 11}, {2, 3, 5, 7, 11}];
test[domainBasisMerge, {4}, {16}, {4}];
test[domainBasisMerge, {25 / 9}, {5 / 3}, {5 / 3}];

(* edge case *)
test[domainBasisMerge, {1}, {1}, {1}];
test[domainBasisMerge, {2, 3, 5}, {2, 3, 5}, {2, 3, 5}];


test[domainBasisIntersection, {2, 3, 5}, {2, 9, 5}, {2, 9, 5}];
test[domainBasisIntersection, {2, 5 / 3, 9 / 7}, {2, 9, 5}, {2, 25 / 9}];
test[domainBasisIntersection, {2, 5 / 3}, {2, 9, 5}, {2, 25 / 9}];
test[domainBasisIntersection, {2, 25 / 9}, {2, 9, 5}, {2, 25 / 9}];
test[domainBasisIntersection, {2, 3, 5, 7}, {2, 3, 5}, {2, 5, 7}, {2, 5}];
test[domainBasisIntersection, {2, 3}, {10, 15}, {3 / 2}];
test[domainBasisIntersection, {2, 5 / 3}, {2, 3, 5}, {2, 5 / 3}];
test[domainBasisIntersection, {2, 9 / 5}, {2, 9, 5}, {2, 9 / 5}];
test[domainBasisIntersection, {2, 3, 5}, {2, 3, 5}, {2, 3, 5}];
test[domainBasisIntersection, {2, 9, 7 / 5}, {2, 3, 7 / 5}, {2, 9, 7 / 5}];
test[domainBasisIntersection, {4}, {8}, {64}];
test[domainBasisIntersection, {9}, {27}, {729}];
test[domainBasisIntersection, {2}, {3}, {1}];
test[domainBasisIntersection, {5}, {15}, {1}];
test[domainBasisIntersection, {4}, {18}, {1}];
test[domainBasisIntersection, {2}, {2}, {2}];
test[domainBasisIntersection, {4}, {4}, {4}];
test[domainBasisIntersection, {6}, {6}, {6}];
test[domainBasisIntersection, {12}, {12}, {12}];
test[domainBasisIntersection, {16, 18, 15}, {4, 18, 5}, {16, 18, 2500}];
test[domainBasisIntersection, {4, 18}, {8, 18}, {64, 18}];
test[domainBasisIntersection, {16, 18}, {16, 18}, {16, 18}];
test[domainBasisIntersection, {4, 18, 5}, {8, 18, 7}, {64, 18}];


test[isSubspaceOf, {2, 9, 5}, {2, 3, 5}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 9, 5}, False];
test[isSubspaceOf, {2, 3, 5, 7}, {2, 3, 5}, False];
test[isSubspaceOf, {4}, {2}, True];
test[isSubspaceOf, {8}, {4}, False];
test[isSubspaceOf, {16}, {4}, True];
test[isSubspaceOf, {3, 5, 7}, {2, 11, 13}, False];
test[isSubspaceOf, {2, 3, 5}, {2, 3, 7}, False];
test[isSubspaceOf, {2, 3, 7}, {2, 3, 5}, False];
test[isSubspaceOf, {2, 5 / 3, 7}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 5 / 3, 7 / 5}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 7 / 5}, {2, 5, 7}, True];
test[isSubspaceOf, {2, 5, 7}, {2, 7 / 5}, False];
test[isSubspaceOf, {2, 105, 11}, {2, 15, 7, 11}, True];
test[isSubspaceOf, {2, 25 / 9, 11 / 7}, {2, 5 / 3, 7, 11}, True];
test[isSubspaceOf, {2, 3 / 2, 5 / 2, 5 / 3}, {2, 3, 5}, True];
test[isSubspaceOf, {2, 9 / 5, 3}, {2, 3, 5}, True];


test[changeDomainBasisForM, {{{12, 19, 28}}, "row"}, {2, 3, 5, 7}, Error];
t = {{{22, 35, 51, 76}}, "row", {2, 3, 5, 11}};
targetSubspaceB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "row", {2, 9, 11}};
test[changeDomainBasisForM, t, targetSubspaceB, expectedT];
test[changeDomainBasisForM, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];


test[changeDomainBasisForC, {{{4, -4, 1}}, "col"}, {2, 9, 7}, Error];
t = {{{0, 1, 0}, {0, -2, 1}}, "col", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "col"}; (*{{{0,2,0,-1},{0,-5,1,2}},"col"}, before canonicalization *)
test[changeDomainBasisForC, t, targetB, expectedT];
test[changeDomainBasisForC, {{{1}}, "col", {27}}, {9}, Error];
test[changeDomainBasisForC, {{{1}}, "col", {81}}, {9}, {{{1}}, "col", {9}}];
test[changeDomainBasisForC, {{{4, -4, 1}}, "col"}, {2, 3, 5}, {{{4, -4, 1}}, "col"}];


test[getDomainBasisChangeForM, {2, 3, 5, 7}, {2, 3, 5}, {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}];
test[getDomainBasisChangeForM, {2, 3, 7}, {2, 9, 7}, {{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}];
test[getDomainBasisChangeForM, {2, 3, 5, 7}, {2, 9 / 7, 5 / 3}, {{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}];


test[getDomainBasisChangeForC, {2, 3, 5}, {2, 3, 5, 7}, {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}];
test[getDomainBasisChangeForC, {2, 9, 7}, {2, 3, 7}, {{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}];
test[getDomainBasisChangeForC, {2, 9 / 7, 5 / 3}, {2, 3, 5, 7}, {{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}];


test[signsMatch, 3, 5, True];
test[signsMatch, -3, -5, True];
test[signsMatch, -3, 5, False];
test[signsMatch, 3, -5, False];
test[signsMatch, 3, 0, True];
test[signsMatch, 0, 5, True];
test[signsMatch, -3, 0, True];
test[signsMatch, 0, - 5, True];


test[isNumeratorFactor, {1, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {2, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {2, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 2, 0}, False];
test[isNumeratorFactor, {1, 0, 0}, {0, 0, 1}, False];


test[isDenominatorFactor, {1, 0, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {0, 1, 0}, True];


test[changeDomainBasisPrivate, {{{12, 19, 28}}, "row"}, {2, 3, 5, 7}, Error];

t = {{{22, 35, 51, 76}}, "row", {2, 3, 5, 11}};
targetSubspaceB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "row", {2, 9, 11}};
test[changeDomainBasisPrivate, t, targetSubspaceB, expectedT];

test[changeDomainBasisPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];

test[changeDomainBasisPrivate, {{{4, -4, 1}}, "col"}, {2, 9, 7}, Error];

t = {{{0, 1, 0}, {0, -2, 1}}, "col", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "col"};
test[changeDomainBasisPrivate, t, targetB, expectedT];

test[changeDomainBasisPrivate, {{{1}}, "col", {27}}, {9}, Error];
test[changeDomainBasisPrivate, {{{1}}, "col", {81}}, {9}, {{{1}}, "col", {9}}];
test[changeDomainBasisPrivate, {{{4, -4, 1}}, "col"}, {2, 3, 5}, {{{4, -4, 1}}, "col"}];


verifyDuals[{{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, {{{-10, 1, 3}}, "col", {2, 3, 7}}];


(* ::Subsection::Closed:: *)
(*addition*)


meantoneC = "[4 -4 1\:27e9";
porcupineC = "[1 -5 3\:27e9";
meantoneM = "[\:27e81 0 -4] \:27e80 1 4]}";
porcupineM = "[\:27e81 2 3] \:27e80 3 5]}";


format = "EBK";


test[sum, meantoneC, porcupineC, "[5 -9 4\:27e9"];
test[sum, meantoneM, porcupineM, "[\:27e81 1 1] \:27e80 4 9]}"];


test[diff, meantoneC, porcupineC, "[-3 -1 2\:27e9"];
test[diff, meantoneM, porcupineM, "[\:27e81 1 2] \:27e80 2 1]}"];


format = "Wolfram";


(* ::Subsubsection::Closed:: *)
(*addable mappings*)


meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "row"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "row"};
test[sumPrivate, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "row"}];
test[diffPrivate, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "row"}];
meantoneC = {{{4, -4, 1}}, "col"};
porcupineC = {{{1, -5, 3}}, "col"};
test[sumPrivate, meantoneC, porcupineC, {{{5, -9, 4}}, "col"}];
test[diffPrivate, meantoneC, porcupineC, {{{-3, -1, 2}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*addable comma bases*)


et7M = {{{7, 11, 16}}, "row"};
et5M = {{{5, 8, 12}}, "row"};
test[sumPrivate, et7M, et5M, {{{12, 19, 28}}, "row"}];
test[diffPrivate, et7M, et5M, {{{2, 3, 4}}, "row"}];
et7C = dualPrivate[et7M];
et5C = dualPrivate[et5M];
test[sumPrivate, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "col"}];
test[diffPrivate, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*not addable - error!*)


septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "row"};
test[sumPrivate, septimalMeantoneM, septimalBlackwoodM, Error];
test[diffPrivate, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dualPrivate[septimalMeantoneM];
septimalBlackwoodC = dualPrivate[septimalBlackwoodM];
test[sumPrivate, septimalMeantoneC, septimalBlackwoodC, Error];
test[diffPrivate, septimalMeantoneC, septimalBlackwoodC, Error];


(* ::Subsubsection::Closed:: *)
(*addable - linear-dependence-2 (comma bases)*)


et12M = {{{12, 19, 28, 34}}, "row"};
et19M = {{{19, 30, 44, 53}}, "row"};
test[sumPrivate, et12M, et19M, {{{31, 49, 72, 87}}, "row"}];
test[diffPrivate, et12M, et19M, {{{7, 11, 16, 19}}, "row"}];
et12C = dualPrivate[et12M];
et19C = dualPrivate[et19M];
test[sumPrivate, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "col"}];
test[diffPrivate, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*examples with themselves*)


test[sumPrivate, meantoneM, meantoneM, meantoneM];
test[diffPrivate, meantoneM, meantoneM, Error];
test[sumPrivate, meantoneC, meantoneC, meantoneC];
test[diffPrivate, meantoneC, meantoneC, Error];
test[sumPrivate, et7M, et7M, et7M];
test[diffPrivate, et7M, et7M, Error];
test[sumPrivate, et7C, et7C, et7C];
test[diffPrivate, et7C, et7C, Error];


(* ::Subsubsection::Closed:: *)
(*mismatched r & n but matching d*)


test[sumPrivate, et7M, meantoneM, Error];
test[diffPrivate, et7M, meantoneM, Error];
test[sumPrivate, et7C, meantoneC, Error];
test[diffPrivate, et7C, meantoneC, Error];


(* ::Subsubsection::Closed:: *)
(*mismatched d but matching r or n*)


test[sumPrivate, et7M, et12M, Error];
test[diffPrivate, et7M, et12M, Error];
test[sumPrivate, et7C, et12C, Error];
test[diffPrivate, et7C, et12C, Error];


(* ::Subsubsection::Closed:: *)
(*some basic examples*)


augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "row"}; (* \:27e8\:27e83 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "row"}; (* \:27e8\:27e84 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "row"}; (* \:27e8\:27e84 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "row"}; (* \:27e8\:27e82 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "row"}; (* \:27e8\:27e82 -4 -11]] *)
test[sumPrivate, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "row"}]; (* \:27e8\:27e83 0 -7]] + \:27e8\:27e84 4 -3]] = \:27e8\:27e87 4 -10]]*)
test[diffPrivate, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "row"}];(* \:27e8\:27e83 0 -7]] - \:27e8\:27e84 4 -3]] = \:27e8\:27e81 4 4]]*)
test[sumPrivate, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "row"}] ;(* \:27e8\:27e83 0 -7]] + \:27e8\:27e84 9 5]] = \:27e8\:27e87 9 -2]]*)
test[diffPrivate, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e84 9 5]] = \:27e8\:27e81 9 12]]*)
test[sumPrivate, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "row"}] ;(* \:27e8\:27e83 0 -7]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e85 1 -10]]*)
test[diffPrivate, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "row"}] ;(* \:27e8\:27e83 0 -7]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e81 -1 -4]]*)
test[sumPrivate, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "row"}] ;(* \:27e8\:27e83 0 -7]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e85 -4 -18]]*)
test[diffPrivate, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e81 4 4]]*)
test[sumPrivate, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "row"}]; (* \:27e8\:27e84 4 -3]] + \:27e8\:27e84 9 5]] = \:27e8\:27e88 13 2]]*)
test[diffPrivate, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "row"}]; (* \:27e8\:27e84 4 -3]] - \:27e8\:27e84 9 5]] = \:27e8\:27e80 5 8]]*)
test[sumPrivate, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "row"}];(* \:27e8\:27e84 4 -3]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e86 5 -6]]*)
test[diffPrivate, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "row"}]; (* \:27e8\:27e84 4 -3]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e82 3 0]]*)
test[sumPrivate, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "row"}]; (* \:27e8\:27e84 4 -3]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e86 0 -14]] \[RightArrow] \:27e8\:27e83 0 -7]] *)
test[diffPrivate, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* \:27e8\:27e84 4 -3]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e82 8 8]] \[RightArrow] \:27e8\:27e81 4 4]] *)
test[sumPrivate, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "row"}]; (* \:27e8\:27e84 9 5]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e86 10 2]] \[RightArrow] \:27e8\:27e83 5 1]] *)
test[diffPrivate, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* \:27e8\:27e84 9 5]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e82 8 8]] \[RightArrow] \:27e8\:27e81 4 4]] *)
test[sumPrivate, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "row"}]; (* \:27e8\:27e84 9 5]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e86 5 -6]] *)
test[diffPrivate, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "row"}];  (* \:27e8\:27e84 9 5]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e82 13 16]] *)
test[sumPrivate, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "row"}]; (* \:27e8\:27e82 1 -3]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e84 -3 -14]] *)
test[diffPrivate, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "row"}]; (* \:27e8\:27e82 1 -3]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e80 5 8]] *)


(* ::Subsubsection::Closed:: *)
(*example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2*)


t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "row"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "row"};
test[sumPrivate, t1, t2, Error];
test[diffPrivate, t1, t2, Error];


(* ::Subsubsection::Closed:: *)
(*example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1*)


t1 = {{{2, 3}}, "col"};
t2 = {{{4, -7}}, "row"};
tSum = {{{9, 7}}, "col"};
tDiff = {{{5, 1}}, "col"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];


(* ::Subsubsection::Closed:: *)
(*example demonstrating how it's important to canonicalize*)


t1 = {{{-2, 4, -2}}, "row"};
t2 = {{{7, 7, 0}}, "row"};
tSum = {{{2, -1, 1}}, "row"};
tDiff = {{{0, 3, -1}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];


(* ::Subsubsection::Closed:: *)
(*example demonstrating how mixed variance inputs are accepted, but the first variance matches the output*)


t1 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
t2 = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "row"};
tSum = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[sumPrivate, dualPrivate[t1], t2, dualPrivate[tSum]];
test[sumPrivate, t1, dualPrivate[t2], tSum];
test[sumPrivate, dualPrivate[t1], dualPrivate[t2], dualPrivate[tSum]];


(* ::Subsubsection::Closed:: *)
(*an example that used to fail for whatever reason, "some problem"*)


test[sumPrivate, {{{1, 2, -1, 1}, {0, 18, -2, -1}}, "row"}, {{{2, 0, -2, 5}, {0, 3, -1, 4}}, "row"}, {{{1, 19, -4, 7}, {0, 24, -4, 7}}, "row"}];


(* ::Subsubsection::Closed:: *)
(*another example that used to fail for whatever reason, "goddam failing mysteries"*)


test[sumPrivate, {{{3, 2, 8, 2}, {0, 5, 31, 10}}, "row"}, {{{1, 22, 32, 0}, {0, 32, 44, -1}}, "row"}, {{{1, 32, 94, 20}, {0, 47, 137, 29}}, "row"}];


(* ::Subsubsection::Closed:: *)
(*another example that used to fail for whatever reason, "more stuff to sort out"*)


test[sumPrivate, {{{5, 0, 1, 0}, {-16, 1, 0, 3}}, "col"}, {{{4, 0, 1, 0}, {-3, 1, 0, 3}}, "col"}, {{{9, 0, 2, 0}, {-5, 1, 1, 3}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors*)


test[sumPrivate, {{{3, 8, -4, -6}}, "row"}, {{{9, 2, -4, 1}}, "row"}, {{{12, 10, -8, -5}}, "row"}];


(* ::Subsubsection::Closed:: *)
(*LA only: example that was intractable unless I defactored piecemeal*)


test[sumPrivate, {{{-97, 73, 45, 16}}, "col"}, {{{-1, 8, 9, 3}}, "col"}, {{{-98, 81, 54, 19}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: example that motivated the existence of the special min-grade-1 path... which no longer exists, but I'll keep this around anyway*)


test[sumPrivate, {{{2, 0, 3}}, "col"}, {{{5, 4, 0}}, "col"}, {{{7, 4, 3}}, "col"}];
test[diffPrivate, {{{2, 0, 3}}, "col"}, {{{5, 4, 0}}, "col"}, {{{-3, -4, 3}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: non-min-grade-1*)


septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "row"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "row"};
meanmagM = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "row"};
test[sumPrivate, septimalMeantoneM, flattoneM, godzillaM];
test[diffPrivate, septimalMeantoneM, flattoneM, meanmagM];


(* ::Subsubsection::Closed:: *)
(*LA only: ensure the largestMinorsL are consulted so that the sum and diff are identified correctly*)


t1 = {{{0, 1, 4}}, "row"};
t2 = {{{5, -6, -2}}, "row"};
tSum = {{{5, -5, 2}}, "row"};
tDiff = {{{5, -7, -6}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];


(* ::Subsubsection::Closed:: *)
(*LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works*)


t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "col"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "col"};
test[sumPrivate, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "col"}];
test[diffPrivate, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)


t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "row"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "row"};
test[sumPrivate, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "row"}];
test[diffPrivate, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "row"}];


(* ::Subsubsection::Closed:: *)
(*LA only: example where the first vectors of the input were not actually linearly independent from the basis for the linearly dependent vectors, things would fail, so now we actually test each one to ensure it's linearly independent before adding it into the initial matrix to be defactored*)


test[sumPrivate, {{{-17, -55, 24, 34}}, "col"}, {{{-1, -7, 0, 2}}, "col"}, {{{-9, -31, 12, 18}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: an example that used to fail for whatever reason, the "languisher"*)


test[sumPrivate, {{{23, -14, 3, 0}, {9, -5, 1, 1}}, "col"}, {{{1, 7, 3, -1}, {0, 25, 14, -1}}, "row"}, {{{23, -14, 14, 0}, {9, -5, 5, 1}}, "col"}];


(* ::Subsubsection::Closed:: *)
(*LA only: an example that used to fail for whatever reason, the "big random"*)


test[sumPrivate, {{{-89, -46, 61, 0, 0}, {-85, -44, 59, 1, 0}, {-39, -21, 26, 0, 1}}, "col"}, {{{-16, -9, 1, 0, 0}, {10, 4, 0, 1, 0}, {16, 8, 0, 0, 1}}, "col"}, Error];


(* ::Subsubsection::Closed:: *)
(*across domain basis - error*)


test[sumPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, Error];


(* ::Subsection::Closed:: *)
(*exterior algebra*)


(* ::Subsubsection::Closed:: *)
(*dimensions*)


test[eaGetD, {{1, 4, 4}, 2, "row"}, 3];


test[eaGetR, {{1, 4, 4}, 2, "row"}, 2];


test[eaGetN, {{1, 4, 4}, 2, "row"}, 1];


(* ::Subsubsection::Closed:: *)
(*canonicalization*)


canonicalMc = {{107, -87, 72, -49, 31}, 4, "col"};
negatedCanonicalMc = {{-107, 87, -72, 49, -31}, 4, "col"};
canonicalMm = {{31, 49, 72, 87, 107}, 1, "row"};
negatedCanonicalMm = {{-31, -49, - 72, -87, -107}, 1, "row"};

test[eaCanonicalForm, canonicalMc, canonicalMc];
test[eaCanonicalForm, negatedCanonicalMc, canonicalMc];
test[eaCanonicalForm, canonicalMm, canonicalMm];
test[eaCanonicalForm, negatedCanonicalMm, canonicalMm];

test[eaCanonicalForm, {{4}, 0, "row", 3}, {{1}, 0, "row", 3}];
test[eaCanonicalForm, {{2, -4, 8, -9, 7, 2}, 2, "row"}, Error];
test[eaCanonicalForm, {{1, 0, 1}, 2, "row"}, {{1, 0, 1}, 2, "row"}];
test[eaCanonicalForm, {{0, 0, 0, 0, 0, 0}, 2, "row"}, {{0, 0, 0, 0, 0, 0}, 2, "row"}];


(* ::Subsubsection::Closed:: *)
(*dual*)


test[eaDual, canonicalMc, canonicalMm];
test[eaDual, negatedCanonicalMc, canonicalMm];
test[eaDual, canonicalMm, canonicalMc];
test[eaDual, negatedCanonicalMm, canonicalMc];


test[eaDual, {{1}, 0, "col", 3}, {{1}, 3, "row"}];
test[eaDual, {{1}, 0, "row", 5}, {{1}, 5, "col"}];
test[eaDual, {{2, -4, 8, -9, 7, 2}, 2, "row"}, Error];
test[eaDual, {{1, 0, 1}, 2, "row"}, {{1, 0, 1}, 1, "col"}];


eaDualTester[multimap_, multicomma_] := Module[{},
  If[
    TrueQ[eaDual[multimap] == multicomma] && TrueQ[eaDual[multicomma] == multimap],
    passes += 1,
    failures += 1;
    printWrapper["eaDualTester[", multimap, ", ", multicomma, "]; actual dual multimap: ", eaDual[multicomma], " and dual multicomma: ", eaDual[multimap]]
  ];
];
eaDualTester[{{1, 4, 4}, 2, "row"}, {{4, -4, 1}, 1, "col"}];


randomTandU[] := Module[{d, grade, ma, t, u},
  d = RandomInteger[{1, 5}];
  grade = RandomInteger[{1, d}];
  ma = RandomInteger[{-9, 9}, {grade, d}];
  
  t = If[RandomInteger[] == 1, {ma, "col"}, {ma, "row"}];
  u = matrixToMultivector[t];
  
  {t, u}
];


Do[
  u = Last[randomTandU[]];
  
  dualU = eaDual[u];
  doubleDualU = eaDual[dualU];
  
  If[
    TrueQ[doubleDualU == u],
    passes += 1,
    failures += 1;
    printWrapper["BAD BAD BAD! multivector: ", u, " computed dual: ", dualU, " and then back: ", doubleDualU]
  ],
  100
];


test[uToTensor, {{1, 4, 4}, 2, "row"}, Symmetrize[{{0, 1, 4}, {-1, 0, 4}, {-4, -4, 0}}, Antisymmetric[{1, 2}]]];


tensorToUTester[{largestMinorsL_, variance_, grade_, d_}] := {largestMinorsL, variance, grade, d} == Module[{},
  If[
    tensorToU[uToTensor[{largestMinorsL, variance, grade, d}], variance, grade, d],
    passes += 1,
    failures += 1;
    printWrapper["tensorToUTester[", {largestMinorsL, variance, grade, d}, "]"]
  ]
];
tensorToUTester[{{1, 4, 4}, 2, "row"}];
tensorToUTester[{{0, 0, 0}, 2, "row"}];


(* ::Subsubsection::Closed:: *)
(*conversion to and from matrix*)


test[multivectorToMatrix, {{1}, 0, "col", 1}, {{{0}}, "col"}];
test[multivectorToMatrix, {{1}, 0, "row", 1}, {{{0}}, "row"}];
test[multivectorToMatrix, {{1}, 0, "col", 3}, {{{0, 0, 0}}, "col"}];
test[multivectorToMatrix, {{1}, 0, "row", 3}, {{{0, 0, 0}}, "row"}];
test[multivectorToMatrix, {{2, -4, 8, -9, 7, 2}, 2, "row"}, Error];
test[multivectorToMatrix, {{0, 0, 0, 0, 0}, 4, "row"}, Error]; (* no equivalent to all-zero multivectors in LA *)


test[matrixToMultivector, {{{0}}, "col"}, {{1}, 0, "col", 1}];
test[matrixToMultivector, {{{0}}, "row"}, {{1}, 0, "row", 1}];
test[matrixToMultivector, {{{0, 0, 0}}, "col"}, {{1}, 0, "col", 3}];
test[matrixToMultivector, {{{0, 0, 0}}, "row"}, {{1}, 0, "row", 3}];
test[matrixToMultivector, {IdentityMatrix[2], "row"}, {{1}, 2, "row"}];
test[matrixToMultivector, {{{1, 1}}, "row"}, {{1, 1}, 1, "row"}];


(* ::Text:: *)
(*multivectorToMatrix & matrixToMultivector: by dimensionality*)


testMultivectorMatrixConversion[u_, t_] := Module[{convertedU, convertedT},
  convertedT = multivectorToMatrix[u];
  convertedU = matrixToMultivector[t];
  
  If[
    TrueQ[convertedT == t] && TrueQ[convertedU == u],
    passes += 1,
    failures += 1;
    printWrapper["testMultivectorMatrixConversion[]; convertedT: ", convertedT, " t: ", t, " convertedU: ", convertedU, " u: ", u]]
];


(* multivectorToMatrix & matrixToMultivector: dimensionality 1 *)

testMultivectorMatrixConversion[{{1}, 1, "row"}, {IdentityMatrix[1], "row"}];
testMultivectorMatrixConversion[{{1}, 0, "col", 1}, {{{0}}, "col"}];

testMultivectorMatrixConversion[{{1}, 0, "row", 1}, {{{0}}, "row"}];
testMultivectorMatrixConversion[{{1}, 1, "col"}, {IdentityMatrix[1], "col"}];


(* multivectorToMatrix & matrixToMultivector: dimensionality 2 *)

testMultivectorMatrixConversion[{{1}, 2, "row"}, {IdentityMatrix[2], "row"}];
testMultivectorMatrixConversion[{{1}, 0, "col", 2}, {{{0, 0}}, "col"}];

testMultivectorMatrixConversion[{{12, 19}, 1, "row"}, {{{12, 19}}, "row"}];
testMultivectorMatrixConversion[{{-19, 12}, 1, "col"}, {{{-19, 12}}, "col"}];

testMultivectorMatrixConversion[{{1}, 0, "row", 2}, {{{0, 0}}, "row"}];
testMultivectorMatrixConversion[{{1}, 2, "col"}, {IdentityMatrix[2], "col"}];


(* multivectorToMatrix & matrixToMultivector: dimensionality 3 *)

testMultivectorMatrixConversion[{{1}, 3, "row"}, {IdentityMatrix[3], "row"}];
testMultivectorMatrixConversion[{{1}, 0, "col", 3}, {{{0, 0, 0}}, "col"}];

testMultivectorMatrixConversion[{{1, 4, 4}, 2, "row"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];
testMultivectorMatrixConversion[{{4, -4, 1}, 1, "col"}, {{{4, -4, 1}}, "col"}];

testMultivectorMatrixConversion[{{19, 30, 44}, 1, "row"}, {{{19, 30, 44}}, "row"}];
testMultivectorMatrixConversion[{{44, -30, 19}, 2, "col"}, {{{-30, 19, 0}, {-26, 15, 1}}, "col"}];

testMultivectorMatrixConversion[{{1}, 0, "row", 3}, {{{0, 0, 0}}, "row"}];
testMultivectorMatrixConversion[{{1}, 3, "col"}, {IdentityMatrix[3], "col"}];


(* multivectorToMatrix & matrixToMultivector: dimensionality 4 *)

testMultivectorMatrixConversion[{{1}, 4, "row"}, {IdentityMatrix[4], "row"}];
testMultivectorMatrixConversion[{{1}, 0, "col", 4}, {{{0, 0, 0, 0}}, "col"}];

testMultivectorMatrixConversion[{{1, 0, 2, 6}, 3, "row"}, {{{1, 0, 0, 6}, {0, 1, 0, -2}, {0, 0, 1, 0}}, "row"}];
testMultivectorMatrixConversion[{{-6, 2, 0, 1}, 1, "col"}, {{{-6, 2, 0, 1}}, "col"}];

testMultivectorMatrixConversion[{{1, 4, 10, 4, 13, 12}, 2, "row"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"}];
testMultivectorMatrixConversion[{{12, -13, 4, 10, -4, 1}, 2, "col"}, {{{4, -4, 1, 0}, {13, -10, 0, 1}}, "col"}];

testMultivectorMatrixConversion[{{31, 49, 72, 87}, 1, "row"}, {{{31, 49, 72, 87}}, "row"}];
testMultivectorMatrixConversion[{{-87, 72, -49, 31}, 3, "col"}, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "col"}];

testMultivectorMatrixConversion[{{1}, 0, "row", 4}, {{{0, 0, 0, 0}}, "row"}];
testMultivectorMatrixConversion[{{1}, 4, "col"}, {IdentityMatrix[4], "col"}];


(* multivectorToMatrix & matrixToMultivector: dimensionality 5 *)

testMultivectorMatrixConversion[{{1}, 5, "row"}, {IdentityMatrix[5], "row"}];
testMultivectorMatrixConversion[{{1}, 0, "col", 5}, {{{0, 0, 0, 0, 0}}, "col"}];

testMultivectorMatrixConversion[{{6, 0, 0, 3, -16}, 4, "row"}, {{{3, 0, 0, 0, 8}, {0, 2, 0, 0, 1}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, "row"}];
testMultivectorMatrixConversion[{{-16, -3, 0, 0, 6}, 1, "col"}, {{{-16, -3, 0, 0, 6}}, "col"}];

testMultivectorMatrixConversion[{{4, -4, 0, -6, 2, -2, 11, 17, -17, -31}, 3, "row"}, {{{2, 1, 0, 7, 8}, {0, 2, 0, 3, -1}, {0, 0, 1, -1, 0}}, "row"}];
testMultivectorMatrixConversion[{{-31, 17, 17, -11, -2, -2, -6, 0, 4, 4}, 2, "col"}, {{{-11, -6, 4, 4, 0}, {-7, -1, 1, 1, 1}}, "col"}];

testMultivectorMatrixConversion[{{2, -16, -28, 5, -30, -50, 1, -20, 67, 111}, 2, "row"}, {{{1, 1, 7, 11, 2}, {0, 2, -16, -28, 5}}, "row"}];
testMultivectorMatrixConversion[{{111, -67, -20, 1, 50, -30, -5, -28, 16, 2}, 3, "col"}, {{{-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {1, -5, 0, 0, 2}}, "col"}];

testMultivectorMatrixConversion[{{72, 114, 167, 202, 249}, 1, "row"}, {{{72, 114, 167, 202, 249}}, "row"}];
testMultivectorMatrixConversion[{{249, -202, 167, -114, 72}, 4, "col"}, {{{-19, 12, 0, 0, 0}, {-25, 7, 6, 0, 0}, {-20, 5, 4, 1, 0}, {-12, 1, 3, 0, 1}}, "col"}];

testMultivectorMatrixConversion[{{1}, 0, "row", 5}, {{{0, 0, 0, 0, 0}}, "row"}];
testMultivectorMatrixConversion[{{1}, 5, "col"}, {IdentityMatrix[5], "col"}];


(* ::Text:: *)
(*multivectorToMatrix & matrixToMultivector: random*)


Do[
  tAndU = randomTandU[];
  t = First[tAndU];
  u = Last[tAndU];
  
  uAndBackToT = multivectorToMatrix[u];
  
  If[
    TrueQ[uAndBackToT == canonicalFormPrivate[t]],
    passes += 1,
    failures += 1;
    printWrapper["BAD BAD BAD! (following all in canonical form) matrix: ", canonicalFormPrivate[t], " computed equiv multivector: ", u, " and then back to matrix: ", uAndBackToT]
  ],
  100
];


(* ::Text:: *)
(*multivectorToMatrix & matrixToMultivector: one-off*)


testMatrix[t_] := If[
  TrueQ[canonicalFormPrivate[t] == multivectorToMatrix[matrixToMultivector[t]]],
  passes += 1,
  failures += 1;
  printWrapper["testMatrix[]", multivectorToMatrix[matrixToMultivector[t]]]
];
testMultivector[u_] := If[
  TrueQ[eaCanonicalForm[u] == matrixToMultivector[multivectorToMatrix[u]]],
  passes += 1,
  failures += 1;
  printWrapper["testMultivector[]", matrixToMultivector[multivectorToMatrix[u]]]
];


testMatrix[{{{-4, -8, 3, 7, -1, -3}, {1, -2, -2, 4, 4, -6}, {2, -9, 9, -8, 0, 7}, {5, -5, 4, -8, 5, -6}, {9, 0, 2, 8, -4, -3}}, "col"}];


testMultivector[{{2, 8, 8}, 2, "row"}];
testMultivector[{{0, 0, 3, 4}, 3, "col"}];
testMultivector[{{1, 0, 1}, 2, "row"}];


(* ::Subsubsection::Closed:: *)
(*merging*)


(* ::Text:: *)
(*d =2, mm*)


d2g1co1 = {{12, 19}, 1, "row"};
d2g1co2 = {{19, 30}, 1, "row"};
d2jiCo = {{1}, 2, "row"};


(* ::Text:: *)
(*d=3, mm*)


d3g1co1 = {{12, 19, 28}, 1, "row"};
d3g1co2 = {{19, 30, 44}, 1, "row"};
d3g1co3 = {{22, 35, 51}, 1, "row"};
d3g2co1 = {{1, 4, 4}, 2, "row"};
d3g2co2 = {{3, 5, 1}, 2, "row"};
d3jiCo = {{1}, 3, "row"};
d3unisonCo = {{1}, 0, "row", 3};


(* ::Text:: *)
(*d=3, mc*)


d3g1contra1 = {{4, -4, 1}, 1, "col"};
d3g1contra2 = {{-10, -1, 5}, 1, "col"};
d3g1contra3 = {{1, -5, 3}, 1, "col"};
d3g2contra1 = {{44, -30, 19}, 2, "col"};
d3g2contra2 = {{28, -19, 12}, 2, "col"};
d3g2contra3 = {{51, -35, 22}, 2, "col"};
d5g3contra = {{19, -33, 14, -46, 46, -46, 29, -29, 29, 0}, 3, "col"};
d3jiContra = {{1}, 0, "col", 3};
d3unisonContra = {{1}, 3, "col"};


(* ::Text:: *)
(*d = 5, mm*)


d5g1co = {{31, 49, 72, 87, 107}, 1, "row"};
d5g2co1 = {{-9, -5, 3, -7, 13, 30, 20, 21, 1, -30}, 2, "row"}; (*progressiveProduct[{{15, 24, 35, 42, 52}, 1, "row"}, {{16, 25, 37, 45, 55}, 1, "row"}];*)
d5g2co2 = {{1, 4, -2, -6, 4, -6, -13, -16, -28, -10}, 2, "row"}; (* progressiveProduct[{{12, 19, 28, 34, 42}, 1, "row"}, {{17, 27, 40, 48, 59}, 1, "row"}]; *)
d5g2co3 = {{6, -7, -2, 15, -25, -20, 3, 15, 59, 49}, 2, "row"}; (*example from interior product page *)
d5g3co = {{1, 2, -3, -2, 1, -4, -5, 12, 9, -19}, 3, "row"};(*example from interior product page *)
d5g4co = {{1, 2, 1, 2, 3}, 4, "row"};(*example from interior product page *)
d5unisonCo = {{1}, 0, "row", 5};


(* ::Text:: *)
(*d=5, mc*)


d5g1contra = {{-3, 2, -1, 2, -1}, 1, "col"};
d5g2contra = {{5, 11, -7, -4, -9, 8, 1, 5, -5, 5}, 2, "col"};
d5jiContra = {{1}, 0, "col", 5};


(* ::Text:: *)
(*super basic progressive product example*)


test[progressiveProduct, d2g1co1, d2g1co2, d2jiCo];


(* ::Text:: *)
(*wedging with oneself equals a zero varianced multivector*)


test[progressiveProduct, d3g1co1, d3g1co1, {{0, 0, 0}, 2, "row"}];


(* ::Text:: *)
(*another basic progressive product example*)


test[progressiveProduct, d5g2co1, d5g2co2, d5g4co];


(* ::Text:: *)
(*show how progressive product can cap out when grade exactly hits the dimensionality, for mc*)


test[progressiveProduct, d3g2contra1, d3g1contra3, d3unisonContra];


(* ::Text:: *)
(*show how progressive product can cap out when grade exceeds at the dimensionality, for mc*)


test[progressiveProduct, d3g2contra1, d3g2contra2, Error];


(* ::Text:: *)
(*show how progressive product can cap out when grade exactly hits the dimensionality, for mm*)


test[progressiveProduct, d3g2co1, d3g1co3, d3jiCo];


(* ::Text:: *)
(*show how progressive product can cap out when grade exceeds the dimensionality, for mm*)


test[progressiveProduct, d3g2co1, d3g2co2, Error];


(* ::Text:: *)
(*a basic regressive product example*)


test[regressiveProduct, d3g2contra1, d3g2contra2, d3g1contra1];


(* ::Text:: *)
(*show how regressive product can cap out when grade hits exactly 0, for mc*)


test[regressiveProduct, d3g1contra1, d3g2contra3, d3jiContra];


(* ::Text:: *)
(*show how regressive product can cap out when grade goes below 0, for mc*)


test[regressiveProduct, d3g1contra1, d3g1contra2, Error];


(* ::Text:: *)
(*show how regressive product can cap out when grade hits exactly 0, for mm*)


test[regressiveProduct, d3g1co1, d3g2co2, d3unisonCo];


(* ::Text:: *)
(*show how regressive product can cap out when grade goes below 0, for mm*)


test[regressiveProduct, d3g1co1, d3g1co2, Error];


(* ::Text:: *)
(*a series of examples working up to the symmetric interior product*)


test[rightInteriorProduct, d5g1contra, d5g3co, Error];
test[rightInteriorProduct, d5g3co, d5g1contra, d5g2co3];


test[leftInteriorProduct, d5g1contra, d5g3co, d5g2co3];
test[leftInteriorProduct, d5g3co, d5g1contra, Error];


test[interiorProduct, d5g1contra, d5g3co, d5g2co3];
test[interiorProduct, d5g3co, d5g1contra, d5g2co3];


(* ::Text:: *)
(*a similar series of examples but with grade of contra > grade of co*)


test[rightInteriorProduct, d5g1co, d5g3contra, Error];
test[rightInteriorProduct, d5g3contra, d5g1co, d5g2contra];


test[leftInteriorProduct, d5g1co, d5g3contra, d5g2contra];
test[leftInteriorProduct, d5g3contra, d5g1co, Error];


test[interiorProduct, d5g1co, d5g3contra, d5g2contra];
test[interiorProduct, d5g3contra, d5g1co, d5g2contra];


(* ::Text:: *)
(*progressive  product  errors  if  it  gets  mixed  variance*)


test[progressiveProduct, d5g1contra, d5g3co, Error];


(* ::Text:: *)
(*regressive product errors if it gets mixed variance*)


test[regressiveProduct, d5g1contra, d5g3co, Error];


(* ::Text:: *)
(*interior product errors if it gets two mm*)


test[rightInteriorProduct, d5g2co1, d5g2co2, Error];
test[leftInteriorProduct, d5g2co1, d5g2co2, Error];
test[interiorProduct, d5g2co1, d5g2co2, Error];


(* ::Text:: *)
(*interior product errors if it gets two mc*)


test[rightInteriorProduct, d3g2contra1, d3g2contra2, Error];
test[leftInteriorProduct, d3g2contra1, d3g2contra2, Error];
test[interiorProduct, d3g2contra1, d3g2contra2, Error];


(* ::Text:: *)
(*same examples as for meet and join*)


et5Mm5 = matrixToMultivector[{{{5, 8, 12}}, "row"}];
et5Mc5 = matrixToMultivector[{{{-8, 5, 0}, {-4, 1, 1}}, "col"}];
et7Mm5 = matrixToMultivector[{{{7, 11, 16}}, "row"}];
et7Mc5 = matrixToMultivector[{{{-11, 7, 0}, {-7, 3, 1}}, "col"}];
meantoneMm5 = matrixToMultivector[{{{1, 0, -4}, {0, 1, 4}}, "row"}];
meantoneMc5 = matrixToMultivector[{{{4, -4, 1}}, "col"}];
porcupineMm5 = matrixToMultivector[{{{1, 2, 3}, {0, 3, 5}}, "row"}];
porcupineMc5 = matrixToMultivector[{{{1, -5, 3}}, "col"}];
d3unisonContra = {{1}, 3, "col"};
d3jiCo = {{1}, 3, "row"};


test[progressiveProduct, et5Mm5, et7Mm5, meantoneMm5];
test[progressiveProduct, et5Mc5, et7Mc5, Error];
test[progressiveProduct, meantoneMm5, porcupineMm5, Error];
test[progressiveProduct, meantoneMc5, porcupineMc5, et7Mc5];


meantoneMm11 = matrixToMultivector[{{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "row"}];
meantoneMc11 = matrixToMultivector[{{meantoneComma11, starlingComma11, mothwellsma11}, "col"}];
meanpopMm11 = matrixToMultivector[{{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "row"}];
meanpopMc11 = matrixToMultivector[{{meantoneComma11, starlingComma11, keenanisma11}, "col"}];
marvelMm11 = matrixToMultivector[{{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "row"}];
marvelMc11 = matrixToMultivector[{{marvelComma11, keenanisma11}, "col"}];
porcupineMm11 = matrixToMultivector[{{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "row"}];
porcupineMc11 = matrixToMultivector[{{telepathma11, septimalComma11, ptolemisma11}, "col"}];
meantoneMm7 = matrixToMultivector[{{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"}];
meantoneMc7 = matrixToMultivector[{{meantoneComma7, starlingComma7}, "col"}];
porcupineMm7 = matrixToMultivector[{{{1, 2, 3, 2}, {0, 3, 5, -6}}, "row"}];
porcupineMc7 = matrixToMultivector[{{septimalComma7, porcupineComma7}, "col"}];
miracleMm11 = matrixToMultivector[{{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "row"}];
miracleMc11 = matrixToMultivector[{{marvelComma11, rastma11, keenanisma11}, "col"}];
magicMm11 = matrixToMultivector[{{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "row"}];
magicMc11 = matrixToMultivector[{{marvelComma11, sensamagicComma11, ptolemisma11}, "col"}];
miracleMm7 = matrixToMultivector[{{{1, 1, 3, 3}, {0, 6, -7, -2}}, "row"}];
miracleMc7 = matrixToMultivector[{{marvelComma7, gamelisma7}, "col"}];
magicMm7 = matrixToMultivector[{{{1, 0, 2, -1}, {0, 5, 1, 12}}, "row"}];
magicMc7 = matrixToMultivector[{{marvelComma7, sensamagicComma7}, "col"}];
mothraMm11 = matrixToMultivector[{{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "row"}];
mothraMc11 = matrixToMultivector[{{meantoneComma11, mothwellsma11, keenanisma11}, "col"}];
mothraMm7 = matrixToMultivector[{{{1, 1, 0, 3}, {0, 3, 12, -1}}, "row"}];
mothraMc7 = matrixToMultivector[{{meantoneComma7, gamelisma7}, "col"}];


(* ::Text:: *)
(*\:22ce = COMMA MERGE, \:22cf = MAP MERGE*)


(*Meantone\:22ceMeanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test[progressiveProduct, meantoneMc11, meanpopMc11, Error];


(*Meantone\:22cfMeanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMm11, meanpopMm11, {{0, 0, 0, 0, 0}, 4, "row"}];


(*Meantone\:22ceMarvel = 31, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMc11, marvelMc11, {{0}, 5, "col"}];


(*Meantone\:22cfMarvel = <225/224>, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMm11, marvelMm11, {{0}, 5, "row"}];


(*Meantone\:22cePorcupine = G = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, meantoneMc11, porcupineMc11, Error];


(*Meantone\:22cfPorcupine = <176/175>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMm11, porcupineMm11, matrixToMultivector[dualPrivate[{{valinorsma11}, "col"}]]];


(*In the 7-limit, that become Meantone\:22cePorcupine = <JI>, Meantone\:22cfPorcupine = <1>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMc7, porcupineMc7, matrixToMultivector[{IdentityMatrix[4], "col"}]];
test[progressiveProduct, meantoneMm7, porcupineMm7, matrixToMultivector[{IdentityMatrix[4], "row"}]];


(*Miracle\:22ceMagic = 41, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, miracleMc11, magicMc11, Error];


(*Miracle\:22cfMagic = Marvel, but they're linearly dependent so EA gives an all-zero result *)
test[progressiveProduct, miracleMm11, magicMm11, {{0, 0, 0, 0, 0}, 4, "row"}];


(*In the 7-limit, again Miracle\:22ceMagic = 41, Miracle\:22cfMagic = Marvel, but they're linearly dependent so EA gives all-zero results*)
test[progressiveProduct, miracleMc7, magicMc7, {{0}, 4, "col"}];
test[progressiveProduct, miracleMm7, magicMm7, {{0}, 4, "row"}];


(*Miracle\:22ceMothra = 31, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, miracleMc11, mothraMc11, Error];


(* Miracle\:22cfMothra = Portent, but they're linearly dependent so EA gives an all-zero result *)
test[progressiveProduct, miracleMm11, mothraMm11, {{0, 0, 0, 0, 0}, 4, "row"}];


(*In the 7-limit, Miracle\:22cfMothra = Gamelan, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, miracleMm7, mothraMm7, {{0}, 4, "row"}];


(*Meantone\:22ceMagic = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test[progressiveProduct, meantoneMc11, magicMc11, Error];


(*Meantone\:22cfMagic = <225/224>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMm11, magicMm11, matrixToMultivector[dualPrivate[{{marvelComma11}, "col"}]]];


(* a basic right interior product example, with grade of a > b, and a being contra *)
test[rightInteriorProduct, d3g2contra1, d3g1co1, d3g1contra1];


(* a weird right interior product example, with grade of a = b, and a being contra; works the same as above, may bottom out *)
test[rightInteriorProduct, d3g1contra1, d3g1co2, Error];


(*a weird right interior product example, with grade of a < b, and a being contra, should bottom out grade of 0 and contra *)
test[rightInteriorProduct, d3g1contra1, d3g2co2, Error];


(* a basic right interior product example, with grade of a > b, and a being co *)
test[rightInteriorProduct, d3g2co1, d3g1contra2, d3g1co2];


(*a weird right interior product example, with grade of a = b, and a being co, works the same as above, may bottom out *)
test[rightInteriorProduct, d3g1co1, d3g1contra2, d3unisonCo];


(* a weird right interior product example, with grade of a < b, and a being co, should bottom out grade of 0 and co *)
test[rightInteriorProduct, d3g1co1, d3g2contra1, Error];


(* a basic left interior product example, with grade of b > a, and b contra *)
test[leftInteriorProduct, d3g1co1, d3g2contra1, d3g1contra1];


(* a weird left interior product example, with grade of b = a, and b contra, works the same as above, may bottom out *)
test[leftInteriorProduct, d3g1co1, d3g1contra2, d3jiContra];


(* a weird left interior product example, with grade of b < a, and b contra, should bottom out at grade of 0 and contra *)
test[leftInteriorProduct, d3g2co1, d3g1contra2, Error];


(* a basic left interior product example, with grade of b > a, and b co *)
test[leftInteriorProduct, d3g1contra2, d3g2co1, d3g1co2];


(* a weird left interior product example, with grade of b = a, and b co, works the same as above, may bottom out *)
test[leftInteriorProduct, d3g1contra1, d3g1co2, Error];


(* a weird left interior product example, with grade of b < a, and b co, should bottom out at grade of 0 and co *)
test[leftInteriorProduct, d3g2contra1, d3g1co1, Error];


(* ::Subsubsection::Closed:: *)
(*addition*)


(* ::Text:: *)
(*addable mm*)


meantoneMm = {{1, 4, 4}, 2, "row"};
porcupineMm = {{3, 5, 1}, 2, "row"};
test[eaSum, meantoneMm, porcupineMm, {{4, 9, 5}, 2, "row"}];
test[eaDiff, meantoneMm, porcupineMm, {{2, 1, -3}, 2, "row"}];
meantoneMc = {{4, -4, 1}, 1, "col"};
porcupineMc = {{1, -5, 3}, 1, "col"};
test[eaSum, meantoneMc, porcupineMc, {{5, -9, 4}, 1, "col"}];
test[eaDiff, meantoneMc, porcupineMc, {{-3, -1, 2}, 1, "col"}];


(* ::Text:: *)
(*addable mc*)


et7Mm = {{7, 11, 16}, 1, "row"};
et5Mm = {{5, 8, 12}, 1, "row"};
test[eaSum, et7Mm, et5Mm, {{12, 19, 28}, 1, "row"}];
test[eaDiff, et7Mm, et5Mm, {{2, 3, 4}, 1, "row"}];
et7Mc = {{16, -11, 7}, 2, "col"};
et5Mc = {{12, -8, 5}, 2, "col"};
test[eaSum, et7Mc, et5Mc, {{28, -19, 12}, 2, "col"}];
test[eaDiff, et7Mc, et5Mc, {{4, -3, 2}, 2, "col"}];


(* ::Text:: *)
(*not  addable - error!*)


septimalMeantoneMm = {{1, 4, 10, 4, 13, 12}, 2, "row"};
septimalBlackwoodMm = {{0, 5, 0, 8, 0, -14}, 2, "row"};
test[eaSum, septimalMeantoneMm, septimalBlackwoodMm, Error];
test[eaDiff, septimalMeantoneMm, septimalBlackwoodMm, Error];
septimalMeantoneMc = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "row"}];
septimalBlackwoodMc = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "row"}];
test[eaSum, septimalMeantoneMc, septimalBlackwoodMc, Error];
test[eaDiff, septimalMeantoneMc, septimalBlackwoodMc, Error];


(* ::Text:: *)
(*addable - linear-dependence-2 (mc)*)


et12Mm = {{12, 19, 28, 34}, 1, "row"};
et19Mm = {{19, 30, 44, 53}, 1, "row"};
test[eaSum, et12Mm, et19Mm, {{31, 49, 72, 87}, 1, "row"}];
test[eaDiff, et12Mm, et19Mm, {{7, 11, 16, 19}, 1, "row"}];
et12Mc = eaDual[et12Mm];
et19Mc = eaDual[et19Mm];
test[eaSum, et12Mc, et19Mc, {{-87, 72, -49, 31}, 3, "col"}];
test[eaDiff, et12Mc, et19Mc, {{-19, 16, -11, 7}, 3, "col"}];


(* ::Text:: *)
(*examples with themselves*)


test[eaSum, meantoneMm, meantoneMm, {{1, 4, 4}, 2, "row"}];
test[eaDiff, meantoneMm, meantoneMm, {{0, 0, 0}, 2, "row"}];
test[eaSum, meantoneMc, meantoneMc, {{4, -4, 1}, 1, "col"}];
test[eaDiff, meantoneMc, meantoneMc, {{0, 0, 0}, 1, "col"}];
test[eaSum, et7Mm, et7Mm, {{7, 11, 16}, 1, "row"}];
test[eaDiff, et7Mm, et7Mm, {{0, 0, 0}, 1, "row"}];
test[eaSum, et7Mc, et7Mc, {{16, -11, 7}, 2, "col"}];
test[eaDiff, et7Mc, et7Mc, {{0, 0, 0}, 2, "col"}];


(* ::Text:: *)
(*mismatched r & n but matching d*)


test[eaSum, et7Mm, meantoneMm, Error];
test[eaDiff, et7Mm, meantoneMm, Error];
test[eaSum, et7Mc, meantoneMc, Error];
test[eaDiff, et7Mc, meantoneMc, Error];


(* ::Text:: *)
(*mismatched d but matching r or n*)


test[eaSum, et7Mm, et12Mm, Error];
test[eaDiff, et7Mm, et12Mm, Error];
test[eaSum, et7Mc, et12Mc, Error];
test[eaDiff, et7Mc, et12Mc, Error];


(* ::Text:: *)
(*some basic examples*)


augmentedMm = {{3, 0, -7}, 2, "row"};
diminishedMm = {{4, 4, -3}, 2, "row"};
tetracotMm = {{4, 9, 5}, 2, "row"};
dicotMm = {{2, 1, -3}, 2, "row"};
srutalMm = {{2, -4, -11}, 2, "row"};
test[eaSum, augmentedMm, diminishedMm, {{7, 4, -10}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] + \:27e8\:27e84 4 -3]] = \:27e8\:27e87 4 -10]] *)
test[eaDiff, augmentedMm, diminishedMm, {{1, 4, 4}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e84 4 -3]] = \:27e8\:27e81 4 4]] *)
test[eaSum, augmentedMm, tetracotMm, {{7, 9, -2}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] + \:27e8\:27e84 9 5]] = \:27e8\:27e87 9 -2]] *)
test[eaDiff, augmentedMm, tetracotMm, {{1, 9, 12}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e84 9 5]] = \:27e8\:27e81 9 12]] *)
test[eaSum, augmentedMm, dicotMm, {{5, 1, -10}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e85 1 -10]] *)
test[eaDiff, augmentedMm, dicotMm, {{1, -1, -4}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e81 -1 -4]] *)
test[eaSum, augmentedMm, srutalMm, {{5, -4, -18}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e85 -4 -18]] *)
test[eaDiff, augmentedMm, srutalMm, {{1, 4, 4}, 2, "row"}]; (* \:27e8\:27e83 0 -7]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e81 4 4]] *)
test[eaSum, diminishedMm, tetracotMm, {{8, 13, 2}, 2, "row"}]; (* \:27e8\:27e84 4 -3]] + \:27e8\:27e84 9 5]] = \:27e8\:27e88 13 2]] *)
test[eaDiff, diminishedMm, tetracotMm, {{0, 5, 8}, 2, "row"}]; (* \:27e8\:27e84 4 -3]] - \:27e8\:27e84 9 5]] = \:27e8\:27e80 5 8]] *)
test[eaSum, diminishedMm, dicotMm, {{6, 5, -6}, 2, "row"}]; (* \:27e8\:27e84 4 -3]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e86 5 -6]] *)
test[eaDiff, diminishedMm, dicotMm, {{2, 3, 0}, 2, "row"}]; (* \:27e8\:27e84 4 -3]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e82 3 0]] *)
test[eaSum, diminishedMm, srutalMm, {{3, 0, -7}, 2, "row"}]; (* \:27e8\:27e84 4 -3]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e86 0 -14]] \[RightArrow] \:27e8\:27e83 0 -7]] *)
test[eaDiff, diminishedMm, srutalMm, {{1, 4, 4}, 2, "row"}]; (*\:27e8\:27e84 4 -3]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e82 8 8]] \[RightArrow] \:27e8\:27e81 4 4]] *)
test[eaSum, tetracotMm, dicotMm, {{3, 5, 1}, 2, "row"}]; (* \:27e8\:27e84 9 5]] + \:27e8\:27e82 1 -3]] = \:27e8\:27e86 10 2]] \[RightArrow] \:27e8\:27e83 5 1]] *)
test[eaDiff, tetracotMm, dicotMm, {{1, 4, 4}, 2, "row"}]; (* \:27e8\:27e84 9 5]] - \:27e8\:27e82 1 -3]] = \:27e8\:27e82 8 8]] \[RightArrow] \:27e8\:27e81 4 4]] *)
test[eaSum, tetracotMm, srutalMm, {{6, 5, -6}, 2, "row"}]; (* \:27e8\:27e84 9 5]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e86 5 -6]] *)
test[eaDiff, tetracotMm, srutalMm, {{2, 13, 16}, 2, "row"}]; (* \:27e8\:27e84 9 5]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e82 13 16]] *)
test[eaSum, dicotMm, srutalMm, {{4, -3, -14}, 2, "row"}]; (* \:27e8\:27e82 1 -3]] + \:27e8\:27e82 -4 -11]] = \:27e8\:27e84 -3 -14]] *)
test[eaDiff, dicotMm, srutalMm, {{0, 5, 8}, 2, "row"}]; (* \:27e8\:27e82 1 -3]] - \:27e8\:27e82 -4 -11]] = \:27e8\:27e80 5 8]] *)


(* ::Text:: *)
(*example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2*)


u1 = {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "row"};
u2 = {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "row"};
test[eaSum, u1, u2, Error];
test[eaDiff, u1, u2, Error];


(* ::Text:: *)
(*example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1*)


u1 = {{2, 3}, 1, "col"};
u2 = {{4, -7}, 1, "row"};
uSum = {{9, 7}, 1, "col"};
uDiff = {{5, 1}, 1, "col"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];


(* ::Text:: *)
(*example demonstrating how it's important to canonicalize*)


u1 = {{-2, 4, -2}, 1, "row"};
u2 = {{7, 7, 0}, 1, "row"};
uSum = {{2, -1, 1}, 1, "row"};
uDiff = {{0, 3, -1}, 1, "row"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];


(* ::Text:: *)
(*example demonstrating how mixed variance inputs are accepted, but the first variance matches the output*)


u1 = {{1, 4, 10, 4, 13, 12}, 2, "row"};
u2 = {{1, 4, -9, 4, -17, -32}, 2, "row"};
uSum = {{2, 8, 1, 8, -4, -20}, 2, "row"};
test[eaSum, u1, u2, uSum];
test[eaSum, eaDual[u1], u2, eaDual[uSum]];
test[eaSum, u1, eaDual[u2], uSum];
test[eaSum, eaDual[u1], eaDual[u2], eaDual[uSum]];


(* ::Text:: *)
(*an example that used to fail for whatever reason, "some problem"*)


test[eaSum, {{18, -2, -1, 14, -20, 3}, 2, "row"}, {{6, -2, 8, 6, -15, -3}, 2, "row"}, {{24, -4, 7, 20, -35, 0}, 2, "row"}];


(* ::Text:: *)
(*another example that used to fail for whatever reason, "goddam failing mysteries"*)


test[eaSum, {{15, 93, 30, 22, 10, 18}, 2, "row"}, {{32, 44, -1, -56, -22, -32}, 2, "row"}, {{47, 137, 29, -34, -12, -14}, 2, "row"}];


(* ::Text:: *)
(*another example that used to fail for whatever reason, "more stuff to sort out"*)


test[eaSum, {{5, 16, 15, -1, 0, 3}, 2, "col"}, {{4, 3, 12, -1, 0, 3}, 2, "col"}, {{9, 19, 27, -2, 0, 6}, 2, "col"}];


(* ::Text:: *)
(*EA only: example that motivated a further simplification and correction of the addability condition*)


test[eaSum, {{1, -5, -14, 9, 23, 11}, 2, "row"}, {{25, -1, 2, -18, -14, 2}, 2, "col"}, Error];


(* ::Text:: *)
(*LA only checks example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors, but I think it's okay to check it here too*)


test[eaSum, {{3, 8, -4, -6}, 1, "row"}, {{9, 2, -4, 1}, 1, "row"}, {{12, 10, -8, -5}, 1, "row"}];


(* ::Text:: *)
(*LA only checks this non-min-grade-1 example, but I think it's okay to check it here too*)


septimalMeantoneU = {{1, 4, 10, 4, 13, 12}, 2, "row"};
flattoneU = {{1, 4, -9, 4, -17, -32}, 2, "row"};
godzillaU = {{2, 8, 1, 8, -4, -20}, 2, "row"};
et19MwithIndependent7U = {{0, 0, 19, 0, 30, 44}, 2, "row"};
test[eaSum, septimalMeantoneU, flattoneU, godzillaU];
test[eaDiff, septimalMeantoneU, flattoneU, et19MwithIndependent7U];


(* ::Text:: *)
(*LA only ensures the largestMinorsL are consulted so that the sum and diff are identified correctly, but I think it's okay to check it here too; this also verifies that for the min-grade-1 case, I think*)


u1 = {{0, 1, -1, 0}, 3, "row"};
u2 = {{20, -144, 87, -59}, 3, "row"};
uSum = {{20, -143, 86, -59}, 3, "row"};
uDiff = {{20, -145, 88, -59}, 3, "row"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];


(* ::Text:: *)
(*LA only ensures intractability beyond the breadth-first search of linear combinations code the first way I wrote it, i.e. using my fancier style essentially using a Wolfram Solve[]... but let's check it here too*)


u1 = {{35, 5, 40, 10, 27, -71, 19, -41, -5, 42}, 2, "row"};
u2 = {{5, -40, 30, -60, 12, -15, 15, 48, 24, -90}, 2, "row"};
uSum = {{40, -35, 70, -50, 39, -86, 34, 7, 19, -48}, 2, "row"};
uDiff = {{30, 45, 10, 70, 15, -56, 4, -89, -29, 132}, 2, "row"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];


(* ::Text:: *)
(*random tests that check for matching between LA and EA*)


randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];


matrixToMultivectorWithPossibleError[a_] := If[a === Error, Error, matrixToMultivector[a]];


match[sumByU_, sumByT_, diffByU_, diffByT_] := Module[{sumsMatch, diffsMatch},
  sumsMatch = sumByU === sumByT;
  diffsMatch = If[
    diffByT === Error,
    If[
      diffByU === Error,
      True,
      allZeros[eaGetLargestMinorsL[diffByU]]
    ],
    TrueQ[diffByU == diffByT]
  ];
  
  sumsMatch && diffsMatch
];


randomTestAdditionMatchesBetweenLaAndEa[d_, r_, linearIndependence_, testCount_] := Module[
  {
    linearDependence,
    linearDependenceBasis,
    t1,
    t2,
    u1,
    u2,
    sumByT,
    sumByU,
    diffByU,
    diffByT
  },
  
  Do[
    linearDependence = r - linearIndependence;
    
    linearDependenceBasis = randomVectors[d, linearDependence];
    t1 = {Join[linearDependenceBasis, randomVectors[d, linearIndependence]], "row"};
    t2 = {Join[linearDependenceBasis, randomVectors[d, linearIndependence]], "row"};
    
    t1 = If[RandomInteger[] == 1, dualPrivate[t1], t1];
    t2 = If[RandomInteger[] == 1, dualPrivate[t2], t2];
    
    u1 = matrixToMultivector[t1];
    u2 = matrixToMultivector[t2];
    
    sumByU = eaSum[u1, u2];
    sumByT = matrixToMultivectorWithPossibleError[sumPrivate[t1, t2]];
    
    diffByU = eaDiff[u1, u2];
    diffByT = matrixToMultivectorWithPossibleError[diffPrivate[t1, t2]];
    
    If[
      match[sumByU, sumByT, diffByU, diffByT],
      passes += 1,
      failures += 1;
      printWrapper["failure: "];
      printWrapper[u1, " + ", u2, " = (OR ", t1, " + ", t2, " = )"];
      printWrapper[sumByU, " (by multivectors)"];
      printWrapper[sumByT, " (by matrices)"];
      printWrapper[u1, " - ", u2, " = (OR ", t1, " - ", t2, " = )"];
      printWrapper[diffByU, " (by multivectors)"];
      printWrapper[diffByT, " (by matrices)\n"];
    ],
    testCount
  ]
];


randomTestAdditionMatchesBetweenLaAndEa[2, 1, 1, 16];


randomTestAdditionMatchesBetweenLaAndEa[3, 1, 1, 8];
randomTestAdditionMatchesBetweenLaAndEa[3, 2, 1, 8];


randomTestAdditionMatchesBetweenLaAndEa[4, 1, 1, 4];
randomTestAdditionMatchesBetweenLaAndEa[4, 2, 1, 4];
randomTestAdditionMatchesBetweenLaAndEa[4, 3, 1, 4];
randomTestAdditionMatchesBetweenLaAndEa[4, 2, 2, 4];


randomTestAdditionMatchesBetweenLaAndEa[5, 1, 1, 2];
randomTestAdditionMatchesBetweenLaAndEa[5, 2, 1, 2];
randomTestAdditionMatchesBetweenLaAndEa[5, 3, 1, 2];
randomTestAdditionMatchesBetweenLaAndEa[5, 4, 1, 2];
randomTestAdditionMatchesBetweenLaAndEa[5, 2, 2, 2];
randomTestAdditionMatchesBetweenLaAndEa[5, 3, 2, 2];


randomTestAdditionMatchesBetweenLaAndEa[6, 1, 1, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 2, 1, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 3, 1, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 4, 1, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 5, 1, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 2, 2, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 3, 2, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 4, 2, 1];
randomTestAdditionMatchesBetweenLaAndEa[6, 3, 3, 1];


(* ::Subsubsection::Closed:: *)
(*multivector utilities*)


If[eaIndices[0, 0] == {{}}, "", f = f + 1; printWrapper["eaIndices[0, 0] == {{}}"]];
If[eaIndices[1, 0] == {{}}, "", f = f + 1; printWrapper["eaIndices[1, 0] == {{}}"]];
If[eaIndices[1, 1] == IdentityMatrix[1], "", f = f + 1; printWrapper["eaIndices[1, 1] == IdentityMatrix[1]"]];
If[eaIndices[2, 0] == {{}}, "", f = f + 1; printWrapper["eaIndices[2, 0] == {{}}"]];
If[eaIndices[2, 1] == {{1}, {2}}, "", f = f + 1; printWrapper["eaIndices[2, 1] == {{1}, {2}}"]];
If[eaIndices[2, 2] == {{1, 2}}, "", f = f + 1; printWrapper["eaIndices[2, 2] == {{1, 2}}"]];
If[eaIndices[3, 0] == {{}}, "", f = f + 1; printWrapper["eaIndices[3, 0] == {{}}"]];
If[eaIndices[3, 1] == {{1}, {2}, {3}}, "", f = f + 1; printWrapper["eaIndices[3, 1] == {{1}, {2}, {3}}"]];
If[eaIndices[3, 2] == {{1, 2}, {1, 3}, {2, 3}}, "", f = f + 1; printWrapper["eaIndices[3, 2] == {{1, 2}, {1, 3}, {2, 3}}"]];
If[eaIndices[3, 3] == {{1, 2, 3}}, "", f = f + 1; printWrapper["eaIndices[3, 3] == {{1, 2, 3}}"]];
If[eaIndices[4, 0] == {{}}, "", f = f + 1; printWrapper["eaIndices[4, 0] == {{}}"]];
If[eaIndices[4, 1] == {{1}, {2}, {3}, {4}}, "", f = f + 1; printWrapper["eaIndices[4, 1] == {{1}, {2}, {3}, {4}}"]];
If[eaIndices[4, 2] == {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, {3, 4}}, "", f = f + 1; printWrapper["eaIndices[4, 2] == {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, {3, 4}}"]];
If[eaIndices[4, 3] == {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}, "", f = f + 1; printWrapper["eaIndices[4, 3] == {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}"]];
If[eaIndices[4, 4] == {{1, 2, 3, 4}}, "", f = f + 1; printWrapper["eaIndices[4, 4] == {{1, 2, 3, 4}}"]];


test[isNondecomposable, {{2, -4, 8, -9, 7, 2}, 2, "row"}, True];
test[isNondecomposable, {{1, 4, 4}, 2, "row"}, False];


test[eaGetLargestMinorsL, {{1, 4, 4}, 2, "row"}, {1, 4, 4}];


test[eaGetGrade, {{1, 4, 4}, 2, "row"}, 2];


test[eaGetVariance, {{1, 4, 4}, 2, "row"}, "row"];


(* ::Section::Closed:: *)
(*tuning*)


accuracy = 3;
format = "EBK";


testClose[fn_, args___, inputExpectation_] := Module[
  {actual, expectation},
  
  actual = parseTemperamentData[Apply[fn, {args}]];
  expectation = parseTemperamentData[inputExpectation];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {getL[actual], getL[expectation]}], TrueQ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[formatOutput[SetAccuracy[actual, accuracy + 1]]];
  ]
];


testDamages[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[
      MapThread[
        Function[
          {actualEntry, expectationEntry},
          ToString[formatNumber[N[actualEntry]]] == ToString[formatNumber[N[expectationEntry]]]
        ],
        {Keys[actual], Keys[expectation]}
      ],
      TrueQ
    ] && AllTrue[
      MapThread[
        Function[
          {actualEntry, expectationEntry},
          ToString[formatNumber[N[actualEntry]]] == ToString[formatNumber[N[expectationEntry]]]
        ],
        {Values[actual], Values[expectation]}
      ],
      TrueQ
    ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];


testDamageMeanOrComplexity[fn_, args___, inputExpectation_] := Module[
  {actual, expectation},
  
  actual = parseTemperamentData[Apply[fn, {args}]];
  expectation = parseTemperamentData[inputExpectation];
  
  If[
    Abs[ToExpression[ToString[actual - expectation]]] < 10^-accuracy,
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];


testTargetSetScheme[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    TrueQ[Sort[actual] == Sort[expectation]],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != `` (order agnostic); actual result was:", fn, {args}, expectation], 14, Red]];
    printWrapper[actual];
  ]
];


(* ::Subsection::Closed:: *)
(*optimization*)


(* ::Subsubsection::Closed:: *)
(*using explicit target-intervals*)


sixTilt = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "\:27e81200.000, 696.578]"];


(* ::Subsubsection::Closed:: *)
(*by individual tuning scheme properties*)


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "\:27e81200.000 696.578]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81202.390 697.176]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81202.728 697.260]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight"}, "\:27e81201.699 697.564]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81201.600 697.531]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81197.610 694.786]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81197.435 694.976]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight"}, "\:27e81197.979 694.711]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81198.423 695.209]"];


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "unityWeight"}, "\:27e81202.081 697.099]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81202.609 697.329]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81202.729 697.210]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight"}, "\:27e81201.617 697.379]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81201.718 697.214]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81200.813 696.570]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81200.522 696.591]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight"}, "\:27e81201.489 696.662]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81201.535 696.760]"];


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "unityWeight"}, "\:27e81204.301 697.654]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight"}, "\:27e81200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81200.000 696.578]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight"}, "\:27e81204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81204.301 697.654]"];


(* ::Subsubsection::Closed:: *)
(*fully by "tuningSchemeSystematicName"*)


tenTilt = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5, 7/3, 7/4, 7/5, 7/6, 8/3, 8/5, 9/4, 9/5, 9/7, 10/7}";

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " minimax-U", "\:27e8600.000 108.128]"];


testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " minimax-copfr-S", "\:27e8596.502 106.708]"];

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " minimax-E-copfr-S", "\:27e8598.078 106.945]"];

testClose[optimizeGeneratorTuningMap, pajara, {"tuningSchemeSystematicName" -> tenTilt <> " minimax-S", "quick" -> True}, "\:27e8598.965 107.215]"]; (* too much computation required to find exact solution with free Wolfram Cloud account *)

testClose[optimizeGeneratorTuningMap, pajara, {"tuningSchemeSystematicName" -> tenTilt <> " minimax-ES", "quick" -> True}, "\:27e8598.815 107.238]"]; (* too much computation required to find exact solution with free Wolfram Cloud account *)


testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " minimax-copfr-C", "\:27e8600.581 107.714]"];

testClose[optimizeGeneratorTuningMap, pajara, {"tuningSchemeSystematicName" -> tenTilt <> " minimax-E-copfr-C", "quick" -> True}, "\:27e8598.779 107.058]"]; (* too much computation required to find exact solution with free Wolfram Cloud account *)

testClose[optimizeGeneratorTuningMap, pajara, {"tuningSchemeSystematicName" -> tenTilt <> " minimax-C", "quick" -> True}, "\:27e8599.031 107.398]"]; (* too much computation required to find exact solution with free Wolfram Cloud account *)

testClose[optimizeGeneratorTuningMap, pajara, {"tuningSchemeSystematicName" -> tenTilt <> " minimax-EC", "quick" -> True}, "\:27e8598.378 107.249]"]; (* too much computation required to find exact solution with free Wolfram Cloud account *)


testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-U", "\:27e8598.247 106.830]"];

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-copfr-S", "\:27e8598.488 106.799]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-E-copfr-S", "\:27e8598.346 106.837]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-S", "\:27e8599.020 106.492]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-ES", "\:27e8598.882 106.594]"];

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-copfr-C", "\:27e8598.518 106.789]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-E-copfr-C", "\:27e8598.655 106.720]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-C", "\:27e8597.875 107.083]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniRMS-EC", "\:27e8597.804 107.013]"];


testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-U", "\:27e8598.914 105.214]"];

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-copfr-S", "\:27e8598.914 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-E-copfr-S", "\:27e8598.914 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-S", "\:27e8598.914 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-ES", "\:27e8598.914 105.214]"];

testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-copfr-C", "\:27e8598.914 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-E-copfr-C", "\:27e8598.914 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-C", "\:27e8598.603 106.145]"];
testClose[optimizeGeneratorTuningMap, pajara, tenTilt <> " miniaverage-EC", "\:27e8598.603 106.145]"];


(* ::Subsubsection::Closed:: *)
(*by "damageSystematicName" plus traits 1 and 2 (target-intervals, and optimization power)*)


testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "U-damage"}, "\:27e8600.000 1905.214]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-S-damage"}, "\:27e8599.425 1903.105]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-S-damage"}, "\:27e8599.362 1902.875]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "S-damage"}, "\:27e8599.555 1903.365]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "ES-damage"}, "\:27e8599.577 1903.449]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-C-damage"}, "\:27e8600.752 1907.971]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-C-damage"}, "\:27e8600.863 1908.379]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "C-damage"}, "\:27e8600.413 1906.917]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "EC-damage"}, "\:27e8600.296 1906.485]"];


testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "U-damage"}, "\:27e8599.131 1902.390]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-S-damage"}, "\:27e8599.219 1902.515]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-S-damage"}, "\:27e8599.156 1902.381]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "S-damage"}, "\:27e8599.431 1903.058]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "ES-damage"}, "\:27e8599.363 1902.960]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-C-damage"}, "\:27e8599.232 1902.839]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-C-damage"}, "\:27e8599.247 1902.882]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "C-damage"}, "\:27e8599.159 1902.609]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "EC-damage"}, "\:27e8599.116 1902.444]"];


testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "U-damage"}, "\:27e8598.914 1901.955]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-S-damage"}, "\:27e8599.054 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-S-damage"}, "\:27e8598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "S-damage"}, "\:27e8599.111 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "ES-damage"}, "\:27e8598.914 1901.955]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-C-damage"}, "\:27e8598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-C-damage"}, "\:27e8598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "C-damage"}, "\:27e8598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "EC-damage"}, "\:27e8598.914 1901.955]"];


(* ::Subsubsection::Closed:: *)
(*by "intervalComplexitySystematicName", plus traits 1, 2, and 3 (target-intervals, optimization power, and damage weight slope)*)


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "\:27e8240.000 2795.336]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8238.612 2784.926]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8238.445 2783.722]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8238.867 2785.650]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity", "quick" -> True}, "\:27e8238.801 2784.928]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8241.504 2811.877]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8241.702 2812.251]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8241.209 2808.887]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "\:27e8240.981 2805.237]"];


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "unityWeight"}, "\:27e8238.408 2781.006]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8238.316 2781.797]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8238.248 2781.458]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8238.779 2784.026]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "\:27e8238.712 2783.815]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8238.916 2784.540]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8239.047 2784.702]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8238.642 2783.284]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "\:27e8238.583 2782.365]"];


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "unityWeight"}, "\:27e8237.744 2775.036]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "\:27e8237.744 2775.036]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "\:27e8237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "\:27e8237.744 2775.036]"];


(* ::Subsubsection::Closed:: *)
(*handling ETs*)


testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT minimax-U", "\:27e822.644]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniRMS-U", "\:27e822.650]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniaverage-U", "\:27e822.642]"];

testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT minimax-C", "\:27e822.638]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniRMS-C", "\:27e822.657]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniaverage-C", "\:27e822.662]"];

testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT minimax-S", "\:27e822.647]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniRMS-S", "\:27e822.644]"];
testClose[optimizeGeneratorTuningMap, "[\:27e853 84 123]}", "TILT miniaverage-S", "\:27e822.642]"];


(* ::Subsubsection::Closed:: *)
(*optimization power continuum*)


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "\:27e8240.000 2795.336]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 5.00, "damageWeightSlope" -> "unityWeight"}, "\:27e8239.174 2787.898]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 3.00, "damageWeightSlope" -> "unityWeight"}, "\:27e8238.745 2784.044]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2.00, "damageWeightSlope" -> "unityWeight"}, "\:27e8238.408 2781.006]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.50, "damageWeightSlope" -> "unityWeight"}, "\:27e8238.045 2777.737]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.25, "damageWeightSlope" -> "unityWeight"}, "\:27e8237.793 2775.471]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.00, "damageWeightSlope" -> "unityWeight"}, "\:27e8237.744 2775.036]"];


(* ::Subsubsection::Closed:: *)
(*interval complexity norm power continuum*)


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> \[Infinity]}, "\:27e81201.191 697.405]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 5.00}, "\:27e81201.381 697.460]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 3.00}, "\:27e81201.513 697.503]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2.00}, "\:27e81201.600 697.531]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.50}, "\:27e81201.648 697.547]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.25}, "\:27e81201.673 697.556]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.00}, "\:27e81201.699 697.564]"];


(* ::Subsubsection::Closed:: *)
(*held-intervals*)


fiveOld = "{2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5}";
heldOctaveFiveOldMiniaverageUResult = "\:27e81200.000 696.578]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "octave"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "2"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "2/1"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2}"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2/1}"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-octave " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2 " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2/1 " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2} " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1} " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];


testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2/1, 3/2}"}, "\:27e81200.000 701.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 3/2} " <> fiveOld <> " miniaverage-U", "\:27e81200.000 701.955]"];


heldOctaveTiltMiniRmsUResult = "\:27e81200.000 696.274]";
testClose[optimizeGeneratorTuningMap, meantone, "held-octave TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2 TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2/1 TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2} TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1} TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-3/2 TILT miniRMS-U", "\:27e81209.926 701.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-5/4 TILT miniRMS-U", "\:27e81201.536 697.347]"];


controlResult = "\:27e81200.000 696.578]";
controlScheme = {"tuningSchemeSystematicName" -> fiveOld <> " minimax-U"};
testClose[optimizeGeneratorTuningMap, meantone, controlScheme, controlResult];
heldIntervalResult = "\:27e81200.000 694.786]";
heldIntervalScheme = Join[controlScheme, {"heldIntervals" -> "5/3"}];
testClose[optimizeGeneratorTuningMap, meantone, heldIntervalScheme, heldIntervalResult];


(* ::Text:: *)
(*should be able to skip the specification of a target-intervals set if you specify the right number of held-intervals (h = r)*)


testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4} minimax-U", "\:27e81200.000 696.578]"];


(* ::Text:: *)
(*gracefully handles held-interval bases that are not actually bases (not linearly independent) *)*)


(* TODO: these are failing with "no target-intervals" for some reason; possibly an earlier commit caused these to start failing for some reason *)
(*
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4, 4/1} minimax-U", "\:27e81200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4, 5/2} minimax-U", "\:27e81200.000 696.578]"];
*)


(* ::Text:: *)
(*the single-free-generator extra points for the coinciding-damage method*)
(*where target-intervals are taken as unchanged-intervals, as is done with the zero-damage method*)


testClose[optimizeGeneratorTuningMap, "[\:27e83 0 7] \:27e80 1 0]}", "held-octave {3/1, 5/1} minimax-U", "\:27e8400.000 1901.955]"];


(* ::Subsubsection::Closed:: *)
(*target-interval set schemes*)


(* ::Text:: *)
(*held-octave OLD minimax-U = "minimax"*)


testClose[optimizeTuningMap, meantone, "held-octave OLD minimax-U", "\:27e81200.000 1896.578 2786.314]"]; (* [7a] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
testClose[optimizeGeneratorTuningMap, porcupine, "held-octave OLD minimax-U", "\:27e81200.000 -162.737]"]; (* [7c] *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorTuningMap, magic, "held-octave OLD minimax-U", "\:27e81200.000 380.391]"]; (* [7d] *)
(* negri *)
testClose[optimizeGeneratorTuningMap, tetracot, "held-octave OLD minimax-U", "\:27e81200.000 176.257]"]; (* [7e] *)
testClose[optimizeGeneratorTuningMap, meantone7, "held-octave OLD minimax-U", "\:27e81200.000, 1200.000 + 696.578]"]; (* [7f] *)
testClose[optimizeGeneratorTuningMap, magic7, "held-octave OLD minimax-U", "\:27e81200.00 380.391]"]; (* [7d] *)
(* pajara *)
accuracy = 1;
testClose[optimizeGeneratorTuningMap, augene, "held-octave OLD minimax-U", "\:27e8400.000, 3 * 400.000 + 708.798]"]; (* [7b] *)
accuracy = 3;
testClose[optimizeGeneratorTuningMap, sensi, "held-octave OLD minimax-U", "\:27e81200.000 443.519]"]; (* [7g] *)
testClose[optimizeTuningMap, sensamagic, "held-octave OLD minimax-U", "\:27e81200.000 1901.955 2781.584 3364.096]"]; (* [7h] *)
(* original name *)
testClose[optimizeTuningMap, meantone, "minimax", "\:27e81200.000 1896.578 2786.314]"];


(* ::Text:: *)
(*held-octave OLD miniRMS-U = "least squares"*)


testClose[optimizeGeneratorTuningMap, meantone, "held-octave OLD miniRMS-U", "\:27e81200.000 696.165]"]; (* [7f] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorTuningMap, magic, "held-octave OLD miniRMS-U", "\:27e81200.000 379.968]"]; (* [7d]] *)
(* negri *)
(* tetracot *)
testClose[optimizeGeneratorTuningMap, meantone7, "held-octave OLD miniRMS-U", "\:27e81200.000, 1200.000 + 696.436]"]; (* [7f] *)
testClose[optimizeGeneratorTuningMap, magic7, "held-octave OLD miniRMS-U", "\:27e81200.000, 380.384]"]; (* [7d]] *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 0 15] \:27e80 1 -8]}", "held-octave OLD miniRMS-U", "\:27e81200.000, 1200.000 + 701.728]"]; (* [2b] has a bunch of least squares tunings... only this one works, though; not sure what's up with the rest. this is the temperament that tempers out 32805/32768, btw. *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "least squares", "\:27e81200.000 696.165]"];


(* ::Text:: *)
(*support strings, not strings, or whatever comes out of the user functions; and whether via the systematic tuning scheme name or the individual tuning property*)


sixTiltString = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";
sixTiltQuotients = {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5};
sixTiltResult = "\:27e81200.000, 696.578]";


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTiltString, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTiltQuotients, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> getTilt[6], "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];


testClose[optimizeGeneratorTuningMap, meantone, sixTiltString <> " minimax-U", sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, quotientLToString[sixTiltQuotients] <> " minimax-U", sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, quotientLToString[getTilt[6]] <> " minimax-U", sixTiltResult];


(* ::Text:: *)
(*the integer limit of the TILT defaults to the integer just less than the next prime, but this default may be overridden*)


tenTiltResult = "\:27e8598.247 106.830]";
eightTiltResult = "\:27e8598.444 107.167]";
testClose[optimizeGeneratorTuningMap, pajara, "TILT miniRMS-U", tenTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, "10-TILT miniRMS-U", tenTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, "8-TILT miniRMS-U", eightTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, quotientLToString[getTilt[8]] <> " miniRMS-U", eightTiltResult];


(* ::Text:: *)
(*full name works too*)


testClose[optimizeGeneratorTuningMap, pajara, "truncated integer limit triangle miniRMS-U", tenTiltResult];


(* ::Text:: *)
(*the odd limit of the OLD defaults to the odd just less than the next prime, but this default may be overridden*)


nineOldResult = "\:27e8600.000 108.128]";
sevenOldResult = "\:27e8600.000 109.363]";
testClose[optimizeGeneratorTuningMap, pajara, "held-octave OLD minimax-U", nineOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "held-octave 9-OLD minimax-U", nineOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "held-octave 7-OLD minimax-U", sevenOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "held-octave " <> quotientLToString[getOld[7]] <> " minimax-U", sevenOldResult];


(* ::Text:: *)
(*full name works too*)


testClose[optimizeGeneratorTuningMap, pajara, "held-octave odd limit diamond minimax-U", nineOldResult];


(* ::Subsubsection::Closed:: *)
(*destretching*)


(* ::Text:: *)
(*destretched-octave minimax-ES = "POTE", "Pure Octave Tenney-Euclidean"*)


(* could double-check with Xen wiki *)
testClose[optimizeTuningMap, meantone, "destretched-octave minimax-ES", "\:27e81200.000 1896.239 2784.955]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "destretched-octave minimax-ES", "\:27e81200.000 1920.000 2799.594]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "destretched-octave minimax-ES", "\:27e81200.000 1897.189 2748.594]"]; (* [3p] *)
testClose[optimizeTuningMap, augmented, "destretched-octave minimax-ES", "\:27e81200.000 1906.638 2800.000]"]; (* [3q] *)
testClose[optimizeTuningMap, mavila, "destretched-octave minimax-ES", "\:27e81200.000 1879.806 2760.582]"]; (* [3r] *)
testClose[optimizeTuningMap, porcupine, "destretched-octave minimax-ES", "\:27e81200.000 1908.149 2780.248]"]; (* [3s] *)
testClose[optimizeTuningMap, srutal, "destretched-octave minimax-ES", "\:27e81200.000 1904.898 2790.204]"]; (* [3t] *)
testClose[optimizeTuningMap, hanson, "destretched-octave minimax-ES", "\:27e81200.000 1902.039 2785.033]"]; (* [3u] *)
testClose[optimizeTuningMap, magic, "destretched-octave minimax-ES", "\:27e81200.000 1900.292 2780.058]"]; (* [3v] *)
testClose[optimizeTuningMap, negri, "destretched-octave minimax-ES", "\:27e81200.000 1896.980 2777.265]"]; (* [3w] *)
testClose[optimizeTuningMap, tetracot, "destretched-octave minimax-ES", "\:27e81200.000 1904.639 2785.438]"]; (* [3x] *)
testClose[optimizeTuningMap, meantone7, "destretched-octave minimax-ES", "\:27e81200.000 1896.495 2785.980 3364.949]"]; (* [3y] *)
testClose[optimizeTuningMap, magic7, "destretched-octave minimax-ES", "\:27e81200.000 1901.760 2780.352 3364.224]"]; (* [3z] *)
testClose[optimizeTuningMap, pajara, "destretched-octave minimax-ES", "\:27e81200.000 1907.048 2785.905 3385.905]"]; (* [3aa] *)
testClose[optimizeTuningMap, augene, "destretched-octave minimax-ES", "\:27e81200.000 1909.257 2800.000 3381.486]"]; (* [3ab] *)
testClose[optimizeTuningMap, sensi, "destretched-octave minimax-ES", "\:27e81200.000 1903.679 2790.444 3363.975]"]; (* [3ac] *)
testClose[optimizeTuningMap, sensamagic, "destretched-octave minimax-ES", "\:27e81200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3ad] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "POTE", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-ES"]];


(* ::Text:: *)
(*destretched-octave minimax-S = "POTOP", "POTT", "Pure Octave Tenney OPtimal", "Pure Octave Tiebreaker-in-polytope Tenney-optimal"*)


(* could double-check against Flora's app, but her TOP results are incorrect for now, so these would be too *)
testClose[optimizeGeneratorTuningMap, "[\:27e82 2 7 8 14 5] \:27e80 1 -2 -2 -6 2]}", "destretched-octave minimax-S", "\:27e8600.000 709.184]"]; (* [7j] has {600.000, 706.843} but that has 7.254 damage and mine has 5.988 *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 -1 0 1] \:27e80 10 9 7]}", "destretched-octave minimax-S", "\:27e81200.000 310.196]"]; (* [7i] *)
accuracy = 1;
testClose[optimizeTuningMap, "[\:27e81 3 0 0 3] \:27e80 -3 5 6 1]}", "destretched-octave minimax-S", "\:27e81200.00 1915.81 2806.98 3368.38 4161.40]"]; (* [1b] has <1200 1915.578 2807.355 3368.826 4161.472|,but  Mike himself says that maybe he got this one wrong because it should have been TIP... and yeah, I can see that this one has a pair of locked primes! *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 2 6 2 10] \:27e80 -1 -9 2 -16]}", "destretched-octave minimax-S", "\:27e81200.0 490.4]"]; (* [1d] *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 2 6 2 1] \:27e80 -1 -9 2 6]}", "destretched-octave minimax-S", "\:27e81200.0 490.9]"]; (* [1d] *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 2 -3 2 1] \:27e80 -1 13 2 6]}", "destretched-octave minimax-S", "\:27e81200.0 491.9]"]; (* [1d] *)
accuracy = 3;
testClose[optimizeGeneratorTuningMap, "[\:27e81 1 2 1] \:27e80 1 0 2] \:27e80 0 1 2]}", "destretched-octave minimax-S", "\:27e81200.0 700.3907806 384.0221726]"]; (* [1e] this was passing with {1200.000, 700.795, 380.759} before introducing the non-unique check code and then went back to passing after maybe switching to Keenan's nested minimax technique...  it really does seem like it should have a unique solution, so the condition on that might be wrong... you should really plot this one visually and see what's happening *)
accuracy = 2;
testClose[optimizeGeneratorTuningMap, "[\:27e81 1 0] \:27e80 1 4]}", "destretched-octave minimax-S", "\:27e81200.0 696.58]"]; (* [1f] *)
testClose[optimizeGeneratorTuningMap, "[\:27e81 1 0 -3] \:27e80 1 4 10]}", "destretched-octave minimax-S", "\:27e81200.0 696.58]"]; (* [1f] *)
accuracy = 3;
(* original name *)
testClose[optimizeTuningMap, meantone, "POTOP", optimizeTuningMap[meantone, "destretched-octave minimax-S"]];
testClose[optimizeTuningMap, meantone, "POTT", optimizeTuningMap[meantone, "destretched-octave minimax-S"]];


(* ::Text:: *)
(*yes, technically it works with non-all-interval tuning schemes, but we do not recommend it, when you could hold intervals instead*)


testClose[optimizeGeneratorTuningMap, meantone, sixTilt <> " miniaverage-U", "\:27e81204.301 697.654]"];
destretchedOctaveSixTiltMiniaverageUResult = "\:27e81204.301 * 1200.000 / 1204.301, 697.654 * 1200.000 / 1204.301]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "octave"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "2"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "2/1"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-octave " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-2 " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-2/1 " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-{2} " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-{2/1} " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];


destretchedFifthSixTiltMiniaverageUResult = "\:27e81204.301 * 701.955 / 697.654, 697.654 * 701.955 / 697.654]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "3/2"}, destretchedFifthSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-3/2 " <> sixTilt <> " miniaverage-U", destretchedFifthSixTiltMiniaverageUResult];


(* ::Subsubsection::Closed:: *)
(*stress tests (how  big  can  we  go  before  crashing?)*)


(* ::Text:: *)
(*TILT minimax-U*)


optimizeGeneratorTuningMap["[\:27e853 84 123]}", "TILT minimax-U"]; (* 5-limit, 6-TILT *)


optimizeGeneratorTuningMap["[\:27e81 1 3 3] \:27e80 6 -7 -2]}", "TILT minimax-U"]; (* 7-limit, 10-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 -5 12] \:27e80 1 0 2 -1] \:27e80 0 1 2 -3]}", "TILT minimax-U"]; (* 11-limit, 12-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 4 -1] \:27e80 2 0 0 -3 3] \:27e80 0 1 0 2 1] \:27e80 0 0 1 -1 0]}", "TILT minimax-U"]; (* 13-limit, 16-TILT *)


(*optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", "TILT minimax-U"]; (* 17-limit, 18-TILT *)*)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", {"tuningSchemeSystematicName" -> "TILT minimax-U", "quick" -> True}]; (* runs with "quick" though *)


(* ::Text:: *)
(*TILT  miniRMS - U*)


optimizeGeneratorTuningMap["[\:27e853 84 123]}", "TILT miniRMS-U"]; (* 5-limit, 6-TILT *)


optimizeGeneratorTuningMap["[\:27e81 1 3 3] \:27e80 6 -7 -2]}", "TILT miniRMS-U"]; (* 7-limit, 10-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 -5 12] \:27e80 1 0 2 -1] \:27e80 0 1 2 -3]}", "TILT miniRMS-U"]; (* 11-limit, 12-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 4 -1] \:27e80 2 0 0 -3 3] \:27e80 0 1 0 2 1] \:27e80 0 0 1 -1 0]}", "TILT miniRMS-U"]; (* 13-limit, 16-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", "TILT miniRMS-U"]; (* 17-limit, 18-TILT *)


(* optimizeGeneratorTuningMap["[\:27e81 0 0 0 0 0 -1 0 0 0 0 0] \:27e80 1 0 0 0 0 -1 0 0 0 0 0] \:27e80 0 1 0 0 0 1 0 0 0 0 0] \:27e80 0 0 1 0 0 -1 0 0 0 0 0] \:27e80 0 0 0 1 0 1 0 0 0 0 0] \:27e80 0 0 0 0 1 1 0 0 0 0 0] \:27e80 0 0 0 0 0 0 1 0 0 0 0] \:27e80 0 0 0 0 0 0 0 1 0 0 0] \:27e80 0 0 0 0 0 0 0 0 1 0 0] \:27e80 0 0 0 0 0 0 0 0 0 1 0] \:27e80 0 0 0 0 0 0 0 0 0 0 1]}", "TILT miniRMS-S"]; *) (* 37-limit, 40-TILT; also makes it to the power limit solver, but fails to converge there and times out, which makes me think that we should nicely immediately user-facing abort this temperament straight away whether minimax or miniRMS, since it's not tractable; would just need to determine what exactly that limit of tractability is *)


(* ::Text:: *)
(*TILT miniaverage-U*)


optimizeGeneratorTuningMap["[\:27e853 84 123]}", "TILT miniaverage-U"]; (* 5-limit, 6-TILT *)


optimizeGeneratorTuningMap["[\:27e81 1 3 3] \:27e80 6 -7 -2]}", "TILT miniaverage-U"]; (* 7-limit, 10-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 -5 12] \:27e80 1 0 2 -1] \:27e80 0 1 2 -3]}", "TILT miniaverage-U"]; (* 11-limit, 12-TILT *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 4 -1] \:27e80 2 0 0 -3 3] \:27e80 0 1 0 2 1] \:27e80 0 0 1 -1 0]}", "TILT miniaverage-U"]; (* 13-limit, 16-TILT *)


(*optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", "TILT miniaverage-U"]; (* 17-limit, 18-TILT *)*)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", {"tuningSchemeSystematicName" -> "TILT miniaverage-U", "quick" -> True}]; (* runs with "quick" though *)


(* ::Subsubsection::Closed:: *)
(*all-interval tuning schemes*)


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight"}, "\:27e81201.699 697.564]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "\:27e81201.397 697.049]"];


testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-S"}, "\:27e8598.447 106.567]"];
testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-ES"}, "\:27e8598.859 106.844]"];


(* ::Text:: *)
(*minimax-S = "TOP", "T1", "TOP-max", "TIPTOP", "Tenney OPtimal", "Tiebreaker-In-Polytope Tenney-OPtimal"*)


(* I had to fudge the factors to make mapping forms match in some places, due to rounding errors those matching factors introduced *)
(* could double-check with Scala, Xen wiki, Flora's app but it has incorrect results for TOP at this time *)
accuracy = 2;
testClose[optimizeGeneratorTuningMap, meantone, "minimax-S", "\:27e81201.70, 1201.70 - 504.13]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-S", "\:27e8238.87, 238.86 * 11.0003 + 158.78]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, dicot, "minimax-S", "\:27e81207.66 353.22]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, augmented, "minimax-S", "\:27e8399.02, 399.018 * 5.00005 - 93.15]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, mavila, "minimax-S", "\:27e81206.55, 1206.55 + 685.03]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-S", "\:27e81196.91, 1034.59 - 1196.91]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, srutal, "minimax-S", "\:27e8599.56, 599.56 * 3.99999 - 494.86]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, hanson, "minimax-S", "\:27e81200.29 317.07]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, magic, "minimax-S", "\:27e81201.28 380.80]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, negri, "minimax-S", "\:27e81201.82, 1201.82 - 1075.68]"]; (* [5] as "negripent" (Table 1) *)
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-S", "\:27e81199.03 176.11]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-S", "\:27e81201.70, 1201.70 * 2 - 504.13]"]; (* [5](Table 2) *)
testClose[optimizeGeneratorTuningMap, magic7, "minimax-S", "\:27e81201.28 380.80]"]; (* [5] (Table 3) *)
testClose[optimizeGeneratorTuningMap, pajara, "minimax-S", "\:27e8598.45, 598.45 - 491.88]"];  (* [5](Table 2) *)
testClose[optimizeGeneratorTuningMap, augene, "minimax-S", "\:27e8399.02, 399.02 * 5 - 90.59]"]; (* [5] (Table 2) *)
testClose[optimizeGeneratorTuningMap, sensi, "minimax-S", "\:27e81198.39, 1198.39 - 755.23]"]; (* [5] as "sensisept" (Table 2) *)
(* original name *)
testClose[optimizeTuningMap, meantone, "TOP", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "T1", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TOP-max", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TIPTOP", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "Tenney", optimizeTuningMap[meantone, "minimax-S"]];
accuracy = 3;


(* ::Text:: *)
(*minimax-ES = "TE", "T2", "TOP-RMS", "Tenney-Euclidean"*)


(* could double-check with Scala, Sintel's app, Flora's app, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-ES", "\:27e81201.397 1898.446 2788.196]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "minimax-ES", "\:27e81194.308 1910.892 2786.314]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "minimax-ES", "\:27e81206.410 1907.322 2763.276]"]; (* [3a] *)
testClose[optimizeTuningMap, augmented, "minimax-ES", "\:27e81197.053 1901.955 2793.123]"]; (* [3b] *)
testClose[optimizeTuningMap, mavila, "minimax-ES", "\:27e81208.380 1892.933 2779.860]"]; (* [3c] *)
testClose[optimizeTuningMap, porcupine, "minimax-ES", "\:27e81199.562 1907.453 2779.234]"]; (* [3d] *)
testClose[optimizeTuningMap, srutal, "minimax-ES", "\:27e81198.823 1903.030 2787.467]"]; (* [3e] *)
testClose[optimizeTuningMap, hanson, "minimax-ES", "\:27e81200.166 1902.303 2785.418]"]; (* [3f] *)
testClose[optimizeTuningMap, magic, "minimax-ES", "\:27e81201.248 1902.269 2782.950]"]; (* [3g] *)
testClose[optimizeTuningMap, negri, "minimax-ES", "\:27e81202.347 1900.691 2782.698]"]; (* [3h] *)
testClose[optimizeTuningMap, tetracot, "minimax-ES", "\:27e81199.561 1903.942 2784.419]"]; (* [3i] *)
testClose[optimizeTuningMap, meantone7, "minimax-ES", "\:27e81201.242 1898.458 2788.863 3368.432]"]; (* [3j] *)
testClose[optimizeTuningMap, magic7, "minimax-ES", "\:27e81201.082 1903.476 2782.860 3367.259]"]; (* [3k] *)
testClose[optimizeTuningMap, pajara, "minimax-ES", "\:27e81197.719 1903.422 2780.608 3379.468]"]; (* [3l] *)
testClose[optimizeTuningMap, augene, "minimax-ES", "\:27e81196.255 1903.298 2791.261 3370.933]"]; (* [3m] *)
testClose[optimizeTuningMap, sensi, "minimax-ES", "\:27e81199.714 1903.225 2789.779 3363.173]"]; (* [3n] *)
testClose[optimizeTuningMap, sensamagic, "minimax-ES", "\:27e81200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3o] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "TE", optimizeGeneratorTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorTuningMap, meantone, "T2", optimizeGeneratorTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorTuningMap, meantone, "TOP-RMS", optimizeGeneratorTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorTuningMap, meantone, "Tenney-Euclidean", optimizeGeneratorTuningMap[meantone, "minimax-ES"]];


(* ::Text:: *)
(*held-octave minimax-ES = "CTE", "Constrained Tenney-Euclidean"*)


testClose[optimizeGeneratorTuningMap, meantone, "held-octave minimax-ES", "\:27e81200.000 697.214]"]; (* [8a] *)
testClose[optimizeGeneratorTuningMap, blackwood, "held-octave minimax-ES", "\:27e8240.000, 1200.000 * 2 + 386.314]"]; (* [8b] *)
testClose[optimizeGeneratorTuningMap, dicot, "held-octave minimax-ES", "\:27e81200.000 354.664]"]; (* [8c] *)
testClose[optimizeGeneratorTuningMap, augmented, "held-octave minimax-ES", "\:27e8400.000, 1200.000 + 701.955]"]; (* [8d] *)
testClose[optimizeGeneratorTuningMap, mavila, "held-octave minimax-ES", "\:27e81200.000, 1200.000 + 677.145]"]; (* [8e] *)
testClose[optimizeGeneratorTuningMap, porcupine, "held-octave minimax-ES", "\:27e81200.000 -164.166]"]; (* [8f] *)
testClose[optimizeGeneratorTuningMap, srutal, "held-octave minimax-ES", "\:27e8600.000, 1200.000 + 705.136]"]; (* [8g] *)
testClose[optimizeGeneratorTuningMap, hanson, "held-octave minimax-ES", "\:27e81200.000 317.059]"]; (* [8h] *)
testClose[optimizeGeneratorTuningMap, magic, "held-octave minimax-ES", "\:27e81200.000 380.499]"]; (* [8i] *)
testClose[optimizeGeneratorTuningMap, negri, "held-octave minimax-ES", "\:27e81200.000 125.396]"]; (* [8j] *)
testClose[optimizeGeneratorTuningMap, tetracot, "held-octave minimax-ES", "\:27e81200.000 176.028]"]; (* [8k] *)
testClose[optimizeGeneratorTuningMap, meantone7, "held-octave minimax-ES", "\:27e81200.000, 1200.000 + 696.952]"]; (* [8l] *)
testClose[optimizeGeneratorTuningMap, magic7, "held-octave minimax-ES", "\:27e81200.000 380.651]"]; (* [8m] *)
testClose[optimizeGeneratorTuningMap, pajara, "held-octave minimax-ES", "\:27e8600.000, 600.000 * -1 + 708.356]"]; (* [8n] *)
testClose[optimizeGeneratorTuningMap, augene, "held-octave minimax-ES", "\:27e8400.000, 1200.000 + 709.595]"]; (* [8o] *)
testClose[optimizeGeneratorTuningMap, sensi, "held-octave minimax-ES", "\:27e81200.000, 1200.000 - 756.683]"]; (* [8p] *)
testClose[optimizeGeneratorTuningMap, sensamagic, "held-octave minimax-ES", "\:27e81200.000, 1200.000 + 703.742, 440.902]"]; (* [8q] *)
testClose[optimizeTuningMap, meantone, "CTE", optimizeTuningMap[meantone, "held-octave minimax-ES"]];
testClose[optimizeTuningMap, meantone, "Constrained Tenney-Euclidean", optimizeTuningMap[meantone, "held-octave minimax-ES"]];


(* ::Text:: *)
(*proving that minimax-ES = primes miniRMS-S*)


testClose[optimizeGeneratorTuningMap, meantone, "minimax-ES", optimizeGeneratorTuningMap[meantone, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-ES", optimizeGeneratorTuningMap[blackwood, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-ES", optimizeGeneratorTuningMap[dicot, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-ES", optimizeGeneratorTuningMap[augmented, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-ES", optimizeGeneratorTuningMap[mavila, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-ES", optimizeGeneratorTuningMap[porcupine, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-ES", optimizeGeneratorTuningMap[srutal, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-ES", optimizeGeneratorTuningMap[hanson, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-ES", optimizeGeneratorTuningMap[magic, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-ES", optimizeGeneratorTuningMap[negri, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-ES", optimizeGeneratorTuningMap[tetracot, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-ES", optimizeGeneratorTuningMap[meantone7, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-ES", optimizeGeneratorTuningMap[magic7, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-ES", optimizeGeneratorTuningMap[pajara, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-ES", optimizeGeneratorTuningMap[augene, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-ES", optimizeGeneratorTuningMap[sensi, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-ES", optimizeGeneratorTuningMap[sensamagic, "primes miniRMS-S"]];


(* ::Text:: *)
(*proving that minimax-S = primes minimax-S*)


testClose[optimizeGeneratorTuningMap, meantone, "minimax-S", optimizeGeneratorTuningMap[meantone, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-S", optimizeGeneratorTuningMap[blackwood, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-S", optimizeGeneratorTuningMap[dicot, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-S", optimizeGeneratorTuningMap[augmented, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-S", optimizeGeneratorTuningMap[mavila, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-S", optimizeGeneratorTuningMap[porcupine, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-S", optimizeGeneratorTuningMap[srutal, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-S", optimizeGeneratorTuningMap[hanson, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-S", optimizeGeneratorTuningMap[magic, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-S", optimizeGeneratorTuningMap[negri, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-S", optimizeGeneratorTuningMap[tetracot, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-S", optimizeGeneratorTuningMap[meantone7, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-S", optimizeGeneratorTuningMap[magic7, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-S", optimizeGeneratorTuningMap[pajara, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-S", optimizeGeneratorTuningMap[augene, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-S", optimizeGeneratorTuningMap[sensi, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-S", optimizeGeneratorTuningMap[sensamagic, "primes minimax-S"]];


(*
sources:
[1] Facebook https://www.facebook.com
[1a] https://www.facebook.com/groups/xenharmonicmath/posts/2363908480416027/?comment_id=2363994823740726
[1b] https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/
[1c] https://www.facebook.com/groups/xenharmonicmath/posts/1035558283251060/?comment_id=1041634519310103&reply_comment_id=1041649585975263
[1d] https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064
[1e] https://www.facebook.com/groups/xenharmonicmath/posts/738498989623659/?comment_id=738515309622027
[1f] (link lost, sorry) "The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents."
[2] Yahoo posts https://yahootuninggroupsultimatebackup.github.io
[2a] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029
[2b] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_15819
[3] Graham's temperament app http://x31eq.com/temper/
[3a] http://x31eq.com/cgi-bin/rt.cgi?ets=3_7&limit=5
[3b] http://x31eq.com/cgi-bin/rt.cgi?ets=12_3&limit=5
[3c] http://x31eq.com/cgi-bin/rt.cgi?ets=7_2p&limit=5
[3d] http://x31eq.com/cgi-bin/rt.cgi?ets=7_15&limit=5
[3e] http://x31eq.com/cgi-bin/rt.cgi?ets=12_34&limit=5
[3f] http://x31eq.com/cgi-bin/rt.cgi?ets=53_19&limit=5
[3g] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=5
[3h] http://x31eq.com/cgi-bin/rt.cgi?ets=19_10&limit=5
[3i] http://x31eq.com/cgi-bin/rt.cgi?ets=7_34&limit=5
[3j] http://x31eq.com/cgi-bin/rt.cgi?ets=12_19&limit=7
[3k] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=7
[3l] http://x31eq.com/cgi-bin/rt.cgi?ets=12_10&limit=7
[3m] http://x31eq.com/cgi-bin/rt.cgi?ets=12_15&limit=7
[3n] http://x31eq.com/cgi-bin/rt.cgi?ets=19_27&limit=7
[3o] http://x31eq.com/cgi-bin/rt.cgi?ets=27_19_22&limit=7
[3p] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=3_7&tuning=po
[3q] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_3&tuning=po
[3r] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_2p&tuning=po
[3s] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_15&tuning=po
[3t] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_34&tuning=po
[3u] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=53_19&tuning=po
[3v] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_22&tuning=po
[3w] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_10&tuning=po
[3x] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_34&tuning=po
[3y] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_19&tuning=po
[3z] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_22&tuning=po
[3aa] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_10&tuning=po
[3ab] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_15&tuning=po
[3ac] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_27&tuning=po
[3ad] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=27_19_22&tuning=po
[4] Flora's temperament app https://github.com/FloraCanou/te_temperament_measures
[5] Paul's papers 
[5a] 
[6] Graham's papers http://x31eq.com/tuning.htm
[6a] 
[7] Xen wiki https://en.xen.wiki
[7a] https://en.xen.wiki/w/Target_tunings#Example
[7b] https://en.xen.wiki/w/Augene
[7c] https://en.xen.wiki/w/Porcupine
[7d] https://en.xen.wiki/w/Magic
[7e] https://en.xen.wiki/w/Tetracot_family#Tetracot
[7f] https://en.xen.wiki/w/Meantone
[7g] https://en.xen.wiki/w/Sensipent_family#Septimal_sensi
[7h] https://en.xen.wiki/w/Sensamagic_family#Sensamagic
[7i] https://en.xen.wiki/w/Myna#Tuning_spectrum
[7j] https://en.xen.wiki/w/Pajara#Tuning_spectrum
[7k] https://en.xen.wiki/w/Chromatic_pairs#Voltage
[8] Sintel's app https://github.com/Sin-tel/temper
[8a] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=81%2F80&submit_comma=submit
[8b] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=256%2F243&submit_comma=submit
[8c] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=25%2F24&submit_comma=submit
[8d] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=128%2F125&submit_comma=submit
[8e] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=135%2F128&submit_comma=submit
[8f] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=250%2F243&submit_comma=submit
[8g] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=2048%2F2025&submit_comma=submit
[8h] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=15625%2F15552&submit_comma=submit
[8i] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=3125%2F3072&submit_comma=submit
[8j] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=16875%2F16384&submit_comma=submit
[8k] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=20000%2F19683&submit_comma=submit
[8l] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=81%2F80%2C+126%2F125&submit_comma=submit
[8m] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=225%2F224%2C+245%2F243&submit_comma=submit
[8n] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=50%2F49%2C+64%2F63&submit_comma=submit
[8o] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=64%2F63%2C+126%2F125&submit_comma=submit
[8p] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=126%2F125%2C+245%2F243&submit_comma=submit
[8q] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=245%2F243&submit_comma=submit
[9] Scala
[10] Discord history https://discord.com/channels/332357996569034752
[10a] https://discord.com/channels/332357996569034752/859884647337033738/969259730839171123
[11] Keenan Pepper's tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/KeenanPepper/tiptop.py
[12] Mike Battaglia's tipweil.py variation on tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/MikeBattaglia/tipweil.py
*)


(* ::Text:: *)
(*stress tests*)


optimizeGeneratorTuningMap["[\:27e853 84 123]}", "minimax-S"]; (* 5-limit *)


optimizeGeneratorTuningMap["[\:27e81 1 3 3] \:27e80 6 -7 -2]}", "minimax-S"]; (* 7-limit *)


optimizeGeneratorTuningMap["[\:27e81 0 0 -5 12] \:27e80 1 0 2 -1] \:27e80 0 1 2 -3]}", "minimax-S"]; (* 11-limit *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 4 -1] \:27e80 2 0 0 -3 3] \:27e80 0 1 0 2 1] \:27e80 0 0 1 -1 0]}", "minimax-S"]; (* 13-limit *)


optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1] \:27e80 1 0 1 2 0 0] \:27e80 0 1 0 -1 0 0] \:27e80 0 0 2 1 0 -1] \:27e80 0 0 0 0 1 1]}", "minimax-S"]; (* 17-limit *)


(* optimizeGeneratorTuningMap["[\:27e81 0 0 0 2 0 1 0] \:27e80 1 0 1 2 0 0 0] \:27e80 0 1 0 -1 0 0 0] \:27e80 0 0 2 1 0 -1 0] \:27e80 0 0 0 0 1 1 0] \:27e80 0 0 0 0 0 0 1]}", "minimax-S"]; *) (* 19-limit *)


(* optimizeGeneratorTuningMap["[\:27e81 0 0 0 0 0 -1 0 0 0 0 0] \:27e80 1 0 0 0 0 -1 0 0 0 0 0] \:27e80 0 1 0 0 0 1 0 0 0 0 0] \:27e80 0 0 1 0 0 -1 0 0 0 0 0] \:27e80 0 0 0 1 0 1 0 0 0 0 0] \:27e80 0 0 0 0 1 1 0 0 0 0 0] \:27e80 0 0 0 0 0 0 1 0 0 0 0] \:27e80 0 0 0 0 0 0 0 1 0 0 0] \:27e80 0 0 0 0 0 0 0 0 1 0 0] \:27e80 0 0 0 0 0 0 0 0 0 1 0] \:27e80 0 0 0 0 0 0 0 0 0 0 1]}", "minimax-S"]; *) (* 37-limit, 40-TILT; makes it to the power limit solver, but fails to converge there and times out *)


(* ::Subsubsection::Closed:: *)
(*alternative complexities*)


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-copfr-C", "\:27e81200.813 696.570]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-C", "\:27e81201.489 696.662]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-C", "\:27e81201.507 696.668]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-copfr-C", "\:27e81200.522 696.591]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-lopfr-C", "\:27e81201.535 696.760]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-sopfr-C", "\:27e81201.503 696.732]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-copfr-limit-C", "\:27e81201.168 696.797]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-limit-C", "\:27e81202.087 696.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-limit-C", "\:27e81201.830 696.851]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-copfr-limit-C", "\:27e81201.024 696.834]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-lopfr-limit-C", "\:27e81202.009 696.981]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-sopfr-limit-C", "\:27e81201.898 696.913]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-copfr-C", "\:27e81200.000 696.182]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-lopfr-C", "\:27e81200.000 695.972]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-sopfr-C", "\:27e81200.000 695.974]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-copfr-C", "\:27e81200.000 696.350]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-lopfr-C", "\:27e81200.000 696.089]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-sopfr-C", "\:27e81200.000 696.078]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-copfr-limit-C", "\:27e81200.000 696.209]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-lopfr-limit-C", "\:27e81200.000 696.075]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-sopfr-limit-C", "\:27e81200.000 696.093]"];


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-copfr-limit-C", "\:27e81200.000 696.354]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-lopfr-limit-C", "\:27e81200.000 696.144]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-sopfr-limit-C", "\:27e81200.000 696.126]"];


(* ::Text:: *)
(*lp = lopfr = [blank]*)


result = "\:27e81201.489 696.662]";
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lp-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-C", result];


(* ::Text:: *)
(*lols = held-octave lils*)


result = "\:27e81200.000 696.075]";
testClose[optimizeGeneratorTuningMap, meantone, "held-octave TILT miniRMS-lils-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lols-C", result];


(* ::Text:: *)
(*prod = sopfr*)


result = "\:27e81201.507 696.668]";
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-prod-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-C", result];


(* ::Text:: *)
(*all-interval tuning schemes*)


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "\:27e81202.390 697.176]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "\:27e81202.607 696.741]"];
testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-copfr-S"}, "\:27e8597.119 103.293]"];
testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-E-copfr-S"}, "\:27e8598.345 106.693]"];


(* minimax-E-copfr-S = "Frobenius" *)
(* could double-check with Scala, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-E-copfr-S", "\:27e81202.6068 1899.3482 2786.9654]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-copfr-S", "\:27e81191.8899 1907.0238 2786.3137]"]; (* [4], though Flora's code does have a bug where prime 5 comes out as a 0 *)
testClose[optimizeTuningMap, dicot, "minimax-E-copfr-S", "\:27e81215.1441 1907.0030 2776.2177]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-copfr-S", "\:27e81195.0446 1901.9550 2788.4374]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-copfr-S", "\:27e81210.9365 1897.2679 2784.7514]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-copfr-S", "\:27e81198.5953 1908.9787 2782.0995]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-copfr-S", "\:27e81198.4746 1902.5097 2786.5911]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-copfr-S", "\:27e81200.5015 1902.3729 2785.8122]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-copfr-S", "\:27e81202.3503 1902.1900 2785.1386]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-copfr-S", "\:27e81203.2384 1901.2611 2785.3885]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-copfr-S", "\:27e81198.8664 1903.9955 2785.4068]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-copfr-S", "\:27e81201.3440 1898.5615 2788.8699 3368.1428]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-copfr-S", "\:27e81202.0285 1904.1849 2784.8940 3368.0151]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-copfr-S", "\:27e81196.6908 1901.7292 2778.3407 3376.6861]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-copfr-S", "\:27e81195.2617 1901.4887 2788.9439 3368.5928]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-copfr-S", "\:27e81198.2677 1904.0314 2790.4025 3364.8772]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-copfr-S", "\:27e81200.0000 1904.3201 2785.8407 3367.8799]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "Frobenius", optimizeGeneratorTuningMap[meantone, "minimax-E-copfr-S"]];


(* minimax-sopfr-S = "BOP", "Benedetti OPtimal" *)
testClose[optimizeTuningMap, meantone, "minimax-sopfr-S", "\:27e81201.7205 1899.3742 2790.6150]"];  (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-sopfr-S", "\:27e81194.179 1910.686 2786.314]"];  (* [4] has \:27e8820.9516 1313.5225 0.0000] due to a bug *)
testClose[optimizeTuningMap, dicot, "minimax-sopfr-S", "\:27e81207.4392 1913.1138 2767.7157]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-sopfr-S", "\:27e81197.1684 1901.9550 2793.3928]"];  (* [4] has \:27e81197.1684 1898.1244 2793.3928] which has the same damage, but prime 3 might as well be tuned pure *)
testClose[optimizeTuningMap, mavila, "minimax-sopfr-S", "\:27e81206.5842 1892.0787 2769.8533]"];  (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-sopfr-S", "\:27e81196.9271 1906.5643 2778.6315]"];  (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-sopfr-S", "\:27e81199.1112 1903.2881 2788.5356]"];  (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-sopfr-S", "\:27e81200.2845 1902.3817 2785.6025]"];  (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-sopfr-S", "\:27e81201.2339 1903.8058 2783.2290]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-sopfr-S", "\:27e81201.7937 1899.2645 2781.8295]"]; (* [4] *)
accuracy = 2;
testClose[optimizeTuningMap, tetracot, "minimax-sopfr-S", "\:27e81199.0293 1903.4111 2783.8883]"];  (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, meantone7, "minimax-sopfr-S", "\:27e81201.721 1899.374 2790.615 3371.376]"]; (* [4] has \:27e81201.7494 1899.4211 2790.6871 3371.4697], but that has 0.875 damage and mine has 0.860 damage *)
accuracy = 2;
testClose[optimizeTuningMap, magic7, "minimax-sopfr-S", "\:27e81201.2340 1903.8044 2783.2288 3367.8966]"];  (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, pajara, "minimax-sopfr-S", "\:27e81197.3094 1902.8073 2779.5873 3378.2420]"];  (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-sopfr-S", "\:27e81197.168 1904.326 2793.393 3374.358]"];  (* [4] has \:27e81197.1684 1902.1518 2793.3928 3378.7064] which has the same damage, but it can be visualized with graphTuningDamage[augene, "minimax-sopfr-S"] that mine does a nested minimax, minimizing the maximum damage between primes 3 and 7 underneath the minimax boundary between primes 2 and 5 *)
testClose[optimizeTuningMap, sensi, "minimax-sopfr-S", "\:27e81198.5891 1903.5233 2789.8411 3363.8876]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-sopfr-S", "\:27e81200.0000 1903.2071 2784.2269 3365.9043]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "BOP", optimizeGeneratorTuningMap[meantone, "minimax-sopfr-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "Benedetti", optimizeGeneratorTuningMap[meantone, "minimax-sopfr-S"]];


(* minimax-E-sopfr-S = "BE", "Benedetti-Euclidean" *)
testClose[optimizeTuningMap, meantone, "minimax-E-sopfr-S", "\:27e81201.4768 1898.6321 2788.6213]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-sopfr-S", "\:27e81193.9975 1910.3960 2786.3137]"]; (* [4] has \:27e81193.9975 1910.3960 0.0000] due to a bug *)
testClose[optimizeTuningMap, dicot, "minimax-E-sopfr-S", "\:27e81205.8488 1906.3416 2761.9439]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-sopfr-S", "\:27e81197.2692 1901.9550 2793.6282]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-sopfr-S", "\:27e81208.5464 1893.7139 2778.683]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-sopfr-S", "\:27e81199.5668 1906.8283 2778.1916]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-sopfr-S", "\:27e81198.8183 1902.9219 2787.6566]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-sopfr-S", "\:27e81200.1533 1902.2425 2785.3554]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-sopfr-S", "\:27e81201.1456 1902.2128 2782.7337]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-sopfr-S", "\:27e81202.2630 1900.8639 2782.2726]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-sopfr-S", "\:27e81199.5499 1903.7780 2784.0631]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-sopfr-S", "\:27e81201.3847 1898.6480 2789.0531 3368.4787]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-sopfr-S", "\:27e81200.9990 1903.1832 2782.6345 3366.6407]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-sopfr-S", "\:27e81197.9072 1903.2635 2781.9626 3380.9162]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-sopfr-S", "\:27e81196.4076 1903.1641 2791.6178 3372.1175]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-sopfr-S", "\:27e81199.7904 1902.7978 2789.2516 3362.3687]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-sopfr-S", "\:27e81200.0000 1903.3868 2785.5183 3365.7078]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "BE", optimizeGeneratorTuningMap[meantone, "minimax-E-sopfr-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "Benedetti-Euclidean", optimizeGeneratorTuningMap[meantone, "minimax-E-sopfr-S"]];


(* minimax-lils-S = "Weil" *)
(* could maybe double-check w/ Flora's app but we're aware at this time that her implementation uses the pseudoinverse
of the Weil interval complexity norm pre-transformer which doesn't work correctly *)
testClose[optimizeTuningMap, meantone, "minimax-lils-S", "\:27e81200.000 1896.578 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, blackwood, "minimax-lils-S", "\:27e81188.722 1901.955 2773.22]"]; (* [2a] *)
testClose[optimizeTuningMap, dicot, "minimax-lils-S", "\:27e81200.000 1901.955 2750.978]"]; (* [2a] *)
testClose[optimizeTuningMap, augmented, "minimax-lils-S", "\:27e81194.134 1897.307 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, mavila, "minimax-lils-S", "\:27e81200.000 1881.31 2756.07]"]; (* [2a] *)
testClose[optimizeTuningMap, porcupine, "minimax-lils-S", "\:27e81193.828 1901.955 2771.982]"]; (* [2a] *)
testClose[optimizeTuningMap, srutal, "minimax-lils-S", "\:27e81198.222 1901.955 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, hanson, "minimax-lils-S", "\:27e81200.000 1901.955 2784.963]"]; (* [2a] *)
testClose[optimizeTuningMap, magic, "minimax-lils-S", "\:27e81200.000 1901.955 2780.391]"]; (* [2a] *)
testClose[optimizeTuningMap, negri, "minimax-lils-S", "\:27e81200.000 1896.185 2777.861]"]; (* [2a] *)
testClose[optimizeTuningMap, tetracot, "minimax-lils-S", "\:27e81198.064 1901.955 2781.819]"]; (* [2a] *)
testClose[optimizeTuningMap, meantone7, "minimax-lils-S", "\:27e81200.000 1896.578 2786.314 3365.784]"]; (* [2a] *)
testClose[optimizeTuningMap, magic7, "minimax-lils-S", "\:27e81200.000 1901.955 2780.391 3364.692]"]; (* [2a] *)
testClose[optimizeTuningMap, pajara, "minimax-lils-S", "\:27e81193.803 1896.996 2771.924 3368.826]"]; (* [2a] *)
testClose[optimizeTuningMap, augene, "minimax-lils-S", "\:27e81194.134 1899.852 2786.314 3365.102]"]; (* [2a] *)
testClose[optimizeTuningMap, sensi, "minimax-lils-S", "\:27e81196.783 1901.181 2786.314 3359.796]"]; (* [2a] *)
(* sensamagic - no examples to work off of*)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "Weil", optimizeGeneratorTuningMap[meantone, "minimax-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "WOP", optimizeGeneratorTuningMap[meantone, "minimax-lils-S"]];


(* minimax-E-lils-S = "WE", "Weil-Euclidean" *)
(* could maybe double check w/ Sintel's app; what he calls Weil is actually Weil-Euclidean, according to Tom here: [10a] and I think he's right 
but unfortunately it's not easily discernible from his code at this time *)
testClose[optimizeTuningMap, meantone, "minimax-E-lils-S", "\:27e81201.3906 1898.4361 2788.1819]"]; (* [4] and [1a] has \:27e81201.391 1898.436 2788.182] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-lils-S", "\:27e81194.2544 1910.8071 2786.1895]"]; (* [1a] has \:27e81194.254 1910.807 2786.189]; [4] has a bug with this *)
testClose[optimizeTuningMap, dicot, "minimax-E-lils-S", "\:27e81206.2832 1907.1223 2762.9860]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-lils-S", "\:27e81197.0385 1901.9322 2793.0898]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-lils-S", "\:27e81208.2873 1892.7881 2779.6466]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-lils-S", "\:27e81199.5444 1907.4244 2779.1926]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-lils-S", "\:27e81198.8214 1903.0273 2787.4633]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-lils-S", "\:27e81200.1659 1902.3024 2785.4179]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-lils-S", "\:27e81201.2449 1902.2636 2782.9425]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-lils-S", "\:27e81202.3403 1900.6800 2782.6811]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-lils-S", "\:27e81199.5586 1903.9387 2784.4138]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-lils-S", "\:27e81201.2358, 1898.4479, 2788.8486, 3368.4143]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-lils-S", "\:27e81201.0786, 1903.4695, 2782.8510, 3367.2482]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-lils-S", "\:27e81197.6967, 1903.3872, 2780.5573, 3379.4056]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-lils-S", "\:27e81196.2383, 1903.2719, 2791.2228, 3370.8863]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-lils-S", "\:27e81199.7081, 1903.2158, 2789.7655, 3363.1568]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-lils-S", "\:27e81199.9983 1903.7398 2785.5426 3366.5781]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "WE", optimizeGeneratorTuningMap[meantone, "minimax-E-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "Weil-Euclidean", optimizeGeneratorTuningMap[meantone, "minimax-E-lils-S"]];


(* destretched-octave minimax-lils-S *)
(* these used to be known as "Kees" but in 2024, along with an analogous rename for "KE" = "Kees-Euclidean", that was redefined to be *held-octave* minimax-lils-S *)
(* this is the only actual example of a Kees tuning ever stated publicly by a human *)
accuracy = 0;
testClose[optimizeTuningMap, "[\:27e81 3 0 0 3] \:27e80 -3 5 6 1]}", "destretched-octave minimax-lils-S", "\:27e81200.000 1915.929 2806.785 3368.142 4161.357]"]; (* [1b] *)
accuracy = 3;
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "Kees", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "KOP", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-lils-S"]];


(* *held-octave* minimax-E-lils-S = minimax-E-lols-S = "CWE" = "constrained Weil-Euclidean" *)
(* "KE" = "Kees-Euclidean" used to be defined as *destretched-octave* minimax-E-lils-S, but in 2024, along with an analogous rename for "Kees", it was changed to *held-octave* minimax-E-lils-S *)
(* besides, the only known historical examples of these were given as held-octave anyway *)
testClose[optimizeTuningMap, meantone, "held-octave minimax-E-lils-S", "\:27e81200.0000 1896.6512 2786.605]"]; (* [4]; [1a] has \:27e81200.000 1896.651 2786.605] *)
testClose[optimizeTuningMap, blackwood, "held-octave minimax-E-lils-S", "\:27e81200.0000 1920.0000 2795.1253]"]; (* [1a] has \:27e81200.000 1920.000 2795.126]; [4] has a bug with this one *)
testClose[optimizeTuningMap, dicot, "held-octave minimax-E-lils-S", "\:27e81200.0000 1902.1712 2751.0856]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "held-octave minimax-E-lils-S", "\:27e81200.0000 1905.0691 2800.0000]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "held-octave minimax-E-lils-S", "\:27e81200.0000 1879.1114 2762.6658]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "held-octave minimax-E-lils-S", "\:27e81200.0000 1907.8138 2779.6896]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "held-octave minimax-E-lils-S", "\:27e81200.0000 1904.9585 2790.0830]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "held-octave minimax-E-lils-S", "\:27e81200.0000 1902.1850 2785.1542]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "held-octave minimax-E-lils-S", "\:27e81200.0000 1901.0972 2780.2194]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "held-octave minimax-E-lils-S", "\:27e81200.0000 1897.3560 2776.9830]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "held-octave minimax-E-lils-S", "\:27e81200.0000 1904.3859 2784.8683]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "held-octave minimax-E-lils-S", "\:27e81200.0000 1896.6562 2786.6248 3366.5620]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "held-octave minimax-E-lils-S", "\:27e81200.0000 1902.2878 2780.4576 3365.4906]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "held-octave minimax-E-lils-S", "\:27e81200.0000 1907.3438 2785.3124 3385.3124]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "held-octave minimax-E-lils-S", "\:27e81200.0000 1909.3248 2800.0000 3381.3503]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "held-octave minimax-E-lils-S", "\:27e81200.0000 1903.4449 2790.1435 3363.5406]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "held-octave minimax-E-lils-S", "\:27e81200.0000 1903.7411 2785.5446 3366.5805]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "KE", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-E-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "Kees-Euclidean", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-E-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "CWE", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-E-lils-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "constrained Weil-Euclidean", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-E-lils-S"]];


(* confirming the relationship between tuning schemes using log-integer-limit and log-product as their interval complexities, for various target-interval sets and optimization powers *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimax-lils-S"}, "\:27e81201.191 697.405]"];                           (* lils    / non-all / max *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-lils-S"}, "\:27e81200.000 696.578]"];                          (* lils    / non-all / sum *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniRMS-lils-S"}, "\:27e81201.648 697.183]"];                            (* lils    / non-all / sos *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " mini-3-mean-lils-S"}, "\:27e81201.621 697.326]"];                       (* lils    / non-all / sop *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lils-S"}, "\:27e81200.000 696.578]"];                                       (* lils    / all     / max *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lils-S", "intervalComplexityNormPower" -> \[Infinity]}, "\:27e81200.000 696.578]"];           (* lils    / all     / sum *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-E-lils-S"}, "\:27e81201.391 697.045]"];                                     (* lils    / all     / sos *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lils-S", "intervalComplexityNormPower" -> 3}, "\:27e81201.038 696.782]"];           (* lils     / all     / sop *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimax-S"}, "\:27e81201.699 697.564]"];                               (* non-lils/ non-all / max *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-S"}, "\:27e81200.000 696.578]"];                              (* non-lils/ non-all / sum *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniRMS-S"}, "\:27e81201.617 697.379]"];                                (* non-lils/ non-all / sos *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " mini-3-mean-S", "optimizationPower" -> 3}, "\:27e81201.603 697.601]"]; (* non-lils/ non-all / sop*)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S"}, "\:27e81201.699 697.564]"];                                           (* non-lils/ all     / max *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPower" -> \[Infinity]}, "\:27e81200.000 696.578]"];               (* non-lils/ all     / sum *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S"}, "\:27e81201.699 697.564]"];                                           (* non-lils/ all     / sos *)
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPower" -> 3}, "\:27e81201.039 696.782]"];               (* non-lils/ all     / sop *)


(* continuum between minimax-S (Mike's k = 0) and minimax-lils-S (Mike's k = 1) as well as beyond (k > 1) *)
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPreTransformerSizeFactor" -> 0.00}, "\:27e81201.699 1899.263 2790.258]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPreTransformerSizeFactor" -> 0.25}, "\:27e81201.273 1898.591 2789.271]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPreTransformerSizeFactor" -> 0.50}, "\:27e81200.849 1897.920 2788.284]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPreTransformerSizeFactor" -> 1.00}, "\:27e81200.000 1896.578 2786.314]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPreTransformerSizeFactor" -> 2.00}, "\:27e81198.306 1893.902 2782.381]"];


(* proving that minimax-E-copfr-S = primes miniRMS-U *)
testClose[optimizeGeneratorTuningMap, meantone, "minimax-E-copfr-S", optimizeGeneratorTuningMap[meantone, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-E-copfr-S", optimizeGeneratorTuningMap[blackwood, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-E-copfr-S", optimizeGeneratorTuningMap[dicot, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-E-copfr-S", optimizeGeneratorTuningMap[augmented, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-E-copfr-S", optimizeGeneratorTuningMap[mavila, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-E-copfr-S", optimizeGeneratorTuningMap[porcupine, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-E-copfr-S", optimizeGeneratorTuningMap[srutal, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-E-copfr-S", optimizeGeneratorTuningMap[hanson, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-E-copfr-S", optimizeGeneratorTuningMap[magic, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-E-copfr-S", optimizeGeneratorTuningMap[negri, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-E-copfr-S", optimizeGeneratorTuningMap[tetracot, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-E-copfr-S", optimizeGeneratorTuningMap[meantone7, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-E-copfr-S", optimizeGeneratorTuningMap[magic7, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-E-copfr-S", optimizeGeneratorTuningMap[pajara, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-E-copfr-S", optimizeGeneratorTuningMap[augene, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-E-copfr-S", optimizeGeneratorTuningMap[sensi, "primes miniRMS-U"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-E-copfr-S", optimizeGeneratorTuningMap[sensamagic, "primes miniRMS-U"]];


(* proving that minimax-copfr-S = primes minimax-U *)
testClose[optimizeGeneratorTuningMap, meantone, "minimax-copfr-S", optimizeGeneratorTuningMap[meantone, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-copfr-S", optimizeGeneratorTuningMap[blackwood, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-copfr-S", optimizeGeneratorTuningMap[dicot, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-copfr-S", optimizeGeneratorTuningMap[augmented, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-copfr-S", optimizeGeneratorTuningMap[mavila, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-copfr-S", optimizeGeneratorTuningMap[porcupine, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-copfr-S", optimizeGeneratorTuningMap[srutal, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-copfr-S", optimizeGeneratorTuningMap[hanson, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-copfr-S", optimizeGeneratorTuningMap[magic, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-copfr-S", optimizeGeneratorTuningMap[negri, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-copfr-S", optimizeGeneratorTuningMap[tetracot, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-copfr-S", optimizeGeneratorTuningMap[meantone7, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-copfr-S", optimizeGeneratorTuningMap[magic7, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-copfr-S", optimizeGeneratorTuningMap[pajara, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-copfr-S", optimizeGeneratorTuningMap[augene, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-copfr-S", optimizeGeneratorTuningMap[sensi, "primes minimax-U"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-copfr-S", optimizeGeneratorTuningMap[sensamagic, "primes minimax-U"]];


(* ::Subsubsection::Closed:: *)
(*nonstandard domain*)


barbados = "2.3.13/5 [\:27e81 2 2] \:27e80 -2 -3]]";
testClose[
  optimizeGeneratorTuningMap,
  barbados,
  {
    "targetIntervals" -> "[[1 0 0\:27e9 [0 1 0\:27e9 [0 0 1\:27e9 [-1 1 0\:27e9 [-1 0 1\:27e9 [0 1 -1\:27e9]", (* note these are in 2.3.13/5, the non-prime-only basis *)
    "nonprimeBasisApproach" -> "nonprime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "\:27e81198.919 248.212]"
];


testClose[
  optimizeGeneratorTuningMap,
  barbados,
  {
    "targetIntervals" -> "[[1 0 0 0\:27e9 [-2 0 1 0\:27e9 [-2 1 0 0\:27e9 [0 1 -1 0\:27e9 [-2 0 0 1\:27e9 [0 0 -1 1\:27e9 [-3 0 0 1\:27e9 [0 -1 0 1\:27e9]", (* note these are in 2.3.5.13, the prime-only basis *)
    "nonprimeBasisApproach" -> "prime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "\:27e81200.370 248.863]"
];


(* ::Text:: *)
(*arbitrary example I picked for article*)


articleExample = "2.7/3.11/3 [\:27e81 1 2] \:27e80 2 -1]]";
scheme = {
  "targetIntervals" -> "TILT",
  "optimizationPower" -> \[Infinity],
  "damageWeightSlope" -> "complexityWeight"
};


(* ::Text:: *)
(*TILT  should  yield  "[[1 0 0\:27e9 [0 1 0\:27e9 [-1 1 0\:27e9 [-1 0 1\:27e9 [0 -1 1\:27e9 [2 -1 0\:27e9]"*)


testClose[
  optimizeGeneratorTuningMap,
  articleExample,
  scheme,
  "\:27e81194.291 135.186]"
];


(* ::Text:: *)
(*TILT  should  yield  "[[1 0 0\:27e9 [0 1 0\:27e9 [-1 1 0\:27e9 [-1 0 1\:27e9 [0 -1 1\:27e9 [2 -1 0\:27e9]"*)


testClose[
  optimizeGeneratorTuningMap,
  articleExample,
  Join[scheme, {"nonprimeBasisApproach" -> "nonprime-based"}],
  "\:27e81192.399 133.768]"
];


(* ::Text:: *)
(*TILT  should  yield  "[[1 0 0 0\:27e9 [0 1 0 0\:27e9 [-1 1 0 0\:27e9 [2 -1 0 0\:27e9 [0 -1 1 0\:27e9 [-2 0 1 0\:27e9 [-1 -1 1 0\:27e9 [3 -1 0 0\:27e9 [-2 2 0 0\:27e9 [0 2 -1 0\:27e9 [-2 0 0 1\:27e9 [-1 -1 0 1\:27e9 [0 0 -1 1\:27e9 [-3 0 0 1\:27e9 [0 -2 0 1\:27e9 [2 1 -1 0\:27e9 ]"*)


(* TODO: this is now failing, finding "\:27e81193.409 135.892]" instead for some reason *)
(*
testClose[
  optimizeGeneratorTuningMap,
  articleExample,
  Join[scheme, {"nonprimeBasisApproach" -> "prime-based"}],
  "\:27e81193.102 135.810]"
];
*)


(* ::Text:: *)
(*trying to figure out this stuff about coprime and when it matters whether you pick prime-based or nonprime-based: non-all-interval edition*)


(* 2.9.7.11 -  non-prime but co-prime, so no difference *)
machine = "2.9.7.11 [\:27e81 3 3 4] \:27e80 1 -1 -3]}";
matchingTuning = "\:27e81197.268 207.170]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "{2/1, 9/4, 11/7} nonprime-based minimax-C"}, matchingTuning ];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "{2/1, 9/4, 11/7} prime-based minimax-C"}, matchingTuning];


(* 2.3.13/5 - non-prime but co-prime, so no difference *)
barbados = "2.3.13/5 [\:27e81 2 2] \:27e80 -2 -3]}";
matchingTuning = "\:27e81197.437 247.741]";
testClose[optimizeGeneratorTuningMap, barbados, {"tuningSchemeSystematicName" -> "{3/2, 13/10, 15/13} nonprime-based minimax-C"}, matchingTuning];
testClose[optimizeGeneratorTuningMap, barbados, {"tuningSchemeSystematicName" -> "{3/2, 13/10, 15/13} prime-based minimax-C"}, matchingTuning];


(* 2.5/3.7/3 - not even co-prime, now there could be a difference, but there's not yet... *)
starlingtet = "2.5/3.7/3 [\:27e81 1 2] \:27e80 -1 -3]}";
matchingTuning = "\:27e81213.795 315.641]";
testClose[optimizeGeneratorTuningMap, starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} nonprime-based minimax-C"}, matchingTuning];
testClose[optimizeGeneratorTuningMap, starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} prime-based minimax-C"}, matchingTuning];


(* ::Text:: *)
(*all-interval set schemes*)


scheme = {
  "targetIntervals" -> {},
  "optimizationPower" -> \[Infinity],
  "damageWeightSlope" -> "simplicityWeight",
  "intervalComplexityNormPower" -> 2
};


t = "2.7/5.11 [\:27e81 1 5] \:27e80 -1 -3]}";
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"nonprimeBasisApproach" -> "nonprime-based"}],
  "\:27e81200.4181 617.7581]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_7%2F5_11&ets=2_33 *)
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"nonprimeBasisApproach" -> "prime-based"}],
  "\:27e81200.0558 616.4318]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_7%2F5_11&ets=2_33&subgroup=on *)


t = "2.9.5.21 [\:27e81 0 -4 0] \:27e80 1 2 0] \:27e80 0 0 1]}";
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"nonprimeBasisApproach" -> "nonprime-based"}],
  "\:27e81201.3969 3796.8919 5270.7809]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?ets=13_12_6&limit=2_9_5_21 *)
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"nonprimeBasisApproach" -> "prime-based"}],
  "\:27e81201.3969 3796.8919 5267.2719]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_9_5_21&ets=13_12_6&subgroup=on *)


(* trying to figure out this stuff about coprime and when it matters whether you pick prime-based or nonprime-based: all-interval edition *)
machine = "2.9.7.11 [\:27e81 3 3 4] \:27e80 1 -1 -3]]";


(* 2.9.7.11 -  non-prime but co-prime, so no difference, as per Graham's online app, since this is minimax-ES, the tuning his thing uses *)
matchingTuning = "\:27e81197.281 213.899]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-ES"(*,"logging" -> True*)}, matchingTuning];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "nonprime-based minimax-ES"(*,"logging" -> True*)}, matchingTuning];


(* 2.9.7.11 -  non-prime but co-prime, so no difference, though not supported by Graham's app, since this is minimax-S, which his app does not use*)
matchingTuning = "\:27e81197.344 215.749]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-S"(*,"logging" -> True*)}, matchingTuning];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "nonprime-based minimax-S"(*,"logging" -> True*)}, matchingTuning];


(* 2.9.7.11 - non-prime but co-prime, however, difference achieved, on account of using E-copfr-complexity *)
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-E-copfr-S"(*,"logging" -> True*)}, "\:27e81195.547 211.194]"];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "nonprime-based minimax-E-copfr-S"(*,"logging" -> True*)}, "\:27e81196.398 212.537]"];


(* 2.9.7.11 - non-prime but co-prime, however, difference achieved, on account of using E-sopfr-complexity *)
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-E-sopfr-S"(*,"logging" -> True*)}, "\:27e81197.440 214.315]"];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "nonprime-based minimax-E-sopfr-S"(*,"logging" -> True*)}, "\:27e81197.766 215.083]"];


(* ::Subsection::Closed:: *)
(*tuning scheme options*)


(* ::Subsubsection::Closed:: *)
(*processTilt - make sure it picks the default max integer for it correctly based on the domain basis, when max integer is unspecified*)


fiveToSixTiltT = {{{2, 3, 5}}, "row"};
test[processTilt, "TILT", fiveToSixTiltT, processTilt["6-TILT", fiveToSixTiltT]];
sevenToTenTiltT = {{{2, 3, 5, 7}}, "row"};
test[processTilt, "TILT", sevenToTenTiltT, processTilt["10-TILT", sevenToTenTiltT]];


(* ::Subsubsection::Closed:: *)
(*processOld - make sure it picks the default max odd for it correctly based on the domain basis, when max odd is unspecified*)


fiveToFiveOldT = {{{2, 3, 5}}, "row"};
test[processOld, "OLD", fiveToFiveOldT, processOld["5-OLD", fiveToFiveOldT]];
sevenToNineOldT = {{{2, 3, 5, 7}}, "row"};
test[processOld, "OLD", sevenToNineOldT, processOld["9-OLD", sevenToNineOldT]];


(* ::Subsubsection::Closed:: *)
(*processOld - make  sure  it  picks  the  default  max  odd  for  it  correctly  based  on  the  domain  basis, when  max  odd  is  unspecified*)


twentyoneToTwentyoneOldT = {{}, "row", {2, 9, 21}};
test[processOld, "OLD", twentyoneToTwentyoneOldT, processOld["21-OLD", twentyoneToTwentyoneOldT]];


(* ::Subsubsection::Closed:: *)
(*processTilt  make  sure  it  picks  the  default  max  integer  for  it  correctly  based  on  the  domain  basis, when  max  integer  is  unspecified*)


twentyoneToTwentytwoTiltT = {{}, "row", {2, 9, 21}};
test[processTilt, "TILT", twentyoneToTwentytwoTiltT, processTilt["22-TILT", twentyoneToTwentytwoTiltT]];


(* ::Subsection::Closed:: *)
(*shared*)


test[getJustTuningMap, {{12, 19, 28}, "row", {2, 3, 5}}, {{1200 * Log2[2], 1200 * Log2[3], 1200 * Log2[5]}, "row"}];
test[getJustTuningMap, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "row", {2, 9, 5, 21}}, {{1200 * Log2[2], 1200 * Log2[9], 1200 * Log2[5], 1200 * Log2[21]}, "row"}];


test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}}, "row"}, {{{1 / Log2[2], 0, 0}, {0, 1 / Log2[3], 0}, {0, 0, 1 / Log2[5]}}, "row"}];
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}, {Log2[2], Log2[3], Log[5]}}, "row"}, {{{1 / Log2[2], 0, 0, 0}, {0, 1 / Log2[3], 0, 0}, {0, 0, 1 / Log2[5], 0}}, "row"}];


(* ::Subsection::Closed:: *)
(*complexity*)


test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 0, 0, 0, "", 3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 2, 0, 0, 0, "", \[Sqrt]3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 1, 0, 0, "", 1 +Log[3]/Log[2]+Log[5]/Log[2]];


(* ::Subsection::Closed:: *)
(*methods, by optimization or dual norm power*)


(* ::Subsubsection:: *)
(*\[Infinity]*)


(* ::Subsubsection:: *)
(*1*)


(* ::Subsubsection:: *)
(*2*)


(* ::Subsubsection:: *)
(*general*)


(* ::Subsection::Closed:: *)
(*mean damage*)


testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81201.70 697.564]", "minimax-S", 1.700];


testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81199.02 695.601]", "held-octave " <> fiveOld <> " miniRMS-U", 3.893];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81200.00 696.578]", "held-octave " <> fiveOld <> " minimax-U", 5.377];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81200.00 696.594]", "TILT miniRMS-S", 1.625];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81200.00 696.594]", "TILT miniaverage-S", 1.185];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81200.00 696.594]", "TILT mini-3-mean-S", 1.901];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "\:27e81200.00 696.594]", "TILT minimax-S", 3.382];


testDamageMeanOrComplexity[getTuningMapMeanDamage, meantone, "\:27e81200.000 1897.564 2786.314]", {"targetIntervals" -> "{2,3,5}", "damageWeightSlope" -> "unityWeight", "optimizationPower" -> \[Infinity]}, 4.391];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "\:27e812 29 28]", "\:27e81200 1900 2800]", sixTilt <> " miniRMS-U", 10.461];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "\:27e812 29 28]", "\:27e81200 1900 2800]", sixTilt <> " miniaverage-U", 8.065];


(* ::Subsection::Closed:: *)
(*conversion*)


meantoneM = "[\:27e81 1 0] \:27e80 1 4]}";
quarterCommaTuningMap = "\:27e81200.000 1896.578 2786.314]";
test[generatorTuningMapFromTAndTuningMap, meantoneM, quarterCommaTuningMap, "\:27e81200.000 696.578]"];


(* ::Subsection::Closed:: *)
(*damages*)


testDamages[getGeneratorTuningMapDamages, meantone, "\:27e81201.7 697.564]", "minimax-S", {2 -> 1.700, 3 -> 1.698, 5 -> 1.698}];
testDamages[getGeneratorTuningMapDamages, meantone, "\:27e81199.02 695.601]", "TILT miniRMS-U", {2 / 1 -> 0.980, 3 / 1 -> 7.334, 3 / 2 -> 6.354, 4 / 3 -> 5.374, 5 / 2 -> 2.930, 5 / 3 -> 3.424, 5 / 4 -> 1.950, 6 / 5 -> 4.404}];
testDamages[getGeneratorTuningMapDamages, meantone, "\:27e81200.0 696.578]", "TILT minimax-U", {2 / 1 -> 0.000, 3 / 1 -> 5.377, 3 / 2 -> 5.377, 4 / 3 -> 5.377, 5 / 2 -> 0.002, 5 / 3 -> 5.375, 5 / 4 -> 0.002, 6 / 5 -> 5.375}];


testDamages[getTuningMapDamages, meantone, "\:27e81200.000 1897.564 2786.314]", {"targetIntervals" -> "{2,3,5}", "damageWeightSlope" -> "unityWeight", "optimizationPower" -> \[Infinity]}, {2 -> 0.000, 3 -> 4.391, 5 -> 0.000}];
testDamages[getTuningMapDamages, "\:27e812 29 28]", "\:27e81200 1900 2800]", sixTilt <> " miniRMS-U", {2/1 -> 0.000, 3/1 -> 1.955, 3/2 -> 1.955, 4/3 -> 1.955, 5/2 -> 13.686, 5/3 -> 15.641, 5/4 -> 13.686, 6/5 -> 15.641}];


(* ::Subsection::Closed:: *)
(*target-interval set schemes*)


(* ::Subsubsection::Closed:: *)
(*truncated integer-limit triangle*)


testTargetSetScheme[getTilt, 4, {2 / 1, 3 / 1, 3 / 2, 4 / 3}]; (* 4/1 first interval excluded due to max size of 13/4 *)
testTargetSetScheme[getTilt, 6, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5}];
testTargetSetScheme[getTilt, 8, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5}]; (* 8/7 first interval excluded due to min size of 15/13 *)
testTargetSetScheme[getTilt, 10, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7}]; (* for 7-prime-limit temperaments, either 8 or 10 are reasonable choices *)
testTargetSetScheme[getTilt, 12, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7}];
testTargetSetScheme[getTilt, 14, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11}];
testTargetSetScheme[getTilt, 16, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13}];
testTargetSetScheme[getTilt, 18, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13, 17 / 6, 17 / 7, 17 / 8, 17 / 9, 17 / 10, 17 / 11, 17 / 12, 17 / 13, 18 / 7, 18 / 11, 18 / 13}]; (* 17/14 first interval excluded due to max complexity *)


(* ::Subsubsection::Closed:: *)
(*odd-limit diamond*)


testTargetSetScheme[getOld, 3, {2 / 1, 3 / 2, 4 / 3}];
testTargetSetScheme[getOld, 5, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5}];
testTargetSetScheme[getOld, 7, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5, 7 / 4, 8 / 7, 7 / 6, 12 / 7, 7 / 5, 10 / 7}];
testTargetSetScheme[getOld, 9, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5, 7 / 4, 8 / 7, 7 / 6, 12 / 7, 7 / 5, 10 / 7, 9 / 8, 16 / 9, 9 / 5, 10 / 9, 9 / 7, 14 / 9}];


(* ::Subsubsection::Closed:: *)
(*otonal chord*)


testTargetSetScheme[getOtonalChord, {4, 5}, {5 / 4}];
testTargetSetScheme[getOtonalChord, {4, 5, 6}, {5 / 4, 3 / 2, 6 / 5}];
testTargetSetScheme[getOtonalChord, {4, 5, 6, 7}, {5 / 4, 3 / 2, 7 / 4, 6 / 5, 7 / 5, 7 / 6}];
testTargetSetScheme[getOtonalChord, {8, 11, 13, 15}, {11 / 8, 13 / 8, 15 / 8, 13 / 11, 15 / 11, 15 / 13}];


(* ::Subsection::Closed:: *)
(*all-interval tuning schemes*)


test[getDualPower, 1, \[Infinity]];
test[getDualPower, 2, 2];
test[getDualPower, \[Infinity], 1];


test[
  augmentedTemperedSideGeneratorsPartArg,
  {{g1, g2}, "row"},
  {{g1, g2, gAugmented}, "row"}
];


test[
  augmentedTemperedSideMappingPartArg,
  {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"},
  2,
  {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {2 * Log2[2], 2 * Log2[3], 2 * Log2[5], 2 * Log2[7], -1}}, "row"}
];


test[
  augmentedJustSideGeneratorsPartArg,
  {{Log2[2], Log2[3], Log2[5], Log2[7]}, "row"},
  {{Log2[2], Log2[3], Log2[5], Log2[7], 0}, "row"}
];


test[
  augmentedJustSideMappingPartArg,
  {IdentityMatrix[4], "row"},
  {IdentityMatrix[5], "row"}
];


test[
  augmentedEitherSideIntervalsPartArg,
  {IdentityMatrix[4], "col"},
  {IdentityMatrix[5], "col"}
];


test[
  augmentedEitherSideMultiplierPartArg,
  {{{1 / Log2[2], 0, 0, 0, 0}, {0, 1 / Log2[3], 0, 0, 0}, {0, 0, 1 / Log2[5], 0, 0}, {0, 0, 0, 1 / Log2[7], 0}}, "row"}, (* already partially augmented per getComplexityPreTransformer *)
  {{{1 / Log2[2], 0, 0, 0, 0}, {0, 1 / Log2[3], 0, 0, 0}, {0, 0, 1 / Log2[5], 0, 0}, {0, 0, 0, 1 / Log2[7], 0}, {0, 0, 0, 0, 1}}, "row"}
];


test[augmentedHeldIntervalsArg, Null, Null];
test[
  augmentedHeldIntervalsArg,
  {{{1, 0, 0, 0}}, "col"},
  {{{1, 0, 0, 0, 1}}, "col"}
];


(* ::Subsection::Closed:: *)
(*domain basis*)


test[getSimplestPrimeOnlyBasis, {2, 5 / 3, 9 / 7}, {2, 3, 5, 7}];


(* ::Subsubsection::Closed:: *)
(*target-interval set schemes*)


testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getOld[5], {{}, "row", {4, 3, 5}}, {4 / 3, 5 / 4, 5 / 3}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getOld[9], {{}, "row", {2, 3, 7}}, {2 / 1, 4 / 3, 8 / 7, 16 / 9, 3 / 2, 12 / 7, 7 / 4, 7 / 6, 14 / 9, 9 / 8, 9 / 7}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getTilt[6], {{}, "row", {4, 3, 5}}, {3 / 1, 4 / 3, 5 / 3, 5 / 4}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getTilt[8], {{}, "row", {2, 3, 7}}, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 7 / 3, 7 / 4, 7 / 6, 8 / 3}];


(* ::Subsection::Closed:: *)
(*graphing*)


qa[fn_] := Do[printWrapper[fn], 1];


(* ::Subsubsection::Closed:: *)
(*2D*)


qa[graphTuningDamage["\:27e812 19 28]", "TILT miniRMS-U"]];


(* ::Subsubsection::Closed:: *)
(*3D*)


qa[graphTuningDamage[meantone, "TILT minimax-U"]];


(* ::Subsubsection::Closed:: *)
(*all-interval*)


(* ::Text:: *)
(*2D*)


qa[graphTuningDamage["\:27e812 19 28]", "minimax-ES"]];


(* ::Text:: *)
(*3 D*)


qa[graphTuningDamage[meantone, "minimax-S"]];


(* ::Chapter:: *)
(*final*)


printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];


