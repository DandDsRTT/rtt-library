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

(* some temperaments to check against *)
meantone = "[⟨1 1 0] ⟨0 1 4]}";
blackwood = "[⟨5 8 0] ⟨0 0 1]}";
dicot = "[⟨1 1 2] ⟨0 2 1]}";
augmented = "[⟨3 0 7] ⟨0 1 0]}";
mavila = "[⟨1 0 7] ⟨0 1 -3]}";
porcupine = "[⟨1 2 3] ⟨0 3 5]}";
srutal = "[⟨2 0 11] ⟨0 1 -2]}";
hanson = "[⟨1 0 1] ⟨0 6 5]}";
magic = "[⟨1 0 2] ⟨0 5 1]}";
negri = "[⟨1 2 2] ⟨0 -4 3]}";
tetracot = "[⟨1 1 1] ⟨0 4 9]}";
meantone7 = "[⟨1 0 -4 -13] ⟨0 1 4 10]}";
magic7 = "[⟨1 0 2 -1] ⟨0 5 1 12]}";
pajara = "[⟨2 3 5 6] ⟨0 1 -2 -2]}";
augene = "[⟨3 0 7 18] ⟨0 1 0 -2]}";
sensi = "[⟨1 -1 -1 -2] ⟨0 7 9 13]}";
sensamagic = "[⟨1 0 0 0] ⟨0 1 1 2] ⟨0 0 2 -1]}";

format = "Wolfram";


(* MATH UTILITIES *)

(* getPrimes *)
test[getPrimes, 5, {2, 3, 5, 7, 11}];

(* quotientToPcv *)
test[quotientToPcv, 22 / 5, {1, 0, -1, 0, 1}];
test[quotientToPcv, 1, {0}];

(* pcvToQuotient *)
test[pcvToQuotient, {1, 0, -1, 0, 1}, 22 / 5];
test[pcvToQuotient, {0}, 1];

(* padVectorsWithZerosUpToD *)
test[padVectorsWithZerosUpToD, {{1, 2, 3}, {4, 5, 6}}, 5, {{1, 2, 3, 0, 0}, {4, 5, 6, 0, 0}}];

(* super *)
test[super, 5 / 3, 5 / 3];
test[super, 3 / 5, 5 / 3];


(* PARSING *)

map = "⟨1200.000 1901.955 2786.314]";
mapping = "[⟨1 0 -4] ⟨0 1 4]}";
comma = "[1 -5 3⟩";
commaBasis = "[[-4 4 -1⟩ [7 0 -3⟩]";

withOuterBrackets = "[⟨1200.000 1901.955 2786.314]]";
withGtLtSigns = "[<1 0 -4] <0 1 4]>";
withPunctuationCommas = "[1, -5, 3⟩";
withLotsOfSpaces = " ⟨ [ -4 4 -1 ⟩ [ 7 0 -3 ⟩ ] ";

mapInWolfram = {{1200.000, 1901.955, 2786.314}, "row"};
mappingInWolfram = {{{1, 0, -4}, {0, 1, 4}}, "row"};
commaInWolfram = {{1, -5, 3}, "col"};
commaBasisInWolfram = {{{-4, 4, -1}, {7, 0, -3}}, "col"};

(* parseEBKVector *)
test[parseEBKVector, "1, 3, 4", {1, 3, 4}];
test[parseEBKVector, "1,3,4", {1, 3, 4}];
test[parseEBKVector, "1 3 4", {1, 3, 4}];
test[parseEBKVector, "1  3  4", {1, 3, 4}];
test[parseEBKVector, "1 ,3 ,4", {1, 3, 4}];
test[parseEBKVector, "1 , 3 , 4", {1, 3, 4}];
test[parseEBKVector, "1 ,, 3 , 4", {1, Null, 3, 4}];

(* isCovariantEBK *)
test[isCovariantEBK, map, True];
test[isCovariantEBK, mapping, True];
test[isCovariantEBK, comma, False];
test[isCovariantEBK, commaBasis, False];
test[isCovariantEBK, withOuterBrackets, True];
test[isCovariantEBK, withGtLtSigns, True];
test[isCovariantEBK, withPunctuationCommas, False];
test[isCovariantEBK, withLotsOfSpaces, False];

(* parseTemperamentData *)
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
test[parseTemperamentData, "2.3.7 [6 -2 -1⟩", {{6, -2, -1}, "col", {2, 3, 7}}];

(* parseQuotientL *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "row"};
test[parseQuotientL, "2", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "2/1", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2}", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2/1}", dummy5limitTemp, {{{1, 0, 0}}, "col"}];
test[parseQuotientL, "{2/1, 3/2}", dummy5limitTemp, {{{1, 0, 0}, {-1, 1, 0}}, "col"}];
test[parseQuotientL, "{11/7}", {{}, "row", {2, 9, 7, 11}}, {{{0, 0, 0, -1, 1}}, "col"}];

(* parseDomainBasis *)
test[parseDomainBasis, "2.3.7", {2, 3, 7}];

(* vectorToEBK *)
test[vectorToEBK, {-4, 4, -1}, dummy5limitTemp, "[-4 4 -1⟩"];
test[vectorToEBK, {-3, 2}, dummy5limitTemp, "[-3 2}"];
test[vectorToEBK, {-3, 2, 0, 0}, dummy5limitTemp, "[-3 2 0 0]"];

(* covectorToEBK *)
test[covectorToEBK, {1, 0, -4}, dummy5limitTemp, "⟨1 0 -4]"];
test[covectorToEBK, {7, 7}, dummy5limitTemp, "{7 7]"];
test[covectorToEBK, {7, 7, 7, 7}, dummy5limitTemp, "[7 7 7 7]"];

(* toEBK *)
test[toEBK, mapInWolfram, "⟨1200.000 1901.955 2786.314]" ];
test[toEBK, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]}" ];
test[toEBK, commaInWolfram, "[1 -5 3⟩"];
test[toEBK, commaBasisInWolfram, "[[-4 4 -1⟩ [7 0 -3⟩]"];
test[toEBK, {{{4}, {5}}, "row"}, "[⟨4] ⟨5]]"];
test[toEBK, {{{4}, {5}}, "col"}, "[[4⟩ [5⟩]"];

(* formatOutput *)
format = "EBK";
test[formatOutput, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]}"];
format = "Wolfram";
test[formatOutput, mappingInWolfram, mappingInWolfram];


(* LIST UTILITIES *)

(* divideOutGcd *)
test[divideOutGcd, {0, -6, 9}, {0, -2, 3}];
test[divideOutGcd, {-1, -2, -3}, {-1, -2, -3}];
test[divideOutGcd, {0, 0, 0}, {0, 0, 0}];

(* multByLcd *)
test[multByLcd, {1 / 3, 1, 2 / 5}, {5, 15, 6}];

(* leadingEntry *)
test[leadingEntry, {0, -6, 9, 0}, -6];

(* trailingEntry *)
test[trailingEntry, {0, -6, 9, 0}, 9];

(* allZerosL *)
test[allZerosL, {0, -6, 9}, False];
test[allZerosL, {0, 0, 0}, True];


(* MATRIX UTILITIES *)

(* allZeros *)
test[allZeros, {{1, 0, -4}, {0, 1, 4}}, False];
test[allZeros, {{0, 0, 0}, {0, 0, 0}}, True];

(* reverseInnerL *)
test[reverseInnerL, {{1, 0, -4}, {0, 1, 4}}, {{-4, 0, 1}, {4, 1, 0}}];

(* reverseOuterL *)
test[reverseOuterL, {{1, 0, -4}, {0, 1, 4}}, {{0, 1, 4}, {1, 0, -4}}];

(* rotate180 *)
test[rotate180, {{1, 0, -4}, {0, 1, 4}}, {{4, 1, 0}, {-4, 0, 1}}];

(* removeAllZeroLists *)
test[removeAllZeroLists, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeAllZeroLists, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeAllZeroLists, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeAllZeroLists, {{0, 0}, {0, 0}}, {}];

(* removeUnneededZeroLists *)
test[removeUnneededZeroLists, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeUnneededZeroLists, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeUnneededZeroLists, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeUnneededZeroLists, {{0, 0}, {0, 0}}, {{0, 0}}];

(* innerLLength *)
test[innerLLength, {{0, 0}, {0, 0}}, 2];
test[innerLLength, {{0}, {0}}, 1];
test[innerLLength, {{0, 0}}, 2];

(* hnf *)
test[hnf, {{5, 8, 12}, {7, 11, 16}}, {{1, 0, -4}, {0, 1, 4}}];
test[hnf, {{3, 0, -1}, {0, 3, 5}}, {{3, 0, -1}, {0, 3, 5}}];


(* VARIANCE UTILITIES *)

(* getAOrLOrS *)
test[getAOrLOrS, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{1, 0, -4}, {0, 1, 4}}];
test[getAOrLOrS, {{12, 19, 28}, "row"}, {12, 19, 28}];
test[getAOrLOrS, 1200, 1200];

(* hasA *)
test[hasA, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[hasA, {{12, 19, 28}, "row"}, False];
test[hasA, 1200, False];

(* hasL *)
test[hasL, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[hasL, {{12, 19, 28}, "row"}, True];
test[hasL, 1200, False];

(* getA *)
test[getA, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{1, 0, -4}, {0, 1, 4}}];
test[getA, {{12, 19, 28}, "row"}, {{12, 19, 28}}];
test[getA, 1200, {{1200}}];

(* getL *)
test[getL, {{{1, 0, -4}, {0, 1, 4}}, "row"}, Error];
test[getL, {{12, 19, 28}, "row"}, {12, 19, 28}];
test[getL, 1200, {1200}];

(* breakByRowsOrCols *)
test[breakByRowsOrCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 0, -4}, "row"}, {{0, 1, 4}, "row"}}];
test[breakByRowsOrCols, {{12, 19, 28}, "row"}, {{{12, 19, 28}, "row"}}];
test[breakByRowsOrCols, 1200, Error];

(* scale *)
test[scale, {{{1, 0, -4}, {0, 1, 4}}, "row"}, 2, {{{2, 0, -8}, {0, 2, 8}}, "row"}];
test[scale, {{12, 19, 28}, "row"}, 2, {{24, 38, 56}, "row"}];
test[scale, 1200, 2, 2400];

(* subtractT *)
test[subtractT, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 1}, {1, 1, 1}}, "row"}, {{{0, -1, -5}, {-1, 0, 3}}, "row"}];
test[subtractT, {{12, 19, 28}, "row"}, {{1, 1, 1}, "row"}, {{11, 18, 27}, "row"}];
test[subtractT, 1200, 1, 1199];

(* addT *)
test[addT, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 1}, {1, 1, 1}}, "row"}, {{{2, 1, -3}, {1, 2, 5}}, "row"}];
test[addT, {{12, 19, 28}, "row"}, {{1, 1, 1}, "row"}, {{13, 20, 29}, "row"}];
test[addT, 1200, 1, 1201];

(* getVariance *)
test[getVariance, {{{1, 0, -4}, {0, 1, 4}}, "row"}, "row"];

(* isCols *)
test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[isCols, {{{1, 2}, {3, 4}, {5, 6}}, "col"}, True];
test[isCols, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "row"}, False];
test[isCols, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "col"}, True];
test[isCols, {{{1, 2}, {0, 0}, {0, 0}}, "col"}, True];
test[isCols, {{{1, 0, 0}, {2, 0, 0}}, "row"}, False];
test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "row"}, False];
test[isCols, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, True];

(* isRows *)
test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[isRows, {{{1, 2}, {3, 4}, {5, 6}}, "col"}, False];
test[isRows, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "row"}, True];
test[isRows, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "col"}, False];
test[isRows, {{{1, 2}, {0, 0}, {0, 0}}, "col"}, False];
test[isRows, {{{1, 0, 0}, {2, 0, 0}}, "row"}, True];
test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "row"}, True];
test[isRows, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, False];

(* multiply *)

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

(* inverse *)
test[inverse, {{{1, 2, 3}, {4, 5, 0}, {0, 0, 9}}, "row"}, {{{-5 / 3, 2 / 3, 5 / 9}, {4 / 3, -1 / 3, -4 / 9}, {0, 0, 1 / 9}}, "row"}];
test[inverse, {{1, 2, 3}, "row"}, {{1, 1 / 2, 1 / 3}, "row"}];
test[inverse, 3, 1 / 3];

(* transpose *)
test[transpose, {{{1, 2, 3}, {4, 5, 6}}, "row"}, {{{1, 2, 3}, {4, 5, 6}}, "col"}];
test[transpose, {{{1, 2, 3}, {4, 5, 6}}, "col"}, {{{1, 2, 3}, {4, 5, 6}}, "row"}];
test[transpose, {{1, 2, 3}, "row"}, {{1, 2, 3}, "col"}];
test[transpose, {{1, 2, 3}, "col"}, {{1, 2, 3}, "row"}];
test[transpose, 1, Error];


(* GENERATOR PREIMAGE TRANSVERSAL *)

(* getGeneratorPreimageTransversal *)
format = "EBK";
test[getGeneratorPreimageTransversal, "[⟨1 1 0] ⟨0 1 4]}", "[[1 0 0⟩ [-1 1 0⟩]"];
test[getGeneratorPreimageTransversal, "[4 -4 1⟩", "[[1 0 0⟩ [0 1 0⟩]"];
format = "Wolfram";


(* TEMPERAMENT UTILITIES *)

(* getStandardPrimeLimitDomainBasis *)
test[getStandardPrimeLimitDomainBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];

(* isStandardPrimeLimitDomainBasis *)
test[isStandardPrimeLimitDomainBasis, {2, 3, 5, 7, 11}, True];
test[isStandardPrimeLimitDomainBasis, {2, 3, 7, 5, 11}, True];
test[isStandardPrimeLimitDomainBasis, {2, 3, 5, 9, 11}, False];

(* getDomainBasis *)
test[getDomainBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];
test[getDomainBasis, {{{11, 35, 31}}, "row", {2, 9, 7}}, {2, 9, 7}];

(* canonicalDomainBasis *)

(* order by prime limit*)
test[canonicalDomainBasis, {2, 7, 9}, {2, 9, 7}];
test[canonicalDomainBasis, {2, 9 / 7, 5}, {2, 5, 9 / 7}];
test[canonicalDomainBasis, {2, 9 / 7, 5 / 3}, {2, 5 / 3, 9 / 7}];

(* consolidate redundancies *)
test[canonicalDomainBasis, {2, 3, 9}, {2, 3}];
test[canonicalDomainBasis, {2, 3, 15}, {2, 3, 5}];
test[canonicalDomainBasis, {2, 3, 5 / 3}, {2, 3, 5}];

(* tricky stuff *)
test[canonicalDomainBasis, {2, 5 / 3, 7 / 5}, {2, 5 / 3, 7 / 3}];
test[canonicalDomainBasis, {1, 1}, {1}];

(* all the subgroups on the wiki page if they are canonical according to this *)
test[canonicalDomainBasis, {2, 3, 7}, {2, 3, 7}];
test[canonicalDomainBasis, {2, 5, 7}, {2, 5, 7}];
test[canonicalDomainBasis, {2, 3, 7 / 5}, {2, 3, 7 / 5}];
test[canonicalDomainBasis, {2, 5 / 3, 7}, {2, 5 / 3, 7}];
test[canonicalDomainBasis, {2, 5, 7 / 3}, {2, 5, 7 / 3}];
test[canonicalDomainBasis, {2, 5 / 3, 7 / 3}, {2, 5 / 3, 7 / 3}];
test[canonicalDomainBasis, {2, 27 / 25, 7 / 3}, {2, 27 / 25, 7 / 3}];
test[canonicalDomainBasis, {2, 9 / 5, 9 / 7}, {2, 9 / 5, 9 / 7}];
test[canonicalDomainBasis, {2, 3, 11}, {2, 3, 11}];
test[canonicalDomainBasis, {2, 5, 11}, {2, 5, 11}];
test[canonicalDomainBasis, {2, 7, 11}, {2, 7, 11}];
test[canonicalDomainBasis, {2, 3, 5, 11}, {2, 3, 5, 11}];
test[canonicalDomainBasis, {2, 3, 7, 11}, {2, 3, 7, 11}];
test[canonicalDomainBasis, {2, 5, 7, 11}, {2, 5, 7, 11}];
test[canonicalDomainBasis, {2, 5 / 3, 7 / 3, 11 / 3}, {2, 5 / 3, 7 / 3, 11 / 3}];
test[canonicalDomainBasis, {2, 3, 13}, {2, 3, 13}];
test[canonicalDomainBasis, {2, 3, 5, 13}, {2, 3, 5, 13}];
test[canonicalDomainBasis, {2, 3, 7, 13}, {2, 3, 7, 13}];
test[canonicalDomainBasis, {2, 5, 7, 13}, {2, 5, 7, 13}];
test[canonicalDomainBasis, {2, 5, 7, 11, 13}, {2, 5, 7, 11, 13}];
test[canonicalDomainBasis, {2, 3, 13 / 5}, {2, 3, 13 / 5}];
test[canonicalDomainBasis, {2, 3, 11 / 5, 13 / 5}, {2, 3, 11 / 5, 13 / 5}];
test[canonicalDomainBasis, {2, 3, 11 / 7, 13 / 7}, {2, 3, 11 / 7, 13 / 7}];
test[canonicalDomainBasis, {2, 7 / 5, 11 / 5, 13 / 5}, {2, 7 / 5, 11 / 5, 13 / 5}];
test[canonicalDomainBasis, {1}, {1}];
test[canonicalDomainBasis, {0}, {1}];

(* getDomainBasisDimension *)
test[getDomainBasisDimension, {2, 9, 7}, 4];
test[getDomainBasisDimension, {1}, 1];

(* getM *)
test[getM, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];
test[getM, {{{4, -4, 1}}, "col"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];

(* dualPrivate *)
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

(* hermiteRightUnimodular *)
test[hermiteRightUnimodular, {{6, 5, -4}, {4, -4, 1}}, {{1, 2, 1}, {-1, 0, 2}, {0, 3, 4}}];

(* colHermiteDefactor *)
test[colHermiteDefactor, {{6, 5, -4}, {4, -4, 1}}, {{6, 5, -4}, {-4, -4, 3}}];

(* canonicalMa *)
test[canonicalMa, {{1, 1, 0}, {0, 1, 4}}, {{1, 0, -4}, {0, 1, 4}}];

(* canonicalCa *)
test[canonicalCa, {{-4, 4, -1}}, {{4, -4, 1}}];
test[canonicalCa, {{8, 2, 9, 8}, {2, 9, -4, -8}, {3, 1, -9, -2}}, {{370, 327, 0, 0}, {150, 133, 1, 0}, {127, 110, 0, 2}}]; (* should put zeroes in the top-right, for larger primes and the first commas in the list *)

(* getBasisA *)
test[getBasisA, {{{11, 35, 31}}, "row", {2, 9, 7}}, {{{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 0, 1}}, "col"}];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
