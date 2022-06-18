failures = 0;
passes = 0;

test[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    actual == expectation,
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was: ``", fn, {args}, expectation, actual], 14, Red]]
  ]
];

format = "Wolfram";


(* TEMPERAMENT UTILITIES *)

(* getDPrivate *)
test[getDPrivate, {{{0}}, "co"}, 1];
test[getDPrivate, {{{0}}, "contra"}, 1];
test[getDPrivate, {{{0, 0}}, "co"}, 2];
test[getDPrivate, {{{0, 0}}, "contra"}, 2];
test[getDPrivate, {{{0}, {0}}, "co"}, 1];
test[getDPrivate, {{{0}, {0}}, "contra"}, 1];
test[getDPrivate, {IdentityMatrix[2], "co"}, 2];
test[getDPrivate, {IdentityMatrix[2], "contra"}, 2];
test[getDPrivate, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 3];
test[getDPrivate, {{{4, -4, 1}}, "contra"}, 3];
test[getDPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 4];
test[getDPrivate, {{{4, -4, 1, 0}}, "contra"}, 4];
test[getDPrivate, {{{1, 1, 3}, {0, 3, -1}}, "co", {2, 3, 7}}, 3];

(* getRPrivate *)
test[getRPrivate, {{{0}}, "co"}, 0];
test[getRPrivate, {{{0}}, "contra"}, 1];
test[getRPrivate, {{{0, 0}}, "co"}, 0];
test[getRPrivate, {{{0, 0}}, "contra"}, 2];
test[getRPrivate, {{{0}, {0}}, "co"}, 0];
test[getRPrivate, {{{0}, {0}}, "contra"}, 1];
test[getRPrivate, {IdentityMatrix[2], "co"}, 2];
test[getRPrivate, {IdentityMatrix[2], "contra"}, 0];
test[getRPrivate, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 2];
test[getRPrivate, {{{4, -4, 1}}, "contra"}, 2];
test[getRPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getRPrivate, {{{4, -4, 1, 0}}, "contra"}, 3];
test[getRPrivate, {{{1, 1, 3}, {0, 3, -1}}, "co", {2, 3, 7}}, 2];

(* getNPrivate *)
test[getNPrivate, {{{0}}, "co"}, 1];
test[getNPrivate, {{{0}}, "contra"}, 0];
test[getNPrivate, {{{0, 0}}, "co"}, 2];
test[getNPrivate, {{{0, 0}}, "contra"}, 0];
test[getNPrivate, {{{0}, {0}}, "co"}, 1];
test[getNPrivate, {{{0}, {0}}, "contra"}, 0];
test[getNPrivate, {IdentityMatrix[2], "co"}, 0];
test[getNPrivate, {IdentityMatrix[2], "contra"}, 2];
test[getNPrivate, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 1];
test[getNPrivate, {{{4, -4, 1}}, "contra"}, 1];
test[getNPrivate, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getNPrivate, {{{4, -4, 1, 0}}, "contra"}, 1];
test[getNPrivate, {{{1, 1, 3}, {0, 3, -1}}, "co", {2, 3, 7}}, 1];




(* ___ PRIVATE ___ *)


(* PARSING *)

map = "⟨1200.000 1901.955 2786.314]";
mapping = "[⟨1 0 -4] ⟨0 1 4]⟩";
comma = "[1 -5 3⟩";
commaBasis = "⟨[-4 4 -1⟩ [7 0 -3⟩]";

withOuterBrackets = "[⟨1200.000 1901.955 2786.314]]";
withGtLtSigns = "[<1 0 -4] <0 1 4]>";
withPunctuationCommas = "[1, -5, 3⟩";
withLotsOfSpaces = " ⟨ [ -4 4 -1 ⟩ [ 7 0 -3 ⟩ ] ";

mapInWolfram = {{{1200.000, 1901.955, 2786.314}}, "co"};
mappingInWolfram = {{{1, 0, -4}, {0, 1, 4}}, "co"};
commaInWolfram = {{{1, -5, 3}}, "contra"};
commaBasisInWolfram = {{{-4, 4, -1}, {7, 0, -3}}, "contra"};

test[parseEBKVector, "1, 3, 4", {1, 3, 4}];
test[parseEBKVector, "1,3,4", {1, 3, 4}];
test[parseEBKVector, "1 3 4", {1, 3, 4}];
test[parseEBKVector, "1  3  4", {1, 3, 4}];
test[parseEBKVector, "1 ,3 ,4", {1, 3, 4}];
test[parseEBKVector, "1 , 3 , 4", {1, 3, 4}];
test[parseEBKVector, "1 ,, 3 , 4", {1, Null, 3, 4}];

test[isCovariantEBK, map, True];
test[isCovariantEBK, mapping, True];
test[isCovariantEBK, comma, False];
test[isCovariantEBK, commaBasis, False];
test[isCovariantEBK, withOuterBrackets, True];
test[isCovariantEBK, withGtLtSigns, True];
test[isCovariantEBK, withPunctuationCommas, False];
test[isCovariantEBK, withLotsOfSpaces, False];

test[parseInput, map, mapInWolfram];
test[parseInput, mapping, mappingInWolfram];
test[parseInput, comma, commaInWolfram];
test[parseInput, commaBasis, commaBasisInWolfram];
test[parseInput, withOuterBrackets, mapInWolfram];
test[parseInput, withGtLtSigns, mappingInWolfram];
test[parseInput, withPunctuationCommas, commaInWolfram];
test[parseInput, withLotsOfSpaces, commaBasisInWolfram];
test[parseInput, mapInWolfram, mapInWolfram];
test[parseInput, mappingInWolfram, mappingInWolfram];
test[parseInput, commaInWolfram, commaInWolfram];
test[parseInput, commaBasisInWolfram, commaBasisInWolfram];

test[parseInput, "2.3.7 [6 -2 -1⟩", {{{6, -2, -1}}, "contra", {2, 3, 7}}];

dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "co"};
test[parseQuotientSet, "2", dummy5limitTemp, {{{1, 0, 0}}, "contra"}];
test[parseQuotientSet, "2/1", dummy5limitTemp, {{{1, 0, 0}}, "contra"}];
test[parseQuotientSet, "{2}", dummy5limitTemp, {{{1, 0, 0}}, "contra"}];
test[parseQuotientSet, "{2/1}", dummy5limitTemp, {{{1, 0, 0}}, "contra"}];
test[parseQuotientSet, "{2/1, 3/2}", dummy5limitTemp, {{{1, 0, 0}, {-1, 1, 0}}, "contra"}];

test[parseIntervalBasis, "2.3.7", {2, 3, 7}];

test[vectorToEBK, {-4, 4, -1}, "[-4 4 -1⟩"];
test[covectorToEBK, {1, 0, -4}, "⟨1 0 -4]"];

test[toEBK, mapInWolfram, "⟨1200.000 1901.955 2786.314]" ];
test[toEBK, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]⟩" ];
test[parseInput, commaInWolfram, "[1 -5 3⟩"];
test[parseInput, commaBasisInWolfram, "⟨[-4 4 -1⟩ [7 0 -3⟩]"];

format = "EBK";
test[parseInput, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]⟩"];
format = "Wolfram";
test[parseInput, mappingInWolfram, mappingInWolfram];


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

(* reverseEachRow *)
test[reverseEachRow, {{1, 0, -4}, {0, 1, 4}}, {{-4, 0, 1}, {4, 1, 0}}];

(* reverseEachCol *)
test[reverseEachCol, {{1, 0, -4}, {0, 1, 4}}, {{0, 1, 4}, {1, 0, -4}}];

(* antiTranspose *)
test[antiTranspose, {{1, 0, -4}, {0, 1, 4}}, {{4, 1, 0}, {-4, 0, 1}}];

(* removeAllZeroRows *)
test[removeAllZeroRows, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeAllZeroRows, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeAllZeroRows, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeAllZeroRows, {{0, 0}, {0, 0}}, {}];

(* removeUnneededZeroRows *)
test[removeUnneededZeroRows, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeUnneededZeroRows, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeUnneededZeroRows, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeUnneededZeroRows, {{0, 0}, {0, 0}}, {{0, 0}}];

(* colCount *)
test[colCount, {{0, 0}, {0, 0}}, 2];
test[colCount, {{0}, {0}}, 1];
test[colCount, {{0, 0}}, 2];


(* TEMPERAMENT UTILITIES *)

(* getA *)
test[getA, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{1, 0, -4}, {0, 1, 4}}];

(* getVariance *)
test[getVariance, {{{1, 0, -4}, {0, 1, 4}}, "co"}, "co"];

(* isContra *)
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[isContra, {{{1, 2}, {3, 4}, {5, 6}}, "contra"}, True];
test[isContra, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "co"}, False];
test[isContra, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "contra"}, True];
test[isContra, {{{1, 2}, {0, 0}, {0, 0}}, "contra"}, True];
test[isContra, {{{1, 0, 0}, {2, 0, 0}}, "co"}, False];
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "mapping"}, False];
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, True];

(* isCo *)
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "co"}, True];
test[isCo, {{{1, 2}, {3, 4}, {5, 6}}, "contra"}, False];
test[isCo, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "co"}, True];
test[isCo, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "contra"}, False];
test[isCo, {{{1, 2}, {0, 0}, {0, 0}}, "contra"}, False];
test[isCo, {{{1, 0, 0}, {2, 0, 0}}, "co"}, True];
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "mapping"}, True];
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, False];




Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
