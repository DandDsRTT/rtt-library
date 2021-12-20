f = 0;
p = 0;



(* MULTIVECTOR UTILITIES *)

(* eaGetD *)
test[eaGetD, {{1, 4, 4}, 2, "co"}, 3];

(* eaGetR *)
test[eaGetR, {{1, 4, 4}, 2, "co"}, 2];

(* eaGetN *)
test[eaGetN, {{1, 4, 4}, 2, "co"}, 1];


(* MULTIVECTOR FORMS & DEFACTORING *)

canonicalMulticomma = {{107, -87, 72, -49, 31}, 4, "contra"};
negatedCanonicalMulticomma = {{-107, 87, -72, 49, -31}, 4, "contra"};
canonicalMultimap = {{31, 49, 72, 87, 107}, 1, "co"};
negatedCanonicalMultimap = {{-31, -49, - 72, -87, -107}, 1, "co"};

(* eaCanonicalForm *)
test[eaCanonicalForm, canonicalMulticomma, canonicalMulticomma];
test[eaCanonicalForm, negatedCanonicalMulticomma, canonicalMulticomma];
test[eaCanonicalForm, canonicalMultimap, canonicalMultimap];
test[eaCanonicalForm, negatedCanonicalMultimap, canonicalMultimap];

test[eaCanonicalForm, {{4}, 0, "co", 3}, {{1}, 0, "co", 3}];
test[eaCanonicalForm, {{2, -4, 8, -9, 7, 2}, 2, "co"}, Error];
test[eaCanonicalForm, {{1, 0, 1}, 2, "co"}, {{1, 0, 1}, 2, "co"}];

test[eaCanonicalForm, {{0, 0, 0, 0, 0, 0}, 2, "co"}, {{0, 0, 0, 0, 0, 0}, 2, "co"}];


(* DUAL *)

(* eaDual *)
test[eaDual, canonicalMulticomma, canonicalMultimap];
test[eaDual, negatedCanonicalMulticomma, canonicalMultimap];
test[eaDual, canonicalMultimap, canonicalMulticomma];
test[eaDual, negatedCanonicalMultimap, canonicalMulticomma];

test[eaDual, {{1}, 0, "contra", 3}, {{1}, 3, "co"}];
test[eaDual, {{1}, 0, "co", 5}, {{1}, 5, "contra"}];
test[eaDual, {{2, -4, 8, -9, 7, 2}, 2, "co"}, Error];
test[eaDual, {{1, 0, 1}, 2, "co"}, {{1, 0, 1}, 1, "contra"}];

eaDualTester[multimap_, multicomma_] := Module[{},
  If[
    eaDual[multimap] == multicomma && eaDual[multicomma] == multimap,
    p += 1,
    f += 1;
    Print["eaDualTester[", multimap, ", ", multicomma, "]; actual dual multimap: ", eaDual[multicomma], " and dual multicomma: ", eaDual[multimap] ]
  ];
];
eaDualTester[{{1, 4, 4}, 2, "co"}, {{4, -4, 1}, 1, "contra"}];

randomMatrixAndMultivector[] := Module[{d, grade, m, t, w},
  d = RandomInteger[{1, 5}];
  grade = RandomInteger[{1, d}];
  m = RandomInteger[{-9, 9}, {grade, d}];

  t = If[RandomInteger[] == 1, {m, "contra"}, {m, "co"}];
  w = matrixToMultivector[t];

  {t, w}
];

Do[
  w = Last[randomMatrixAndMultivector[]];

  dualW = eaDual[w];
  doubleDualW = eaDual[dualW];

  If[
    doubleDualW == w,
    p += 1,
    f += 1;
    Print["BAD BAD BAD! multivector: ", w, " computed dual: ", dualW, " and then back: ", doubleDualW]
  ],
  100
];



(* CONVERSION TO AND FROM MATRIX *)

(* multivectorToMatrix *)
test[multivectorToMatrix, {{1}, 0, "contra", 1}, {{{0}}, "contra"}];
test[multivectorToMatrix, {{1}, 0, "co", 1}, {{{0}}, "co"}];
test[multivectorToMatrix, {{1}, 0, "contra", 3}, {{{0, 0, 0}}, "contra"}];
test[multivectorToMatrix, {{1}, 0, "co", 3}, {{{0, 0, 0}}, "co"}];
test[multivectorToMatrix, {{2, -4, 8, -9, 7, 2}, 2, "co"}, Error];
test[multivectorToMatrix, {{0, 0, 0, 0, 0}, 4, "co"}, Error]; (* no equivalent to all-zero multivectors in LA *)


(* matrixToMultivector *)
test[matrixToMultivector, {{{0}}, "contra"}, {{1}, 0, "contra", 1}];
test[matrixToMultivector, {{{0}}, "co"}, {{1}, 0, "co", 1}];
test[matrixToMultivector, {{{0, 0, 0}}, "contra"}, {{1}, 0, "contra", 3}];
test[matrixToMultivector, {{{0, 0, 0}}, "co"}, {{1}, 0, "co", 3}];
test[matrixToMultivector, {IdentityMatrix[2], "co"}, {{1}, 2, "co"}];
test[matrixToMultivector, {{{1, 1}}, "co"}, {{1, 1}, 1, "co"}];


(* multivectorToMatrix & matrixToMultivector: by dimensionality *)
testMultivectorMatrixConversion[w_, t_] := Module[{convertedW, convertedT},
  convertedT = multivectorToMatrix[w];
  convertedW = matrixToMultivector[t];

  If[
    convertedT == t && convertedW == w,
    p += 1,
    f += 1;
    Print["testMultivectorMatrixConversion[]; convertedT: ", convertedT, " t: ", t, " convertedW: ", convertedW, " w: ", w]]
];

(* multivectorToMatrix & matrixToMultivector: dimensionality 1 *)

testMultivectorMatrixConversion[{{1}, 1, "co"}, {IdentityMatrix[1], "co"}];
testMultivectorMatrixConversion[{{1}, 0, "contra", 1}, {{{0}}, "contra"}];

testMultivectorMatrixConversion[{{1}, 0, "co", 1}, {{{0}}, "co"}];
testMultivectorMatrixConversion[{{1}, 1, "contra"}, {IdentityMatrix[1], "contra"}];

(* multivectorToMatrix & matrixToMultivector: dimensionality 2 *)

testMultivectorMatrixConversion[{{1}, 2, "co"}, {IdentityMatrix[2], "co"}];
testMultivectorMatrixConversion[{{1}, 0, "contra", 2}, {{{0, 0}}, "contra"}];

testMultivectorMatrixConversion[{{12, 19}, 1, "co"}, {{{12, 19}}, "co"}];
testMultivectorMatrixConversion[{{-19, 12}, 1, "contra"}, {{{-19, 12}}, "contra"}];

testMultivectorMatrixConversion[{{1}, 0, "co", 2}, {{{0, 0}}, "co"}];
testMultivectorMatrixConversion[{{1}, 2, "contra"}, {IdentityMatrix[2], "contra"}];

(* multivectorToMatrix & matrixToMultivector: dimensionality 3 *)

testMultivectorMatrixConversion[{{1}, 3, "co"}, {IdentityMatrix[3], "co"}];
testMultivectorMatrixConversion[{{1}, 0, "contra", 3}, {{{0, 0, 0}}, "contra"}];

testMultivectorMatrixConversion[{{1, 4, 4}, 2, "co"}, {{{1, 0, -4}, {0, 1, 4}}, "co"}];
testMultivectorMatrixConversion[{{4, -4, 1}, 1, "contra"}, {{{4, -4, 1}}, "contra"}];

testMultivectorMatrixConversion[{{19, 30, 44}, 1, "co"}, {{{19, 30, 44}}, "co"}];
testMultivectorMatrixConversion[{{44, -30, 19}, 2, "contra"}, {{{-30, 19, 0}, {-26, 15, 1}}, "contra"}];

testMultivectorMatrixConversion[{{1}, 0, "co", 3}, {{{0, 0, 0}}, "co"}];
testMultivectorMatrixConversion[{{1}, 3, "contra"}, {IdentityMatrix[3], "contra"}];

(* multivectorToMatrix & matrixToMultivector: dimensionality 4 *)

testMultivectorMatrixConversion[{{1}, 4, "co"}, {IdentityMatrix[4], "co"}];
testMultivectorMatrixConversion[{{1}, 0, "contra", 4}, {{{0, 0, 0, 0}}, "contra"}];

testMultivectorMatrixConversion[{{1, 0, 2, 6}, 3, "co"}, {{{1, 0, 0, 6}, {0, 1, 0, -2}, {0, 0, 1, 0}}, "co"}];
testMultivectorMatrixConversion[{{-6, 2, 0, 1}, 1, "contra"}, {{{-6, 2, 0, 1}}, "contra"}];

testMultivectorMatrixConversion[{{1, 4, 10, 4, 13, 12}, 2, "co"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"}];
testMultivectorMatrixConversion[{{12, -13, 4, 10, -4, 1}, 2, "contra"}, {{{4, -4, 1, 0}, {13, -10, 0, 1}}, "contra"}];

testMultivectorMatrixConversion[{{31, 49, 72, 87}, 1, "co"}, {{{31, 49, 72, 87}}, "co"}];
testMultivectorMatrixConversion[{{-87, 72, -49, 31}, 3, "contra"}, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "contra"}];

testMultivectorMatrixConversion[{{1}, 0, "co", 4}, {{{0, 0, 0, 0}}, "co"}];
testMultivectorMatrixConversion[{{1}, 4, "contra"}, {IdentityMatrix[4], "contra"}];

(* multivectorToMatrix & matrixToMultivector: dimensionality 5 *)

testMultivectorMatrixConversion[{{1}, 5, "co"}, {IdentityMatrix[5], "co"}];
testMultivectorMatrixConversion[{{1}, 0, "contra", 5}, {{{0, 0, 0, 0, 0}}, "contra"}];

testMultivectorMatrixConversion[{{6, 0, 0, 3, -16}, 4, "co"}, {{{3, 0, 0, 0, 8}, {0, 2, 0, 0, 1}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, "co"}];
testMultivectorMatrixConversion[{{-16, -3, 0, 0, 6}, 1, "contra"}, {{{-16, -3, 0, 0, 6}}, "contra"}];

testMultivectorMatrixConversion[{{4, -4, 0, -6, 2, -2, 11, 17, -17, -31}, 3, "co"}, {{{2, 1, 0, 7, 8}, {0, 2, 0, 3, -1}, {0, 0, 1, -1, 0}}, "co"}];
testMultivectorMatrixConversion[{{-31, 17, 17, -11, -2, -2, -6, 0, 4, 4}, 2, "contra"}, {{{-11, -6, 4, 4, 0}, {-7, -1, 1, 1, 1}}, "contra"}];

testMultivectorMatrixConversion[{{2, -16, -28, 5, -30, -50, 1, -20, 67, 111}, 2, "co"}, {{{1, 1, 7, 11, 2}, {0, 2, -16, -28, 5}}, "co"}];
testMultivectorMatrixConversion[{{111, -67, -20, 1, 50, -30, -5, -28, 16, 2}, 3, "contra"}, {{{-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {1, -5, 0, 0, 2}}, "contra"}];

testMultivectorMatrixConversion[{{72, 114, 167, 202, 249}, 1, "co"}, {{{72, 114, 167, 202, 249}}, "co"}];
testMultivectorMatrixConversion[{{249, -202, 167, -114, 72}, 4, "contra"}, {{{-19, 12, 0, 0, 0}, {-25, 7, 6, 0, 0}, {-20, 5, 4, 1, 0}, {-12, 1, 3, 0, 1}}, "contra"}];

testMultivectorMatrixConversion[{{1}, 0, "co", 5}, {{{0, 0, 0, 0, 0}}, "co"}];
testMultivectorMatrixConversion[{{1}, 5, "contra"}, {IdentityMatrix[5], "contra"}];

(* multivectorToMatrix & matrixToMultivector: random *)

Do[
  tAndW = randomMatrixAndMultivector[];
  t = First[tAndW];
  w = Last[tAndW];

  wAndBackToT = multivectorToMatrix[w];

  If[
    wAndBackToT == canonicalForm[t],
    p += 1,
    f += 1;
    Print["BAD BAD BAD! (following all in canonical form) matrix: ", canonicalForm[t], " computed equiv multivector: ", w, " and then back to matrix: ", wAndBackToT]
  ],
  100
];

(* multivectorToMatrix & matrixToMultivector: one-off *)

testMatrix[t_] := If[
  canonicalForm[t] == multivectorToMatrix[matrixToMultivector[t]],
  p += 1,
  f += 1;
  Print["testMatrix[]", multivectorToMatrix[matrixToMultivector[t]]]
];
testMultivector[v_] := If[
  eaCanonicalForm[v] == matrixToMultivector[multivectorToMatrix[v]],
  p += 1,
  f += 1;
  Print["testMultivector[]", matrixToMultivector[multivectorToMatrix[v]]]
];

testMatrix[{{{-4, -8, 3, 7, -1, -3}, {1, -2, -2, 4, 4, -6}, {2, -9, 9, -8, 0, 7}, {5, -5, 4, -8, 5, -6}, {9, 0, 2, 8, -4, -3}}, "contra"}];

testMultivector[{{2, 8, 8}, 2, "co"}];
testMultivector[{{0, 0, 3, 4}, 3, "contra"}];
testMultivector[{{1, 0, 1}, 2, "co"}];


(* MEET AND JOIN *)

(* d =2, multimaps *)
d2g1co1 = {{12, 19}, 1, "co"};
d2g1co2 = {{19, 30}, 1, "co"};
d2jico = {{1}, 2, "co"};

(* d=3, multimaps *)
d3g1co1 = {{12, 19, 28}, 1, "co"};
d3g1co2 = {{19, 30, 44}, 1, "co"};
d3g1co3 = {{22, 35, 51}, 1, "co"};
d3g2co1 = {{1, 4, 4}, 2, "co"};
d3g2co2 = {{3, 5, 1}, 2, "co"};
d3jico = {{1}, 3, "co"};
d3unisonco = {{1}, 0, "co", 3};

(* d=3, multicommas *)
d3g1contra1 = {{4, -4, 1}, 1, "contra"};
d3g1contra2 = {{-10, -1, 5}, 1, "contra"};
d3g1contra3 = {{1, -5, 3}, 1, "contra"};
d3g2contra1 = {{44, -30, 19}, 2, "contra"};
d3g2contra2 = {{28, -19, 12}, 2, "contra"};
d3g2contra3 = {{51, -35, 22}, 2, "contra"};
d5g3contra = {{19, -33, 14, -46, 46, -46, 29, -29, 29, 0}, 3, "contra"};
d3jicontra = {{1}, 0, "contra", 3};
d3unisoncontra = {{1}, 3, "contra"};

(* d=5, multimaps *)
d5g1co = {{31, 49, 72, 87, 107}, 1, "co"};
d5g2co1 = {{-9, -5, 3, -7, 13, 30, 20, 21, 1, -30}, 2, "co"}; (*progressiveProduct[{{15, 24, 35, 42, 52}, 1, "co"}, {{16, 25, 37, 45, 55}, 1, "co"}];*)
d5g2co2 = {{1, 4, -2, -6, 4, -6, -13, -16, -28, -10}, 2, "co"}; (* progressiveProduct[{{12, 19, 28, 34, 42}, 1, "co"}, {{17, 27, 40, 48, 59}, 1, "co"}]; *)
d5g2co3 = {{6, -7, -2, 15, -25, -20, 3, 15, 59, 49}, 2, "co"}; (*example from interior product page *)
d5g3co = {{1, 2, -3, -2, 1, -4, -5, 12, 9, -19}, 3, "co"};(*example from interior product page *)
d5g4co = {{1, 2, 1, 2, 3}, 4, "co"};(*example from interior product page *)
d5unisonco = {{1}, 0, "co", 5};

(* d=5, multicommas *)
d5g1contra = {{-3, 2, -1, 2, -1}, 1, "contra"};
d5g2contra = {{5, 11, -7, -4, -9, 8, 1, 5, -5, 5}, 2, "contra"};
d5jicontra = {{1}, 0, "contra", 5};

(* super basic progressive product example *)
test2args[progressiveProduct, d2g1co1, d2g1co2, d2jico];

(* wedging with oneself equals a zero varianced multivector *)
test2args[progressiveProduct, d3g1co1, d3g1co1, {{0, 0, 0}, 2, "co"}];

(* another basic progressive product example *)
test2args[progressiveProduct, d5g2co1, d5g2co2, d5g4co];
(* show how progressive product can cap out when grade exactly hits the dimensionality, for multicommas *)
test2args[progressiveProduct, d3g2contra1, d3g1contra3, d3unisoncontra];
(* show how progressive product can cap out when grade exceeds at the dimensionality, for multicommas *)
test2args[progressiveProduct, d3g2contra1, d3g2contra2, Error];
(* show how progressive product can cap out when grade exactly hits the dimensionality, for multimaps*)
test2args[progressiveProduct, d3g2co1, d3g1co3, d3jico];
(* show how progressive product can cap out when grade exceeds the dimensionality, for multimaps*)
test2args[progressiveProduct, d3g2co1, d3g2co2, Error];

(* a basic regressive product example *)
test2args[regressiveProduct, d3g2contra1, d3g2contra2, d3g1contra1];
(* show how regressive product can cap out when grade hits exactly 0, for multicommas *)
test2args[regressiveProduct, d3g1contra1, d3g2contra3, d3jicontra];
(* show how regressive product can cap out when grade goes below 0, for multicommas *)
test2args[regressiveProduct, d3g1contra1, d3g1contra2, Error];
(* show how regressive product can cap out when grade hits exactly 0, for multimaps *)
test2args[regressiveProduct, d3g1co1, d3g2co2, d3unisonco];
(* show how regressive product can cap out when grade goes below 0, for multimaps*)
test2args[regressiveProduct, d3g1co1, d3g1co2, Error];

(* a series of examples working up to the symmetric interior product *)

test2args[rightInteriorProduct, d5g1contra, d5g3co, Error];
test2args[rightInteriorProduct, d5g3co, d5g1contra, d5g2co3];

test2args[leftInteriorProduct, d5g1contra, d5g3co, d5g2co3];
test2args[leftInteriorProduct, d5g3co, d5g1contra, Error];

test2args[interiorProduct, d5g1contra, d5g3co, d5g2co3];
test2args[interiorProduct, d5g3co, d5g1contra, d5g2co3];

(* a similar series of examples but with grade of contra > grade of co *)

test2args[rightInteriorProduct, d5g1co, d5g3contra, Error];
test2args[rightInteriorProduct, d5g3contra, d5g1co, d5g2contra ];

test2args[leftInteriorProduct, d5g1co, d5g3contra, d5g2contra];
test2args[leftInteriorProduct, d5g3contra, d5g1co, Error];

test2args[interiorProduct, d5g1co, d5g3contra, d5g2contra];
test2args[interiorProduct, d5g3contra, d5g1co, d5g2contra];

(* progressive product errors if it gets mixed variance *)
test2args[progressiveProduct, d5g1contra, d5g3co, Error];

(* regressive product errors if it gets mixed variance *)
test2args[regressiveProduct, d5g1contra, d5g3co, Error];

(* interior product errors if it gets two multimaps *)
test2args[rightInteriorProduct, d5g2co1, d5g2co2, Error];
test2args[leftInteriorProduct, d5g2co1, d5g2co2, Error];
test2args[interiorProduct, d5g2co1, d5g2co2, Error];

(* interior product errors if it gets two multicommas *)
test2args[rightInteriorProduct, d3g2contra1, d3g2contra2, Error];
test2args[leftInteriorProduct, d3g2contra1, d3g2contra2, Error];
test2args[interiorProduct, d3g2contra1, d3g2contra2, Error];

(* same examples as for meet and join *)

et5M5 = matrixToMultivector[{{{5, 8, 12}}, "co"}];
et5C5 = matrixToMultivector[ {{{-8, 5, 0}, {-4, 1, 1}}, "contra"}];
et7M5 = matrixToMultivector[ {{{7, 11, 16}}, "co"}];
et7C5 = matrixToMultivector[{{{-11, 7, 0}, {-7, 3, 1}}, "contra"}];
meantoneM5 = matrixToMultivector[{{{1, 0, -4}, {0, 1, 4}}, "co"}];
meantoneC5 = matrixToMultivector[ {{{4, -4, 1}}, "contra"}];
porcupineM5 = matrixToMultivector[{{{1, 2, 3}, {0, 3, 5}}, "co"}];
porcupineC5 = matrixToMultivector[{{{1, -5, 3}}, "contra"}];
d3unisoncontra = {{1}, 3, "contra"};
d3jico = {{1}, 3, "co"};

test2args[progressiveProduct, et5M5, et7M5, meantoneM5];
test2args[progressiveProduct, et5C5, et7C5, Error];
test2args[progressiveProduct, meantoneM5, porcupineM5, Error];
test2args[progressiveProduct, meantoneC5, porcupineC5, et7C5];

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

meantoneM11 = matrixToMultivector[{{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "co"}];
meantoneC11 = matrixToMultivector[{{meantoneComma11, starlingComma11, mothwellsma11} , "contra"}];
meanpopM11 = matrixToMultivector[{{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "co"}];
meanpopC11 = matrixToMultivector[{{meantoneComma11, starlingComma11, keenanisma11} , "contra"}];
marvelM11 = matrixToMultivector[{{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"}];
marvelC11 = matrixToMultivector[{{marvelComma11, keenanisma11} , "contra"}];
porcupineM11 = matrixToMultivector[{{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "co"}];
porcupineC11 = matrixToMultivector[{{telepathma11, septimalComma11, ptolemisma11} , "contra"}];
et31M11 = matrixToMultivector[{{{31, 49, 72, 87, 107}}, "co"}];
et31C11 = matrixToMultivector[{{{-49, 31, 0, 0, 0}, {-45, 27, 1, 0, 0}, {-36, 21, 0, 1, 0}, {-24, 13, 0, 0, 1}} , "contra"}];
meantoneM7 = matrixToMultivector[{{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"}];
meantoneC7 = matrixToMultivector[{{meantoneComma7, starlingComma7} , "contra"}];
porcupineM7 = matrixToMultivector[{{{1, 2, 3, 2}, {0, 3, 5, -6}}, "co"}];
porcupineC7 = matrixToMultivector[{{septimalComma7, porcupineComma7} , "contra"}];
miracleM11 = matrixToMultivector[{{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "co"}];
miracleC11 = matrixToMultivector[{{marvelComma11, rastma11, keenanisma11} , "contra"}];
magicM11 = matrixToMultivector[{{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "co"}];
magicC11 = matrixToMultivector[{{marvelComma11, sensamagicComma11, ptolemisma11} , "contra"}];
et41M11 = matrixToMultivector[{{{41, 65, 95, 115, 142}}, "co"}];
et41C11 = matrixToMultivector[{{{-65, 41, 0, 0, 0}, {-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {-32, 18, 0, 0, 1}} , "contra"}];
miracleM7 = matrixToMultivector[{{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}];
miracleC7 = matrixToMultivector[{{marvelComma7, gamelisma7} , "contra"}];
magicM7 = matrixToMultivector[{{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"}];
magicC7 = matrixToMultivector[{{marvelComma7, sensamagicComma7} , "contra"}];
et41M7 = matrixToMultivector[{{{41, 65, 95, 115}}, "co"}];
et41C7 = matrixToMultivector[{{{-65, 41, 0, 0}, {-15, 8, 1, 0}, {-25, 14, 0, 1}} , "contra"}];
mothraM11 = matrixToMultivector[{{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "co"}];
mothraC11 = matrixToMultivector[{{meantoneComma11, mothwellsma11, keenanisma11} , "contra"}];
mothraM7 = matrixToMultivector[{{{1, 1, 0, 3}, {0, 3, 12, -1}}, "co"}];
mothraC7 = matrixToMultivector[{{meantoneComma7, gamelisma7} , "contra"}];
portentM11 = matrixToMultivector[{{{1, 1, 0, 3, 5}, {0, 3, 0, -1, 4}, {0, 0, 1, 0, -1}}, "co"}];
portentC11 = matrixToMultivector[{{keenanisma11, werckisma11} , "contra"}];
gamelanM7 = matrixToMultivector[{{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "co"}];
gamelanC7 = matrixToMultivector[{{gamelisma7}, "contra"}];
marvelM7 = matrixToMultivector[{{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}];
marvelC7 = matrixToMultivector[{{marvelComma7}, "contra"}];

(*⋎ = MEET, ⋏ = JOIN *)

(*Meantone⋎Meanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test2args[progressiveProduct, meantoneC11, meanpopC11, Error];

(*Meantone⋏Meanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>, but they're linearly dependent so EA gives an all-zero result*)
test2args[progressiveProduct, meantoneM11, meanpopM11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*Meantone⋎Marvel = 31, but they're linearly dependent so EA gives an all-zero result*)
test2args[progressiveProduct, meantoneC11, marvelC11, {{0}, 5, "contra"}];


(*Meantone⋏Marvel = <225/224>, but they're linearly dependent so EA gives an all-zero result*)
test2args[progressiveProduct, meantoneM11, marvelM11, {{0}, 5, "co"}];

(*Meantone⋎Porcupine = G = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test2args[progressiveProduct, meantoneC11, porcupineC11, Error];

(*Meantone⋏Porcupine = <176/175>, and these are linearly independent so the result is the same in EA*)
test2args[progressiveProduct, meantoneM11, porcupineM11, matrixToMultivector[dual[{{valinorsma11}, "contra"}]]];

(*In the 7-limit, that become Meantone⋎Porcupine = <JI>, Meantone⋏Porcupine = <1>, and these are linearly independent so the result is the same in EA*)
test2args[progressiveProduct, meantoneC7, porcupineC7, matrixToMultivector[{IdentityMatrix[4], "contra"}]];
test2args[progressiveProduct, meantoneM7, porcupineM7, matrixToMultivector[{IdentityMatrix[4], "co"}]];

(*Miracle⋎Magic = 41, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test2args[progressiveProduct, miracleC11, magicC11, Error];

(*Miracle⋏Magic = Marvel, but they're linearly dependent so EA gives an all-zero result *)
test2args[progressiveProduct, miracleM11, magicM11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*In the 7-limit, again Miracle⋎Magic = 41, Miracle⋏Magic = Marvel, but they're linearly dependent so EA gives all-zero results*)
test2args[progressiveProduct, miracleC7, magicC7, {{0}, 4, "contra"}];
test2args[progressiveProduct, miracleM7, magicM7, {{0}, 4, "co"}];

(*Miracle⋎Mothra = 31, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test2args[progressiveProduct, miracleC11, mothraC11, Error];

(* Miracle⋏Mothra = Portent, but they're linearly dependent so EA gives an all-zero result *)
test2args[progressiveProduct, miracleM11, mothraM11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*In the 7-limit, Miracle⋏Mothra = Gamelan, but they're linearly dependent so EA gives an all-zero result*)
test2args[progressiveProduct, miracleM7, mothraM7, {{0}, 4, "co"}];

(*Meantone⋎Magic = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test2args[progressiveProduct, meantoneC11, magicC11, Error];

(*Meantone⋏Magic = <225/224>, and these are linearly independent so the result is the same in EA*)
test2args[progressiveProduct, meantoneM11, magicM11, matrixToMultivector[dual[{{marvelComma11}, "contra"}]]];


(* ARITHMETIC *)

(* addable multimaps *)
meantoneMultimap = {{1, 4, 4}, 2, "co"};
porcupineMultimap = {{3, 5, 1}, 2, "co"};
test2args[eaSum, meantoneMultimap, porcupineMultimap, {{4, 9, 5}, 2, "co"}];
test2args[eaDiff, meantoneMultimap, porcupineMultimap, {{2, 1, -3}, 2, "co"}];
meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
porcupineMulticomma = {{1, -5, 3}, 1, "contra"};
test2args[eaSum, meantoneMulticomma, porcupineMulticomma, {{5, -9, 4}, 1, "contra"}];
test2args[eaDiff, meantoneMulticomma, porcupineMulticomma, {{-3, -1, 2}, 1, "contra"}];

(* addable multicommas *)
et7Multimap = {{7, 11, 16}, 1, "co"};
et5Multimap = {{5, 8, 12}, 1, "co"};
test2args[eaSum, et7Multimap, et5Multimap, {{12, 19, 28}, 1, "co"}];
test2args[eaDiff, et7Multimap, et5Multimap, {{2, 3, 4}, 1, "co"}];
et7Multicomma = {{16, -11, 7}, 2, "contra"};
et5Multicomma = {{12, -8, 5}, 2, "contra"};
test2args[eaSum, et7Multicomma, et5Multicomma, {{28, -19, 12}, 2, "contra"}];
test2args[eaDiff, et7Multicomma, et5Multicomma, {{4, -3, 2}, 2, "contra"}];

(* not addable - error! *)
septimalMeantoneMultimap = {{1, 4, 10, 4, 13, 12}, 2, "co"};
septimalBlackwoodMultimap = {{0, 5, 0, 8, 0, -14}, 2, "co"};
test2args[eaSum, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
test2args[eaDiff, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
septimalMeantoneMulticomma = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "co"}];
septimalBlackwoodMulticomma = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "co"}];
test2args[eaSum, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];
test2args[eaDiff, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];

(* addable - linear-dependence-2 (multicommas) *)
et12Multimap = {{12, 19, 28, 34}, 1, "co"};
et19Multimap = {{19, 30, 44, 53}, 1, "co"};
test2args[eaSum, et12Multimap, et19Multimap, {{31, 49, 72, 87}, 1, "co"}];
test2args[eaDiff, et12Multimap, et19Multimap, {{7, 11, 16, 19}, 1, "co"}];
et12Multicomma = eaDual[et12Multimap];
et19Multicomma = eaDual[et19Multimap];
test2args[eaSum, et12Multicomma, et19Multicomma, {{-87, 72, -49, 31}, 3, "contra"}];
test2args[eaDiff, et12Multicomma, et19Multicomma, {{-19, 16, -11, 7}, 3, "contra"}];

(* examples with themselves *)
test2args[eaSum, meantoneMultimap, meantoneMultimap, {{1, 4, 4}, 2, "co"}];
test2args[eaDiff, meantoneMultimap, meantoneMultimap, {{0, 0, 0}, 2, "co"}];
test2args[eaSum, meantoneMulticomma, meantoneMulticomma, {{4, -4, 1}, 1, "contra"}];
test2args[eaDiff, meantoneMulticomma, meantoneMulticomma, {{0, 0, 0}, 1, "contra"}];
test2args[eaSum, et7Multimap, et7Multimap, {{7, 11, 16}, 1, "co"}];
test2args[eaDiff, et7Multimap, et7Multimap, {{0, 0, 0}, 1, "co"}];
test2args[eaSum, et7Multicomma, et7Multicomma, {{16, -11, 7}, 2, "contra"}];
test2args[eaDiff, et7Multicomma, et7Multicomma, {{0, 0, 0}, 2, "contra"}];

(* mismatched r & n but matching d *)
test2args[eaSum, et7Multimap, meantoneMultimap, Error];
test2args[eaDiff, et7Multimap, meantoneMultimap, Error];
test2args[eaSum, et7Multicomma, meantoneMulticomma, Error];
test2args[eaDiff, et7Multicomma, meantoneMulticomma, Error];

(* mismatched d but matching r or n *)
test2args[eaSum, et7Multimap, et12Multimap, Error];
test2args[eaDiff, et7Multimap, et12Multimap, Error];
test2args[eaSum, et7Multicomma, et12Multicomma, Error];
test2args[eaDiff, et7Multicomma, et12Multicomma, Error];

(* some basic examples *)
augmentedMultimap = {{3 , 0, -7}, 2, "co"};
diminishedMultimap = {{4, 4, -3}, 2, "co"};
tetracotMultimap = {{4, 9, 5}, 2, "co"};
dicotMultimap = {{2, 1, -3}, 2, "co"};
srutalMultimap = {{2, -4, -11}, 2, "co"};
test2args[eaSum, augmentedMultimap, diminishedMultimap, {{7, 4, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]] *)
test2args[eaDiff, augmentedMultimap, diminishedMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]] *)
test2args[eaSum, augmentedMultimap, tetracotMultimap, {{7, 9, -2}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]] *)
test2args[eaDiff, augmentedMultimap, tetracotMultimap, {{1, 9, 12}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]] *)
test2args[eaSum, augmentedMultimap, dicotMultimap, {{5, 1, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]] *)
test2args[eaDiff, augmentedMultimap, dicotMultimap, {{1, -1, -4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]] *)
test2args[eaSum, augmentedMultimap, srutalMultimap, {{5, -4, -18}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]] *)
test2args[eaDiff, augmentedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]] *)
test2args[eaSum, diminishedMultimap, tetracotMultimap, {{8, 13, 2}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]] *)
test2args[eaDiff, diminishedMultimap, tetracotMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]] *)
test2args[eaSum, diminishedMultimap, dicotMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]] *)
test2args[eaDiff, diminishedMultimap, dicotMultimap, {{2, 3, 0}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]] *)
test2args[eaSum, diminishedMultimap, srutalMultimap, {{3, 0, -7}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[eaDiff, diminishedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (*⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaSum, tetracotMultimap, dicotMultimap, {{3, 5, 1}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[eaDiff, tetracotMultimap, dicotMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaSum, tetracotMultimap, srutalMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[eaDiff, tetracotMultimap, srutalMultimap, {{2, 13, 16}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[eaSum, dicotMultimap, srutalMultimap, {{4, -3, -14}, 2, "co"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[eaDiff, dicotMultimap, srutalMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2 *)
w1 = {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "co"};
w2 = {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "co"};
test2args[eaSum, w1, w2, Error];
test2args[eaDiff, w1, w2, Error];

(* example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1 *)
w1 = {{2, 3}, 1, "contra"};
w2 = {{4, -7}, 1, "co"};
wSum = {{9, 7}, 1, "contra"};
wDiff = {{5, 1}, 1, "contra"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* example demonstrating how it's important to canonicalize *)
w1 = {{-2, 4, -2}, 1, "co"};
w2 = {{7, 7, 0}, 1, "co"};
wSum = {{2, -1, 1}, 1, "co"};
wDiff = {{0, 3, -1}, 1, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
w1 = {{1, 4, 10, 4, 13, 12}, 2, "co"};
w2 = {{1 , 4, -9, 4, -17, -32}, 2, "co"};
wSum = {{2, 8, 1, 8, -4, -20}, 2, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaSum, eaDual[w1], w2, eaDual[wSum]];
test2args[eaSum, w1, eaDual[w2], wSum];
test2args[eaSum, eaDual[w1], eaDual[w2], eaDual[wSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test2args[eaSum,{{18,-2,-1,14,-20,3},2,"co"} , {{6,-2,8,6,-15,-3},2,"co"}, {{24,-4,7,20,-35,0},2,"co"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test2args[eaSum, {{15,93,30,22,10,18},2,"co"}, {{32,44,-1,-56,-22,-32},2,"co"}, {{47,137,29,-34,-12,-14},2,"co"}];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test2args[eaSum, {{5,16,15,-1,0,3},2,"contra"}, {{4,3,12,-1,0,3},2,"contra"}, {{9,19,27,-2,0,6},2,"contra"}];

(* EA only: example that motivated a further simplification and correction of the addability condition *)
test2args[eaSum, {{1, -5, -14, 9, 23, 11}, 2, "co"}, {{25, -1, 2, -18, -14, 2}, 2, "contra"}, Error];

(* LA only checks example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors, but I think it's okay to check it here too *)
test2args[eaSum, {{3, 8, -4, -6}, 1, "co"}, {{9, 2, -4, 1}, 1 , "co"}, {{12, 10, -8, -5}, 1, "co"}];

(* LA only checks this non-min-grade-1 example, but I think it's okay to check it here too *)
septimalMeantoneW = {{1, 4, 10, 4, 13, 12}, 2, "co"};
flattoneW = {{1 , 4, -9, 4, -17, -32}, 2, "co"};
godzillaW = {{2, 8, 1, 8, -4, -20}, 2, "co"};
et19MwithIndependent7W = {{0, 0, 19, 0, 30, 44}, 2, "co"};
test2args[eaSum, septimalMeantoneW, flattoneW, godzillaW];
test2args[eaDiff, septimalMeantoneW, flattoneW, et19MwithIndependent7W];

(* LA only ensures the minors are consulted so that the sum and diff are identified correctly, but I think it's okay to check it here too *)
(* this also verifies that for the min-grade-1 case, I think *)
w1 = {{0, 1, -1, 0}, 3, "co"};
w2 = {{20, -144, 87, -59}, 3, "co"};
wSum = {{20, -143, 86, -59}, 3, "co"};
wDiff = {{20, -145, 88, -59}, 3, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* LA only ensures intractability beyond the breadth-first search of linear combinations code the first way I wrote it, i.e. using my fancier style essentially using a Wolfram Solve[]... but let's check it here too *)
w1 = {{35, 5, 40, 10, 27, -71, 19, -41, -5, 42}, 2, "co"};
w2 = {{5, -40, 30, -60, 12, -15, 15, 48, 24, -90}, 2, "co"};
wSum = {{40, -35, 70, -50, 39, -86, 34, 7, 19, -48}, 2, "co"};
wDiff = {{30, 45, 10, 70, 15, -56, 4, -89, -29, 132}, 2, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* random tests that check for matching between LA and EA *)

randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];

matrixToMultivectorWithPossibleError[a_] := If[a === Error, Error, matrixToMultivector[a]];

match[sumByMultivectors_, sumByMatrices_, diffByMultivectors_, diffByMatrices_] := Module[{sumsMatch, diffsMatch},
  sumsMatch = sumByMultivectors === sumByMatrices;
  diffsMatch = If[
    diffByMatrices === Error,
    If[
      diffByMultivectors === Error,
      True,
      allZeros[eaGetMinors[diffByMultivectors]]
    ],
    diffByMultivectors == diffByMatrices
  ];

  sumsMatch && diffsMatch
];

randomTestArithmeticMatchesBetweenLaAndEa[d_, r_, linearIndependence_, testCount_] := Module[
  {
    linearDependence,
    sharedVectors,
    t1,
    t2,
    w1,
    w2,
    sumByMatrices,
    sumByMultivectors,
    diffByMultivectors,
    diffByMatrices
  },

  Do[
    linearDependence = r - linearIndependence;

    sharedVectors = randomVectors[d, linearDependence];
    t1 = {Join[sharedVectors, randomVectors[d, linearIndependence]], "co"};
    t2 = {Join[sharedVectors, randomVectors[d, linearIndependence]], "co"};

    t1 = If[RandomInteger[] == 1, dual[t1], t1];
    t2 = If[RandomInteger[] == 1, dual[t2], t2];

    w1 = matrixToMultivector[t1];
    w2 = matrixToMultivector[t2];

    sumByMultivectors = eaSum[w1, w2];
    sumByMatrices = matrixToMultivectorWithPossibleError[sum[t1, t2]];

    diffByMultivectors = eaDiff[w1, w2];
    diffByMatrices = matrixToMultivectorWithPossibleError[diff[t1, t2]];

    If[
      match[sumByMultivectors, sumByMatrices, diffByMultivectors, diffByMatrices],
      p += 1,
      f += 1;
      Print["failure: "];
      Print[w1, " + ", w2, " = (OR ", t1, " + ", t2, " = )"];
      Print[sumByMultivectors, " (by multivectors)"];
      Print[sumByMatrices, " (by matrices)"];
      Print[w1, " - ", w2, " = (OR ", t1, " - ", t2, " = )"];
      Print[diffByMultivectors, " (by multivectors)"];
      Print[diffByMatrices, " (by matrices)\n"];
    ],
    testCount
  ]
];

randomTestArithmeticMatchesBetweenLaAndEa[2, 1, 1, 16];

randomTestArithmeticMatchesBetweenLaAndEa[3, 1, 1, 8];
randomTestArithmeticMatchesBetweenLaAndEa[3, 2, 1, 8];

randomTestArithmeticMatchesBetweenLaAndEa[4, 1, 1, 4];
randomTestArithmeticMatchesBetweenLaAndEa[4, 2, 1, 4];
randomTestArithmeticMatchesBetweenLaAndEa[4, 3, 1, 4];
randomTestArithmeticMatchesBetweenLaAndEa[4, 2, 2, 4];

randomTestArithmeticMatchesBetweenLaAndEa[5, 1, 1, 2];
randomTestArithmeticMatchesBetweenLaAndEa[5, 2, 1, 2];
randomTestArithmeticMatchesBetweenLaAndEa[5, 3, 1, 2];
randomTestArithmeticMatchesBetweenLaAndEa[5, 4, 1, 2];
randomTestArithmeticMatchesBetweenLaAndEa[5, 2, 2, 2];
randomTestArithmeticMatchesBetweenLaAndEa[5, 3, 2, 2];

randomTestArithmeticMatchesBetweenLaAndEa[6, 1, 1, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 2, 1, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 3, 1, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 4, 1, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 5, 1, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 2, 2, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 3, 2, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 4, 2, 1];
randomTestArithmeticMatchesBetweenLaAndEa[6, 3, 3, 1];



(* ___ PRIVATE ___ *)


(* MULTIVECTOR UTILITIES *)

(* eaIndices *)
If[eaIndices[0, 0] == {{}}, "", f = f + 1; Print["eaIndices[0, 0] == {{}}"]];
If[eaIndices[1, 0] == {{}}, "", f = f + 1; Print["eaIndices[1, 0] == {{}}"]];
If[eaIndices[1, 1] == IdentityMatrix[1], "", f = f + 1; Print["eaIndices[1, 1] == IdentityMatrix[1]"]];
If[eaIndices[2, 0] == {{}}, "", f = f + 1; Print["eaIndices[2, 0] == {{}}"]];
If[eaIndices[2, 1] == {{1}, {2}}, "", f = f + 1; Print["eaIndices[2, 1] == {{1}, {2}}"]];
If[eaIndices[2, 2] == {{1, 2}}, "", f = f + 1; Print["eaIndices[2, 2] == {{1, 2}}"]];
If[eaIndices[3, 0] == {{}}, "", f = f + 1; Print["eaIndices[3, 0] == {{}}"]];
If[eaIndices[3, 1] == {{1}, {2}, {3}}, "", f = f + 1; Print["eaIndices[3, 1] == {{1}, {2}, {3}}"]];
If[eaIndices[3, 2] == {{1, 2}, {1, 3}, {2, 3}}, "", f = f + 1; Print["eaIndices[3, 2] == {{1, 2}, {1, 3}, {2, 3}}"]];
If[eaIndices[3, 3] == {{1, 2, 3}}, "", f = f + 1; Print["eaIndices[3, 3] == {{1, 2, 3}}"]];
If[eaIndices[4, 0] == {{}}, "", f = f + 1; Print["eaIndices[4, 0] == {{}}"]];
If[eaIndices[4, 1] == {{1}, {2}, {3}, {4}}, "", f = f + 1; Print["eaIndices[4, 1] == {{1}, {2}, {3}, {4}}"]];
If[eaIndices[4, 2] == {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, {3, 4}}, "", f = f + 1; Print["eaIndices[4, 2] == {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, {3, 4}}"]];
If[eaIndices[4, 3] == {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}, "", f = f + 1; Print["eaIndices[4, 3] == {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}"]];
If[eaIndices[4, 4] == {{1, 2, 3, 4}}, "", f = f + 1; Print["eaIndices[4, 4] == {{1, 2, 3, 4}}"]];

(* isNondecomposable *)
test[isNondecomposable, {{2, -4, 8, -9, 7, 2}, 2, "co"}, True];
test[isNondecomposable, {{1, 4, 4}, 2, "co"}, False];

(* eaGetMinors *)
test[eaGetMinors, {{1, 4, 4}, 2, "co"}, {1, 4, 4}];

(* eaGetGrade *)
test[eaGetGrade, {{1, 4, 4}, 2, "co"}, 2];

(* eaGetV *)
test[eaGetV, {{1, 4, 4}, 2, "co"}, "co"];


(* DUAL *)

(* multivectorToTensor *)
test[multivectorToTensor, {{1, 4, 4}, 2, "co"}, Symmetrize[{{0, 1, 4}, {-1, 0, 4}, {-4, -4, 0}}, Antisymmetric[{1, 2}]]];

(* tensorToMultivector *)
tensorToMultivectorTester[{minors_, v_, grade_, d_}] := {minors, v, grade, d} == Module[{},
  If[
    tensorToMultivector[multivectorToTensor[{minors, v, grade, d}], v, grade, d],
    p += 1,
    f += 1;
    Print["tensorToMultivectorTester[", {minors, v, grade, d}, "]"]
  ]
];
tensorToMultivectorTester[{{1, 4, 4}, 2, "co"}];
tensorToMultivectorTester[{{0, 0, 0}, 2, "co"}];

(* CONVERSION TO AND FROM MATRIX *)

(* minorsList *)
test[minorsList, {{17, 16, -4}, {4, -4, 1}}, {-4, 1, 0}];


(* MEET AND JOIN *)

(* a basic right interior product example, with grade of a > b, and a being contra *)
test2args[rightInteriorProduct, d3g2contra1, d3g1co1, d3g1contra1];
(* a weird right interior product example, with grade of a = b, and a being contra; works the same as above, may bottom out *)
test2args[rightInteriorProduct, d3g1contra1, d3g1co2, Error];
(*a weird right interior product example, with grade of a < b, and a being contra, should bottom out grade of 0 and contra *)
test2args[rightInteriorProduct, d3g1contra1, d3g2co2, Error];

(* a basic right interior product example, with grade of a > b, and a being co *)
test2args[rightInteriorProduct, d3g2co1, d3g1contra2, d3g1co2];
(*a weird right interior product example, with grade of a = b, and a being co, works the same as above, may bottom out *)
test2args[rightInteriorProduct, d3g1co1, d3g1contra2, d3unisonco];
(* a weird right interior product example, with grade of a < b, and a being co, should bottom out grade of 0 and co *)
test2args[rightInteriorProduct, d3g1co1, d3g2contra1, Error];

(* a basic left interior product example, with grade of b > a, and b contra *)
test2args[leftInteriorProduct, d3g1co1, d3g2contra1, d3g1contra1];
(* a weird left interior product example, with grade of b = a, and b contra, works the same as above, may bottom out *)
test2args[leftInteriorProduct, d3g1co1, d3g1contra2, d3jicontra];
(* a weird left interior product example, with grade of b < a, and b contra, should bottom out at grade of 0 and contra *)
test2args[leftInteriorProduct, d3g2co1, d3g1contra2, Error];
(* a basic left interior product example, with grade of b > a, and b co *)
test2args[leftInteriorProduct, d3g1contra2, d3g2co1, d3g1co2];
(* a weird left interior product example, with grade of b = a, and b co, works the same as above, may bottom out *)
test2args[leftInteriorProduct, d3g1contra1, d3g1co2, Error];
(* a weird left interior product example, with grade of b < a, and b co, should bottom out at grade of 0 and co *)
test2args[leftInteriorProduct, d3g2contra1, d3g1co1, Error];



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
