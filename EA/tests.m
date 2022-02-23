failures = 0;
passes = 0;



(* MULTIVECTOR UTILITIES *)

(* eaGetD *)
test[eaGetD, {{1, 4, 4}, 2, "co"}, 3];

(* eaGetR *)
test[eaGetR, {{1, 4, 4}, 2, "co"}, 2];

(* eaGetN *)
test[eaGetN, {{1, 4, 4}, 2, "co"}, 1];


(* MULTIVECTOR FORMS & DEFACTORING *)

canonicalMc = {{107, -87, 72, -49, 31}, 4, "contra"};
negatedCanonicalMc = {{-107, 87, -72, 49, -31}, 4, "contra"};
canonicalMm = {{31, 49, 72, 87, 107}, 1, "co"};
negatedCanonicalMm = {{-31, -49, - 72, -87, -107}, 1, "co"};

(* eaCanonicalForm *)
test[eaCanonicalForm, canonicalMc, canonicalMc];
test[eaCanonicalForm, negatedCanonicalMc, canonicalMc];
test[eaCanonicalForm, canonicalMm, canonicalMm];
test[eaCanonicalForm, negatedCanonicalMm, canonicalMm];

test[eaCanonicalForm, {{4}, 0, "co", 3}, {{1}, 0, "co", 3}];
test[eaCanonicalForm, {{2, -4, 8, -9, 7, 2}, 2, "co"}, Error];
test[eaCanonicalForm, {{1, 0, 1}, 2, "co"}, {{1, 0, 1}, 2, "co"}];

test[eaCanonicalForm, {{0, 0, 0, 0, 0, 0}, 2, "co"}, {{0, 0, 0, 0, 0, 0}, 2, "co"}];


(* DUAL *)

(* eaDual *)
test[eaDual, canonicalMc, canonicalMm];
test[eaDual, negatedCanonicalMc, canonicalMm];
test[eaDual, canonicalMm, canonicalMc];
test[eaDual, negatedCanonicalMm, canonicalMc];

test[eaDual, {{1}, 0, "contra", 3}, {{1}, 3, "co"}];
test[eaDual, {{1}, 0, "co", 5}, {{1}, 5, "contra"}];
test[eaDual, {{2, -4, 8, -9, 7, 2}, 2, "co"}, Error];
test[eaDual, {{1, 0, 1}, 2, "co"}, {{1, 0, 1}, 1, "contra"}];

eaDualTester[multimap_, multicomma_] := Module[{},
  If[
    eaDual[multimap] == multicomma && eaDual[multicomma] == multimap,
    passes += 1,
    failures += 1;
    Print["eaDualTester[", multimap, ", ", multicomma, "]; actual dual multimap: ", eaDual[multicomma], " and dual multicomma: ", eaDual[multimap]]
  ];
];
eaDualTester[{{1, 4, 4}, 2, "co"}, {{4, -4, 1}, 1, "contra"}];

randomTandU[] := Module[{d, grade, ma, t, u},
  d = RandomInteger[{1, 5}];
  grade = RandomInteger[{1, d}];
  ma = RandomInteger[{-9, 9}, {grade, d}];
  
  t = If[RandomInteger[] == 1, {ma, "contra"}, {ma, "co"}];
  u = matrixToMultivector[t];
  
  {t, u}
];

Do[
  u = Last[randomTandU[]];
  
  dualU = eaDual[u];
  doubleDualU = eaDual[dualU];
  
  If[
    doubleDualU == u,
    passes += 1,
    failures += 1;
    Print["BAD BAD BAD! multivector: ", u, " computed dual: ", dualU, " and then back: ", doubleDualU]
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
testMultivectorMatrixConversion[u_, t_] := Module[{convertedU, convertedT},
  convertedT = multivectorToMatrix[u];
  convertedU = matrixToMultivector[t];
  
  If[
    convertedT == t && convertedU == u,
    passes += 1,
    failures += 1;
    Print["testMultivectorMatrixConversion[]; convertedT: ", convertedT, " t: ", t, " convertedU: ", convertedU, " u: ", u]]
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
  tAndU = randomTandU[];
  t = First[tAndU];
  u = Last[tAndU];
  
  uAndBackToT = multivectorToMatrix[u];
  
  If[
    uAndBackToT == canonicalForm[t],
    passes += 1,
    failures += 1;
    Print["BAD BAD BAD! (following all in canonical form) matrix: ", canonicalForm[t], " computed equiv multivector: ", u, " and then back to matrix: ", uAndBackToT]
  ],
  100
];

(* multivectorToMatrix & matrixToMultivector: one-off *)

testMatrix[t_] := If[
  canonicalForm[t] == multivectorToMatrix[matrixToMultivector[t]],
  passes += 1,
  failures += 1;
  Print["testMatrix[]", multivectorToMatrix[matrixToMultivector[t]]]
];
testMultivector[v_] := If[
  eaCanonicalForm[v] == matrixToMultivector[multivectorToMatrix[v]],
  passes += 1,
  failures += 1;
  Print["testMultivector[]", matrixToMultivector[multivectorToMatrix[v]]]
];

testMatrix[{{{-4, -8, 3, 7, -1, -3}, {1, -2, -2, 4, 4, -6}, {2, -9, 9, -8, 0, 7}, {5, -5, 4, -8, 5, -6}, {9, 0, 2, 8, -4, -3}}, "contra"}];

testMultivector[{{2, 8, 8}, 2, "co"}];
testMultivector[{{0, 0, 3, 4}, 3, "contra"}];
testMultivector[{{1, 0, 1}, 2, "co"}];


(* MERGE *)

(* d =2, mm *)
d2g1co1 = {{12, 19}, 1, "co"};
d2g1co2 = {{19, 30}, 1, "co"};
d2jiCo = {{1}, 2, "co"};

(* d=3, mm *)
d3g1co1 = {{12, 19, 28}, 1, "co"};
d3g1co2 = {{19, 30, 44}, 1, "co"};
d3g1co3 = {{22, 35, 51}, 1, "co"};
d3g2co1 = {{1, 4, 4}, 2, "co"};
d3g2co2 = {{3, 5, 1}, 2, "co"};
d3jiCo = {{1}, 3, "co"};
d3unisonCo = {{1}, 0, "co", 3};

(* d=3, mc *)
d3g1contra1 = {{4, -4, 1}, 1, "contra"};
d3g1contra2 = {{-10, -1, 5}, 1, "contra"};
d3g1contra3 = {{1, -5, 3}, 1, "contra"};
d3g2contra1 = {{44, -30, 19}, 2, "contra"};
d3g2contra2 = {{28, -19, 12}, 2, "contra"};
d3g2contra3 = {{51, -35, 22}, 2, "contra"};
d5g3contra = {{19, -33, 14, -46, 46, -46, 29, -29, 29, 0}, 3, "contra"};
d3jiContra = {{1}, 0, "contra", 3};
d3unisonContra = {{1}, 3, "contra"};

(* d=5, mm *)
d5g1co = {{31, 49, 72, 87, 107}, 1, "co"};
d5g2co1 = {{-9, -5, 3, -7, 13, 30, 20, 21, 1, -30}, 2, "co"}; (*progressiveProduct[{{15, 24, 35, 42, 52}, 1, "co"}, {{16, 25, 37, 45, 55}, 1, "co"}];*)
d5g2co2 = {{1, 4, -2, -6, 4, -6, -13, -16, -28, -10}, 2, "co"}; (* progressiveProduct[{{12, 19, 28, 34, 42}, 1, "co"}, {{17, 27, 40, 48, 59}, 1, "co"}]; *)
d5g2co3 = {{6, -7, -2, 15, -25, -20, 3, 15, 59, 49}, 2, "co"}; (*example from interior product page *)
d5g3co = {{1, 2, -3, -2, 1, -4, -5, 12, 9, -19}, 3, "co"};(*example from interior product page *)
d5g4co = {{1, 2, 1, 2, 3}, 4, "co"};(*example from interior product page *)
d5unisonCo = {{1}, 0, "co", 5};

(* d=5, mc *)
d5g1contra = {{-3, 2, -1, 2, -1}, 1, "contra"};
d5g2contra = {{5, 11, -7, -4, -9, 8, 1, 5, -5, 5}, 2, "contra"};
d5jiContra = {{1}, 0, "contra", 5};

(* super basic progressive product example *)
test[progressiveProduct, d2g1co1, d2g1co2, d2jiCo];

(* wedging with oneself equals a zero varianced multivector *)
test[progressiveProduct, d3g1co1, d3g1co1, {{0, 0, 0}, 2, "co"}];

(* another basic progressive product example *)
test[progressiveProduct, d5g2co1, d5g2co2, d5g4co];
(* show how progressive product can cap out when grade exactly hits the dimensionality, for mc *)
test[progressiveProduct, d3g2contra1, d3g1contra3, d3unisonContra];
(* show how progressive product can cap out when grade exceeds at the dimensionality, for mc *)
test[progressiveProduct, d3g2contra1, d3g2contra2, Error];
(* show how progressive product can cap out when grade exactly hits the dimensionality, for mm*)
test[progressiveProduct, d3g2co1, d3g1co3, d3jiCo];
(* show how progressive product can cap out when grade exceeds the dimensionality, for mm*)
test[progressiveProduct, d3g2co1, d3g2co2, Error];

(* a basic regressive product example *)
test[regressiveProduct, d3g2contra1, d3g2contra2, d3g1contra1];
(* show how regressive product can cap out when grade hits exactly 0, for mc *)
test[regressiveProduct, d3g1contra1, d3g2contra3, d3jiContra];
(* show how regressive product can cap out when grade goes below 0, for mc *)
test[regressiveProduct, d3g1contra1, d3g1contra2, Error];
(* show how regressive product can cap out when grade hits exactly 0, for mm *)
test[regressiveProduct, d3g1co1, d3g2co2, d3unisonCo];
(* show how regressive product can cap out when grade goes below 0, for mm*)
test[regressiveProduct, d3g1co1, d3g1co2, Error];

(* a series of examples working up to the symmetric interior product *)

test[rightInteriorProduct, d5g1contra, d5g3co, Error];
test[rightInteriorProduct, d5g3co, d5g1contra, d5g2co3];

test[leftInteriorProduct, d5g1contra, d5g3co, d5g2co3];
test[leftInteriorProduct, d5g3co, d5g1contra, Error];

test[interiorProduct, d5g1contra, d5g3co, d5g2co3];
test[interiorProduct, d5g3co, d5g1contra, d5g2co3];

(* a similar series of examples but with grade of contra > grade of co *)

test[rightInteriorProduct, d5g1co, d5g3contra, Error];
test[rightInteriorProduct, d5g3contra, d5g1co, d5g2contra];

test[leftInteriorProduct, d5g1co, d5g3contra, d5g2contra];
test[leftInteriorProduct, d5g3contra, d5g1co, Error];

test[interiorProduct, d5g1co, d5g3contra, d5g2contra];
test[interiorProduct, d5g3contra, d5g1co, d5g2contra];

(* progressive product errors if it gets mixed variance *)
test[progressiveProduct, d5g1contra, d5g3co, Error];

(* regressive product errors if it gets mixed variance *)
test[regressiveProduct, d5g1contra, d5g3co, Error];

(* interior product errors if it gets two mm *)
test[rightInteriorProduct, d5g2co1, d5g2co2, Error];
test[leftInteriorProduct, d5g2co1, d5g2co2, Error];
test[interiorProduct, d5g2co1, d5g2co2, Error];

(* interior product errors if it gets two mc *)
test[rightInteriorProduct, d3g2contra1, d3g2contra2, Error];
test[leftInteriorProduct, d3g2contra1, d3g2contra2, Error];
test[interiorProduct, d3g2contra1, d3g2contra2, Error];

(* same examples as for meet and join *)

et5Mm5 = matrixToMultivector[{{{5, 8, 12}}, "co"}];
et5Mc5 = matrixToMultivector[{{{-8, 5, 0}, {-4, 1, 1}}, "contra"}];
et7Mm5 = matrixToMultivector[{{{7, 11, 16}}, "co"}];
et7Mc5 = matrixToMultivector[{{{-11, 7, 0}, {-7, 3, 1}}, "contra"}];
meantoneMm5 = matrixToMultivector[{{{1, 0, -4}, {0, 1, 4}}, "co"}];
meantoneMc5 = matrixToMultivector[{{{4, -4, 1}}, "contra"}];
porcupineMm5 = matrixToMultivector[{{{1, 2, 3}, {0, 3, 5}}, "co"}];
porcupineMc5 = matrixToMultivector[{{{1, -5, 3}}, "contra"}];
d3unisonContra = {{1}, 3, "contra"};
d3jiCo = {{1}, 3, "co"};

test[progressiveProduct, et5Mm5, et7Mm5, meantoneMm5];
test[progressiveProduct, et5Mc5, et7Mc5, Error];
test[progressiveProduct, meantoneMm5, porcupineMm5, Error];
test[progressiveProduct, meantoneMc5, porcupineMc5, et7Mc5];

meantoneMm11 = matrixToMultivector[{{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "co"}];
meantoneMc11 = matrixToMultivector[{{meantoneComma11, starlingComma11, mothwellsma11}, "contra"}];
meanpopMm11 = matrixToMultivector[{{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "co"}];
meanpopMc11 = matrixToMultivector[{{meantoneComma11, starlingComma11, keenanisma11}, "contra"}];
marvelMm11 = matrixToMultivector[{{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"}];
marvelMc11 = matrixToMultivector[{{marvelComma11, keenanisma11}, "contra"}];
porcupineMm11 = matrixToMultivector[{{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "co"}];
porcupineMc11 = matrixToMultivector[{{telepathma11, septimalComma11, ptolemisma11}, "contra"}];
meantoneMm7 = matrixToMultivector[{{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"}];
meantoneMc7 = matrixToMultivector[{{meantoneComma7, starlingComma7}, "contra"}];
porcupineMm7 = matrixToMultivector[{{{1, 2, 3, 2}, {0, 3, 5, -6}}, "co"}];
porcupineMc7 = matrixToMultivector[{{septimalComma7, porcupineComma7}, "contra"}];
miracleMm11 = matrixToMultivector[{{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "co"}];
miracleMc11 = matrixToMultivector[{{marvelComma11, rastma11, keenanisma11}, "contra"}];
magicMm11 = matrixToMultivector[{{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "co"}];
magicMc11 = matrixToMultivector[{{marvelComma11, sensamagicComma11, ptolemisma11}, "contra"}];
miracleMm7 = matrixToMultivector[{{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}];
miracleMc7 = matrixToMultivector[{{marvelComma7, gamelisma7}, "contra"}];
magicMm7 = matrixToMultivector[{{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"}];
magicMc7 = matrixToMultivector[{{marvelComma7, sensamagicComma7}, "contra"}];
mothraMm11 = matrixToMultivector[{{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "co"}];
mothraMc11 = matrixToMultivector[{{meantoneComma11, mothwellsma11, keenanisma11}, "contra"}];
mothraMm7 = matrixToMultivector[{{{1, 1, 0, 3}, {0, 3, 12, -1}}, "co"}];
mothraMc7 = matrixToMultivector[{{meantoneComma7, gamelisma7}, "contra"}];

(*⋎ = COMMA MERGE, ⋏ = MAP MERGE *)

(*Meantone⋎Meanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test[progressiveProduct, meantoneMc11, meanpopMc11, Error];

(*Meantone⋏Meanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMm11, meanpopMm11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*Meantone⋎Marvel = 31, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMc11, marvelMc11, {{0}, 5, "contra"}];


(*Meantone⋏Marvel = <225/224>, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, meantoneMm11, marvelMm11, {{0}, 5, "co"}];

(*Meantone⋎Porcupine = G = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, meantoneMc11, porcupineMc11, Error];

(*Meantone⋏Porcupine = <176/175>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMm11, porcupineMm11, matrixToMultivector[dual[{{valinorsma11}, "contra"}]]];

(*In the 7-limit, that become Meantone⋎Porcupine = <JI>, Meantone⋏Porcupine = <1>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMc7, porcupineMc7, matrixToMultivector[{IdentityMatrix[4], "contra"}]];
test[progressiveProduct, meantoneMm7, porcupineMm7, matrixToMultivector[{IdentityMatrix[4], "co"}]];

(*Miracle⋎Magic = 41, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, miracleMc11, magicMc11, Error];

(*Miracle⋏Magic = Marvel, but they're linearly dependent so EA gives an all-zero result *)
test[progressiveProduct, miracleMm11, magicMm11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*In the 7-limit, again Miracle⋎Magic = 41, Miracle⋏Magic = Marvel, but they're linearly dependent so EA gives all-zero results*)
test[progressiveProduct, miracleMc7, magicMc7, {{0}, 4, "contra"}];
test[progressiveProduct, miracleMm7, magicMm7, {{0}, 4, "co"}];

(*Miracle⋎Mothra = 31, but the sum of their grades is greater than the dimensionality so EA gives an error *)
test[progressiveProduct, miracleMc11, mothraMc11, Error];

(* Miracle⋏Mothra = Portent, but they're linearly dependent so EA gives an all-zero result *)
test[progressiveProduct, miracleMm11, mothraMm11, {{0, 0, 0, 0, 0}, 4, "co"}];

(*In the 7-limit, Miracle⋏Mothra = Gamelan, but they're linearly dependent so EA gives an all-zero result*)
test[progressiveProduct, miracleMm7, mothraMm7, {{0}, 4, "co"}];

(*Meantone⋎Magic = <JI>, but the sum of their grades is greater than the dimensionality so EA gives an error*)
test[progressiveProduct, meantoneMc11, magicMc11, Error];

(*Meantone⋏Magic = <225/224>, and these are linearly independent so the result is the same in EA*)
test[progressiveProduct, meantoneMm11, magicMm11, matrixToMultivector[dual[{{marvelComma11}, "contra"}]]];


(* ADDITION *)

(* addable mm *)
meantoneMm = {{1, 4, 4}, 2, "co"};
porcupineMm = {{3, 5, 1}, 2, "co"};
test[eaSum, meantoneMm, porcupineMm, {{4, 9, 5}, 2, "co"}];
test[eaDiff, meantoneMm, porcupineMm, {{2, 1, -3}, 2, "co"}];
meantoneMc = {{4, -4, 1}, 1, "contra"};
porcupineMc = {{1, -5, 3}, 1, "contra"};
test[eaSum, meantoneMc, porcupineMc, {{5, -9, 4}, 1, "contra"}];
test[eaDiff, meantoneMc, porcupineMc, {{-3, -1, 2}, 1, "contra"}];

(* addable mc *)
et7Mm = {{7, 11, 16}, 1, "co"};
et5Mm = {{5, 8, 12}, 1, "co"};
test[eaSum, et7Mm, et5Mm, {{12, 19, 28}, 1, "co"}];
test[eaDiff, et7Mm, et5Mm, {{2, 3, 4}, 1, "co"}];
et7Mc = {{16, -11, 7}, 2, "contra"};
et5Mc = {{12, -8, 5}, 2, "contra"};
test[eaSum, et7Mc, et5Mc, {{28, -19, 12}, 2, "contra"}];
test[eaDiff, et7Mc, et5Mc, {{4, -3, 2}, 2, "contra"}];

(* not addable - error! *)
septimalMeantoneMm = {{1, 4, 10, 4, 13, 12}, 2, "co"};
septimalBlackwoodMm = {{0, 5, 0, 8, 0, -14}, 2, "co"};
test[eaSum, septimalMeantoneMm, septimalBlackwoodMm, Error];
test[eaDiff, septimalMeantoneMm, septimalBlackwoodMm, Error];
septimalMeantoneMc = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "co"}];
septimalBlackwoodMc = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "co"}];
test[eaSum, septimalMeantoneMc, septimalBlackwoodMc, Error];
test[eaDiff, septimalMeantoneMc, septimalBlackwoodMc, Error];

(* addable - linear-dependence-2 (mc) *)
et12Mm = {{12, 19, 28, 34}, 1, "co"};
et19Mm = {{19, 30, 44, 53}, 1, "co"};
test[eaSum, et12Mm, et19Mm, {{31, 49, 72, 87}, 1, "co"}];
test[eaDiff, et12Mm, et19Mm, {{7, 11, 16, 19}, 1, "co"}];
et12Mc = eaDual[et12Mm];
et19Mc = eaDual[et19Mm];
test[eaSum, et12Mc, et19Mc, {{-87, 72, -49, 31}, 3, "contra"}];
test[eaDiff, et12Mc, et19Mc, {{-19, 16, -11, 7}, 3, "contra"}];

(* examples with themselves *)
test[eaSum, meantoneMm, meantoneMm, {{1, 4, 4}, 2, "co"}];
test[eaDiff, meantoneMm, meantoneMm, {{0, 0, 0}, 2, "co"}];
test[eaSum, meantoneMc, meantoneMc, {{4, -4, 1}, 1, "contra"}];
test[eaDiff, meantoneMc, meantoneMc, {{0, 0, 0}, 1, "contra"}];
test[eaSum, et7Mm, et7Mm, {{7, 11, 16}, 1, "co"}];
test[eaDiff, et7Mm, et7Mm, {{0, 0, 0}, 1, "co"}];
test[eaSum, et7Mc, et7Mc, {{16, -11, 7}, 2, "contra"}];
test[eaDiff, et7Mc, et7Mc, {{0, 0, 0}, 2, "contra"}];

(* mismatched r & n but matching d *)
test[eaSum, et7Mm, meantoneMm, Error];
test[eaDiff, et7Mm, meantoneMm, Error];
test[eaSum, et7Mc, meantoneMc, Error];
test[eaDiff, et7Mc, meantoneMc, Error];

(* mismatched d but matching r or n *)
test[eaSum, et7Mm, et12Mm, Error];
test[eaDiff, et7Mm, et12Mm, Error];
test[eaSum, et7Mc, et12Mc, Error];
test[eaDiff, et7Mc, et12Mc, Error];

(* some basic examples *)
augmentedMm = {{3, 0, -7}, 2, "co"};
diminishedMm = {{4, 4, -3}, 2, "co"};
tetracotMm = {{4, 9, 5}, 2, "co"};
dicotMm = {{2, 1, -3}, 2, "co"};
srutalMm = {{2, -4, -11}, 2, "co"};
test[eaSum, augmentedMm, diminishedMm, {{7, 4, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]] *)
test[eaDiff, augmentedMm, diminishedMm, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]] *)
test[eaSum, augmentedMm, tetracotMm, {{7, 9, -2}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]] *)
test[eaDiff, augmentedMm, tetracotMm, {{1, 9, 12}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]] *)
test[eaSum, augmentedMm, dicotMm, {{5, 1, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]] *)
test[eaDiff, augmentedMm, dicotMm, {{1, -1, -4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]] *)
test[eaSum, augmentedMm, srutalMm, {{5, -4, -18}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]] *)
test[eaDiff, augmentedMm, srutalMm, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]] *)
test[eaSum, diminishedMm, tetracotMm, {{8, 13, 2}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]] *)
test[eaDiff, diminishedMm, tetracotMm, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]] *)
test[eaSum, diminishedMm, dicotMm, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]] *)
test[eaDiff, diminishedMm, dicotMm, {{2, 3, 0}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]] *)
test[eaSum, diminishedMm, srutalMm, {{3, 0, -7}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test[eaDiff, diminishedMm, srutalMm, {{1, 4, 4}, 2, "co"}]; (*⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[eaSum, tetracotMm, dicotMm, {{3, 5, 1}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test[eaDiff, tetracotMm, dicotMm, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[eaSum, tetracotMm, srutalMm, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test[eaDiff, tetracotMm, srutalMm, {{2, 13, 16}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test[eaSum, dicotMm, srutalMm, {{4, -3, -14}, 2, "co"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test[eaDiff, dicotMm, srutalMm, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2 *)
u1 = {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "co"};
u2 = {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "co"};
test[eaSum, u1, u2, Error];
test[eaDiff, u1, u2, Error];

(* example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1 *)
u1 = {{2, 3}, 1, "contra"};
u2 = {{4, -7}, 1, "co"};
uSum = {{9, 7}, 1, "contra"};
uDiff = {{5, 1}, 1, "contra"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* example demonstrating how it's important to canonicalize *)
u1 = {{-2, 4, -2}, 1, "co"};
u2 = {{7, 7, 0}, 1, "co"};
uSum = {{2, -1, 1}, 1, "co"};
uDiff = {{0, 3, -1}, 1, "co"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
u1 = {{1, 4, 10, 4, 13, 12}, 2, "co"};
u2 = {{1, 4, -9, 4, -17, -32}, 2, "co"};
uSum = {{2, 8, 1, 8, -4, -20}, 2, "co"};
test[eaSum, u1, u2, uSum];
test[eaSum, eaDual[u1], u2, eaDual[uSum]];
test[eaSum, u1, eaDual[u2], uSum];
test[eaSum, eaDual[u1], eaDual[u2], eaDual[uSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test[eaSum, {{18, -2, -1, 14, -20, 3}, 2, "co"}, {{6, -2, 8, 6, -15, -3}, 2, "co"}, {{24, -4, 7, 20, -35, 0}, 2, "co"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test[eaSum, {{15, 93, 30, 22, 10, 18}, 2, "co"}, {{32, 44, -1, -56, -22, -32}, 2, "co"}, {{47, 137, 29, -34, -12, -14}, 2, "co"}];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test[eaSum, {{5, 16, 15, -1, 0, 3}, 2, "contra"}, {{4, 3, 12, -1, 0, 3}, 2, "contra"}, {{9, 19, 27, -2, 0, 6}, 2, "contra"}];

(* EA only: example that motivated a further simplification and correction of the addability condition *)
test[eaSum, {{1, -5, -14, 9, 23, 11}, 2, "co"}, {{25, -1, 2, -18, -14, 2}, 2, "contra"}, Error];

(* LA only checks example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors, but I think it's okay to check it here too *)
test[eaSum, {{3, 8, -4, -6}, 1, "co"}, {{9, 2, -4, 1}, 1, "co"}, {{12, 10, -8, -5}, 1, "co"}];

(* LA only checks this non-min-grade-1 example, but I think it's okay to check it here too *)
septimalMeantoneU = {{1, 4, 10, 4, 13, 12}, 2, "co"};
flattoneU = {{1, 4, -9, 4, -17, -32}, 2, "co"};
godzillaU = {{2, 8, 1, 8, -4, -20}, 2, "co"};
et19MwithIndependent7U = {{0, 0, 19, 0, 30, 44}, 2, "co"};
test[eaSum, septimalMeantoneU, flattoneU, godzillaU];
test[eaDiff, septimalMeantoneU, flattoneU, et19MwithIndependent7U];

(* LA only ensures the lm are consulted so that the sum and diff are identified correctly, but I think it's okay to check it here too *)
(* this also verifies that for the min-grade-1 case, I think *)
u1 = {{0, 1, -1, 0}, 3, "co"};
u2 = {{20, -144, 87, -59}, 3, "co"};
uSum = {{20, -143, 86, -59}, 3, "co"};
uDiff = {{20, -145, 88, -59}, 3, "co"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* LA only ensures intractability beyond the breadth-first search of linear combinations code the first way I wrote it, i.e. using my fancier style essentially using a Wolfram Solve[]... but let's check it here too *)
u1 = {{35, 5, 40, 10, 27, -71, 19, -41, -5, 42}, 2, "co"};
u2 = {{5, -40, 30, -60, 12, -15, 15, 48, 24, -90}, 2, "co"};
uSum = {{40, -35, 70, -50, 39, -86, 34, 7, 19, -48}, 2, "co"};
uDiff = {{30, 45, 10, 70, 15, -56, 4, -89, -29, 132}, 2, "co"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* random tests that check for matching between LA and EA *)

randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];

matrixToMultivectorWithPossibleError[a_] := If[a === Error, Error, matrixToMultivector[a]];

match[sumByU_, sumByT_, diffByU_, diffByT_] := Module[{sumsMatch, diffsMatch},
  sumsMatch = sumByU === sumByT;
  diffsMatch = If[
    diffByT === Error,
    If[
      diffByU === Error,
      True,
      allZeros[eaGetLm[diffByU]]
    ],
    diffByU == diffByT
  ];
  
  sumsMatch && diffsMatch
];

randomTestAdditionMatchesBetweenLaAndEa[d_, r_, li_, testCount_] := Module[
  {
    ld,
    ldb,
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
    ld = r - li;
    
    ldb = randomVectors[d, ld];
    t1 = {Join[ldb, randomVectors[d, li]], "co"};
    t2 = {Join[ldb, randomVectors[d, li]], "co"};
    
    t1 = If[RandomInteger[] == 1, dual[t1], t1];
    t2 = If[RandomInteger[] == 1, dual[t2], t2];
    
    u1 = matrixToMultivector[t1];
    u2 = matrixToMultivector[t2];
    
    sumByU = eaSum[u1, u2];
    sumByT = matrixToMultivectorWithPossibleError[sum[t1, t2]];
    
    diffByU = eaDiff[u1, u2];
    diffByT = matrixToMultivectorWithPossibleError[diff[t1, t2]];
    
    If[
      match[sumByU, sumByT, diffByU, diffByT],
      passes += 1,
      failures += 1;
      Print["failure: "];
      Print[u1, " + ", u2, " = (OR ", t1, " + ", t2, " = )"];
      Print[sumByU, " (by multivectors)"];
      Print[sumByT, " (by matrices)"];
      Print[u1, " - ", u2, " = (OR ", t1, " - ", t2, " = )"];
      Print[diffByU, " (by multivectors)"];
      Print[diffByT, " (by matrices)\n"];
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

(* eaGetLm *)
test[eaGetLm, {{1, 4, 4}, 2, "co"}, {1, 4, 4}];

(* eaGetGrade *)
test[eaGetGrade, {{1, 4, 4}, 2, "co"}, 2];

(* eaGetV *)
test[eaGetV, {{1, 4, 4}, 2, "co"}, "co"];


(* DUAL *)

(* uToTensor *)
test[uToTensor, {{1, 4, 4}, 2, "co"}, Symmetrize[{{0, 1, 4}, {-1, 0, 4}, {-4, -4, 0}}, Antisymmetric[{1, 2}]]];

(* tensorToU *)
tensorToUTester[{lm_, v_, grade_, d_}] := {lm, v, grade, d} == Module[{},
  If[
    tensorToU[uToTensor[{lm, v, grade, d}], v, grade, d],
    passes += 1,
    failures += 1;
    Print["tensorToUTester[", {lm, v, grade, d}, "]"]
  ]
];
tensorToUTester[{{1, 4, 4}, 2, "co"}];
tensorToUTester[{{0, 0, 0}, 2, "co"}];

(* CONVERSION TO AND FROM MATRIX *)

(* getLm *)
test[getLm, {{17, 16, -4}, {4, -4, 1}}, {-4, 1, 0}];


(* MERGE *)

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



Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
