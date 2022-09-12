failures = 0;
passes = 0;


(* ADDITION *)

(* addable mm *)
meantoneMm = {{1, 4, 4}, 2, "row"};
porcupineMm = {{3, 5, 1}, 2, "row"};
test[eaSum, meantoneMm, porcupineMm, {{4, 9, 5}, 2, "row"}];
test[eaDiff, meantoneMm, porcupineMm, {{2, 1, -3}, 2, "row"}];
meantoneMc = {{4, -4, 1}, 1, "col"};
porcupineMc = {{1, -5, 3}, 1, "col"};
test[eaSum, meantoneMc, porcupineMc, {{5, -9, 4}, 1, "col"}];
test[eaDiff, meantoneMc, porcupineMc, {{-3, -1, 2}, 1, "col"}];

(* addable mc *)
et7Mm = {{7, 11, 16}, 1, "row"};
et5Mm = {{5, 8, 12}, 1, "row"};
test[eaSum, et7Mm, et5Mm, {{12, 19, 28}, 1, "row"}];
test[eaDiff, et7Mm, et5Mm, {{2, 3, 4}, 1, "row"}];
et7Mc = {{16, -11, 7}, 2, "col"};
et5Mc = {{12, -8, 5}, 2, "col"};
test[eaSum, et7Mc, et5Mc, {{28, -19, 12}, 2, "col"}];
test[eaDiff, et7Mc, et5Mc, {{4, -3, 2}, 2, "col"}];

(* not addable - error! *)
septimalMeantoneMm = {{1, 4, 10, 4, 13, 12}, 2, "row"};
septimalBlackwoodMm = {{0, 5, 0, 8, 0, -14}, 2, "row"};
test[eaSum, septimalMeantoneMm, septimalBlackwoodMm, Error];
test[eaDiff, septimalMeantoneMm, septimalBlackwoodMm, Error];
septimalMeantoneMc = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "row"}];
septimalBlackwoodMc = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "row"}];
test[eaSum, septimalMeantoneMc, septimalBlackwoodMc, Error];
test[eaDiff, septimalMeantoneMc, septimalBlackwoodMc, Error];

(* addable - linear-dependence-2 (mc) *)
et12Mm = {{12, 19, 28, 34}, 1, "row"};
et19Mm = {{19, 30, 44, 53}, 1, "row"};
test[eaSum, et12Mm, et19Mm, {{31, 49, 72, 87}, 1, "row"}];
test[eaDiff, et12Mm, et19Mm, {{7, 11, 16, 19}, 1, "row"}];
et12Mc = eaDual[et12Mm];
et19Mc = eaDual[et19Mm];
test[eaSum, et12Mc, et19Mc, {{-87, 72, -49, 31}, 3, "col"}];
test[eaDiff, et12Mc, et19Mc, {{-19, 16, -11, 7}, 3, "col"}];

(* examples with themselves *)
test[eaSum, meantoneMm, meantoneMm, {{1, 4, 4}, 2, "row"}];
test[eaDiff, meantoneMm, meantoneMm, {{0, 0, 0}, 2, "row"}];
test[eaSum, meantoneMc, meantoneMc, {{4, -4, 1}, 1, "col"}];
test[eaDiff, meantoneMc, meantoneMc, {{0, 0, 0}, 1, "col"}];
test[eaSum, et7Mm, et7Mm, {{7, 11, 16}, 1, "row"}];
test[eaDiff, et7Mm, et7Mm, {{0, 0, 0}, 1, "row"}];
test[eaSum, et7Mc, et7Mc, {{16, -11, 7}, 2, "col"}];
test[eaDiff, et7Mc, et7Mc, {{0, 0, 0}, 2, "col"}];

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
augmentedMm = {{3, 0, -7}, 2, "row"};
diminishedMm = {{4, 4, -3}, 2, "row"};
tetracotMm = {{4, 9, 5}, 2, "row"};
dicotMm = {{2, 1, -3}, 2, "row"};
srutalMm = {{2, -4, -11}, 2, "row"};
test[eaSum, augmentedMm, diminishedMm, {{7, 4, -10}, 2, "row"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]] *)
test[eaDiff, augmentedMm, diminishedMm, {{1, 4, 4}, 2, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]] *)
test[eaSum, augmentedMm, tetracotMm, {{7, 9, -2}, 2, "row"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]] *)
test[eaDiff, augmentedMm, tetracotMm, {{1, 9, 12}, 2, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]] *)
test[eaSum, augmentedMm, dicotMm, {{5, 1, -10}, 2, "row"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]] *)
test[eaDiff, augmentedMm, dicotMm, {{1, -1, -4}, 2, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]] *)
test[eaSum, augmentedMm, srutalMm, {{5, -4, -18}, 2, "row"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]] *)
test[eaDiff, augmentedMm, srutalMm, {{1, 4, 4}, 2, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]] *)
test[eaSum, diminishedMm, tetracotMm, {{8, 13, 2}, 2, "row"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]] *)
test[eaDiff, diminishedMm, tetracotMm, {{0, 5, 8}, 2, "row"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]] *)
test[eaSum, diminishedMm, dicotMm, {{6, 5, -6}, 2, "row"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]] *)
test[eaDiff, diminishedMm, dicotMm, {{2, 3, 0}, 2, "row"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]] *)
test[eaSum, diminishedMm, srutalMm, {{3, 0, -7}, 2, "row"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test[eaDiff, diminishedMm, srutalMm, {{1, 4, 4}, 2, "row"}]; (*⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[eaSum, tetracotMm, dicotMm, {{3, 5, 1}, 2, "row"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test[eaDiff, tetracotMm, dicotMm, {{1, 4, 4}, 2, "row"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[eaSum, tetracotMm, srutalMm, {{6, 5, -6}, 2, "row"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test[eaDiff, tetracotMm, srutalMm, {{2, 13, 16}, 2, "row"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test[eaSum, dicotMm, srutalMm, {{4, -3, -14}, 2, "row"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test[eaDiff, dicotMm, srutalMm, {{0, 5, 8}, 2, "row"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2 *)
u1 = {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "row"};
u2 = {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "row"};
test[eaSum, u1, u2, Error];
test[eaDiff, u1, u2, Error];

(* example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1 *)
u1 = {{2, 3}, 1, "col"};
u2 = {{4, -7}, 1, "row"};
uSum = {{9, 7}, 1, "col"};
uDiff = {{5, 1}, 1, "col"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* example demonstrating how it's important to canonicalize *)
u1 = {{-2, 4, -2}, 1, "row"};
u2 = {{7, 7, 0}, 1, "row"};
uSum = {{2, -1, 1}, 1, "row"};
uDiff = {{0, 3, -1}, 1, "row"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
u1 = {{1, 4, 10, 4, 13, 12}, 2, "row"};
u2 = {{1, 4, -9, 4, -17, -32}, 2, "row"};
uSum = {{2, 8, 1, 8, -4, -20}, 2, "row"};
test[eaSum, u1, u2, uSum];
test[eaSum, eaDual[u1], u2, eaDual[uSum]];
test[eaSum, u1, eaDual[u2], uSum];
test[eaSum, eaDual[u1], eaDual[u2], eaDual[uSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test[eaSum, {{18, -2, -1, 14, -20, 3}, 2, "row"}, {{6, -2, 8, 6, -15, -3}, 2, "row"}, {{24, -4, 7, 20, -35, 0}, 2, "row"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test[eaSum, {{15, 93, 30, 22, 10, 18}, 2, "row"}, {{32, 44, -1, -56, -22, -32}, 2, "row"}, {{47, 137, 29, -34, -12, -14}, 2, "row"}];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test[eaSum, {{5, 16, 15, -1, 0, 3}, 2, "col"}, {{4, 3, 12, -1, 0, 3}, 2, "col"}, {{9, 19, 27, -2, 0, 6}, 2, "col"}];

(* EA only: example that motivated a further simplification and correction of the addability condition *)
test[eaSum, {{1, -5, -14, 9, 23, 11}, 2, "row"}, {{25, -1, 2, -18, -14, 2}, 2, "col"}, Error];

(* LA only checks example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors, but I think it's okay to check it here too *)
test[eaSum, {{3, 8, -4, -6}, 1, "row"}, {{9, 2, -4, 1}, 1, "row"}, {{12, 10, -8, -5}, 1, "row"}];

(* LA only checks this non-min-grade-1 example, but I think it's okay to check it here too *)
septimalMeantoneU = {{1, 4, 10, 4, 13, 12}, 2, "row"};
flattoneU = {{1, 4, -9, 4, -17, -32}, 2, "row"};
godzillaU = {{2, 8, 1, 8, -4, -20}, 2, "row"};
et19MwithIndependent7U = {{0, 0, 19, 0, 30, 44}, 2, "row"};
test[eaSum, septimalMeantoneU, flattoneU, godzillaU];
test[eaDiff, septimalMeantoneU, flattoneU, et19MwithIndependent7U];

(* LA only ensures the largestMinorsL are consulted so that the sum and diff are identified correctly, but I think it's okay to check it here too *)
(* this also verifies that for the min-grade-1 case, I think *)
u1 = {{0, 1, -1, 0}, 3, "row"};
u2 = {{20, -144, 87, -59}, 3, "row"};
uSum = {{20, -143, 86, -59}, 3, "row"};
uDiff = {{20, -145, 88, -59}, 3, "row"};
test[eaSum, u1, u2, uSum];
test[eaDiff, u1, u2, uDiff];

(* LA only ensures intractability beyond the breadth-first search of linear combinations code the first way I wrote it, i.e. using my fancier style essentially using a Wolfram Solve[]... but let's check it here too *)
u1 = {{35, 5, 40, 10, 27, -71, 19, -41, -5, 42}, 2, "row"};
u2 = {{5, -40, 30, -60, 12, -15, 15, 48, 24, -90}, 2, "row"};
uSum = {{40, -35, 70, -50, 39, -86, 34, 7, 19, -48}, 2, "row"};
uDiff = {{30, 45, 10, 70, 15, -56, 4, -89, -29, 132}, 2, "row"};
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

Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
