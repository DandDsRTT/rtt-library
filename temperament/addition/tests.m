failures = 0;
passes = 0;

format = "Wolfram";


(* addable mappings*)
meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "row"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "row"};
test[sumPrivate, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "row"}];
test[diffPrivate, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "row"}];
meantoneC = {{{4, -4, 1}}, "col"};
porcupineC = {{{1, -5, 3}}, "col"};
test[sumPrivate, meantoneC, porcupineC, {{{5, -9, 4}}, "col"}];
test[diffPrivate, meantoneC, porcupineC, {{{-3, -1, 2}}, "col"}];

(* addable comma bases *)
et7M = {{{7, 11, 16}}, "row"};
et5M = {{{5, 8, 12}}, "row"};
test[sumPrivate, et7M, et5M, {{{12, 19, 28}}, "row"}];
test[diffPrivate, et7M, et5M, {{{2, 3, 4}}, "row"}];
et7C = dualPrivate[et7M];
et5C = dualPrivate[et5M];
test[sumPrivate, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "col"}];
test[diffPrivate, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "col"}];

(* not addable - error! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "row"};
test[sumPrivate, septimalMeantoneM, septimalBlackwoodM, Error];
test[diffPrivate, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dualPrivate[septimalMeantoneM];
septimalBlackwoodC = dualPrivate[septimalBlackwoodM];
test[sumPrivate, septimalMeantoneC, septimalBlackwoodC, Error];
test[diffPrivate, septimalMeantoneC, septimalBlackwoodC, Error];

(* addable - linear-dependence-2 (comma bases) *)
et12M = {{{12, 19, 28, 34}}, "row"};
et19M = {{{19, 30, 44, 53}}, "row"};
test[sumPrivate, et12M, et19M, {{{31, 49, 72, 87}}, "row"}];
test[diffPrivate, et12M, et19M, {{{7, 11, 16, 19}}, "row"}];
et12C = dualPrivate[et12M];
et19C = dualPrivate[et19M];
test[sumPrivate, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "col"}];
test[diffPrivate, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "col"}];

(* examples with themselves *)
test[sumPrivate, meantoneM, meantoneM, meantoneM];
test[diffPrivate, meantoneM, meantoneM, Error];
test[sumPrivate, meantoneC, meantoneC, meantoneC];
test[diffPrivate, meantoneC, meantoneC, Error];
test[sumPrivate, et7M, et7M, et7M];
test[diffPrivate, et7M, et7M, Error];
test[sumPrivate, et7C, et7C, et7C];
test[diffPrivate, et7C, et7C, Error];

(* mismatched r & n but matching d *)
test[sumPrivate, et7M, meantoneM, Error];
test[diffPrivate, et7M, meantoneM, Error];
test[sumPrivate, et7C, meantoneC, Error];
test[diffPrivate, et7C, meantoneC, Error];

(* mismatched d but matching r or n *)
test[sumPrivate, et7M, et12M, Error];
test[diffPrivate, et7M, et12M, Error];
test[sumPrivate, et7C, et12C, Error];
test[diffPrivate, et7C, et12C, Error];

(* some basic examples *)
augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "row"}; (* ⟨⟨3 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "row"}; (* ⟨⟨4 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "row"}; (* ⟨⟨4 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "row"}; (* ⟨⟨2 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "row"}; (* ⟨⟨2 -4 -11]] *)
test[sumPrivate, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "row"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]]*)
test[diffPrivate, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "row"}];(* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]]*)
test[sumPrivate, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "row"}] ;(* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]]*)
test[diffPrivate, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]]*)
test[sumPrivate, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "row"}] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]]*)
test[diffPrivate, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "row"}] ;(* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]]*)
test[sumPrivate, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "row"}] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]]*)
test[diffPrivate, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]]*)
test[sumPrivate, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "row"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]]*)
test[diffPrivate, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "row"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]]*)
test[sumPrivate, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "row"}];(* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]]*)
test[diffPrivate, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "row"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]]*)
test[sumPrivate, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "row"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test[diffPrivate, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[sumPrivate, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "row"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test[diffPrivate, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "row"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test[sumPrivate, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "row"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test[diffPrivate, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "row"}];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test[sumPrivate, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "row"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test[diffPrivate, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "row"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2 *)
t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "row"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "row"};
test[sumPrivate, t1, t2, Error];
test[diffPrivate, t1, t2, Error];

(* example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1 *)
t1 = {{{2, 3}}, "col"};
t2 = {{{4, -7}}, "row"};
tSum = {{{9, 7}}, "col"};
tDiff = {{{5, 1}}, "col"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];

(* example demonstrating how it's important to canonicalize *)
t1 = {{{-2, 4, -2}}, "row"};
t2 = {{{7, 7, 0}}, "row"};
tSum = {{{2, -1, 1}}, "row"};
tDiff = {{{0, 3, -1}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
t1 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
t2 = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "row"};
tSum = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[sumPrivate, dualPrivate[t1], t2, dualPrivate[tSum]];
test[sumPrivate, t1, dualPrivate[t2], tSum];
test[sumPrivate, dualPrivate[t1], dualPrivate[t2], dualPrivate[tSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test[sumPrivate, {{{1, 2, -1, 1}, {0, 18, -2, -1}}, "row"}, {{{2, 0, -2, 5}, {0, 3, -1, 4}}, "row"}, {{{1, 19, -4, 7}, {0, 24, -4, 7}}, "row"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test[sumPrivate, {{{3, 2, 8, 2}, {0, 5, 31, 10}}, "row"}, {{{1, 22, 32, 0}, {0, 32, 44, -1}}, "row"}, {{{1, 32, 94, 20}, {0, 47, 137, 29}}, "row"}];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test[sumPrivate, {{{5, 0, 1, 0}, {-16, 1, 0, 3}}, "col"}, {{{4, 0, 1, 0}, {-3, 1, 0, 3}}, "col"}, {{{9, 0, 2, 0}, {-5, 1, 1, 3}}, "col"}];

(* LA only: example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors *)
test[sumPrivate, {{{3, 8, -4, -6}}, "row"}, {{{9, 2, -4, 1}}, "row"}, {{{12, 10, -8, -5}}, "row"}];

(* LA only: example that was intractable unless I defactored piecemeal *)
test[sumPrivate, {{{-97, 73, 45, 16}}, "col"}, {{{-1, 8, 9, 3}}, "col"}, {{{-98, 81, 54, 19}}, "col"}];

(* LA only: example that motivated the existence of the special min-grade-1 path... which no longer exists, but I'll keep this around anyway *)
test[sumPrivate, {{{2, 0, 3}}, "col"}, {{{5, 4, 0}}, "col"}, {{{7, 4, 3}}, "col"}];
test[diffPrivate, {{{2, 0, 3}}, "col"}, {{{5, 4, 0}}, "col"}, {{{-3, -4, 3}}, "col"}];

(* LA only: non-min-grade-1 *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "row"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "row"};
meanmagM = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "row"};
test[sumPrivate, septimalMeantoneM, flattoneM, godzillaM];
test[diffPrivate, septimalMeantoneM, flattoneM, meanmagM];

(* LA only: ensure the largestMinorsL are consulted so that the sum and diff are identified correctly *)
t1 = {{{0, 1, 4}}, "row"};
t2 = {{{5, -6, -2}}, "row"};
tSum = {{{5, -5, 2}}, "row"};
tDiff = {{{5, -7, -6}}, "row"};
test[sumPrivate, t1, t2, tSum];
test[diffPrivate, t1, t2, tDiff];

(* LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works *)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "col"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "col"};
test[sumPrivate, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "col"}];
test[diffPrivate, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "col"}];

(* LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)
t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "row"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "row"};
test[sumPrivate, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "row"}];
test[diffPrivate, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "row"}];

(* LA only: example where the first vectors of the input were not actually linearly independent from the basis for the linearly dependent vectors, things would fail, so now we actually test each one to ensure it's linearly independent before adding it into the initial matrix to be defactored *)
test[sumPrivate, {{{-17, -55, 24, 34}}, "col"}, {{{-1, -7, 0, 2}}, "col"}, {{{-9, -31, 12, 18}}, "col"}];

(* LA only: an example that used to fail for whatever reason, the "languisher" *)
test[sumPrivate, {{{23, -14, 3, 0}, {9, -5, 1, 1}}, "col"}, {{{1, 7, 3, -1}, {0, 25, 14, -1}}, "row"}, {{{23, -14, 14, 0}, {9, -5, 5, 1}}, "col"}];

(* LA only: an example that used to fail for whatever reason, the "big random" *)
test[sumPrivate, {{{-89, -46, 61, 0, 0}, {-85, -44, 59, 1, 0}, {-39, -21, 26, 0, 1}}, "col"}, {{{-16, -9, 1, 0, 0}, {10, 4, 0, 1, 0}, {16, 8, 0, 0, 1}}, "col"}, Error];

(* across domain basis - error *)
test[sumPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, Error];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
