f = 0;
p = 0;

test[fn_, arg_, expectation_] := Module[{actual},
  actual = fn[arg];
  
  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg, "] != ", expectation, "; actual result was: ", actual]
  ]
];

test2args[fn_, arg1_, arg2_, expectation_] := Module[{actual},
  actual = fn[arg1, arg2];
  
  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg1, ",", arg2, "] != ", expectation, "; actual result was: ", actual]
  ]
];

test3args[fn_, arg1_, arg2_, arg3_, expectation_] := Module[{actual},
  actual = fn[arg1, arg2, arg3];
  
  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg1, ",", arg2, "," arg3, "] != ", expectation, "; actual result was: ", actual]
  ]
];


(* TEMPERAMENT UTILITIES *)

(* getD *)
test[getD, {{{0}}, "co"}, 1];
test[getD, {{{0}}, "contra"}, 1];
test[getD, {{{0, 0}}, "co"} , 2];
test[getD, {{{0, 0}}, "contra"} , 2];
test[getD, {{{0}, {0}}, "co"}, 1];
test[getD, {{{0}, {0}}, "contra"}, 1];
test[getD, {IdentityMatrix[2], "co"}, 2];
test[getD, {IdentityMatrix[2], "contra"}, 2];
test[getD, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 3];
test[getD, {{{4, -4, 1}}, "contra"}, 3];
test[getD, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 4];
test[getD, {{{4, -4, 1, 0}}, "contra"}, 4];

(* getR *)
test[getR, {{{0}}, "co"}, 0];
test[getR, {{{0}}, "contra"}, 1];
test[getR, {{{0, 0}}, "co"} , 0];
test[getR, {{{0, 0}}, "contra"} , 2];
test[getR, {{{0}, {0}}, "co"}, 0];
test[getR, {{{0}, {0}}, "contra"}, 1];
test[getR, {IdentityMatrix[2], "co"}, 2];
test[getR, {IdentityMatrix[2], "contra"}, 0];
test[getR, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 2];
test[getR, {{{4, -4, 1}}, "contra"}, 2];
test[getR, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getR, {{{4, -4, 1, 0}}, "contra"}, 3];

(* getN *)
test[getN, {{{0}}, "co"}, 1];
test[getN, {{{0}}, "contra"}, 0];
test[getN, {{{0, 0}}, "co"} , 2];
test[getN, {{{0, 0}}, "contra"} , 0];
test[getN, {{{0}, {0}}, "co"}, 1];
test[getN, {{{0}, {0}}, "contra"}, 0];
test[getN, {IdentityMatrix[2], "co"}, 0];
test[getN, {IdentityMatrix[2], "contra"}, 2];
test[getN, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 1];
test[getN, {{{4, -4, 1}}, "contra"}, 1];
test[getN, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getN, {{{4, -4, 1, 0}}, "contra"}, 1];


(* CANONICALIZATION *)

(* canonicalForm *)
test[canonicalForm, {{{12, 0, 0}, {19, 0, 0}}, "a"}, {{{1, 0, 0}}, "a"}];
test[canonicalForm, {{{1, 1, 0}, {0, 1, 4}}, "a"}, {{{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalForm, {{{12, 19, 28}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalForm, {{{7, 11, 16}, {22, 35, 51}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{3, 0, -1}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{1, 2, 3}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{0, 1, 4, 10}, {1, 0, -4, -13}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalForm, {{{10, 13, 12, 0}, {-1, -1, 0, 3}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalForm, {{{5, 8, 0}, {0, 0, 1}}, "a"}, {{{5, 8, 0}, {0, 0, 1}}, "a"}];
test[canonicalForm, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}];
test[canonicalForm, {{{12, 19, 28}, {26, 43, 60}}, "a"}, {{{1, 8, 0}, {0, 11, -4}}, "a"}];
test[canonicalForm, {{{17, 16, -4}, {4, -4, 1}}, "a"}, {{{1, 0, 0}, {0, 4, -1}}, "a"}];
test[canonicalForm, {{{6, 5, -4}, {4, -4, 1}}, "a"}, {{{2, 1, -1}, {0, 2, -1}}, "a"}];
test[canonicalForm, {{{12, 19, 28}, {0, 0, 0}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {1, 1, 0, -3}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}}, "a"}];
test[canonicalForm, {{{0, 0}}, "a"}, {{{0, 0}}, "a"}];
test[canonicalForm, {IdentityMatrix[3], "a"}, {IdentityMatrix[3], "a"}];
test[canonicalForm, {{{1, 0, -4}, {0, 1, 4}, {0, 0, 0}}, "a", {{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalForm, {{{12, 19, 28, 0}}, "a"}, {{{12, 19, 28, 0}}, "a"}];
test[canonicalForm, {{{0, 0, 0}, {0, 0, 0}}, "a"}, {{{0, 0, 0}}, "a"}];


(* DUAL *)

(* dual *)
verifyDuals[m_, c_] := Module[{dualM, dualC},
  dualC = dual[m];
  dualM = dual[c];
  
  If[
    dualC == canonicalForm[c] && dualM == canonicalForm[m],
    p += 1,
    f += 1;
    Print["verifyDuals[", m, ", ", c, "]; dualC: ", dualC, " canonicalForm[c]: ", canonicalForm[c], " dualM: ", dualM, " canonicalForm[m]: ", canonicalForm[m]]
  ];
];

verifyDuals[{{{1, 0, -4}, {0, 1, 4}}, "co"}, {{{4, -4, 1}}, "contra"}];
verifyDuals[{{{1, 0, 0}, {0, -4, 9}}, "co"}, {{{0, 9, 4}}, "contra"}];
verifyDuals[{{{0}}, "co"}, {IdentityMatrix[1], "contra"}];
verifyDuals[{{{0, 0}}, "co"}, {IdentityMatrix[2], "contra"}];
verifyDuals[{{{0, 0, 0}}, "co"}, {IdentityMatrix[3], "contra"}];
verifyDuals[{IdentityMatrix[1], "co"}, {{{0}}, "contra"}];
verifyDuals[{IdentityMatrix[2], "co"}, {{{0, 0}}, "contra"}];
verifyDuals[{IdentityMatrix[3], "co"}, {{{0, 0, 0}}, "contra"}];
verifyDuals[{{{12, 19}}, "co"}, {{{-19, 12}}, "contra"}];


(* MEET AND JOIN *)

(* basic examples *)

et5M5 = {{{5, 8, 12}}, "co"};
et5C5 = {{{-8, 5, 0}, {-4, 1, 1}}, "contra"};
et7M5 = {{{7, 11, 16}}, "co"};
et7C5 = {{{-11, 7, 0}, {-7, 3, 1}}, "contra"};
meantoneM5 = {{{1, 0, -4}, {0, 1, 4}}, "co"};
meantoneC5 = {{{4, -4, 1}}, "contra"};
porcupineM5 = {{{1, 2, 3}, {0, 3, 5}}, "co"};
porcupineC5 = {{{1, -5, 3}}, "contra"};

test[dual, et5C5, et5M5];
test[dual, et7C5, et7M5];
test[dual, meantoneC5, meantoneM5];
test[dual, porcupineC5, porcupineM5];

test2args[join, et5M5, et7M5, meantoneM5];
test2args[meet, meantoneC5, porcupineC5, et7C5];

(* prove out that you can specify temperaments by either their mappings or their comma bases *)

test2args[join, {et5M5, et7C5}, meantoneM5];
test2args[meet, {meantoneM5, porcupineC5}, et7C5];
test2args[join, {et5C5, et7M5}, meantoneM5];
test2args[meet, {meantoneC5, porcupineM5}, et7C5];
test2args[join, {et5C5, et7C5}, meantoneM5];
test2args[meet, {meantoneM5, porcupineM5}, et7C5];

(* prove out that you can meet or join more than 2 temperaments at a time *)

et7dLimit7 = {{{7, 11, 16, 19}}, "co"};
et12Limit7 = {{{12, 19, 28, 34}}, "co"};
et22Limit7 = {{{22, 35, 51, 62}}, "co"};
marvel = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
test3args[join, et7dLimit7, et12Limit7, et22Limit7, marvel];

mintC7 = {{{2, 2, -1, -1}}, "contra"};
meantoneC7 = {{{4, -4, 1, 0}}, "contra"};
negriC7 = {{{-14, 3, 4, 0}}, "contra"};
et19dC7 = dual[{{{19, 30, 44, 54}}, "co"}];
test3args[meet, mintC7, meantoneC7, negriC7, et19dC7];

(* examples from Meet and Join page *)

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

meantoneM11 = {{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "co"};
meantoneC11 = {{meantoneComma11, starlingComma11, mothwellsma11} , "contra"};
meanpopM11 = {{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "co"};
meanpopC11 = {{meantoneComma11, starlingComma11, keenanisma11} , "contra"};
marvelM11 = {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"};
marvelC11 = {{marvelComma11, keenanisma11} , "contra"};
porcupineM11 = {{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "co"};
porcupineC11 = {{telepathma11, septimalComma11, ptolemisma11} , "contra"};
et31M11 = {{{31, 49, 72, 87, 107}}, "co"};
et31C11 = {{{-49, 31, 0, 0, 0}, {-45, 27, 1, 0, 0}, {-36, 21, 0, 1, 0}, {-24, 13, 0, 0, 1}} , "contra"};
meantoneM7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
meantoneC7 = {{meantoneComma7, starlingComma7} , "contra"};
porcupineM7 = {{{1, 2, 3, 2}, {0, 3, 5, -6}}, "co"};
porcupineC7 = {{septimalComma7, porcupineComma7} , "contra"};
miracleM11 = {{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "co"};
miracleC11 = {{marvelComma11, rastma11, keenanisma11} , "contra"};
magicM11 = {{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "co"};
magicC11 = {{marvelComma11, sensamagicComma11, ptolemisma11} , "contra"};
et41M11 = {{{41, 65, 95, 115, 142}}, "co"};
et41C11 = {{{-65, 41, 0, 0, 0}, {-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {-32, 18, 0, 0, 1}} , "contra"};
miracleM7 = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"};
miracleC7 = {{marvelComma7, gamelisma7} , "contra"};
magicM7 = {{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"};
magicC7 = {{marvelComma7, sensamagicComma7} , "contra"};
et41M7 = {{{41, 65, 95, 115}}, "co"};
et41C7 = {{{-65, 41, 0, 0}, {-15, 8, 1, 0}, {-25, 14, 0, 1}} , "contra"};
mothraM11 = {{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "co"};
mothraC11 = {{meantoneComma11, mothwellsma11, keenanisma11} , "contra"};
mothraM7 = {{{1, 1, 0, 3}, {0, 3, 12, -1}}, "co"};
mothraC7 = {{meantoneComma7, gamelisma7} , "contra"};
portentM11 = {{{1, 1, 0, 3, 5}, {0, 3, 0, -1, 4}, {0, 0, 1, 0, -1}}, "co"};
portentC11 = {{keenanisma11, werckisma11} , "contra"};
gamelanM7 = {{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "co"};
gamelanC7 = {{gamelisma7}, "contra"};
marvelM7 = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
marvelC7 = {{marvelComma7}, "contra"};

test[dual, meantoneC11, meantoneM11];
test[dual, meanpopC11, meanpopM11];
test[dual, marvelC11, marvelM11];
test[dual, porcupineC11, porcupineM11];
test[dual, et31C11, et31M11];
test[dual, meantoneC7, meantoneM7];
test[dual, porcupineC7, porcupineM7];
test[dual, miracleC11, miracleM11];
test[dual, magicC11, magicM11];
test[dual, et41C11, et41M11];
test[dual, miracleC7, miracleM7];
test[dual, magicC7, magicM7];
test[dual, et41C7, et41M7];
test[dual, mothraC11, mothraM11];
test[dual, mothraC7, mothraM7];
test[dual, portentC11, portentM11];
test[dual, gamelanC7, gamelanM7];
test[dual, marvelC7, marvelM7];

(*⋎ = MEET, ⋏ = JOIN *)

(*Meantone⋎Meanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val.*)
test2args[meet, meantoneC11, meanpopC11, et31C11];

(*Meantone⋏Meanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>*)
test2args[join, meantoneM11, meanpopM11, {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {0, 0, 0, 0, 1}}, "co"}];

(*Meantone⋎Marvel = 31*)
test2args[meet, meantoneC11, marvelC11, et31C11];

(*Meantone⋏Marvel = <225/224>*)
test2args[join, meantoneM11, marvelM11, dual[{{marvelComma11}, "contra"}]];

(*Meantone⋎Porcupine = G = <JI>*)
test2args[meet, meantoneC11, porcupineC11, {IdentityMatrix[5], "contra"}];

(*Meantone⋏Porcupine = <176/175>*)
test2args[join, meantoneM11, porcupineM11, dual[{{valinorsma11}, "contra"}]];

(*In the 7-limit, that become Meantone⋎Porcupine = <JI>, Meantone⋏Porcupine = <1>*)
test2args[meet, meantoneC7, porcupineC7, {IdentityMatrix[4], "contra"}];
test2args[join, meantoneM7, porcupineM7, {IdentityMatrix[4], "co"}];

(*Miracle⋎Magic = 41 *)
test2args[meet, miracleC11, magicC11, et41C11];

(*Miracle⋏Magic = Marvel *)
test2args[join, miracleM11, magicM11, marvelM11];

(*In the 7-limit, again Miracle⋎Magic = 41, Miracle⋏Magic = Marvel*)
test2args[meet, miracleC7, magicC7, et41C7];
test2args[join, miracleM7, magicM7, marvelM7];

(*Miracle⋎Mothra = 31 *)
test2args[meet, miracleC11, mothraC11, et31C11];

(* Miracle⋏Mothra = Portent *)
test2args[join, miracleM11, mothraM11, portentM11];

(*In the 7-limit, Miracle⋏Mothra = Gamelan.*)
test2args[join, miracleM7, mothraM7, gamelanM7];

(*Meantone⋎Magic = <JI>,*)
test2args[meet, meantoneC11, magicC11, {IdentityMatrix[5], "contra"}];

(*Meantone⋏Magic = <225/224>*)
test2args[join, meantoneM11, magicM11, dual[{{marvelComma11}, "contra"}]];


(* ARITHMETIC *)



(* addable mappings*)
meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
test2args[sum, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "co"}];
test2args[diff, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "co"}];
meantoneC = {{{4, -4, 1}}, "contra"};
porcupineC = {{{1, -5, 3}}, "contra"};
test2args[sum, meantoneC, porcupineC, {{{5, -9, 4}}, "contra"}];
test2args[diff, meantoneC, porcupineC, {{{-3, -1, 2}}, "contra"}];

(* addable comma bases *)
et7M = {{{7, 11, 16}}, "co"};
et5M = {{{5, 8, 12}}, "co"};
test2args[sum, et7M, et5M, {{{12, 19, 28}}, "co"}];
test2args[diff, et7M, et5M, {{{2, 3, 4}}, "co"}];
et7C = dual[et7M];
et5C = dual[et5M];
test2args[sum, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "contra"}];
test2args[diff, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "contra"}];

(* not addable - error! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
test2args[sum, septimalMeantoneM, septimalBlackwoodM, Error];
test2args[diff, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dual[septimalMeantoneM];
septimalBlackwoodC = dual[septimalBlackwoodM];
test2args[sum, septimalMeantoneC, septimalBlackwoodC, Error];
test2args[diff, septimalMeantoneC, septimalBlackwoodC, Error];

(* addable - linear-dependence-2 (comma bases) *)
et12M = {{{12, 19, 28, 34}}, "co"};
et19M = {{{19, 30, 44, 53}}, "co"};
test2args[sum, et12M, et19M, {{{31, 49, 72, 87}}, "co"}];
test2args[diff, et12M, et19M, {{{7, 11, 16, 19}}, "co"}];
et12C = dual[et12M];
et19C = dual[et19M];
test2args[sum, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "contra"}];
test2args[diff, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "contra"}];

(* examples with themselves *)
test2args[sum, meantoneM, meantoneM, meantoneM];
test2args[diff, meantoneM, meantoneM, Error];
test2args[sum, meantoneC, meantoneC, meantoneC];
test2args[diff, meantoneC, meantoneC, Error];
test2args[sum, et7M, et7M, et7M];
test2args[diff, et7M, et7M, Error];
test2args[sum, et7C, et7C, et7C];
test2args[diff, et7C, et7C, Error];

(* mismatched r & n but matching d *)
test2args[sum, et7M, meantoneM, Error];
test2args[diff, et7M, meantoneM, Error];
test2args[sum, et7C, meantoneC, Error];
test2args[diff, et7C, meantoneC, Error];

(* mismatched d but matching r or n *)
test2args[sum, et7M, et12M, Error];
test2args[diff, et7M, et12M, Error];
test2args[sum, et7C, et12C, Error];
test2args[diff, et7C, et12C, Error];

(* some basic examples *)
augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "co"}; (* ⟨⟨3 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "co"}; (* ⟨⟨4 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"}; (* ⟨⟨4 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "co"}; (* ⟨⟨2 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "co"}; (* ⟨⟨2 -4 -11]] *)
test2args[sum, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]]*)
test2args[diff, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ];(* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]]*)
test2args[sum, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]]*)
test2args[diff, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]]*)
test2args[sum, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]]*)
test2args[diff, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "co"} ] ;(* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]]*)
test2args[sum, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]]*)
test2args[diff, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]]*)
test2args[sum, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]]*)
test2args[diff, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]]*)
test2args[sum, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ];(* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]]*)
test2args[diff, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]]*)
test2args[sum, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[diff, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[sum, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[diff, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[sum, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[diff, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "co"} ];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[sum, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "co"} ]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[diff, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of linearly dependent, but not addable: d = 5, min-grade = 2, linear-independence = 2 *)
t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "co"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "co"};
test2args[sum, t1, t2, Error];
test2args[diff, t1, t2, Error];

(* example of addable, but not linearly dependent: d = 2, min-grade = 1, linear-independence = 1 *)
t1 = {{{2, 3}}, "contra"};
t2 = {{{4, -7}}, "co"};
tSum = {{{9, 7}}, "contra"};
tDiff = {{{5, 1}}, "contra"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* example demonstrating how it's important to canonicalize *)
t1 = {{{-2, 4, -2}}, "co"};
t2 = {{{7, 7, 0}}, "co"};
tSum = {{{2, -1, 1}}, "co"};
tDiff = {{{0, 3, -1}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
t1 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
t2 = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
tSum = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[sum, dual[t1], t2, dual[tSum]];
test2args[sum, t1, dual[t2], tSum];
test2args[sum, dual[t1], dual[t2], dual[tSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test2args[sum, {{{1, 2, -1, 1}, {0, 18, -2, -1}}, "co"}, {{{2, 0, -2, 5}, {0, 3, -1, 4}}, "co"}, {{{1, 19, -4, 7}, {0, 24, -4, 7}}, "co"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test2args[sum, {{{3, 2, 8, 2}, {0, 5, 31, 10}}, "co"}, {{{1, 22, 32, 0}, {0, 32, 44, -1}}, "co"} ];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test2args[sum, {{{5, 0, 1, 0}, {-16, 1, 0, 3}}, "contra"}, {{{4, 0, 1, 0}, {-3, 1, 0, 3}}, "contra"}, {{{9, 0, 2, 0}, {-5, 1, 1, 3}}, "contra"}];

(* LA only: example that required the breadth-first search of linear combinations of multiple linearly dependent basis vectors *)
test2args[sum, {{{3, 8, -4, -6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* LA only: example that was intractable unless I defactored piecemeal *)
test2args[sum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];

(* LA only: example that motivated the existence of the special min-grade-1 path... which no longer exists, but I'll keep this around anyway *)
test2args[sum, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{7, 4, 3}}, "contra"}];
test2args[diff, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{-3, -4, 3}}, "contra"}];

(* LA only: non-min-grade-1 *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
et19MwithIndependent7 = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "co"};
test2args[sum, septimalMeantoneM, flattoneM, godzillaM];
test2args[diff, septimalMeantoneM, flattoneM, et19MwithIndependent7];

(* LA only: ensure the minors are consulted so that the sum and diff are identified correctly *)
t1 = {{{0, 1, 4}}, "co"};
t2 = {{{5, -6, -2}}, "co"};
tSum = {{{5, -5, 2}}, "co"};
tDiff = {{{5, -7, -6}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works *)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "contra"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "contra"};
test2args[sum, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "contra"}];
test2args[diff, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "contra"}];

(* LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)
t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "co"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "co"};
test2args[sum, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "co"}];
test2args[diff, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "co"}];

(* LA only: example where the first vectors of the input were not actually linearly independent from the basis for the linearly dependent vectors, things would fail, so now we actually test each one to ensure it's linearly independent before adding it into the initial matrix to be defactored *)
test2args[sum, {{{-17, -55, 24, 34}}, "contra"}, {{{-1, -7, 0, 2}}, "contra"}, {{{-9, -31, 12, 18}}, "contra"}];

(* LA only: an example that used to fail for whatever reason, the "languisher" *)
test2args[sum, {{{23, -14, 3, 0}, {9, -5, 1, 1}}, "contra"}, {{{1, 7, 3, -1}, {0, 25, 14, -1}}, "co"}, {{{23, -14, 14, 0}, {9, -5, 5, 1}}, "contra"}];

(* LA only: an example that used to fail for whatever reason, the "big random" *)
test2args[sum, {{{-89, -46, 61, 0, 0}, {-85, -44, 59, 1, 0}, {-39, -21, 26, 0, 1}}, "contra"}, {{{-16, -9, 1, 0, 0}, {10, 4, 0, 1, 0}, {16, 8, 0, 0, 1}}, "contra"}, Error];



(* ___ PRIVATE ___ *)


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

(* getV *)
test[getV, {{{1, 0, -4}, {0, 1, 4}}, "co"}, "co"];

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


(* CANONICALIZATION *)

(* hnf *)
test[hnf, {{5, 8, 12}, {7, 11, 16}}, {{1, 0, -4}, {0, 1, 4}}];
test[hnf, {{3, 0, -1}, {0, 3, 5}}, {{3, 0, -1}, {0, 3, 5}}];

(* hermiteRightUnimodular *)
test[hermiteRightUnimodular, {{6, 5, -4}, {4, -4, 1}}, {{1, 2, 1}, {-1, 0, 2}, {0, 3, 4}}];

(* colHermiteDefactor *)
test[colHermiteDefactor, {{6, 5, -4}, {4, -4, 1}}, {{6, 5, -4}, {-4, -4, 3}}];

(* canonicalM *)
test[canonicalM, {{1, 1, 0}, {0, 1, 4}}, {{1, 0, -4}, {0, 1, 4}}];

(* canonicalC *)
test[canonicalC, {{-4, 4, -1}}, {{4, -4, 1}}];


(* DUAL *)

(* noncanonicalNullSpaceBasis *)
test[noncanonicalNullSpaceBasis, {{19, 30, 44}}, {{-30, 19, 0}, {-44, 0, 19}}];

(* noncanonicalAntiNullSpaceBasis *)
test[noncanonicalAntiNullSpaceBasis, {{-30, 19, 0}, {-44, 0, 19}}, {{19, 30, 44}}];

(* nullSpaceBasis *)
test[nullSpaceBasis, {{1, 0, -4}, {0, 1, 4}}, {{4, -4, 1}}];
test[nullSpaceBasis, {{0, 9, 4}}, {{1, 0, 0}, {0, -4, 9}}];
test[nullSpaceBasis, {{0}}, IdentityMatrix[1]];
test[nullSpaceBasis, {{0, 0}}, IdentityMatrix[2]];
test[nullSpaceBasis, {{0, 0, 0}}, IdentityMatrix[3]];
test[nullSpaceBasis, IdentityMatrix[1], {{0}}];
test[nullSpaceBasis, IdentityMatrix[2], {{0, 0}}];
test[nullSpaceBasis, IdentityMatrix[3], {{0, 0, 0}}];
test[nullSpaceBasis, {{12, 19}}, {{-19, 12}}];

(* antiNullSpaceBasis *)
test[antiNullSpaceBasis, {{4, -4, 1}}, {{1, 0, -4}, {0, 1, 4}}];
test[antiNullSpaceBasis, {{1, 0, 0}, {0, -4, 9}}, {{0, 9, 4}}];
test[antiNullSpaceBasis, {{0}}, IdentityMatrix[1]];
test[antiNullSpaceBasis, {{0, 0}}, IdentityMatrix[2]];
test[antiNullSpaceBasis, {{0, 0, 0}}, IdentityMatrix[3]];
test[antiNullSpaceBasis, IdentityMatrix[1], {{0}}];
test[antiNullSpaceBasis, IdentityMatrix[2], {{0, 0}}];
test[antiNullSpaceBasis, IdentityMatrix[3], {{0, 0, 0}}];
test[antiNullSpaceBasis, {{-19, 12}}, {{12, 19}}];


(* MEET AND JOIN *)

(* getM *)
test[getM, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{1, 0, -4}, {0, 1, 4}}];
test[getM, {{{4, -4, 1}}, "contra"}, {{-1, 0, 4}, {1, 1, 0}}];

(* getC *)
test[getC, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{4, -4, 1}}];
test[getC, {{{4, -4, 1}}, "contra"}, {{4, -4, 1}}];




Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
