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


(* TEMPERAMENT UTILITIES *)

(* getDPrivate *)
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

(* getRPrivate *)
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

(* getNPrivate *)
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


(* CANONICALIZATION *)

(* canonicalFormPrivate *)
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


(* DUAL *)

(* dual *)
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
verifyDuals[{{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, {{{-10, 1, 3}}, "col", {2, 3, 7}}];


(* MERGE *)

(* basic examples *)

et5M5 = {{{5, 8, 12}}, "row"};
et5C5 = {{{-8, 5, 0}, {-4, 1, 1}}, "col"};
et7M5 = {{{7, 11, 16}}, "row"};
et7C5 = {{{-11, 7, 0}, {-7, 3, 1}}, "col"};
meantoneM5 = {{{1, 0, -4}, {0, 1, 4}}, "row"};
meantoneC5 = {{{4, -4, 1}}, "col"};
porcupineM5 = {{{1, 2, 3}, {0, 3, 5}}, "row"};
porcupineC5 = {{{1, -5, 3}}, "col"};

test[dualPrivate, et5C5, et5M5];
test[dualPrivate, et7C5, et7M5];
test[dualPrivate, meantoneC5, meantoneM5];
test[dualPrivate, porcupineC5, porcupineM5];

test[mapMergePrivate, et5M5, et7M5, meantoneM5];
test[commaMergePrivate, meantoneC5, porcupineC5, et7C5];

(* prove out that you can specify temperaments by either their mappings or their comma bases *)

test[mapMergePrivate, et5M5, et7C5, meantoneM5];
test[commaMergePrivate, meantoneM5, porcupineC5, et7C5];
test[mapMergePrivate, et5C5, et7M5, meantoneM5];
test[commaMergePrivate, meantoneC5, porcupineM5, et7C5];
test[mapMergePrivate, et5C5, et7C5, meantoneM5];
test[commaMergePrivate, meantoneM5, porcupineM5, et7C5];

(* prove out that you can comma-merge or map-merge more than 2 temperaments at a time *)

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

(*⋎ = COMMA MERGE, ⋏ = MAP MERGE *)

(*Meantone⋎Meanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val.*)
test[commaMergePrivate, meantoneC11, meanpopC11, et31C11];

(*Meantone⋏Meanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>*)
test[mapMergePrivate, meantoneM11, meanpopM11, {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {0, 0, 0, 0, 1}}, "row"}];

(*Meantone⋎Marvel = 31*)
test[commaMergePrivate, meantoneC11, marvelC11, et31C11];

(*Meantone⋏Marvel = <225/224>*)
test[mapMergePrivate, meantoneM11, marvelM11, dualPrivate[{{marvelComma11}, "col"}]];

(*Meantone⋎Porcupine = G = <JI>*)
test[commaMergePrivate, meantoneC11, porcupineC11, {IdentityMatrix[5], "col"}];

(*Meantone⋏Porcupine = <176/175>*)
test[mapMergePrivate, meantoneM11, porcupineM11, dualPrivate[{{valinorsma11}, "col"}]];

(*In the 7-limit, that become Meantone⋎Porcupine = <JI>, Meantone⋏Porcupine = <1>*)
test[commaMergePrivate, meantoneC7, porcupineC7, {IdentityMatrix[4], "col"}];
test[mapMergePrivate, meantoneM7, porcupineM7, {IdentityMatrix[4], "row"}];

(*Miracle⋎Magic = 41 *)
test[commaMergePrivate, miracleC11, magicC11, et41C11];

(*Miracle⋏Magic = Marvel *)
test[mapMergePrivate, miracleM11, magicM11, marvelM11];

(*In the 7-limit, again Miracle⋎Magic = 41, Miracle⋏Magic = Marvel*)
test[commaMergePrivate, miracleC7, magicC7, et41C7];
test[mapMergePrivate, miracleM7, magicM7, marvelM7];

(*Miracle⋎Mothra = 31 *)
test[commaMergePrivate, miracleC11, mothraC11, et31C11];

(* Miracle⋏Mothra = Portent *)
test[mapMergePrivate, miracleM11, mothraM11, portentM11];

(*In the 7-limit, Miracle⋏Mothra = Gamelan.*)
test[mapMergePrivate, miracleM7, mothraM7, gamelanM7];

(*Meantone⋎Magic = <JI>,*)
test[commaMergePrivate, meantoneC11, magicC11, {IdentityMatrix[5], "col"}];

(*Meantone⋏Magic = <225/224>*)
test[mapMergePrivate, meantoneM11, magicM11, dualPrivate[{{marvelComma11}, "col"}]];


(* across interval basis examples *)

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


(* INTERVAL BASIS *)

test[changeIntervalBasisPrivate, {{{12, 19, 28}}, "row"}, {2, 3, 5, 7}, Error];

t = {{{22, 35, 51, 76}}, "row", {2, 3, 5, 11}};
targetSubspaceB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "row", {2, 9, 11}};
test[changeIntervalBasisPrivate, t, targetSubspaceB, expectedT];

test[changeIntervalBasisPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];

test[changeIntervalBasisPrivate, {{{4, -4, 1}}, "col"}, {2, 9, 7}, Error];

t = {{{0, 1, 0}, {0, -2, 1}}, "col", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "col"};
test[changeIntervalBasisPrivate, t, targetB, expectedT];

test[changeIntervalBasisPrivate, {{{1}}, "col", {27}}, {9}, Error];
test[changeIntervalBasisPrivate, {{{1}}, "col", {81}}, {9}, {{{1}}, "col", {9}}];
test[changeIntervalBasisPrivate, {{{4, -4, 1}}, "col"}, {2, 3, 5}, {{{4, -4, 1}}, "col"}];



(* ADDITION *)



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

(* across interval basis - error *)
test[sumPrivate, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, Error];


(* GENERATORS PREIMAGE TRANSVERSAL *)

(* getGeneratorsPreimageTransversal *)
format = "EBK";
test[getGeneratorsPreimageTransversal, "[⟨1 1 0] ⟨0 1 4]⟩", "⟨[1 0 0⟩ [-1 1 0⟩]"];
test[getGeneratorsPreimageTransversal, "[4 -4 1⟩", "⟨[1 0 0⟩ [0 1 0⟩]"];
format = "Wolfram";




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

(* parseIntervalBasis *)
test[parseIntervalBasis, "2.3.7", {2, 3, 7}];

(* vectorToEBK *)
test[vectorToEBK, {-4, 4, -1}, "[-4 4 -1⟩"];

(* covectorToEBK *)
test[covectorToEBK, {1, 0, -4}, "⟨1 0 -4]"];

(* toEBK *)
test[toEBK, mapInWolfram, "⟨1200.000 1901.955 2786.314]" ];
test[toEBK, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]⟩" ];
test[toEBK, commaInWolfram, "[1 -5 3⟩"];
test[toEBK, commaBasisInWolfram, "⟨[-4 4 -1⟩ [7 0 -3⟩]"];
test[toEBK, {{{4}, {5}}, "row"}, "[⟨4] ⟨5]⟩"];
test[toEBK, {{{4}, {5}}, "col"}, "⟨[4⟩ [5⟩]"];

(* formatOutput *)
format = "EBK";
test[formatOutput, mappingInWolfram, "[⟨1 0 -4] ⟨0 1 4]⟩"];
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


(* CANONICALIZATION *)

(* hnf *)
test[hnf, {{5, 8, 12}, {7, 11, 16}}, {{1, 0, -4}, {0, 1, 4}}];
test[hnf, {{3, 0, -1}, {0, 3, 5}}, {{3, 0, -1}, {0, 3, 5}}];

(* hermiteRightUnimodular *)
test[hermiteRightUnimodular, {{6, 5, -4}, {4, -4, 1}}, {{1, 2, 1}, {-1, 0, 2}, {0, 3, 4}}];

(* colHermiteDefactor *)
test[colHermiteDefactor, {{6, 5, -4}, {4, -4, 1}}, {{6, 5, -4}, {-4, -4, 3}}];

(* canonicalMa *)
test[canonicalMa, {{1, 1, 0}, {0, 1, 4}}, {{1, 0, -4}, {0, 1, 4}}];

(* canonicalCa *)
test[canonicalCa, {{-4, 4, -1}}, {{4, -4, 1}}];


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


(* MERGE *)

(* getM *)
test[getM, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];
test[getM, {{{4, -4, 1}}, "col"}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];

(* getC *)
test[getC, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {{{4, -4, 1}}, "col"}];
test[getC, {{{4, -4, 1}}, "col"}, {{{4, -4, 1}}, "col"}];


(* INTERVAL BASIS *)

(* intervalBasisMerge *)

(* returns the supergroup, when one is a subgroup of the other *)
test[intervalBasisMerge, {2, 3, 5}, {2, 9, 5}, {2, 3, 5}];

(* basically works *)
test[intervalBasisMerge, {2, 3, 5}, {2, 9, 7}, {2, 3, 5, 7}];

(* can handle more than two interval bases at once *)
test[intervalBasisMerge, {2, 3, 5}, {2, 9, 7}, {2, 5 / 7, 11}, {2, 3, 5, 7, 11}];
test[intervalBasisMerge, {4}, {16}, {4}];
test[intervalBasisMerge, {25 / 9}, {5 / 3}, {5 / 3}];

(* edge case *)
test[intervalBasisMerge, {1}, {1}, {1}];
test[intervalBasisMerge, {2, 3, 5}, {2, 3, 5}, {2, 3, 5}];


(* intervalBasisIntersection *)

test[intervalBasisIntersection, {2, 3, 5}, {2, 9, 5}, {2, 9, 5}];
test[intervalBasisIntersection, {2, 5 / 3, 9 / 7}, {2, 9, 5}, {2, 25 / 9}];
test[intervalBasisIntersection, {2, 5 / 3}, {2, 9, 5}, {2, 25 / 9}];
test[intervalBasisIntersection, {2, 25 / 9}, {2, 9, 5}, {2, 25 / 9}];
test[intervalBasisIntersection, {2, 3, 5, 7}, {2, 3, 5}, {2, 5, 7}, {2, 5}];
test[intervalBasisIntersection, {2, 3}, {10, 15}, {3 / 2}];
test[intervalBasisIntersection, {2, 5 / 3}, {2, 3, 5}, {2, 5 / 3}];
test[intervalBasisIntersection, {2, 9 / 5}, {2, 9, 5}, {2, 9 / 5}];
test[intervalBasisIntersection, {2, 3, 5}, {2, 3, 5}, {2, 3, 5}];
test[intervalBasisIntersection, {2, 9, 7 / 5}, {2, 3, 7 / 5}, {2, 9, 7 / 5}];
test[intervalBasisIntersection, {4}, {8}, {64}];
test[intervalBasisIntersection, {9}, {27}, {729}];
test[intervalBasisIntersection, {2}, {3}, {1}];
test[intervalBasisIntersection, {5}, {15}, {1}];
test[intervalBasisIntersection, {4}, {18}, {1}];
test[intervalBasisIntersection, {2}, {2}, {2}];
test[intervalBasisIntersection, {4}, {4}, {4}];
test[intervalBasisIntersection, {6}, {6}, {6}];
test[intervalBasisIntersection, {12}, {12}, {12}];
test[intervalBasisIntersection, {16, 18, 15}, {4, 18, 5}, {16, 18, 2500}];
test[intervalBasisIntersection, {4, 18}, {8, 18}, {64, 18}];
test[intervalBasisIntersection, {16, 18}, {16, 18}, {16, 18}];
test[intervalBasisIntersection, {4, 18, 5}, {8, 18, 7}, {64, 18}];


(* isSubspaceOf *)
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


(* canonicalIntervalBasis *)

(* order by prime-limit*)
test[canonicalIntervalBasis, {2, 7, 9}, {2, 9, 7}];
test[canonicalIntervalBasis, {2, 9 / 7, 5}, {2, 5, 9 / 7}];
test[canonicalIntervalBasis, {2, 9 / 7, 5 / 3}, {2, 5 / 3, 9 / 7}];

(* consolidate redundancies *)
test[canonicalIntervalBasis, {2, 3, 9}, {2, 3}];
test[canonicalIntervalBasis, {2, 3, 15}, {2, 3, 5}];
test[canonicalIntervalBasis, {2, 3, 5 / 3}, {2, 3, 5}];

(* tricky stuff *)
test[canonicalIntervalBasis, {2, 5 / 3, 7 / 5}, {2, 5 / 3, 7 / 3}];
test[canonicalIntervalBasis, {1, 1}, {1}];

(* all the subgroups on the wiki page if they are canonical according to this *)
test[canonicalIntervalBasis, {2, 3, 7}, {2, 3, 7}];
test[canonicalIntervalBasis, {2, 5, 7}, {2, 5, 7}];
test[canonicalIntervalBasis, {2, 3, 7 / 5}, {2, 3, 7 / 5}];
test[canonicalIntervalBasis, {2, 5 / 3, 7}, {2, 5 / 3, 7}];
test[canonicalIntervalBasis, {2, 5, 7 / 3}, {2, 5, 7 / 3}];
test[canonicalIntervalBasis, {2, 5 / 3, 7 / 3}, {2, 5 / 3, 7 / 3}];
test[canonicalIntervalBasis, {2, 27 / 25, 7 / 3}, {2, 27 / 25, 7 / 3}];
test[canonicalIntervalBasis, {2, 9 / 5, 9 / 7}, {2, 9 / 5, 9 / 7}];
test[canonicalIntervalBasis, {2, 3, 11}, {2, 3, 11}];
test[canonicalIntervalBasis, {2, 5, 11}, {2, 5, 11}];
test[canonicalIntervalBasis, {2, 7, 11}, {2, 7, 11}];
test[canonicalIntervalBasis, {2, 3, 5, 11}, {2, 3, 5, 11}];
test[canonicalIntervalBasis, {2, 3, 7, 11}, {2, 3, 7, 11}];
test[canonicalIntervalBasis, {2, 5, 7, 11}, {2, 5, 7, 11}];
test[canonicalIntervalBasis, {2, 5 / 3, 7 / 3, 11 / 3}, {2, 5 / 3, 7 / 3, 11 / 3}];
test[canonicalIntervalBasis, {2, 3, 13}, {2, 3, 13}];
test[canonicalIntervalBasis, {2, 3, 5, 13}, {2, 3, 5, 13}];
test[canonicalIntervalBasis, {2, 3, 7, 13}, {2, 3, 7, 13}];
test[canonicalIntervalBasis, {2, 5, 7, 13}, {2, 5, 7, 13}];
test[canonicalIntervalBasis, {2, 5, 7, 11, 13}, {2, 5, 7, 11, 13}];
test[canonicalIntervalBasis, {2, 3, 13 / 5}, {2, 3, 13 / 5}];
test[canonicalIntervalBasis, {2, 3, 11 / 5, 13 / 5}, {2, 3, 11 / 5, 13 / 5}];
test[canonicalIntervalBasis, {2, 3, 11 / 7, 13 / 7}, {2, 3, 11 / 7, 13 / 7}];
test[canonicalIntervalBasis, {2, 7 / 5, 11 / 5, 13 / 5}, {2, 7 / 5, 11 / 5, 13 / 5}];
test[canonicalIntervalBasis, {1}, {1}];
test[canonicalIntervalBasis, {0}, {1}];


(* changeIntervalBasisForM *)
test[changeIntervalBasisForM, {{{12, 19, 28}}, "row"}, {2, 3, 5, 7}, Error];
t = {{{22, 35, 51, 76}}, "row", {2, 3, 5, 11}};
targetSubspaceB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "row", {2, 9, 11}};
test[changeIntervalBasisForM, t, targetSubspaceB, expectedT];
test[changeIntervalBasisForM, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}, {{{1, 0, -4}, {0, 1, 4}}, "row"}];


(* changeIntervalBasisForC *)
test[changeIntervalBasisForC, {{{4, -4, 1}}, "col"}, {2, 9, 7}, Error];
t = {{{0, 1, 0}, {0, -2, 1}}, "col", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "col"}; (*{{{0,2,0,-1},{0,-5,1,2}},"col"}, before canonicalization *)
test[changeIntervalBasisForC, t, targetB, expectedT];
test[changeIntervalBasisForC, {{{1}}, "col", {27}}, {9}, Error];
test[changeIntervalBasisForC, {{{1}}, "col", {81}}, {9}, {{{1}}, "col", {9}}];
test[changeIntervalBasisForC, {{{4, -4, 1}}, "col"}, {2, 3, 5}, {{{4, -4, 1}}, "col"}];


(* getIntervalRebaseForM *)
test[getIntervalRebaseForM, {2, 3, 5, 7}, {2, 3, 5}, {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}];
test[getIntervalRebaseForM, {2, 3, 7}, {2, 9, 7}, {{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}];
test[getIntervalRebaseForM, {2, 3, 5, 7}, {2, 9 / 7, 5 / 3}, {{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}];


(* getIntervalRebaseForC *)
test[getIntervalRebaseForC, {2, 3, 5}, {2, 3, 5, 7}, {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}];
test[getIntervalRebaseForC, {2, 9, 7}, {2, 3, 7}, {{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}];
test[getIntervalRebaseForC, {2, 9 / 7, 5 / 3}, {2, 3, 5, 7}, {{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}];


(* getPrimes *)
test[getPrimes, 5, {2, 3, 5, 7, 11}];

(* quotientToPcv *)
test[quotientToPcv, 22 / 5, {1, 0, -1, 0, 1}];
test[quotientToPcv, 1, {0}];

(* pcvToQuotient *)
test[pcvToQuotient, {1, 0, -1, 0, 1}, 22 / 5];
test[pcvToQuotient, {0}, 1];

(* getIntervalBasisDimension *)
test[getIntervalBasisDimension, {2, 9, 7}, 4];
test[getIntervalBasisDimension, {1}, 1];

(* padVectorsWithZerosUpToD *)
test[padVectorsWithZerosUpToD, {{1, 2, 3}, {4, 5, 6}}, 5, {{1, 2, 3, 0, 0}, {4, 5, 6, 0, 0}}];

(* super *)
test[super, 5 / 3, 5 / 3];
test[super, 3 / 5, 5 / 3];

(* getStandardPrimeLimitIntervalBasis *)
test[getStandardPrimeLimitIntervalBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];

(* isStandardPrimeLimitIntervalBasis *)
test[isStandardPrimeLimitIntervalBasis, {2, 3, 5, 7, 11}, True];
test[isStandardPrimeLimitIntervalBasis, {2, 3, 7, 5, 11}, True];
test[isStandardPrimeLimitIntervalBasis, {2, 3, 5, 9, 11}, False];

(* getIntervalBasis *)
test[getIntervalBasis, {{{1, 0, -4}, {0, 1, 4}}, "row"}, {2, 3, 5}];
test[getIntervalBasis, {{{11, 35, 31}}, "row", {2, 9, 7}}, {2, 9, 7}];

(* signsMatch *)
test[signsMatch, 3, 5, True];
test[signsMatch, -3, -5, True];
test[signsMatch, -3, 5, False];
test[signsMatch, 3, -5, False];
test[signsMatch, 3, 0, True];
test[signsMatch, 0, 5, True];
test[signsMatch, -3, 0, True];
test[signsMatch, 0, - 5, True];

(* isNumeratorFactor *)
test[isNumeratorFactor, {1, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {2, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {2, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 2, 0}, False];
test[isNumeratorFactor, {1, 0, 0}, {0, 0, 1}, False];

(* isDenominatorFactor *)
test[isDenominatorFactor, {1, 0, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {0, 1, 0}, True];

(* getFormalPrimes *)
test[getFormalPrimes, {{{11, 35, 31}}, "row", {2, 9, 7}}, {{{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 0, 1}}, "col"}];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
