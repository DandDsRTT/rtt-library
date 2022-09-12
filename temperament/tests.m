failures = 0;
passes = 0;

format = "Wolfram";


(* DIMENSIONS *)

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


(* GENERATORS PREIMAGE TRANSVERSAL *)

(* getGeneratorsPreimageTransversal *)
format = "EBK";
test[getGeneratorsPreimageTransversal, "[⟨1 1 0] ⟨0 1 4]⟩", "⟨[1 0 0⟩ [-1 1 0⟩]"];
test[getGeneratorsPreimageTransversal, "[4 -4 1⟩", "⟨[1 0 0⟩ [0 1 0⟩]"];
format = "Wolfram";




(* ___ PRIVATE ___ *)



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

(* order by prime limit*)
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

(* getIntervalBasisDimension *)
test[getIntervalBasisDimension, {2, 9, 7}, 4];
test[getIntervalBasisDimension, {1}, 1];

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





printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
