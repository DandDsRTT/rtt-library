failures = 0;
passes = 0;

format = "Wolfram";


(* changeDomainBasisPrivate *)

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


(* duals *)

verifyDuals[{{{1, 1, 3}, {0, 3, -1}}, "row", {2, 3, 7}}, {{{-10, 1, 3}}, "col", {2, 3, 7}}];


(* temperament merging - across domain basis examples *)

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




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
