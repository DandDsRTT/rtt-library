failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


(* unchanged-octave OLD minimax-U = "minimax" *)
testClose[optimizeTuningMap, meantone, "unchanged-octave OLD minimax-U", "⟨1200.000 1896.578 2786.314]"]; (* [7a] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
testClose[optimizeGeneratorTuningMap, porcupine, "unchanged-octave OLD minimax-U", "⟨1200.000 -162.737]"]; (* [7c] *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorTuningMap, magic, "unchanged-octave OLD minimax-U", "⟨1200.000 380.391]"]; (* [7d] *)
(* negri *)
testClose[optimizeGeneratorTuningMap, tetracot, "unchanged-octave OLD minimax-U", "⟨1200.000 176.257]"]; (* [7e] *)
testClose[optimizeGeneratorTuningMap, meantone7, "unchanged-octave OLD minimax-U", "⟨1200.000, 1200.000 + 696.578]"]; (* [7f] *)
testClose[optimizeGeneratorTuningMap, magic7, "unchanged-octave OLD minimax-U", "⟨1200.00 380.391]"]; (* [7d] *)
(* pajara *)
accuracy = 1;
testClose[optimizeGeneratorTuningMap, augene, "unchanged-octave OLD minimax-U", "⟨400.000, 3 * 400.000 + 708.798]"]; (* [7b] *)
accuracy = 3;
testClose[optimizeGeneratorTuningMap, sensi, "unchanged-octave OLD minimax-U", "⟨1200.000 443.519]"]; (* [7g] *)
testClose[optimizeTuningMap, sensamagic, "unchanged-octave OLD minimax-U", "⟨1200.000 1901.955 2781.584 3364.096]"]; (* [7h] *)
(* original name *)
testClose[optimizeTuningMap, meantone, "minimax", "⟨1200.000 1896.578 2786.314]"];

(* unchanged-octave OLD miniRMS-U = "least squares" *)
testClose[optimizeGeneratorTuningMap, meantone, "unchanged-octave OLD miniRMS-U", "⟨1200.000 696.165]"]; (* [7f] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorTuningMap, magic, "unchanged-octave OLD miniRMS-U", "⟨1200.000 379.968]"]; (* [7d]] *)
(* negri *)
(* tetracot *)
testClose[optimizeGeneratorTuningMap, meantone7, "unchanged-octave OLD miniRMS-U", "⟨1200.000, 1200.000 + 696.436]"]; (* [7f] *)
testClose[optimizeGeneratorTuningMap, magic7, "unchanged-octave OLD miniRMS-U", "⟨1200.000, 380.384]"]; (* [7d]] *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)
testClose[optimizeGeneratorTuningMap, "[⟨1 0 15] ⟨0 1 -8]}", "unchanged-octave OLD miniRMS-U", "⟨1200.000, 1200.000 + 701.728]"]; (* [2b] has a bunch of least squares tunings... only this one works, though; not sure what's up with the rest. this is the temperament that tempers out 32805/32768, btw. *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "least squares", "⟨1200.000 696.165]"];


(* support strings, not strings, or whatever comes out of the user functions; and whether via the systematic tuning scheme name or the individual tuning property *)

sixTiltString = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";
sixTiltQuotients = {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5};
sixTiltResult = "⟨1200.000, 696.578]";

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTiltString, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTiltQuotients, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> getTilt[6], "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, sixTiltResult];

testClose[optimizeGeneratorTuningMap, meantone, sixTiltString <> " minimax-U", sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, quotientLToString[sixTiltQuotients] <> " minimax-U", sixTiltResult];
testClose[optimizeGeneratorTuningMap, meantone, quotientLToString[getTilt[6]] <> " minimax-U", sixTiltResult];



(* OLD *)

(* getOld *)

testTargetSetScheme[getOld, 3, {2 / 1, 3 / 2, 4 / 3}];
testTargetSetScheme[getOld, 5, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5}];
testTargetSetScheme[getOld, 7, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5, 7 / 4, 8 / 7, 7 / 6, 12 / 7, 7 / 5, 10 / 7}];
testTargetSetScheme[getOld, 9, {2 / 1, 3 / 2, 4 / 3, 5 / 4, 8 / 5, 5 / 3, 6 / 5, 7 / 4, 8 / 7, 7 / 6, 12 / 7, 7 / 5, 10 / 7, 9 / 8, 16 / 9, 9 / 5, 10 / 9, 9 / 7, 14 / 9}];

(* the odd limit of the OLD defaults to the odd just less than the next prime, but this default may be overridden *)

nineOldResult = "⟨600.000 108.128]";
sevenOldResult = "⟨600.000 109.363]";
testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave OLD minimax-U", nineOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave 9-OLD minimax-U", nineOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave 7-OLD minimax-U", sevenOldResult];
testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave " <> quotientLToString[getOld[7]] <> " minimax-U", sevenOldResult];

(* full name works too *)

testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave odd limit diamond minimax-U", nineOldResult];


(* getOtonalChord *)

testTargetSetScheme[getOtonalChord, {4, 5}, {5 / 4}];
testTargetSetScheme[getOtonalChord, {4, 5, 6}, {5 / 4, 3 / 2, 6 / 5}];
testTargetSetScheme[getOtonalChord, {4, 5, 6, 7}, {5 / 4, 3 / 2, 7 / 4, 6 / 5, 7 / 5, 7 / 6}];
testTargetSetScheme[getOtonalChord, {8, 11, 13, 15}, {11 / 8, 13 / 8, 15 / 8, 13 / 11, 15 / 11, 15 / 13}];


(* octaveReduce *)
test[octaveReduce, 3, 3 / 2];
test[octaveReduce, 5, 5 / 4];
test[octaveReduce, 2 / 3, 4 / 3];


(* processOld - make sure it picks the default max odd for it correctly based on the domain basis, 
when max odd is unspecified *)
fiveToFiveOldT = {{{2, 3, 5}}, "row"};
test[processOld, "OLD", fiveToFiveOldT, processOld["5-OLD", fiveToFiveOldT]];
sevenToNineOldT = {{{2, 3, 5, 7}}, "row"};
test[processOld, "OLD", sevenToNineOldT, processOld["9-OLD", sevenToNineOldT]];



(* TILT *)

(* getTilt *)

testTargetSetScheme[getTilt, 4, {2 / 1, 3 / 1, 3 / 2, 4 / 3}]; (* 4/1 first interval excluded due to max size of 13/4 *)
testTargetSetScheme[getTilt, 6, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5}];
testTargetSetScheme[getTilt, 8, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5}]; (* 8/7 first interval excluded due to min size of 15/13 *)
testTargetSetScheme[getTilt, 10, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7}]; (* for 7-prime-limit temperaments, either 8 or 10 are reasonable choices *)
testTargetSetScheme[getTilt, 12, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7}];
testTargetSetScheme[getTilt, 14, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11}];
testTargetSetScheme[getTilt, 16, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13}];
testTargetSetScheme[getTilt, 18, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13, 17 / 6, 17 / 7, 17 / 8, 17 / 9, 17 / 10, 17 / 11, 17 / 12, 17 / 13, 18 / 7, 18 / 11, 18 / 13}]; (* 17/14 first interval excluded due to max complexity *)


(* processTilt - make sure it picks the default max integer for it correctly based on the domain basis, 
when max integer is unspecified *)
fiveToSixTiltT = {{{2, 3, 5}}, "row"};
test[processTilt, "TILT", fiveToSixTiltT, processTilt["6-TILT", fiveToSixTiltT]];
sevenToTenTiltT = {{{2, 3, 5, 7}}, "row"};
test[processTilt, "TILT", sevenToTenTiltT, processTilt["10-TILT", sevenToTenTiltT]];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
