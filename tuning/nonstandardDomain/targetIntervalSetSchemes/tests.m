failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


(* filterTargetIntervalsForNonstandardDomainBasis *)
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getOld[5], {{}, "row", {4, 3, 5}}, {4 / 3, 5 / 4, 5 / 3}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getOld[9], {{}, "row", {2, 3, 7}}, {2 / 1, 4 / 3, 8 / 7, 16 / 9, 3 / 2, 12 / 7, 7 / 4, 7 / 6, 14 / 9, 9 / 8, 9 / 7}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getTilt[6], {{}, "row", {4, 3, 5}}, {3 / 1, 4 / 3, 5 / 3, 5 / 4}];
testTargetSetScheme[filterTargetIntervalsForNonstandardDomainBasis, getTilt[8], {{}, "row", {2, 3, 7}}, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 7 / 3, 7 / 4, 7 / 6, 8 / 3}];


(* processOld - make sure it picks the default max odd for it correctly based on the domain basis, 
when max odd is unspecified *)
twentyoneToTwentyoneOldT = {{}, "row", {2, 9, 21}};
test[processOld, "OLD", twentyoneToTwentyoneOldT, processOld["21-OLD", twentyoneToTwentyoneOldT]];

(* processTilt make sure it picks the default max integer for it correctly based on the domain basis, 
when max integer is unspecified *)
twentyoneToTwentytwoTiltT = {{}, "row", {2, 9, 21}};
test[processTilt, "TILT", twentyoneToTwentytwoTiltT, processTilt["22-TILT", twentyoneToTwentytwoTiltT]];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
