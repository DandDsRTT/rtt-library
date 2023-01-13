failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


(* destretched-octave minimax-ES = "POTE", "Pure Octave Tenney-Euclidean" *)
(* could double-check with Xen wiki *)
testClose[optimizeTuningMap, meantone, "destretched-octave minimax-ES", "⟨1200.000 1896.239 2784.955]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "destretched-octave minimax-ES", "⟨1200.000 1920.000 2799.594]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "destretched-octave minimax-ES", "⟨1200.000 1897.189 2748.594]"]; (* [3p] *)
testClose[optimizeTuningMap, augmented, "destretched-octave minimax-ES", "⟨1200.000 1906.638 2800.000]"]; (* [3q] *)
testClose[optimizeTuningMap, mavila, "destretched-octave minimax-ES", "⟨1200.000 1879.806 2760.582]"]; (* [3r] *)
testClose[optimizeTuningMap, porcupine, "destretched-octave minimax-ES", "⟨1200.000 1908.149 2780.248]"]; (* [3s] *)
testClose[optimizeTuningMap, srutal, "destretched-octave minimax-ES", "⟨1200.000 1904.898 2790.204]"]; (* [3t] *)
testClose[optimizeTuningMap, hanson, "destretched-octave minimax-ES", "⟨1200.000 1902.039 2785.033]"]; (* [3u] *)
testClose[optimizeTuningMap, magic, "destretched-octave minimax-ES", "⟨1200.000 1900.292 2780.058]"]; (* [3v] *)
testClose[optimizeTuningMap, negri, "destretched-octave minimax-ES", "⟨1200.000 1896.980 2777.265]"]; (* [3w] *)
testClose[optimizeTuningMap, tetracot, "destretched-octave minimax-ES", "⟨1200.000 1904.639 2785.438]"]; (* [3x] *)
testClose[optimizeTuningMap, meantone7, "destretched-octave minimax-ES", "⟨1200.000 1896.495 2785.980 3364.949]"]; (* [3y] *)
testClose[optimizeTuningMap, magic7, "destretched-octave minimax-ES", "⟨1200.000 1901.760 2780.352 3364.224]"]; (* [3z] *)
testClose[optimizeTuningMap, pajara, "destretched-octave minimax-ES", "⟨1200.000 1907.048 2785.905 3385.905]"]; (* [3aa] *)
testClose[optimizeTuningMap, augene, "destretched-octave minimax-ES", "⟨1200.000 1909.257 2800.000 3381.486]"]; (* [3ab] *)
testClose[optimizeTuningMap, sensi, "destretched-octave minimax-ES", "⟨1200.000 1903.679 2790.444 3363.975]"]; (* [3ac] *)
testClose[optimizeTuningMap, sensamagic, "destretched-octave minimax-ES", "⟨1200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3ad] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "POTE", optimizeGeneratorTuningMap[meantone, "destretched-octave minimax-ES"]];

(* destretched-octave minimax-S = "POTOP", "POTT", "Pure Octave Tenney OPtimal", "Pure Octave Tiebreaker-in-polytope Tenney-optimal" *)
(* could double-check against Flora's app, but her TOP results are incorrect for now, so these would be too *)
testClose[optimizeGeneratorTuningMap, "[⟨2 2 7 8 14 5] ⟨0 1 -2 -2 -6 2]}", "destretched-octave minimax-S", "⟨600.000 709.184]"]; (* [7j] has {600.000, 706.843} but that has 7.254 damage and mine has 5.988 *)
testClose[optimizeGeneratorTuningMap, "[⟨1 -1 0 1] ⟨0 10 9 7]}", "destretched-octave minimax-S", "⟨1200.000 310.196]"]; (* [7i] *)
accuracy = 1;
testClose[optimizeTuningMap, "[⟨1 3 0 0 3] ⟨0 -3 5 6 1]}", "destretched-octave minimax-S", "⟨1200.00 1915.81 2806.98 3368.38 4161.40]"]; (* [1b] has <1200 1915.578 2807.355 3368.826 4161.472|,but  Mike himself says that maybe he got this one wrong because it should have been TIP... and yeah, I can see that this one has a pair of locked primes! *)
testClose[optimizeGeneratorTuningMap, "[⟨1 2 6 2 10] ⟨0 -1 -9 2 -16]}", "destretched-octave minimax-S", "⟨1200.0 490.4]"]; (* [1d] *)
testClose[optimizeGeneratorTuningMap, "[⟨1 2 6 2 1] ⟨0 -1 -9 2 6]}", "destretched-octave minimax-S", "⟨1200.0 490.9]"]; (* [1d] *)
testClose[optimizeGeneratorTuningMap, "[⟨1 2 -3 2 1] ⟨0 -1 13 2 6]}", "destretched-octave minimax-S", "⟨1200.0 491.9]"]; (* [1d] *)
accuracy = 3;
testClose[optimizeGeneratorTuningMap, "[⟨1 1 2 1] ⟨0 1 0 2] ⟨0 0 1 2]}", "destretched-octave minimax-S", "⟨1200.0 700.3907806 384.0221726]"]; (* [1e] this was passing with {1200.000, 700.795, 380.759} before introducing the non-unique check code and then went back to passing after maybe switching to Keenan's nested minimax technique...  it really does seem like it should have a unique solution, so the condition on that might be wrong... you should really plot this one visually and see what's happening *)
accuracy = 2;
testClose[optimizeGeneratorTuningMap, "[⟨1 1 0] ⟨0 1 4]}", "destretched-octave minimax-S", "⟨1200.0 696.58]"]; (* [1f] *)
testClose[optimizeGeneratorTuningMap, "[⟨1 1 0 -3] ⟨0 1 4 10]}", "destretched-octave minimax-S", "⟨1200.0 696.58]"]; (* [1f] *)
accuracy = 3;
(* original name *)
testClose[optimizeTuningMap, meantone, "POTOP", optimizeTuningMap[meantone, "destretched-octave minimax-S"]];
testClose[optimizeTuningMap, meantone, "POTT", optimizeTuningMap[meantone, "destretched-octave minimax-S"]];


(* yes, technically it works with non-all-interval tuning schemes, but we do not recommend it, when you could hold intervals instead *)

testClose[optimizeGeneratorTuningMap, meantone, sixTilt <> " miniaverage-U", "⟨1204.301 697.654]"];
destretchedOctaveSixTiltMiniaverageUResult = "⟨1204.301 * 1200.000 / 1204.301, 697.654 * 1200.000 / 1204.301]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "octave"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "2"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "2/1"}, destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-octave " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-2 " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-2/1 " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-{2} " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-{2/1} " <> sixTilt <> " miniaverage-U", destretchedOctaveSixTiltMiniaverageUResult];

destretchedFifthSixTiltMiniaverageUResult = "⟨1204.301 * 701.955 / 697.654, 697.654 * 701.955 / 697.654]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniaverage-U", "destretchedInterval" -> "3/2"}, destretchedFifthSixTiltMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "destretched-3/2 " <> sixTilt <> " miniaverage-U", destretchedFifthSixTiltMiniaverageUResult];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
