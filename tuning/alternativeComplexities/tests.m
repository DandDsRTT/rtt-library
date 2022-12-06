failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-copfr-C", "⟨1200.813 696.570]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-C", "⟨1201.489 696.662]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-C", "⟨1201.507 696.668]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-copfr-C", "⟨1200.522 696.591]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-lopfr-C", "⟨1201.535 696.760]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-sopfr-C", "⟨1201.503 696.732]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-copfr-limit-C", "⟨1201.168 696.797]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-limit-C", "⟨1202.087 696.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-limit-C", "⟨1201.830 696.851]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-copfr-limit-C", "⟨1201.024 696.834]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-lopfr-limit-C", "⟨1202.009 696.981]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-E-sopfr-limit-C", "⟨1201.898 696.913]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-copfr-C", "⟨1200.000 696.182]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-lopfr-C", "⟨1200.000 695.972]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-sopfr-C", "⟨1200.000 695.974]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-copfr-C", "⟨1200.000 696.350]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-lopfr-C", "⟨1200.000 696.089]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-sopfr-C", "⟨1200.000 696.078]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-copfr-limit-C", "⟨1200.000 696.209]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-lopfr-limit-C", "⟨1200.000 696.075]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-sopfr-limit-C", "⟨1200.000 696.093]"];

testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-copfr-limit-C", "⟨1200.000 696.354]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-lopfr-limit-C", "⟨1200.000 696.144]"];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-odd-E-sopfr-limit-C", "⟨1200.000 696.126]"];


(* lp = lopfr = [blank] *)
result = "⟨1201.489 696.662]";
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lopfr-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lp-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-C", result];

(* lil = unchanged-octave lol *)
result = "⟨1200.000 696.075]";
testClose[optimizeGeneratorTuningMap, meantone, "unchanged-octave TILT miniRMS-lil-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-lol-C", result];

(* prod = sopfr *)
result = "⟨1201.507 696.668]";
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-prod-C", result];
testClose[optimizeGeneratorTuningMap, meantone, "TILT miniRMS-sopfr-C", result];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
