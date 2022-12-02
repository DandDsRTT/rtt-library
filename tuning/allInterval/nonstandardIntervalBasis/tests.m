failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


t = "2.7/5.11 [⟨1 1 5] ⟨0 -1 -3]}";
testClose[optimizeGeneratorTuningMap, t, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2, "tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based"}, "⟨1200.4181 617.7581]"];
testClose[optimizeGeneratorTuningMap, t, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2, "tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based"}, "⟨1200.0558 616.4318]"];

t = "2.9.5.21 [⟨1 0 -4 0] ⟨0 1 2 0] ⟨0 0 0 1]}";
testClose[optimizeGeneratorTuningMap, t, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2, "tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based"}, "⟨1201.3969 3796.8919 5270.7809]"];
testClose[optimizeGeneratorTuningMap, t, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2, "tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based"}, "⟨1201.3969 3796.8919 5267.2719]"];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
