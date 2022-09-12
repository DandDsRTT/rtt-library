failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


t = "2.7/5.11 [⟨1 1 5] ⟨0 -1 -3]⟩";
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "formalPrimes"}, "⟨1200.4181 617.7581]"];
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "primes"}, "⟨1200.0558 616.4318]"];

t = "2.9.5.21 [⟨1 0 -4 0] ⟨0 1 2 0] ⟨0 0 0 1]⟩";
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "formalPrimes"}, "⟨1201.3969 3796.8919 5270.7809]"];
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "primes"}, "⟨1201.3969 3796.8919 5267.2719]"];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
