failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";

scheme = {
  "targetIntervals" -> {},
  "optimizationPower" -> \[Infinity],
  "damageWeightSlope" -> "simplicityWeight",
  "intervalComplexityNormPower" -> 2
};

t = "2.7/5.11 [⟨1 1 5] ⟨0 -1 -3]}";
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"tuningSchemeNonstandardDomainBasisApproach" -> "non-prime-based"}],
  "⟨1200.4181 617.7581]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_7%2F5_11&ets=2_33 *)
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"tuningSchemeNonstandardDomainBasisApproach" -> "prime-based"}],
  "⟨1200.0558 616.4318]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_7%2F5_11&ets=2_33&subgroup=on *)

t = "2.9.5.21 [⟨1 0 -4 0] ⟨0 1 2 0] ⟨0 0 0 1]}";
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"tuningSchemeNonstandardDomainBasisApproach" -> "non-prime-based"}],
  "⟨1201.3969 3796.8919 5270.7809]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?ets=13_12_6&limit=2_9_5_21 *)
testClose[
  optimizeGeneratorTuningMap,
  t,
  Join[scheme, {"tuningSchemeNonstandardDomainBasisApproach" -> "prime-based"}],
  "⟨1201.3969 3796.8919 5267.2719]"
];
(* http://x31eq.com/cgi-bin/rt.cgi?limit=2_9_5_21&ets=13_12_6&subgroup=on *)



(* trying to figure out this stuff about coprime and when it matters whether you pick prime-based or non-prime-based: all-interval edition *)
machine = "2.9.7.11 [⟨1 3 3 4] ⟨0 1 -1 -3]]";

(* 2.9.7.11 -  non-prime but co-prime, so no difference, as per Graham's online app, since this is minimax-ES, the tuning his thing uses *)
matchingTuning = "⟨1197.281 213.899]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-ES"(*,"logging" -> True*)}, matchingTuning];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "non-prime-based minimax-ES"(*,"logging" -> True*)}, matchingTuning];

(* 2.9.7.11 -  non-prime but co-prime, so no difference, though not supported by Graham's app, since this is minimax-S, which his app does not use*)
matchingTuning = "⟨1197.344 215.749]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-S"(*,"logging" -> True*)}, matchingTuning];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "non-prime-based minimax-S"(*,"logging" -> True*)}, matchingTuning];

(* 2.9.7.11 - non-prime but co-prime, however, difference achieved, on account of using E-copfr-complexity *)
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-E-copfr-S"(*,"logging" -> True*)}, "⟨1195.547 211.194]"];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "non-prime-based minimax-E-copfr-S"(*,"logging" -> True*)}, "⟨1196.398 212.537]"];

(* 2.9.7.11 - non-prime but co-prime, however, difference achieved, on account of using E-sopfr-complexity *)
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "prime-based minimax-E-sopfr-S"(*,"logging" -> True*)}, "⟨1197.440 214.315]"];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "non-prime-based minimax-E-sopfr-S"(*,"logging" -> True*)}, "⟨1197.766 215.083]"];


printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
