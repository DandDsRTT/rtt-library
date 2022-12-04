failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";

test[getMinimumStandardDomainBasis, {2, 5 / 3, 9 / 7}, {2, 3, 5, 7}];


barbados = "2.3.13/5 [⟨1 2 2] ⟨0 -2 -3]]";
testClose[
  optimizeGeneratorTuningMap,
  barbados,
  {
    "targetIntervals" -> "[[1 0 0⟩ [0 1 0⟩ [0 0 1⟩ [-1 1 0⟩ [-1 0 1⟩ [0 1 -1⟩]", (* note these are in 2.3.13/5, the non-prime-only basis *)
    "tuningSchemeNonstandardDomainBasisApproach" -> "non-prime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "⟨1198.919 248.212]"
];

testClose[
  optimizeGeneratorTuningMap,
  barbados,
  {
    "targetIntervals" -> "[[1 0 0 0⟩ [-2 0 1 0⟩ [-2 1 0 0⟩ [0 1 -1 0⟩ [-2 0 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -1 0 1⟩]", (* note these are in 2.3.5.13, the prime-only basis *)
    "tuningSchemeNonstandardDomainBasisApproach" -> "prime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "⟨1200.370 248.863]"
];


(* arbitrary example I picked for article  *)
articleExample = "2.7/3.11/3 [⟨1 1 2] ⟨0 2 -1]]";
testClose[
  optimizeGeneratorTuningMap,
  articleExample,
  {
    "targetIntervals" -> "[[1 0 0⟩ [0 1 0⟩ [-1 1 0⟩ [-1 0 1⟩ [0 -1 1⟩ [2 -1 0⟩]",
    "tuningSchemeNonstandardDomainBasisApproach" -> "non-prime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "⟨1192.399 133.768]"
];
testClose[
  optimizeGeneratorTuningMap,
  articleExample,
  {
    "targetIntervals" -> "[[1 0 0 0⟩ [0 1 0 0⟩ [-1 1 0 0⟩ [2 -1 0 0⟩ [0 -1 1 0⟩ [-2 0 1 0⟩ [-1 -1 1 0⟩ [3 -1 0 0⟩ [-2 2 0 0⟩ [0 2 -1 0⟩ [-2 0 0 1⟩ [-1 -1 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -2 0 1⟩ [2 1 -1 0⟩ ]",
    "tuningSchemeNonstandardDomainBasisApproach" -> "prime-based",
    "optimizationPower" -> \[Infinity],
    "damageWeightSlope" -> "complexityWeight"
  },
  "⟨1193.102 135.810]"
];


(* trying to figure out this stuff about coprime and when it matters whether you pick prime-based or non-prime-based: non-all-interval edition *)

(* 2.9.7.11 -  non-prime but co-prime, so no difference *)
machine = "2.9.7.11 [⟨1 3 3 4] ⟨0 1 -1 -3]}";
matchingTuning = "⟨1197.268 207.170]";
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "{2/1, 9/4, 11/7} non-prime-based minimax-C"}, matchingTuning ];
testClose[optimizeGeneratorTuningMap, machine, {"tuningSchemeSystematicName" -> "{2/1, 9/4, 11/7} prime-based minimax-C"}, matchingTuning];


(* 2.3.13/5 - non-prime but co-prime, so no difference *)
barbados = "2.3.13/5 [⟨1 2 2] ⟨0 -2 -3]}";
matchingTuning = "⟨1197.437 247.741]";
testClose[optimizeGeneratorTuningMap, barbados, {"tuningSchemeSystematicName" -> "{3/2, 13/10, 15/13} non-prime-based minimax-C"}, matchingTuning];
testClose[optimizeGeneratorTuningMap, barbados, {"tuningSchemeSystematicName" -> "{3/2, 13/10, 15/13} prime-based minimax-C"}, matchingTuning];


(* 2.5/3.7/3 - not even co-prime, now there could be a difference, but there's not yet... *)
starlingtet = "2.5/3.7/3 [⟨1 1 2] ⟨0 -1 -3]}";
matchingTuning = "⟨1213.795 315.641]";
testClose[optimizeGeneratorTuningMap, starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} non-prime-based minimax-C"}, matchingTuning];
testClose[optimizeGeneratorTuningMap, starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} prime-based minimax-C"}, matchingTuning];


optimizeGeneratorTuningMap[starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} non-prime-based miniRMS-copfr-C", "logging" -> True}];
optimizeGeneratorTuningMap[starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} non-prime-based miniRMS-C", "logging" -> True}];
optimizeGeneratorTuningMap[starlingtet, {"tuningSchemeSystematicName" -> "{7/5, 7/6, 6/5} non-prime-based miniRMS-sopfr-C", "logging" -> True}];

(*optimizeGeneratorTuningMap[starlingtet,{"tuningSchemeSystematicName"->"{2/1, 5/3, 6/5, 7/3, 7/5, 7/6, 10/7} non-prime-based miniRMS-U", "logging" -> True}]*)
optimizeGeneratorTuningMap[starlingtet, {"tuningSchemeSystematicName" -> "{2/1, 5/3, 6/5, 7/3, 7/5, 7/6, 10/7} prime-based mini-2.001-mean-U", "logging" -> True}]



printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
