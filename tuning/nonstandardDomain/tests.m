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
  "⟨1199.065 248.140]"
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

(* TODO: should also add tests for when you just ask for the TILT, to make sure it can handle that *)




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
