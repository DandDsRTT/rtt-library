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




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
