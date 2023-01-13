failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


(* how big can we go before crashing? *)


(* TILT minimax-U *)

optimizeGeneratorTuningMap["[⟨53 84 123]}", "TILT minimax-U"]; (* 5-limit, 6-TILT *)

optimizeGeneratorTuningMap["[⟨1 1 3 3] ⟨0 6 -7 -2]}", "TILT minimax-U"]; (* 7-limit, 10-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 -5 12] ⟨0 1 0 2 -1] ⟨0 0 1 2 -3]}", "TILT minimax-U"]; (* 11-limit, 12-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 0 4 -1] ⟨0 2 0 0 -3 3] ⟨0 0 1 0 2 1] ⟨0 0 0 1 -1 0]}", "TILT minimax-U"]; (* 13-limit, 16-TILT *)

(*optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]}", "TILT minimax-U"]; (* 17-limit, 18-TILT *)*)
optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]}", {"tuningSchemeSystematicName" -> "TILT minimax-U", "quick" -> True}]; (* runs with "quick" though *)


(* TILT miniRMS-U *)

optimizeGeneratorTuningMap["[⟨53 84 123]}", "TILT miniRMS-U"]; (* 5-limit, 6-TILT *)

optimizeGeneratorTuningMap["[⟨1 1 3 3] ⟨0 6 -7 -2]}", "TILT miniRMS-U"]; (* 7-limit, 10-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 -5 12] ⟨0 1 0 2 -1] ⟨0 0 1 2 -3]}", "TILT miniRMS-U"]; (* 11-limit, 12-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 0 4 -1] ⟨0 2 0 0 -3 3] ⟨0 0 1 0 2 1] ⟨0 0 0 1 -1 0]}", "TILT miniRMS-U"]; (* 13-limit, 16-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]}", "TILT miniRMS-U"]; (* 17-limit, 18-TILT *)

(* ... *)
(* optimizeGeneratorTuningMap["[⟨1 0 0 0 0 0 -1 0 0 0 0 0] ⟨0 1 0 0 0 0 -1 0 0 0 0 0] ⟨0 0 1 0 0 0 1 0 0 0 0 0] ⟨0 0 0 1 0 0 -1 0 0 0 0 0] ⟨0 0 0 0 1 0 1 0 0 0 0 0] ⟨0 0 0 0 0 1 1 0 0 0 0 0] ⟨0 0 0 0 0 0 0 1 0 0 0 0] ⟨0 0 0 0 0 0 0 0 1 0 0 0] ⟨0 0 0 0 0 0 0 0 0 1 0 0] ⟨0 0 0 0 0 0 0 0 0 0 1 0] ⟨0 0 0 0 0 0 0 0 0 0 0 1]}", "TILT miniRMS-S"]; *) (* 37-limit, 40-TILT; also makes it to the power limit solver, but fails to converge there and times out, which makes me think that we should nicely immediately user-facing abort this temperament straight away whether minimax or miniRMS, since it's not tractable; would just need to determine what exactly that limit of tractability is *)


(* TILT miniaverage-U *)

optimizeGeneratorTuningMap["[⟨53 84 123]}", "TILT miniaverage-U"]; (* 5-limit, 6-TILT *)

optimizeGeneratorTuningMap["[⟨1 1 3 3] ⟨0 6 -7 -2]}", "TILT miniaverage-U"]; (* 7-limit, 10-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 -5 12] ⟨0 1 0 2 -1] ⟨0 0 1 2 -3]}", "TILT miniaverage-U"]; (* 11-limit, 12-TILT *)

optimizeGeneratorTuningMap["[⟨1 0 0 0 4 -1] ⟨0 2 0 0 -3 3] ⟨0 0 1 0 2 1] ⟨0 0 0 1 -1 0]}", "TILT miniaverage-U"]; (* 13-limit, 16-TILT *)

(*optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]}", "TILT miniaverage-U"]; (* 17-limit, 18-TILT *)*)
optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]}", {"tuningSchemeSystematicName" -> "TILT miniaverage-U", "quick" -> True}]; (* runs with "quick" though *)




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
