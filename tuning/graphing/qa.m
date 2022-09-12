(* HUMAN QA: GRAPHING *)

qa[fn_] := Do[Print[fn], 1];

(* 2D *)
qa[graphTuningDamage["⟨12 19 28]", "TILT miniRMS-U"]];

(* 3D *)
qa[graphTuningDamage[meantone, "TILT minimax-U"]];
