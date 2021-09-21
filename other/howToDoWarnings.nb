warningTester[n_] := Module[{m}, m = Mod[n, 3]; If[m == 0, Message[warningTest::warning, n]]; m];
warningTest::warning = "The argument `1` is a multiple of 3.";

(* examples *)

warningTester[5];
warningTester[6];
