(* ADDITION *)

eaSum[u1_, u2_] := eaAddition[u1, u2, True];

eaDiff[u1_, u2_] := eaAddition[u1, u2, False];




(* ___ PRIVATE ___ *)



eaAddition[u1input_, u2input_, isSum_] := Module[{u1, u2},
  u1 = eaCanonicalForm[u1input];
  u2 = If[eaGetVariance[u2input] != eaGetVariance[u1], eaDual[u2input], eaCanonicalForm[u2input]];
  
  If[
    eaGetR[u1] != eaGetR[u2] || eaGetD[u1] != eaGetD[u2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetLargestMinorsL[u1] + eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}],
      eaCanonicalForm[{eaGetLargestMinorsL[u1] - eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}]
    ]
  ]
];
