gens[m_] := cents[weightedG[m]];


fixM[m_, i_] := Module[{localM, rp, rg, mGens, p, g, fixedRpRg, fixedM},
  localM = m;
  rp = m[[i]];
  rg = m[[i + 1]];

  mGens = gens[m];
  p = mGens[[i]];
  g = mGens[[i + 1]];

  fixedRpRg = Which[
    g < -p, {rp - 2 * rg, rg},
    -p <= g < -p / 2, {rp - rg, rg},
    -p / 2 <= g < 0, {rp, -rg},
    0 <= g <= p / 2, {rp, rg},
    p / 2 < g <= p, {rp + rg, -rg},
    p < g, {rp + 2 * rg, -rg}
  ];

  fixedM = Take[localM, i - 1] ~ Join ~ fixedRpRg ~ Join ~ Drop[localM, i + 1];

  If[
    g < -p || p < g,
    fixM[fixedM, i],
    fixedM
  ]
];

mingen[m_] := Module[{localM, rp, rg, fixedM},
  localM = m;

  For[i = 1, i < Length[localM], i++,
    localM = fixM[localM, i];
  ];

  localM
];


(* examples *)

mingenTester[m_] := Module[{ming},
  ming = mingen[m];
  "in: " <> ToString@m <> " w/ gens: " <> ToString@gens[m] <> "\nout: " <> ToString@ming <> " w/ gens: " <> ToString@gens[ming]
];

mingenTester[{{1, 0, -4}, {0, -1, -4}}]
mingenTester[{{1, 1, 0}, {0, -1, -4}}]
mingenTester[{{1, 2, 4}, {0, 1, 4}}]
mingenTester[{{1, 2, 4}, {0, -1, -4}}]
mingenTester[{{1, 1, 0}, {0, 1, 4}}]
mingenTester[{{1, 0, -4}, {0, 1, 4}}]


mingenTester[{{12, 19, 28}}]
mingenTester[{{1, 2, 3}, {0, 3, 5}}]
mingenTester[{{1, 0, -4, -13}, {0, 1, 4, 10}}]
mingenTester[{{5, 8, 0}, {0, 0, 1}}]
mingenTester[{{2, 0, 11, 12}, {0, 1, -2, -2}}]
mingenTester[{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}]
mingenTester[{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}]
mingenTester[{{1, 8, 0}, {0, 11, -4}}]


dimensionality = RandomInteger[{3, 6}];
rank = RandomInteger[{2, dimensionality}];
randomM = RandomInteger[{-10, 10}, {rank, dimensionality}]
mingenTester[randomM]
