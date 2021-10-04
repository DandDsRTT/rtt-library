top[mapping_] := Module[{mappingRowCount, mappingColCount, generatorRowCount, generatorColCount, i, j, k, commaDetunings, commaBasis, commaScale, signs, primeApproxs, generators, elements, result},
  mappingRowCount = Length[mapping];
  mappingColCount = Length[First[mapping]];
  generatorRowCount = mappingColCount;
  generatorColCount = mappingRowCount;

  commaBasis = nullSpaceBasis[mapping]; (* only works for nullity-1 temperaments *)
  (* commaScale = Abs[First[octaves[commaBasis]]]/Abs[First[absOctaves[commaBasis]]];*)
  signs = Map[Sign, commaBasis];

  commaDetunings = Table[Symbol["c" <> ToString@k], {k, 1, mappingColCount - mappingRowCount}]

      primeApproxs = MapThread[#2 * (1 + #1 * #3)&, {signs, jip[Length[commaBasis]], commaDetunings}];

  generators = Table[
    Table[
      Symbol["x" <> ToString@i <> ToString@j],
      {j, 1, generatorColCount}
    ],
    {i, 1, generatorRowCount}
  ];

  projectionMapping = octaves[generators.mapping];

  elements = Flatten[generators];
  result = Solve[
    {
      (* First[projectionMapping]== First[primeApproxs],
      Last[projectionMapping]== Last[primeApproxs],*) (* okay, so this one works sometimes but not other times *)
      projectionMapping == primeApproxs, (* then this one works other times butnot other times *)
      (* I just give up trying to figure out some way to set the precision such that this always works *)
      (* as you can see, it struggles with commas that are big *)
      mapping.generators == IdentityMatrix[Length[mapping]]
    },
    elements
  ];

  (* Print["commaBasis: ", commaBasis];
  Print["commaScale: ", commaScale," (", commaScale // N, ")"];
  Print["signs: ", signs];
  Print["primeApproxs: ", primeApproxs, " (", primeApproxs // N, ")"];
  Print["generators: ", generators];
  Print["result: ", result];
  Print["mapping.generators: ", mapping.generators];
  Print["generators.mapping: ", generators.mapping]; *)

  generators /. Last[result] // N
];





topTester[name_, m_] := Module[{tuning},
  tuning = top[m];
  Print[name, ": ", tuning, " (", cents[tuning], ")"];
];

topTester["meantone", {{1, 2, 4}, {0, -1, -4}}];
topTester["porcupine", {{1, 2, 3}, {0, -3, -5}}];
topTester["hanson", {{1, 0, 1}, {0, 6, 5}}];
topTester["magic", {{1, 0, 2}, {0, 5, 1}}];
topTester["mavila", {{1, 2, 1}, {0, -1, 3}}];
topTester["blackwood", {{5, 8, 12}, {0, 0, -1}}];
topTester["augmented", {{3, 5, 7}, {0, -1, 0}}];
topTester["dunno", {{1, 0, 0, 6}, {0, 1, 0, -2}, {0, 0, 1, 0}}];
