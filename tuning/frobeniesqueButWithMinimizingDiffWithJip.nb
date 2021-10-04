Frobeniesque[mapping_, power_] := Module[{mappingRowCount, mappingColCount, generatorRowCount, generatorColCount, i, j, generators, result},
  mappingRowCount = Length[mapping];
  mappingColCount = Length[First[mapping]];
  generatorRowCount = mappingColCount;
  generatorColCount = mappingRowCount;

  (* generators = Table[Symbol["g" <> ToString@i],{i,1,Length[mapping]}];*)

  generators = Table[
    Table[
      Symbol["x" <> ToString@i <> ToString@j],
      {j, 1, generatorColCount}
    ],
    {i, 1, generatorRowCount}
  ];

  thing[g_] := Module[{},
    Print[g];
    octaves[g].mapping - just
  ];

  just = jip[mappingColCount];
  result = Minimize[
    Norm[thing[generators], power],
    Flatten[generators]
  ];
  generators /. Last[result]
]

m = {{1, 2, 4}, {0, -1, -4}};

g = PseudoInverse[m] // N
octavesG = octaves[g] // N
primeApproxs = octavesG.m // N
j = jip[Length[First[m]]] // N
primeApproxs - j // N

Frobeniesque[m, 1] // N
Frobeniesque[m, 2] // N
Frobeniesque[m, Infinity] // N








Frobeniesque[mapping_, power_] := Module[{mappingRowCount, mappingColCount, generatorRowCount, generatorColCount, i, j, generators, result},
  mappingRowCount = Length[mapping];
  mappingColCount = Length[First[mapping]];
  generatorRowCount = mappingColCount;
  generatorColCount = mappingRowCount;

  (* generators = Table[Symbol["g" <> ToString@i],{i,1,Length[mapping]}];*)

  (* generators = Table[
  Table[
  Symbol["x" <> ToString@i <> ToString@j],
  {j, 1,generatorColCount}
  ],
  {i,1,generatorRowCount}
  ];*)

  (* thing[g_] := Module[{},
  Print[g];
  octaves[g].mapping -  just
  ];*)

  just = jip[mappingColCount];
  result = Minimize[
    Norm[octaves[{{x11, x21}, {x12, x22}, {x13, x23}}].mapping - just, power],
    {x11, x12, x13, x21, x22, x23}
  ];
  {x11, x12, x13, x21, x22, x23} /. Last[result]
];

m = {{1, 2, 4}, {0, -1, -4}};

g = PseudoInverse[m] // N
octavesG = octaves[g] // N
primeApproxs = octavesG.m // N
j = jip[Length[First[m]]] // N
primeApproxs - j // N

Frobeniesque[m, 1] // N
Frobeniesque[m, 2] // N
Frobeniesque[m, Infinity] // N
