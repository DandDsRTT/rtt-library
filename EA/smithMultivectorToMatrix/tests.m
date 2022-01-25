smithMultivectorToMatrix[{{1, 2, -2, -5}, 3, "co"}] == {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}
smithMultivectorToMatrix[{{1, 4, 4}, 2, "co"}] == {{{1, 0, -4}, {0, 1, 4}}, "co"}
smithMultivectorToMatrix[{{4, -4, 1}, 1, "contra"}] == {{{4, -4, 1}}, "contra"}
smithMultivectorToMatrix[{{1}, 3, "co"}] == {IdentityMatrix[3], "co"}
smithMultivectorToMatrix[{{1}, 3, "contra"}] == {IdentityMatrix[3], "contra"}
smithMultivectorToMatrix[{{1}, 0, "co", 2}] == {{{0, 0}}, "co"}
smithMultivectorToMatrix[{{1, 0, 1}, 2, "co"}] == {{{1, 0, -1}, {0, 1, 0}}, "co"}
smithMultivectorToMatrix[{{2, 2, 7, 7}, 1, "co"}] == {{{2, 2, 7, 7}}, "co"}
smithMultivectorToMatrix[{{1}, 1, "contra"}] == {{{1}}, "contra"}

Do[
  tAndU = randomTandW[];
  t = First[tAndU];
  u = Last[tAndU];
  
  tensorFlattenedT = multivectorToMatrix[u];
  smithT = smithMultivectorToMatrix[u];
  
  If[tensorFlattenedA != smithA, Print["BAD BAD BAD! u: ", u, " tensor-flattened t: ", tensorFlattenedT, " Smith's t: ", smithT], "good"],
  100
];
Print["done"];
