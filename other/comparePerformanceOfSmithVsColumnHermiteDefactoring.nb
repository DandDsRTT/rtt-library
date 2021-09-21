ms = {};
Do[
  d = RandomInteger[{2, 6}];
  r = RandomInteger[{1, d}];
  m = RandomInteger[{-99, 99}, {r, d}];
  ms = Join[ms, {m}],
  1000
];

AbsoluteTiming[Do[smithDefactor[m], {m, ms}]]
