failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


testClose[fn_, args___, inputExpectation_] := Module[
  {actual, expectation},
  
  actual = parseTemperamentData[Apply[fn, {args}]];
  expectation = parseTemperamentData[inputExpectation];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {getL[actual], getL[expectation]}], TrueQ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[formatOutput[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testDamages[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[
      MapThread[
        Function[
          {actualEntry, expectationEntry},
          ToString[formatNumber[N[actualEntry]]] == ToString[formatNumber[N[expectationEntry]]]
        ],
        {Keys[actual], Keys[expectation]}
      ],
      TrueQ
    ] && AllTrue[
      MapThread[
        Function[
          {actualEntry, expectationEntry},
          ToString[formatNumber[N[actualEntry]]] == ToString[formatNumber[N[expectationEntry]]]
        ],
        {Values[actual], Values[expectation]}
      ],
      TrueQ
    ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testDamageMeanOrComplexity[fn_, args___, inputExpectation_] := Module[
  {actual, expectation},
  
  actual = parseTemperamentData[Apply[fn, {args}]];
  expectation = parseTemperamentData[inputExpectation];
  
  If[
    Abs[ToExpression[ToString[actual - expectation]]] < 10^-accuracy,
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testTargetSetScheme[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    TrueQ[Sort[actual] == Sort[expectation]],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != `` (order agnostic); actual result was:", fn, {args}, expectation], 14, Red]];
    printWrapper[actual];
  ]
];


(* some temperaments to check against *)

meantone = "[⟨1 1 0] ⟨0 1 4]⟩";
blackwood = "[⟨5 8 0] ⟨0 0 1]⟩";
dicot = "[⟨1 1 2] ⟨0 2 1]⟩";
augmented = "[⟨3 0 7] ⟨0 1 0]⟩";
mavila = "[⟨1 0 7] ⟨0 1 -3]⟩";
porcupine = "[⟨1 2 3] ⟨0 3 5]⟩";
srutal = "[⟨2 0 11] ⟨0 1 -2]⟩";
hanson = "[⟨1 0 1] ⟨0 6 5]⟩";
magic = "[⟨1 0 2] ⟨0 5 1]⟩";
negri = "[⟨1 2 2] ⟨0 -4 3]⟩";
tetracot = "[⟨1 1 1] ⟨0 4 9]⟩";
meantone7 = "[⟨1 0 -4 -13] ⟨0 1 4 10]⟩";
magic7 = "[⟨1 0 2 -1] ⟨0 5 1 12]⟩";
pajara = "[⟨2 3 5 6] ⟨0 1 -2 -2]⟩";
augene = "[⟨3 0 7 18] ⟨0 1 0 -2]⟩";
sensi = "[⟨1 -1 -1 -2] ⟨0 7 9 13]⟩";
sensamagic = "[⟨1 0 0 0] ⟨0 1 1 2] ⟨0 0 2 -1]⟩";



(* OPTIMIZATION *)

(* optimizeGeneratorsTuningMap, using explicit targeted intervals *)

sixTilt = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨1200.000, 696.578]"];


(* optimizeGeneratorsTuningMap, by individual tuning scheme properties *)

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1202.390 697.176]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1202.728 697.260]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1201.699 697.564]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1201.600 697.531]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1197.610 694.786]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1197.435 694.976]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted"}, "⟨1197.979 694.711]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1198.423 695.209]"];


testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted"}, "⟨1202.081 697.099]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1202.609 697.329]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1202.729 697.210]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1201.617 697.379]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1201.718 697.214]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1200.813 696.570]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1200.522 696.591]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted"}, "⟨1201.489 696.662]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1201.535 696.760]"];


testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"}, "⟨1204.301 697.654]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted"}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1204.301 697.654]"];


(* optimizeGeneratorsTuningMap, fully by "tuningSchemeSystematicName" *)

sevenOld = "{2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5, 7/4, 8/7, 7/6, 12/7, 7/5, 10/7, 9/8, 16/9, 9/5, 10/9, 9/7, 14/9}";

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-U", "⟨600.000 108.128]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-copfr-S", "⟨596.502 106.058]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-E-copfr-S", "⟨598.233 106.938]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-S", "⟨598.447 107.711]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-E-S", "⟨599.682 108.375]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-copfr-C", "⟨601.515 108.014]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-E-copfr-C", "⟨601.826 108.325]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-C", "⟨601.553 108.015]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimax-E-C", "⟨600.318 108.188]"];


testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-U", "⟨599.534 107.165]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-copfr-S", "⟨599.162 106.904]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-E-copfr-S", "⟨599.043 106.949]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-S", "⟨599.699 106.900]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-E-S", "⟨599.593 106.982]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-copfr-C", "⟨601.631 107.284]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-E-copfr-C", "⟨601.482 107.170]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-C", "⟨600.651 107.425]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " miniRMS-E-C", "⟨600.260 107.258]"];


testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-U", "⟨600.000 106.843]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-copfr-S", "⟨600.000 106.843]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-E-copfr-S", "⟨600.000 106.843]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-S", "⟨600.000 105.214]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-E-S", "⟨600.000 105.214]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-copfr-C", "⟨601.397 106.145]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-E-copfr-C", "⟨601.397 106.145]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-C", "⟨600.000 106.843]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOld <> " minimean-E-C", "⟨600.000 106.843]"];


(* optimizeGeneratorsTuningMap, by "damageSystematicName" plus traits 1 and 2 (targeted intervals, and optimization power) *)

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "U-damage"}, "⟨600.000 1905.214]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-S-damage"}, "⟨599.425 1903.105]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-S-damage"}, "⟨599.362 1902.875]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "S-damage"}, "⟨599.555 1903.365]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-S-damage"}, "⟨599.577 1903.449]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-C-damage"}, "⟨600.752 1907.971]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-C-damage"}, "⟨600.863 1908.379]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "C-damage"}, "⟨600.413 1906.917]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-C-damage"}, "⟨600.296 1906.485]"];


testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "U-damage"}, "⟨599.131 1902.390]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-S-damage"}, "⟨599.219 1902.515]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-S-damage"}, "⟨599.156 1902.381]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "S-damage"}, "⟨599.431 1903.058]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-S-damage"}, "⟨599.363 1902.960]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-C-damage"}, "⟨599.232 1902.839]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-C-damage"}, "⟨599.247 1902.882]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "C-damage"}, "⟨599.159 1902.609]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-C-damage"}, "⟨599.116 1902.444]"];


testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "U-damage"}, "⟨598.914 1901.955]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-S-damage"}, "⟨599.054 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-S-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "S-damage"}, "⟨599.111 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-S-damage"}, "⟨598.914 1901.955]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-C-damage"}, "⟨598.914 1901.955]"];


(* optimizeGeneratorsTuningMap, by "intervalComplexitySystematicName", plus traits 1, 2, and 3 (targeted intervals, optimization power, and damage weighting slope) *)

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨240.000 2795.336]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.612 2784.926]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨238.445 2783.722]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.867 2785.650]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.927 2786.118]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨241.504 2811.877]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨241.702 2812.251]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨241.209 2808.887]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨240.981 2805.237]"];


testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted"}, "⟨238.408 2781.006]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.316 2781.797]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨238.248 2781.458]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.779 2784.026]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.712 2783.815]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.916 2784.540]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨239.047 2784.702]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.642 2783.284]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.583 2782.365]"];


testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"}, "⟨237.744 2775.036]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨237.744 2775.036]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨237.744 2775.036]"];


(* handling ETs *)

testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimax-U", "⟨22.644]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT miniRMS-U", "⟨22.650]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimean-U", "⟨22.642]"];

testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimax-C", "⟨22.638]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT miniRMS-C", "⟨22.657]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimean-C", "⟨22.662]"];

testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimax-S", "⟨22.647]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT miniRMS-S", "⟨22.644]"];
testClose[optimizeGeneratorsTuningMap, "[⟨53 84 123]⟩", "TILT minimean-S", "⟨22.642]"];


(* optimization power continuum *)
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨240.000 2795.336]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 5.00, "damageWeightingSlope" -> "unweighted"}, "⟨239.174 2787.898]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 3.00, "damageWeightingSlope" -> "unweighted"}, "⟨238.745 2784.044]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 2.00, "damageWeightingSlope" -> "unweighted"}, "⟨238.408 2781.006]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1.50, "damageWeightingSlope" -> "unweighted"}, "⟨238.045 2777.737]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1.25, "damageWeightingSlope" -> "unweighted"}, "⟨237.793 2775.471]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> sixTilt, "optimizationPower" -> 1.00, "damageWeightingSlope" -> "unweighted"}, "⟨237.744 2775.036]"];


(* interval complexity norm power continuum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> \[Infinity]}, "⟨1201.191 697.405]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 5.00}, "⟨1201.381 697.460]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 3.00}, "⟨1201.513 697.503]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2.00}, "⟨1201.600 697.531]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 1.50}, "⟨1201.648 697.547]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 1.25}, "⟨1201.673 697.556]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 1.00}, "⟨1201.699 697.564]"];


(* unchanged interval *)
fiveOld = "{2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5}";
unchangedOctaveFiveOldMinimeanUResult = "⟨1200.000 696.578]";
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "octave"}, unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "2"}, unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "2/1"}, unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "{2}"}, unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "{2/1}"}, unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-octave " <> fiveOld <> " minimean-U", unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2 " <> fiveOld <> " minimean-U", unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2/1 " <> fiveOld <> " minimean-U", unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2} " <> fiveOld <> " minimean-U", unchangedOctaveFiveOldMinimeanUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1} " <> fiveOld <> " minimean-U", unchangedOctaveFiveOldMinimeanUResult];

testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " minimean-U", "unchangedIntervals" -> "{2/1, 3/2}"}, "⟨1200.000 701.955]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1, 3/2} " <> fiveOld <> " minimean-U", "⟨1200.000 701.955]"];

unchangedOctaveTiltMiniRmsUResult = "⟨1200.000 696.274]";
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-octave TILT miniRMS-U", unchangedOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2 TILT miniRMS-U", unchangedOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2/1 TILT miniRMS-U", unchangedOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2} TILT miniRMS-U", unchangedOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1} TILT miniRMS-U", unchangedOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-3/2 TILT miniRMS-U", "⟨1209.926 701.955]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-5/4 TILT miniRMS-U", "⟨1201.536 697.347]"];

(* should be able to skip the specification of a targeted intervals set if you specify the right number of unchanged intervals (u = r) *)
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1, 5/4} minimax-U", "⟨1200.000 696.578]"];


(* MEAN DAMAGE *)

(* getGeneratorsTuningMapMeanDamage *)
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1199.02 695.601]", "unchanged-octave " <> fiveOld <> " miniRMS-U", 3.893];
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.578]", "unchanged-octave " <> fiveOld <> " minimax-U", 5.377];
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT miniRMS-S", 1.625];
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT minimean-S", 1.185];
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT mini-3-mean-S", 1.901];
testDamageMeanOrComplexity[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT minimax-S", 3.382];

(* getTuningMapMeanDamage *)
testDamageMeanOrComplexity[getTuningMapMeanDamage, meantone, "⟨1200.000 1897.564 2786.314]", {"targetedIntervals" -> "{2,3,5}", "damageWeightingSlope" -> "unweighted", "optimizationPower" -> \[Infinity]}, 4.391];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " miniRMS-U", 10.461];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " minimean-U", 8.065];


(* DAMAGES *)

(* getGeneratorsTuningMapDamages *)
testDamages[getGeneratorsTuningMapDamages, meantone, "⟨1201.7 697.564]", "minimax-S", {2 -> 1.700, 3 -> 1.698, 5 -> 1.698}];
testDamages[getGeneratorsTuningMapDamages, meantone, "⟨1199.02 695.601]", "TILT miniRMS-U", {2 / 1 -> 0.980, 3 / 1 -> 7.334, 3 / 2 -> 6.354, 4 / 3 -> 5.374, 5 / 2 -> 2.930, 5 / 3 -> 3.424, 5 / 4 -> 1.950, 6 / 5 -> 4.404}];
testDamages[getGeneratorsTuningMapDamages, meantone, "⟨1200.0 696.578]", "TILT minimax-U", {2 / 1 -> 0.000, 3 / 1 -> 5.377, 3 / 2 -> 5.377, 4 / 3 -> 5.377, 5 / 2 -> 0.002, 5 / 3 -> 5.375, 5 / 4 -> 0.002, 6 / 5 -> 5.375}];

(* getTuningMapDamages *)
testDamages[getTuningMapDamages, meantone, "⟨1200.000 1897.564 2786.314]", {"targetedIntervals" -> "{2,3,5}", "damageWeightingSlope" -> "unweighted", "optimizationPower" -> \[Infinity]}, {2 -> 0.000, 3 -> 4.391, 5 -> 0.000}];
testDamages[getTuningMapDamages, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " miniRMS-U", {FractionBox["2", "1"] -> 0.000, FractionBox["3", "1"] -> 1.955, FractionBox["3", "2"] -> 1.955, FractionBox["4", "3"] -> 1.955, FractionBox["5", "2"] -> 13.686, FractionBox["5", "3"] -> 15.641, FractionBox["5", "4"] -> 13.686, FractionBox["6", "5"] -> 15.641}];


(* TARGETED INTERVAL SET SCHEMES *)

(* getTilt *)

testTargetSetScheme[getTilt, 4, {2 / 1, 3 / 1, 3 / 2, 4 / 3}]; (* 4/1 first interval excluded due to max size of 13/4 *)
testTargetSetScheme[getTilt, 6, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5}];
testTargetSetScheme[getTilt, 8, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5}]; (* 8/7 first interval excluded due to min size of 15/13 *)
testTargetSetScheme[getTilt, 10, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7}]; (* for 7-prime-limit temperaments, either 8 or 10 are reasonable choices *)
testTargetSetScheme[getTilt, 12, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7}];
testTargetSetScheme[getTilt, 14, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11}];
testTargetSetScheme[getTilt, 16, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13}];
testTargetSetScheme[getTilt, 18, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5, 7 / 3, 7 / 4, 7 / 5, 7 / 6, 8 / 3, 8 / 5, 9 / 4, 9 / 5, 9 / 7, 10 / 7, 11 / 4, 11 / 5, 11 / 6, 11 / 7, 11 / 8, 11 / 9, 12 / 5, 12 / 7, 13 / 4, 13 / 5, 13 / 6, 13 / 7, 13 / 8, 13 / 9, 13 / 10, 13 / 11, 14 / 5, 14 / 9, 14 / 11, 15 / 7, 15 / 8, 15 / 11, 15 / 13, 16 / 5, 16 / 7, 16 / 9, 16 / 11, 16 / 13, 17 / 6, 17 / 7, 17 / 8, 17 / 9, 17 / 10, 17 / 11, 17 / 12, 17 / 13, 18 / 7, 18 / 11, 18 / 13}]; (* 17/14 first interval excluded due to max complexity *)

(* the integer limit of the TILT defaults to the integer just less than the next prime, but this default may be overridden *)

tenTiltResult = "⟨600.000 108.128]";
eightTiltResult = "⟨596.443 105.214]";
testClose[optimizeGeneratorsTuningMap, pajara, "TILT minimax-U", tenTiltResult];
testClose[optimizeGeneratorsTuningMap, pajara, "10-TILT minimax-U", tenTiltResult];
testClose[optimizeGeneratorsTuningMap, pajara, "8-TILT minimax-U", eightTiltResult];
testClose[optimizeGeneratorsTuningMap, pajara, quotientLToString[getTilt[8]] <> " minimax-U", eightTiltResult];

(* full name works too *)

testClose[optimizeGeneratorsTuningMap, pajara, "truncated integer limit triangle minimax-U", tenTiltResult];




(* ___ PRIVATE ___ *)

(* tuningInverse *)
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}}, "row"}, {{{1 / Log2[2], 0, 0}, {0, 1 / Log2[3], 0}, {0, 0, 1 / Log2[5]}}, "row"}];
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}, {Log2[2], Log2[3], Log[5]}}, "row"}, {{{1 / Log2[2], 0, 0, 0}, {0, 1 / Log2[3], 0, 0}, {0, 0, 1 / Log2[5], 0}}, "row"}];

(* getcentsSummationMapAndLogPrimeMultiplier *)
test[getcentsSummationMapAndLogPrimeMultiplier, {{12, 19, 28}, "row", {2, 3, 5}}, {{1200 * Log2[2], 1200 * Log2[3], 1200 * Log2[5]}, "row"}];
test[getcentsSummationMapAndLogPrimeMultiplier, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "row", {2, 9, 5, 21}}, {{1200 * Log2[2], 1200 * Log2[9], 1200 * Log2[5], 1200 * Log2[21]}, "row"}];

(* getComplexity *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "row"};
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 0, 0, 0, 3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 2, 0, 0, 0, \[Sqrt]3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 1, 0, 0, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
