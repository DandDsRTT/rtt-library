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


(* OPTIMIZATION *)

(* optimizeGeneratorTuningMap, using explicit target-intervals *)

sixTilt = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "⟨1200.000, 696.578]"];


(* optimizeGeneratorTuningMap, by individual tuning scheme properties *)

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1202.390 697.176]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1202.728 697.260]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight"}, "⟨1201.699 697.564]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "⟨1201.600 697.531]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1197.610 694.786]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1197.435 694.976]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight"}, "⟨1197.979 694.711]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "⟨1198.423 695.209]"];


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "unityWeight"}, "⟨1202.081 697.099]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1202.609 697.329]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1202.729 697.210]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight"}, "⟨1201.617 697.379]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "⟨1201.718 697.214]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1200.813 696.570]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1200.522 696.591]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight"}, "⟨1201.489 696.662]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "⟨1201.535 696.760]"];


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "unityWeight"}, "⟨1204.301 697.654]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPreTransformerLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight"}, "⟨1204.301 697.654]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexityNormPower" -> 2}, "⟨1204.301 697.654]"];


(* optimizeGeneratorTuningMap, fully by "tuningSchemeSystematicName" *)

nineOld = "{2/1, 4/3, 8/5, 8/7, 16/9, 3/2, 6/5, 12/7, 5/4, 5/3, 10/7, 10/9, 7/4, 7/6, 7/5, 14/9, 9/8, 9/5, 9/7}";

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-U", "⟨600.000 108.128]"];


testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-copfr-S", "⟨596.502 106.058]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-E-copfr-S", "⟨598.233 106.938]"];

(*testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-S", "⟨598.447 107.711]"];*) 

(*testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-ES", "⟨599.682 108.375]"];*)


testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-copfr-C", "⟨601.515 108.014]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-E-copfr-C", "⟨601.826 108.325]"];

(*testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-C", "⟨601.553 108.015]"];*) 

(*testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " minimax-EC", "⟨600.318 108.188]"];*) 
(* TODO: exact 𝒃 per this commit makes these four commented out ones too slow; falls back to general method *)


testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-U", "⟨599.534 107.165]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-copfr-S", "⟨599.162 106.904]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-E-copfr-S", "⟨599.043 106.949]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-S", "⟨599.699 106.900]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-ES", "⟨599.593 106.982]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-copfr-C", "⟨601.631 107.284]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-E-copfr-C", "⟨601.482 107.170]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-C", "⟨600.651 107.425]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniRMS-EC", "⟨600.260 107.258]"];


testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-U", "⟨600.000 106.843]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-copfr-S", "⟨600.000 106.843]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-E-copfr-S", "⟨600.000 106.843]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-S", "⟨600.000 105.214]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-ES", "⟨600.000 105.214]"];

testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-copfr-C", "⟨601.397 106.145]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-E-copfr-C", "⟨601.397 106.145]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-C", "⟨600.000 106.843]"];
testClose[optimizeGeneratorTuningMap, pajara, nineOld <> " miniaverage-EC", "⟨600.000 106.843]"];


(* optimizeGeneratorTuningMap, by "damageSystematicName" plus traits 1 and 2 (target-intervals, and optimization power) *)

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "U-damage"}, "⟨600.000 1905.214]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-S-damage"}, "⟨599.425 1903.105]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-S-damage"}, "⟨599.362 1902.875]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "S-damage"}, "⟨599.555 1903.365]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "ES-damage"}, "⟨599.577 1903.449]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "copfr-C-damage"}, "⟨600.752 1907.971]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "E-copfr-C-damage"}, "⟨600.863 1908.379]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "C-damage"}, "⟨600.413 1906.917]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageSystematicName" -> "EC-damage"}, "⟨600.296 1906.485]"];


testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "U-damage"}, "⟨599.131 1902.390]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-S-damage"}, "⟨599.219 1902.515]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-S-damage"}, "⟨599.156 1902.381]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "S-damage"}, "⟨599.431 1903.058]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "ES-damage"}, "⟨599.363 1902.960]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "copfr-C-damage"}, "⟨599.232 1902.839]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "E-copfr-C-damage"}, "⟨599.247 1902.882]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "C-damage"}, "⟨599.159 1902.609]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageSystematicName" -> "EC-damage"}, "⟨599.116 1902.444]"];


testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "U-damage"}, "⟨598.914 1901.955]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-S-damage"}, "⟨599.054 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-S-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "S-damage"}, "⟨599.111 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "ES-damage"}, "⟨598.914 1901.955]"];

testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "copfr-C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "E-copfr-C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "C-damage"}, "⟨598.914 1901.955]"];
testClose[optimizeGeneratorTuningMap, srutal, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageSystematicName" -> "EC-damage"}, "⟨598.914 1901.955]"];


(* optimizeGeneratorTuningMap, by "intervalComplexitySystematicName", plus traits 1, 2, and 3 (target-intervals, optimization power, and damage weight slope) *)

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "⟨240.000 2795.336]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.612 2784.926]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨238.445 2783.722]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.867 2785.650]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.927 2786.118]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨241.504 2811.877]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨241.702 2812.251]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨241.209 2808.887]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨240.981 2805.237]"];


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "unityWeight"}, "⟨238.408 2781.006]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.316 2781.797]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨238.248 2781.458]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.779 2784.026]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.712 2783.815]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨238.916 2784.540]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨239.047 2784.702]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨238.642 2783.284]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨238.583 2782.365]"];


testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "unityWeight"}, "⟨237.744 2775.036]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "simplicityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨237.744 2775.036]"];

testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "copfr-E-complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "complexity"}, "⟨237.744 2775.036]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "intervalComplexitySystematicName" -> "E-complexity"}, "⟨237.744 2775.036]"];


(* handling ETs *)

testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT minimax-U", "⟨22.644]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniRMS-U", "⟨22.650]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniaverage-U", "⟨22.642]"];

testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT minimax-C", "⟨22.638]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniRMS-C", "⟨22.657]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniaverage-C", "⟨22.662]"];

testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT minimax-S", "⟨22.647]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniRMS-S", "⟨22.644]"];
testClose[optimizeGeneratorTuningMap, "[⟨53 84 123]}", "TILT miniaverage-S", "⟨22.642]"];


(* optimization power continuum *)
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "unityWeight"}, "⟨240.000 2795.336]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 5.00, "damageWeightSlope" -> "unityWeight"}, "⟨239.174 2787.898]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 3.00, "damageWeightSlope" -> "unityWeight"}, "⟨238.745 2784.044]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 2.00, "damageWeightSlope" -> "unityWeight"}, "⟨238.408 2781.006]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.50, "damageWeightSlope" -> "unityWeight"}, "⟨238.045 2777.737]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.25, "damageWeightSlope" -> "unityWeight"}, "⟨237.793 2775.471]"];
testClose[optimizeGeneratorTuningMap, blackwood, {"targetIntervals" -> sixTilt, "optimizationPower" -> 1.00, "damageWeightSlope" -> "unityWeight"}, "⟨237.744 2775.036]"];


(* interval complexity norm power continuum *)
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> \[Infinity]}, "⟨1201.191 697.405]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 5.00}, "⟨1201.381 697.460]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 3.00}, "⟨1201.513 697.503]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 2.00}, "⟨1201.600 697.531]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.50}, "⟨1201.648 697.547]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.25}, "⟨1201.673 697.556]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> sixTilt, "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "simplicityWeight", "intervalComplexityNormPower" -> 1.00}, "⟨1201.699 697.564]"];


(* held-intervals *)
fiveOld = "{2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5}";
heldOctaveFiveOldMiniaverageUResult = "⟨1200.000 696.578]";
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "octave"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "2"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "2/1"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2}"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2/1}"}, heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-octave " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2 " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2/1 " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2} " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1} " <> fiveOld <> " miniaverage-U", heldOctaveFiveOldMiniaverageUResult];

testClose[optimizeGeneratorTuningMap, meantone, {"tuningSchemeSystematicName" -> fiveOld <> " miniaverage-U", "heldIntervals" -> "{2/1, 3/2}"}, "⟨1200.000 701.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 3/2} " <> fiveOld <> " miniaverage-U", "⟨1200.000 701.955]"];

heldOctaveTiltMiniRmsUResult = "⟨1200.000 696.274]";
testClose[optimizeGeneratorTuningMap, meantone, "held-octave TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2 TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-2/1 TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2} TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1} TILT miniRMS-U", heldOctaveTiltMiniRmsUResult];
testClose[optimizeGeneratorTuningMap, meantone, "held-3/2 TILT miniRMS-U", "⟨1209.926 701.955]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-5/4 TILT miniRMS-U", "⟨1201.536 697.347]"];

(* TODO: possibly an earlier commit caused these to start failing for some reason *)
controlResult = "⟨1200.000 696.578]";
controlScheme = {"tuningSchemeSystematicName" -> fiveOld <> " minimax-U"};
testClose[optimizeGeneratorTuningMap, meantone, controlScheme, controlResult];
heldIntervalResult = "⟨1197.181 693.847]";
heldIntervalScheme = Join[controlScheme, {"heldIntervals" -> "5/3"}];
testClose[optimizeGeneratorTuningMap, meantone, heldIntervalScheme, heldIntervalResult];

(* should be able to skip the specification of a target-intervals set if you specify the right number of held-intervals (h = r) *)
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4} minimax-U", "⟨1200.000 696.578]"];

(* gracefully handles held-interval bases that are not actually bases (not linearly independent) *) (* TODO: these are failing with "no target-intervals" for some reason *)
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4, 4/1} minimax-U", "⟨1200.000 696.578]"];
testClose[optimizeGeneratorTuningMap, meantone, "held-{2/1, 5/4, 5/2} minimax-U", "⟨1200.000 696.578]"];


(* MEAN DAMAGE *)

(* getGeneratorTuningMapMeanDamage *)
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1199.02 695.601]", "held-octave " <> fiveOld <> " miniRMS-U", 3.893];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1200.00 696.578]", "held-octave " <> fiveOld <> " minimax-U", 5.377];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT miniRMS-S", 1.625];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT miniaverage-S", 1.185];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT mini-3-mean-S", 1.901];
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1200.00 696.594]", "TILT minimax-S", 3.382];

(* getTuningMapMeanDamage *)
testDamageMeanOrComplexity[getTuningMapMeanDamage, meantone, "⟨1200.000 1897.564 2786.314]", {"targetIntervals" -> "{2,3,5}", "damageWeightSlope" -> "unityWeight", "optimizationPower" -> \[Infinity]}, 4.391];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " miniRMS-U", 10.461];
testDamageMeanOrComplexity[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " miniaverage-U", 8.065];


(* DAMAGES *)

(* getGeneratorTuningMapDamages *)
testDamages[getGeneratorTuningMapDamages, meantone, "⟨1201.7 697.564]", "minimax-S", {2 -> 1.700, 3 -> 1.698, 5 -> 1.698}];
testDamages[getGeneratorTuningMapDamages, meantone, "⟨1199.02 695.601]", "TILT miniRMS-U", {2 / 1 -> 0.980, 3 / 1 -> 7.334, 3 / 2 -> 6.354, 4 / 3 -> 5.374, 5 / 2 -> 2.930, 5 / 3 -> 3.424, 5 / 4 -> 1.950, 6 / 5 -> 4.404}];
testDamages[getGeneratorTuningMapDamages, meantone, "⟨1200.0 696.578]", "TILT minimax-U", {2 / 1 -> 0.000, 3 / 1 -> 5.377, 3 / 2 -> 5.377, 4 / 3 -> 5.377, 5 / 2 -> 0.002, 5 / 3 -> 5.375, 5 / 4 -> 0.002, 6 / 5 -> 5.375}];

(* getTuningMapDamages *)
testDamages[getTuningMapDamages, meantone, "⟨1200.000 1897.564 2786.314]", {"targetIntervals" -> "{2,3,5}", "damageWeightSlope" -> "unityWeight", "optimizationPower" -> \[Infinity]}, {2 -> 0.000, 3 -> 4.391, 5 -> 0.000}];
testDamages[getTuningMapDamages, "⟨12 29 28]", "⟨1200 1900 2800]", sixTilt <> " miniRMS-U", {FractionBox["2", "1"] -> 0.000, FractionBox["3", "1"] -> 1.955, FractionBox["3", "2"] -> 1.955, FractionBox["4", "3"] -> 1.955, FractionBox["5", "2"] -> 13.686, FractionBox["5", "3"] -> 15.641, FractionBox["5", "4"] -> 13.686, FractionBox["6", "5"] -> 15.641}];


(* TARGET-INTERVAL SET SCHEMES *)

(* the integer limit of the TILT defaults to the integer just less than the next prime, but this default may be overridden *)

tenTiltResult = "⟨600.000 108.128]";
eightTiltResult = "⟨596.443 105.214]";
testClose[optimizeGeneratorTuningMap, pajara, "TILT minimax-U", tenTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, "10-TILT minimax-U", tenTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, "8-TILT minimax-U", eightTiltResult];
testClose[optimizeGeneratorTuningMap, pajara, quotientLToString[getTilt[8]] <> " minimax-U", eightTiltResult];

(* full name works too *)

testClose[optimizeGeneratorTuningMap, pajara, "truncated integer limit triangle minimax-U", tenTiltResult];




(* ___ PRIVATE ___ *)

(* tuningInverse *)
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}}, "row"}, {{{1 / Log2[2], 0, 0}, {0, 1 / Log2[3], 0}, {0, 0, 1 / Log2[5]}}, "row"}];
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}, {Log2[2], Log2[3], Log[5]}}, "row"}, {{{1 / Log2[2], 0, 0, 0}, {0, 1 / Log2[3], 0, 0}, {0, 0, 1 / Log2[5], 0}}, "row"}];

(* getJustTuningMap *)
test[getJustTuningMap, {{12, 19, 28}, "row", {2, 3, 5}}, {{1200 * Log2[2], 1200 * Log2[3], 1200 * Log2[5]}, "row"}];
test[getJustTuningMap, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "row", {2, 9, 5, 21}}, {{1200 * Log2[2], 1200 * Log2[9], 1200 * Log2[5], 1200 * Log2[21]}, "row"}];

(* getComplexity *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "row"};
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 0, 0, 0, "", 3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 2, 0, 0, 0, "", \[Sqrt]3];
test[getComplexity, {{1, 1, -1}, "col"}, dummy5limitTemp, 1, 1, 0, 0, "", 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
