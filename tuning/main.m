optimizeGeneratorsTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorsTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorsTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    tPossiblyWithChangedIntervalBasis,
    targetedIntervals,
    complexitySizeFactor,
    tuningSchemeIntervalBasis,
    pureStretchedIntervalV,
    logging,
    
    tuningMethodArgs,
    powerArg,
    unchangedIntervalsArg,
    
    solution,
    
    optimumGeneratorsTuningMap
  },
  
  forDamage = False;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tPossiblyWithChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  tuningSchemeIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "tuningSchemeIntervalBasis"]; (* trait 8 *)
  pureStretchedIntervalV = tuningSchemeProperty[tuningSchemeProperties, "pureStretchedIntervalV"]; (* trait 9 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null",
    getInfiniteTargetSetTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  unchangedIntervalsArg = tuningMethodArg[tuningMethodArgs, "unchangedIntervalsArg"];
  
  solution = If[
    ToString[unchangedIntervalsArg] != "Null",
    
    (* covers minimax-lol-ES "KE", unchanged-octave minimax-ES "CTE" *)
    If[logging == True, printWrapper["power solver"]];
    powerSumSolution[tuningMethodArgs],
    
    If[
      powerArg == 2,
      
      (* covers odd-diamond minisos-U "least squares", 
      minimax-ES "TE", minimax-copfr-ES "Frobenius", pure-stretched-octave minimax-ES "POTE", 
      minimax-lil-ES "WE", minimax-sopfr-ES "BE" *)
      If[logging == True, printWrapper["pseudoinverse"]];
      pseudoinverseSolution[tuningMethodArgs],
      
      If[
        powerArg == \[Infinity],
        
        (* covers odd-diamond minimax-U "minimax", 
        minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
        minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
        If[logging == True, printWrapper["max polytope"]];
        maxPolytopeSolution[tuningMethodArgs],
        
        If[
          powerArg == 1,
          
          (* no historically described tuning schemes use this *)
          If[logging == True, printWrapper["sum polytope"]];
          sumPolytopeSolution[tuningMethodArgs],
          
          (* no historically described tuning schemes go here *)
          If[logging == True, printWrapper["power solver"]];
          powerSumSolution[tuningMethodArgs]
        ]
      ]
    ]
  ];
  
  (* this only happens if the sum polytope method fails to find a unique solution *)
  If[
    solution == Null,
    If[logging == True, printWrapper["power limit solver"]];
    solution = powerSumLimitSolution[tuningMethodArgs]
  ];
  
  optimumGeneratorsTuningMap = solution;
  
  (* for e.g. minimax-lil "Weil" "WE" and minimax-lol "Kees" "KE" tunings, remove the junk final entry from the augmentation *)
  If[
    ToString[targetedIntervals] == "Null" && complexitySizeFactor != 0,
    optimumGeneratorsTuningMap = {{Drop[First[getA[optimumGeneratorsTuningMap]], -1]}, "map"}
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningSchemeIntervalBasis == "primes",
    optimumGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    ToString[pureStretchedIntervalV] != "Null",
    optimumGeneratorsTuningMap = getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap, t, pureStretchedIntervalV]
  ];
  
  SetAccuracy[N[optimumGeneratorsTuningMap], outputPrecision]
];


optimizeTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeTuningMapPrivate[t_, tuningSchemeSpec_] := multiply[{optimizeGeneratorsTuningMapPrivate[t, tuningSchemeSpec], t}, "map"];

getGeneratorsTuningMapMeanDamage[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapMeanDamagePrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiply[{generatorsTuningMap, getM[t]}, "map"];
  
  getTuningMapMeanDamagePrivate[t, tuningMap, tuningSchemeSpec]
];

getTuningMapMeanDamage[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapMeanDamagePrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetedIntervals,
    tuningMethodArgs
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null",
    getInfiniteTargetSetTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  SetAccuracy[N[getPowerMeanAbsError[tuningMethodArgs]], outputPrecision]
];

getGeneratorsTuningMapDamages[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapDamagesPrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiply[{generatorsTuningMap, getM[t]}, "map"];
  
  getTuningMapDamagesPrivate[t, tuningMap, tuningSchemeSpec]
];

getTuningMapDamages[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapDamagesPrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetedIntervals,
    tuningMethodArgs,
    damages
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null",
    getInfiniteTargetSetTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  damages = First[getA[SetAccuracy[N[getAbsErrors[tuningMethodArgs]], outputPrecision]]];
  targetedIntervals = Map[pcvToQuotient, getA[targetedIntervals]];
  
  MapThread[#1 -> #2&, {targetedIntervals, damages}]
];

graphTuningDamage[unparsedT_, tuningSchemeSpec_] := Module[
  {
    t,
    
    forDamage,
    
    tuningSchemeOptions,
    optimumGeneratorsTuningMap,
    
    tuningSchemeProperties,
    
    optimizationPower,
    damageWeightingSlope,
    complexityNormPower,
    complexityNegateLogPrimeCoordinator,
    complexityPrimePower,
    complexitySizeFactor,
    complexityMakeOdd,
    
    tWithPossiblyChangedIntervalBasis,
    targetedIntervals,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    
    normPower,
    plotArgs,
    targetedIntervalGraphs,
    r,
    plotStyle,
    image
  },
  
  t = parseTemperamentData[unparsedT];
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapPrivate[t, tuningSchemeOptions];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tWithPossiblyChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 4d *)
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      Abs[multiply[{generatorsTuningMap, m, targetedIntervalPcv}, "map"] - multiply[{primeCentsMap, targetedIntervalPcv}, "map"]] / getComplexity[
        targetedIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordinator, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ],
    targetedIntervals
  ];
  
  normPower = If[
    optimizationPower == \[Infinity] && damageWeightingSlope == "simplicityWeighted" && ToString[targetedIntervals] == "Null",
    getDualPower[complexityNormPower],
    optimizationPower
  ];
  AppendTo[plotArgs, {targetedIntervalGraphs, Norm[targetedIntervalGraphs, normPower]}];
  
  image = Image[
    Map[
      Map[
        If[
          # == 1,
          {0, 0, 0, 1},
          {0, 0, 0, 0}
        ]&,
        #
      ]&,
      Array[(-1)^+ ## &, {32, 32}]
    ],
    ColorSpace -> "RGB"
  ];
  image = ImageResize[image, 256, Resampling -> "Constant"];
  
  plotStyle = Join[Table[Auto, Length[targetedIntervalGraphs]], {If[r == 1, {Black, Dashed}, {Texture[image]}]}];
  
  If[debug == True, printWrapper[plotStyle]];
  
  (* range *)
  MapIndexed[AppendTo[plotArgs, {Part[generatorsTuningMap, First[#2]], #1 - 2, #1 + 2}]&, optimumGeneratorsTuningMap];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  AppendTo[plotArgs, PlotStyle -> plotStyle];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
  (* plot type *)
  r = getRPrivate[tWithPossiblyChangedIntervalBasis];
  If[
    r == 1,
    Apply[Plot, plotArgs],
    If[
      r == 2,
      Apply[Plot3D, plotArgs],
      Throw["4D and higher visualizations not supported"]
    ]
  ]
];

generatorsTuningMapFromTAndTuningMap[unparsedT_, unparsedTuningMap_] := formatOutput[generatorsTuningMapFromTAndTuningMapPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap]]];
generatorsTuningMapFromTAndTuningMapPrivate[t_, tuningMap_] := Module[
  {generatorsTuningMap, m, primeCentsMap, solution},
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  solution = NMinimize[Norm[First[getA[multiply[{generatorsTuningMap, m}, "map"]]] - First[getA[tuningMap]]], generatorsTuningMap];
  
  {{generatorsTuningMap /. Last[solution]}, "map"}
];




(* ___ PRIVATE ___ *)



(* TUNING SCHEME OPTIONS *)

linearSolvePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

processTuningSchemeSpec[tuningSchemeSpec_] := If[
  StringQ[tuningSchemeSpec],
  If[
    StringMatchQ[tuningSchemeSpec, RegularExpression["(?:.* )?mini(?:max|sos|sum)-(?:\\w+-)?E?[UCS]"]],
    {"systematicTuningSchemeName" -> tuningSchemeSpec},
    {"originalTuningSchemeName" -> tuningSchemeSpec}
  ],
  tuningSchemeSpec
];

tuningSchemeOptions = {
  "targetedIntervals" -> Null, (* trait 0a *)
  "unchangedIntervals" -> Null, (* trait 0b *)
  "optimizationPower" -> Null, (* trait 1: \[Infinity] = minimax, 2 = minisos, 1 = minisum *)
  "damageWeightingSlope" -> "", (* trait 2: unweighted, complexityWeighted, or simplicityWeighted *)
  "complexityNormPower" -> 1, (* trait 3: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "complexityNegateLogPrimeCoordinator" -> False, (* trait 4a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "complexityMakeOdd" -> False, (* trait 4d: False = do nothing, True = achieve odd limit from integer limit, etc. *)
  "tuningSchemeIntervalBasis" -> "primes", (* trait 8: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-ES ("TE") tuning, but it can be used for any tuning *)
  "pureStretchedInterval" -> Null, (* trait 9 *)
  "systematicTuningSchemeName" -> "",
  "originalTuningSchemeName" -> "",
  "systematicDamageName" -> "",
  "originalDamageName" -> "",
  "systematicComplexityName" -> "",
  "originalComplexityName" -> "",
  "logging" -> False
};
Options[processTuningSchemeOptions] = tuningSchemeOptions;
processTuningSchemeOptions[t_, forDamage_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0a *)
    unchangedIntervals, (* trait 0b *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningSchemeIntervalBasis, (* trait 8 *)
    pureStretchedInterval, (* trait 9 *)
    systematicTuningSchemeName,
    originalTuningSchemeName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    logging,
    tPossiblyWithChangedIntervalBasis,
    pureStretchedIntervalV,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0a *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 0b *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordinator = OptionValue["complexityNegateLogPrimeCoordinator"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningSchemeIntervalBasis = OptionValue["tuningSchemeIntervalBasis"]; (* trait 8 *)
  pureStretchedInterval = OptionValue["pureStretchedInterval"]; (* trait 9 *)
  systematicTuningSchemeName = OptionValue["systematicTuningSchemeName"];
  originalTuningSchemeName = OptionValue["originalTuningSchemeName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  logging = OptionValue["logging"];
  
  If[
    originalTuningSchemeName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "TOP" || originalTuningSchemeName === "TIPTOP" || originalTuningSchemeName === "T1" || originalTuningSchemeName === "TOP-max",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    originalTuningSchemeName === "TE" || originalTuningSchemeName === "Tenney-Euclidean" || originalTuningSchemeName === "T2" || originalTuningSchemeName === "TOP-RMS",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E";
  ];
  If[
    originalTuningSchemeName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "copfr-E";
  ];
  If[
    originalTuningSchemeName === "BOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "sopfr";
  ];
  If[
    originalTuningSchemeName === "BE" || originalTuningSchemeName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "sopfr-E";
  ];
  If[
    originalTuningSchemeName === "Weil",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";systematicComplexityName = "lil";
  ];
  If[
    originalTuningSchemeName === "WE" || originalTuningSchemeName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "lil-E";
  ];
  If[
    originalTuningSchemeName === "Kees",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "lol";
  ];
  If[
    originalTuningSchemeName === "KE" || originalTuningSchemeName === "Kees-Euclidean",
    (* Note how this tuning scheme works by enforcing an unchanged octave via a solver constraint, rather than through the complexity units multiplier *)
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "lol-E"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "POTOP" || originalTuningSchemeName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; pureStretchedInterval = "octave";
  ];
  If[
    originalTuningSchemeName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; pureStretchedInterval = "octave";
  ];
  If[
    originalTuningSchemeName === "CTE" || originalTuningSchemeName === "Constrained Tenney-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; unchangedIntervals = "octave";
  ];
  
  If[
    originalDamageName === "topDamage",
    damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 1; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight" || originalComplexityName === "harmonicDistance",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "rosossopfr",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ]; *)
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    originalComplexityName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
  (* trait 0a - targeted intervals *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*infinite-target-set*"] || (StringMatchQ[systematicTuningSchemeName, "*minimax*"] && StringMatchQ[systematicTuningSchemeName, "*S*"]),
    targetedIntervals = {};
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*odd-diamond*"],
    targetedIntervals = "odd-diamond"; unchangedIntervals = "octave";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*primes*"],
    targetedIntervals = "primes";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?\\{[\\d\\/\\,\\s]*\\}\\s+.*"]],
    targetedIntervals = First[StringCases[systematicTuningSchemeName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?(\\{[\\d\\/\\,\\s]*\\})\\s+.*"] -> "$1"]],
  ];
  
  (* trait 0b - unchanged intervals *)
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+.*"]],
    unchangedIntervals = First[StringCases[systematicTuningSchemeName, RegularExpression["unchanged\\-(\\{?[\\w\\s\\,\\/]+\\}?)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 1 - optimization power *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*minimax*"],
    optimizationPower = \[Infinity];
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*minisos*"],
    optimizationPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*minisum*"],
    optimizationPower = 1;
  ];
  
  (* trait 2 - damage weighting slope *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*S*"] || StringMatchQ[systematicDamageName, "*S*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*C*"] || StringMatchQ[systematicDamageName, "*C*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*U*"] || StringMatchQ[systematicDamageName, "*U*"],
    damageWeightingSlope = "unweighted";
  ];
  
  (* trait 3 - interval complexity norm power *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*E*"] || StringMatchQ[systematicDamageName, "*E*"] || StringMatchQ[systematicComplexityName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*T*"] || StringMatchQ[systematicDamageName, "*T*"] || StringMatchQ[systematicComplexityName, "*T*"],
    complexityNormPower = 1;
  ];
  
  (* trait 4 - interval complexity coordinate change *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*copfr*"] || StringMatchQ[systematicDamageName, "*copfr*"] || StringMatchQ[systematicComplexityName, "*copfr*"],
    complexityNegateLogPrimeCoordinator = True;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*sopfr*"] || StringMatchQ[systematicDamageName, "*sopfr*"] || StringMatchQ[systematicComplexityName, "*sopfr*"],
    complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*lil*"] || StringMatchQ[systematicDamageName, "*lil*"] || StringMatchQ[systematicComplexityName, "*lil*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*lol*"] || StringMatchQ[systematicDamageName, "*lol*"] || StringMatchQ[systematicComplexityName, "*lol*"],
    complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  
  (* trait 8 - tuning scheme interval basis *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*formal-primes-basis*"],
    tuningSchemeIntervalBasis = "primes";
  ];
  
  (* trait 9 - pure-stretched interval *)
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["pure\\-stretched\\-\\S+\\s+.*"]],
    pureStretchedInterval = First[StringCases[systematicTuningSchemeName, RegularExpression["pure\\-stretched\\-(\\S+)\\s+.*"] -> "$1"]];
  ];
  
  (* complexityMakeOdd is enough to get odd limit complexity from integer limit complexity, 
  but when actually solving for tunings, it's necessary to lock down prime 2 (the octave) as an unchanged interval. *)
  If[complexityMakeOdd == True, unchangedIntervals = "octave"];
  
  (* This has to go below the systematic tuning scheme name gating, so that targetedIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningSchemeIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasisPrivate[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    targetedIntervals = rebase[intervalRebase, processTargetedIntervals[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage]];
    unchangedIntervals = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[unchangedIntervals, t]];
    pureStretchedIntervalV = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t]],
    
    tPossiblyWithChangedIntervalBasis = t;
    targetedIntervals = processTargetedIntervals[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage];
    unchangedIntervals = processUnchangedOrPureStretchedIntervals[unchangedIntervals, t];
    pureStretchedIntervalV = processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t];
  ];
  
  If[
    logging == True,
    printWrapper["tPossiblyWithChangedIntervalBasis: ", formatOutput[tPossiblyWithChangedIntervalBasis]];
    printWrapper["targetedIntervals: ", formatOutput[targetedIntervals]]; (* trait 0a *)
    printWrapper["unchangedIntervals: ", formatOutput[unchangedIntervals]]; (* trait 0b *)
    printWrapper["optimizationPower: ", formatOutput[optimizationPower]]; (* trait 1 *)
    printWrapper["damageWeightingSlope: ", formatOutput[damageWeightingSlope]]; (* trait 2 *)
    printWrapper["complexityNormPower: ", formatOutput[complexityNormPower]]; (* trait 3 *)
    printWrapper["complexityNegateLogPrimeCoordinator: ", formatOutput[complexityNegateLogPrimeCoordinator]]; (* trait 4a *)
    printWrapper["complexityPrimePower: ", formatOutput[complexityPrimePower]]; (* trait 4b *)
    printWrapper["complexitySizeFactor: ", formatOutput[complexitySizeFactor]]; (* trait 4c *)
    printWrapper["complexityMakeOdd: ", formatOutput[complexityMakeOdd]]; (* trait 4d *)
    printWrapper["tuningSchemeIntervalBasis: ", formatOutput[tuningSchemeIntervalBasis]]; (* trait 8 *)
    printWrapper["pureStretchedIntervalV: ", formatOutput[pureStretchedIntervalV]]; (* trait 9 *)
  ];
  
  If[
    !NumericQ[optimizationPower] && optimizationPower != \[Infinity],
    Throw["no optimization power"]
  ];
  If[
    damageWeightingSlope == "",
    Throw["no damage weighting slope"]
  ];
  If[
    ToString[targetedIntervals] == "Null" && optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."]
  ];
  If[
    ToString[targetedIntervals] == "Null" && damageWeightingSlope != "simplicityWeighted",
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."]
  ];
  
  {
    tPossiblyWithChangedIntervalBasis,
    targetedIntervals, (* trait 0a *)
    unchangedIntervals, (* trait 0b *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningSchemeIntervalBasis, (* trait 8 *)
    pureStretchedIntervalV, (* trait 9 *)
    logging
  }
];

tuningSchemePropertiesPartsByOptionName = <|
  "t" -> 1,
  "targetedIntervals" -> 2, (* trait 0a *)
  "unchangedIntervals" -> 3, (* trait 0b *)
  "optimizationPower" -> 4, (* trait 1 *)
  "damageWeightingSlope" -> 5, (* trait 2 *)
  "complexityNormPower" -> 6, (* trait 3 *)
  "complexityNegateLogPrimeCoordinator" -> 7, (* trait 4a *)
  "complexityPrimePower" -> 8, (* trait 4b *)
  "complexitySizeFactor" -> 9, (* trait 4c *)
  "complexityMakeOdd" -> 10, (* trait 4d *)
  "tuningSchemeIntervalBasis" -> 11, (* trait 8 *)
  "pureStretchedIntervalV" -> 12, (* trait 9 *)
  "logging" -> 13
|>;
tuningSchemeProperty[tuningSchemeProperties_, optionName_] := Part[tuningSchemeProperties, tuningSchemePropertiesPartsByOptionName[optionName]];

processTargetedIntervals[targetedIntervals_, t_, tPossiblyWithChangedIntervalBasis_, forDamage_] := If[
  ToString[targetedIntervals] == "Null",
  Throw["no targeted intervals"],
  If[
    ToString[targetedIntervals] == "{}",
    If[
      forDamage,
      getFormalPrimes[tPossiblyWithChangedIntervalBasis],
      Null
    ],
    If[
      ToString[targetedIntervals] == "odd-diamond",
      getOddDiamond[getDPrivate[tPossiblyWithChangedIntervalBasis]],
      If[
        ToString[targetedIntervals] == "primes",
        {IdentityMatrix[getDPrivate[tPossiblyWithChangedIntervalBasis]], "vector"},
        {getA[parseQuotientSet[targetedIntervals, t]], "vector"}
      ]
    ]
  ]
];

processUnchangedOrPureStretchedIntervals[unchangedOrPureStretchedIntervals_, t_] := If[
  ToString[unchangedOrPureStretchedIntervals] == "Null",
  Null,
  If[
    ToString[unchangedOrPureStretchedIntervals] == "octave",
    getOctave[t],
    parseQuotientSet[unchangedOrPureStretchedIntervals, t]
  ]
];

rebase[intervalRebase_, t_] := If[t == Null, t, multiply[{intervalRebase, t}, "map"]];


(* PARTS *)

getTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    targetedIntervals,
    unchangedIntervals,
    optimizationPower,
    logging,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    
    temperedSideGeneratorsPartArg,
    temperedSideMappingPartArg,
    justSideGeneratorsPartArg,
    justSideMappingPartArg,
    eitherSideIntervalsPartArg,
    eitherSideMultiplierPartArg,
    powerArg,
    unchangedIntervalsArg
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0b *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  temperedSideGeneratorsPartArg = generatorsTuningMap;
  temperedSideMappingPartArg = m;
  justSideGeneratorsPartArg = primeCentsMap;
  justSideMappingPartArg = getPrimesI[t];
  eitherSideIntervalsPartArg = targetedIntervals;
  eitherSideMultiplierPartArg = getDamageWeights[tuningSchemeProperties];
  powerArg = optimizationPower;
  unchangedIntervalsArg = unchangedIntervals;
  
  If[
    logging == True,
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* T *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* W *)
    printWrapper["powerArg: ", powerArg];
    printWrapper["unchangedIntervalsArg: ", unchangedIntervalsArg];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* g *)
    temperedSideMappingPartArg, (* M *)
    justSideGeneratorsPartArg, (* p *)
    justSideMappingPartArg, (* Mₚ *)
    eitherSideIntervalsPartArg, (* T *)
    eitherSideMultiplierPartArg, (* W *)
    powerArg,
    unchangedIntervalsArg
  }
];

tuningMethodArgsByName = <|
  "temperedSideGeneratorsPartArg" -> 1,
  "temperedSideMappingPartArg" -> 2,
  "justSideGeneratorsPartArg" -> 3,
  "justSideMappingPartArg" -> 4,
  "eitherSideIntervalsPartArg" -> 5,
  "eitherSideMultiplierPartArg" -> 6,
  "powerArg" -> 7,
  "unchangedIntervalsArg" -> 8
|>;
tuningMethodArg[tuningMethodArgs_, partName_] := Part[tuningMethodArgs, tuningMethodArgsByName[partName]];


(* SHARED *)

getOctave[t_] := {{Join[{1}, Table[0, getDPrivate[t] - 1]]}, "vector"};
getSummationMap[t_] := {{Table[1, getDPrivate[t]]}, "map"};
getLogPrimeCoordinator[t_] := {DiagonalMatrix[Log2[getIntervalBasis[t]]], "map"};
getPrimeCentsMap[t_] := {1200 * getA[multiply[{getSummationMap[t], getLogPrimeCoordinator[t]}, "map"]], "map"};
getPrimesI[t_] := {IdentityMatrix[getDPrivate[t]], "map"};

getTuningSchemeMappings[t_] := Module[
  {generatorsTuningMap, m, primeCentsMap},
  
  generatorsTuningMap = {{Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getRPrivate[t]}]}, "map"};
  m = getM[t];
  primeCentsMap = getPrimeCentsMap[t];
  
  {generatorsTuningMap, m, primeCentsMap}
];

(* similar to pseudoinverse, but works for any tuning so far described *)
tuningInverse[damageWeightsOrComplexityMultiplier_] := {MapThread[
  Function[
    {dataRow, zerosRow},
    MapIndexed[
      Function[
        {zerosEl, index},
        zerosEl + If[
          First[index] > Length[dataRow],
          0,
          Part[dataRow, First[index]]
        ]
      ],
      zerosRow
    ]
  ],
  {
    (* note: this is pseudo not because of non-square, due to complexity size factor,
    but because of when complexity is odd and the top-left entry is a 0 so det is 0 so it's singular *)
    PseudoInverse[
      getA[damageWeightsOrComplexityMultiplier][[1 ;; Last[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]]]
    ],
    Table[
      Table[
        0,
        First[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]
      ],
      Last[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]
    ]
  }
], "map"};


(* DAMAGE *)

(* compare with getDualMultiplier *)
getDamageWeights[tuningSchemeProperties_] := Module[
  {
    t,
    targetedIntervals, (* trait 0a *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    damageWeights
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 0a *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 4d *)
  
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    {IdentityMatrix[Length[getA[targetedIntervals]]], "map"},
    
    {DiagonalMatrix[Map[Function[
      {targetedIntervalPcv},
      getComplexity[
        targetedIntervalPcv,
        t,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordinator, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ], getA[targetedIntervals]]], "map"}
  ];
  
  If[
    damageWeightingSlope == "simplicityWeighted",
    
    tuningInverse[damageWeights],
    
    damageWeights
  ]
];


(* ERROR *)

getPowerSumAbsError[tuningMethodArgs_] := If[
  tuningMethodArg[tuningMethodArgs, "powerArg"] == \[Infinity],
  
  (* I thought it would be fine, but apparently Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
  Max[First[getA[getAbsErrors[tuningMethodArgs]]]],
  
  Total[Power[First[getA[getAbsErrors[tuningMethodArgs]]], tuningMethodArg[tuningMethodArgs, "powerArg"]]]
];
getPowerNormAbsError[tuningMethodArgs_] := Norm[getA[getAbsErrors[tuningMethodArgs]], tuningMethodArg[tuningMethodArgs, "powerArg"]];
getPowerMeanAbsError[tuningMethodArgs_] := Module[
  {absErrors, powerArg, targetedIntervalCount, result},
  
  absErrors = getAbsErrors[tuningMethodArgs];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  targetedIntervalCount = Last[Dimensions[tuningMethodArg[tuningMethodArgs, "eitherSideIntervalsPartArg"]]]; (* k *)
  
  If[debug == True, printWrapper["absErrors: ", absErrors]];
  
  result = If[
    powerArg == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
    Max[getA[absErrors]],
    
    Power[
      Total[Power[
        getA[absErrors],
        powerArg
      ]] / targetedIntervalCount,
      1 / powerArg
    ]
  ];
  
  result
];

(* returns errors in octaves *)
getAbsErrors[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := Module[
  {temperedSide, justSide, absErrors},
  
  temperedSide = getTemperedOrJustSide[temperedSideGeneratorsPartArg, temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  
  absErrors = {{Abs[N[
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, #]&,
      First[getA[temperedSide] - getA[justSide]]
    ],
    absoluteValuePrecision
  ]]}, "map"};
  
  If[
    debug == True,
    printWrapper[formatOutput[temperedSide]];
    printWrapper[formatOutput[justSide]];
    printWrapper["absErrors: ", Map[If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, 4]]&, getA[absErrors]]]
  ];
  
  absErrors
];

(* COMPLEXITY *)

(* returns complexities in weighted octaves *)
getComplexity[
  pcv_,
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordinator_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {complexityMultiplierAndLogPrimeCoordinator},
  
  complexityMultiplierAndLogPrimeCoordinator = getComplexityMultiplierAndLogPrimeCoordinator[
    t,
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  Norm[First[Transpose[getA[multiply[{complexityMultiplierAndLogPrimeCoordinator, {{pcv}, "vector"}}, "map"]]]], complexityNormPower] / (1 + complexitySizeFactor)
];

(* Note that we don't actually use any of these functions directly; they're just around to test understanding *)
getPcvCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getPcvProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getPcvLogProductComplexity[pcv_, t_] := Log2[getPcvProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find minimax-sopfr-S ("BOP") tuning scheme *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find minimax-S ("TOP") tuning scheme *)
getPcvLogSopfrComplexity[pcv_, t_] := Log2[getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for minimax-lil-S ("Weil") tuning scheme *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log2[getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility", used for minimax-lol-S ("Kees") tuning scheme *)
getPcvLogOddLimitComplexity[pcv_, t_] := Log2[getPcvOddLimitComplexity[pcv, t]];

(* This is different than getDamageWeights, this is nested within it;
this is to weight the quantities of the PC-vector entries before taking their norm to get an interval complexity, 
and these complexities are then gathered for each interval and applied 
(or their reciprocals applied, in the case of simplicity-weighting) as damageWeights;
when this method is used by getDamageWeights in getTuningMethodArgs, 
it covers any finite-target-set tuning scheme using this for its damage's complexity *)
getComplexityMultiplier[
  t_,
  complexityNegateLogPrimeCoordinator_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{complexityMultiplier},
  (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeTuningMethodArgs, covers minimax-S ("TOP") and minimax-ES ("TE") *)
  complexityMultiplier = {IdentityMatrix[getDPrivate[t]], "map"};
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeTuningMethodArgs, covers minimax-copfr-S (the L1 version of "Frobenius") and minimax-copfr-ES ("Frobenius") *)
    complexityNegateLogPrimeCoordinator == True,
    complexityMultiplier = multiply[{complexityMultiplier, inverse[getLogPrimeCoordinator[t]]}, "map"]
  ];
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeTuningMethodArgs, covers minimax-sopfr-S ("BOP") and minimax-sopfr-ES ("BE") *)
    complexityPrimePower > 0,
    complexityMultiplier = multiply[{complexityMultiplier, {DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]], "map"}}, "map"]
  ];
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeTuningMethodArgs, covers minimax-lil-S ("Weil"), minimax-lil-ES ("WE"), minimax-lol-S ("Kees"), and minimax-lol-ES ("KE")
    (yes, surprisingly, when computing minimax-lol-S and minimax-lol-ES tunings, we do not use the below, though user calls for odd-limit complexity do use it;
    the tuning calculations instead use only this size-sensitizer effect, and apply an unchanged octave constraint to achieve the oddness aspect) *)
    complexitySizeFactor > 0,
    complexityMultiplier = multiply[{{Join[getA[getPrimesI[t]], {Table[complexitySizeFactor, getDPrivate[t]]}], "map"}, complexityMultiplier}, "map"]
  ];
  
  If[
    (* When minimax-lol-S ("Kees") and minimax-lol-ES ("KE") need their dual norms, they don't use this; see note above *)
    complexityMakeOdd == True,
    complexityMultiplier = multiply[{complexityMultiplier, {DiagonalMatrix[Join[{0}, Table[1, getDPrivate[t] - 1]]], "map"}}, "map"]
  ];
  
  complexityMultiplier
];

getComplexityMultiplierAndLogPrimeCoordinator[
  t_,
  complexityNegateLogPrimeCoordinator_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := multiply[
  {
    getComplexityMultiplier[
      t,
      complexityNegateLogPrimeCoordinator, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd (* trait 4d *)
    ],
    getLogPrimeCoordinator[t]
  },
  "map"
];


(* INFINITE-TARGET-SET *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningSchemeProperties_] := Module[
  {
    t,
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    complexityMultiplierAndLogPrimeCoordinator
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 4d *)
  
  complexityMultiplierAndLogPrimeCoordinator = getComplexityMultiplierAndLogPrimeCoordinator[
    t,
    complexityNegateLogPrimeCoordinator, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplierAndLogPrimeCoordinator]
];

(* compare with getTuningMethodArgs *)
getInfiniteTargetSetTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    unchangedIntervals,
    complexityNormPower,
    complexitySizeFactor,
    logging,
    
    generatorsTuningMap,
    m,
    d,
    mappingAugmentation,
    primeCentsMap,
    
    dualMultiplier,
    primesErrorMagnitudeNormPower,
    
    temperedSideGeneratorsPartArg,
    temperedSideMappingPartArg,
    justSideGeneratorsPartArg,
    justSideMappingPartArg,
    eitherSideIntervalsPartArg,
    eitherSideMultiplierPartArg,
    powerArg,
    unchangedIntervalsArg
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0b *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  dualMultiplier = getDualMultiplier[tuningSchemeProperties];
  primesErrorMagnitudeNormPower = getDualPower[complexityNormPower];
  
  justSideMappingPartArg = getPrimesI[t];
  eitherSideIntervalsPartArg = transpose[getPrimesI[t]];
  powerArg = primesErrorMagnitudeNormPower;
  unchangedIntervalsArg = unchangedIntervals;
  
  If[
    complexitySizeFactor != 0,
    
    generatorsTuningMap = {{Join[First[getA[generatorsTuningMap]], {Symbol["gAugmented"]}]}, "map"};
    
    d = getD[m];
    m = {Map[Join[#, {0}]&, getA[m]], "map"};
    mappingAugmentation = {Join[
      First[getA[multiply[
        {
          {{Table[complexitySizeFactor, d]}, "map"},
          getLogPrimeCoordinator[t]
        },
        "map"
      ]]],
      {-1}
    ]};
    m = {Join[getA[m], mappingAugmentation], "map"};
    
    primeCentsMap = {{Join[First[getA[primeCentsMap]], {0}]}, "map"};
    
    justSideMappingPartArg = {basicComplexitySizeFactorAugmentation[getA[justSideMappingPartArg]], "map"};
    
    eitherSideIntervalsPartArg = {basicComplexitySizeFactorAugmentation[getA[eitherSideIntervalsPartArg]], "vector"};
    
    dualMultiplier = {Join[getA[dualMultiplier], {Join[Table[0, Last[Dimensions[getA[dualMultiplier]]] - 1], {1}]}], "map"};
    
    unchangedIntervalsArg = If[
      ToString[unchangedIntervalsArg] == "Null",
      unchangedIntervalsArg,
      {Map[Join[#, {0}]&, getA[unchangedIntervalsArg]], "vector"}
    ];
  ];
  
  temperedSideGeneratorsPartArg = generatorsTuningMap;
  temperedSideMappingPartArg = m;
  justSideGeneratorsPartArg = primeCentsMap;
  eitherSideMultiplierPartArg = dualMultiplier;
  
  If[
    logging == True,
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* Tₚ *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* X⁻¹ *)
    printWrapper["powerArg: ", powerArg];
    printWrapper["unchangedIntervalsArg: ", unchangedIntervalsArg];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* g *)
    temperedSideMappingPartArg, (* M *)
    justSideGeneratorsPartArg, (* p *)
    justSideMappingPartArg, (* Mₚ *)
    eitherSideIntervalsPartArg, (* Tₚ *)
    eitherSideMultiplierPartArg, (* X⁻¹ *)
    powerArg,
    unchangedIntervalsArg
  }
];

basicComplexitySizeFactorAugmentation[a_] := Module[
  {augmentedA},
  
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  augmentedA
];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorsPreimageTransversal, f},
  
  m = getM[t];
  optimumTuningMap = multiply[{optimumGeneratorsTuningMap, m}, "map"];
  generatorsPreimageTransversal = {getA[getGeneratorsPreimageTransversalPrivate[originalT]], "vector"};
  f = getFormalPrimes[originalT];
  
  multiply[{optimumTuningMap, f, generatorsPreimageTransversal}, "map"]
];


(* PURE-STRETCHED INTERVAL *)

getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedIntervalV_] := Module[
  {
    generatorsTuningMap,
    m,
    primeCentsMap,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiply[{primeCentsMap, pureStretchedIntervalV}, "vector"];
  temperedIntervalSize = multiply[{optimumGeneratorsTuningMap, m, pureStretchedIntervalV}, "vector"];
  
  {First[First[getA[justIntervalSize] / getA[temperedIntervalSize]]] * getA[optimumGeneratorsTuningMap], "map"}
];


(* TARGETED INTERVAL SETS *)

getOddDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  (* for when you want the tonality diamond to be in the natural order for a 5-limit diamond, 
  as when developing pedagogical materials and using this library, 
  because it normally doesn't end up getting them in the natural order
  {{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}} *)
  
  {padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d], "vector"}
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* SOLUTIONS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers odd-diamond minimax-U "minimax", minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
(* a semi-analytical solution *)
(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
maxPolytopeSolution[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := Module[
  {
    temperedSideButWithoutGeneratorsPart,
    justSide,
    
    generatorCount,
    maxCountOfNestedMinimaxibleDamages,
    minimaxTunings,
    minimaxLockForTemperedSide,
    minimaxLockForJustSide,
    undoMinimaxLocksForTemperedSide,
    undoMinimaxLocksForJustSide,
    uniqueOptimumTuning
  },
  
  (* the mapped and weighted targeted intervals on one side, and the just and weighted targeted intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  temperedSideButWithoutGeneratorsPart = multiply[{temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg}, "map"];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  
  (* our goal is to find the generator tuning map not merely with minimaxed damage, 
  but where the next-highest damage is minimaxed as well, and in fact every next-highest damage is minimaxed, all the way down.
  the tuning which has all damages minimaxed within minimaxed all the way down like this we can call a "nested-minimax".
  it's the only sensible optimum given a desire for minimax damage, so in general we can simply still call it "minimax".
  though people have sometimes distinguished this tuning scheme from the range of minimax tuning schemes with a prefix, 
  such as "TIPTOP tuning" versus "TOP tunings", although there is no value in "TOP tunings" given the existence of "TIPTOP",
  so you may as well just keep calling it "TOP" and refine its definition. anyway...
  
  the `findAllNestedMinimaxTuningsFromMaxPolytopeVertices` function this function calls may come back with more than one result. 
  (sometimes it pulls off some nested-minimaxing on its own, but that's a really subtle point, and we won't worry about it here.)
  the clever way we compute a nested-minimax uses the same polytope vertex searching method used for that first pass, but now with a twist.
  so in the basic case, this method finds the vertices of a max polytope for a temperament.
  so now, instead of running it on the case of the original temperament versus JI, we run it on a distorted version of this case.
  specifically, we run it on a case distorted so that the previous minimaxes are locked down.
  
  we achieve this by picking one of these minimax tunings and offset the just side by it. 
  it doesn't matter which minimax tuning we choose, by the way; they're not sorted, and we simply take the first one.
  the corresponding distortion to the tempered side is trickier, 
  involving the differences between this arbitrarily-chosen minimax tuning and each of the other minimax tunings.
  note that after this distortion, the original rank and dimensionality of the temperament will no longer be recognizable.
  
  we then search for polytope vertices of this minimax-locked distorted situation.
  and we repeatedly do this until we eventually find a unique, nested-minimax optimum. 
  once we've done that, though, our result isn't in the form of a generators tuning map yet. it's still distorted.
  well, with each iteration, we've been keeping track of the distortion applied, so that in the end we could undo them all.
  after undoing those, voilà, we're done! *)
  
  (* the same as rank here, but named this for correlation with elsewhere in this code *)
  generatorCount = getRPrivate[temperedSideButWithoutGeneratorsPart];
  
  (* this is too complicated to be explained here and will be explained later *)
  maxCountOfNestedMinimaxibleDamages = 0;
  
  (* the candidate generator tuning maps which minimax damage to the targets*)
  minimaxTunings = findAllNestedMinimaxTuningsFromMaxPolytopeVertices[
    temperedSideButWithoutGeneratorsPart,
    justSide,
    maxCountOfNestedMinimaxibleDamages
  ];
  maxCountOfNestedMinimaxibleDamages = generatorCount + 1;
  
  (* no minimax-damage-locking transformations yet, so the transformation trackers are identities 
  per their respective operations of matrix multiplication and addition *)
  undoMinimaxLocksForTemperedSide = {IdentityMatrix[generatorCount], "map"};
  undoMinimaxLocksForJustSide = {Table[{0}, generatorCount], "map"};
  
  While[
    (* a unique optimum has not yet been found *)
    Length[minimaxTunings] > 1,
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    minimaxLockForJustSide = First[minimaxTunings];
    (* list of differences between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one *)
    minimaxLockForTemperedSide = {Map[Flatten, Transpose[Map[
      getA[Part[minimaxTunings, #]] - getA[minimaxLockForJustSide]&,
      Range[2, Length[minimaxTunings]]
    ]]], "map"};
    
    (* apply the minimax-damage-locking transformation to the just side, and track it to undo later *)
    justSide = {getA[justSide] - getA[multiply[{minimaxLockForJustSide, temperedSideButWithoutGeneratorsPart}, "map"]], "map"};
    undoMinimaxLocksForJustSide = {getA[undoMinimaxLocksForJustSide] + Transpose[getA[multiply[{minimaxLockForJustSide, undoMinimaxLocksForTemperedSide}, "map"]]], "map"};
    
    (* apply the minimax-damage-locking transformation to the tempered side, and track it to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the justSide, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    temperedSideButWithoutGeneratorsPart = multiply[{minimaxLockForTemperedSide, temperedSideButWithoutGeneratorsPart }, "map"];
    undoMinimaxLocksForTemperedSide = multiply[{minimaxLockForTemperedSide, undoMinimaxLocksForTemperedSide}, "map"];
    
    (* search again, now in this transformed state *)
    minimaxTunings = findAllNestedMinimaxTuningsFromMaxPolytopeVertices[temperedSideButWithoutGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];
    maxCountOfNestedMinimaxibleDamages += generatorCount + 1;
  ];
  
  uniqueOptimumTuning = First[minimaxTunings];
  
  SetAccuracy[{
    (* here's that left-multiplication mentioned earlier *)
    getA[multiply[{uniqueOptimumTuning, undoMinimaxLocksForTemperedSide}, "map"]] + Transpose[getA[undoMinimaxLocksForJustSide]],
    "map"
  }, linearSolvePrecision]
];

findAllNestedMinimaxTuningsFromMaxPolytopeVertices[temperedSideButWithoutGeneratorsPart_, justSide_, maxCountOfNestedMinimaxibleDamages_] := Module[
  {
    targetCount,
    generatorCount,
    nthmostMinDamage,
    vertexConstraints,
    targetIndices,
    candidateTunings,
    sortedDamagesByCandidateTuning,
    candidateTuning,
    sortedDamagesForThisCandidateTuning,
    newCandidateTunings,
    newSortedDamagesByCandidateTuning
  },
  
  (* in the basic case where no minimax-damage-locking transformations have been applied, 
  these will be the same as the count of original targeted intervals and the rank of the temperament, respectively *)
  targetCount = getDPrivate[temperedSideButWithoutGeneratorsPart];
  generatorCount = getRPrivate[temperedSideButWithoutGeneratorsPart];
  
  (* here's the meat of it: solving a linear problem for each vertex of the of tuning polytope;
  more details on this in the constraint matrix gathering function's comments below *)
  candidateTunings = {};
  vertexConstraints = getTuningMaxPolytopeVertexConstraints[generatorCount, targetCount];
  Do[
    AppendTo[
      candidateTunings,
      {Quiet[Check[
        LinearSolve[
          N[getA[multiply[{vertexConstraint, transpose[temperedSideButWithoutGeneratorsPart]}, "map"]], linearSolvePrecision],
          N[getA[multiply[{vertexConstraint, transpose[justSide]}, "map"]], linearSolvePrecision]
        ],
        "err"
      ]], "map"}
    ],
    {vertexConstraint, vertexConstraints}
  ];
  
  (* each damages list is sorted in descending order; 
  the list of lists itself is sorted corresponding to the candidate tunings*)
  sortedDamagesByCandidateTuning = Quiet[Map[
    Function[
      {candidateTuning},
      If[
        ToString[getA[candidateTuning]] == "err",
        "err",
        {Abs[fixUpZeros[getA[multiply[{transpose[candidateTuning], temperedSideButWithoutGeneratorsPart}, "map"]] - getA[justSide]]], "map"}
      ]
    ],
    candidateTunings
  ]];
  
  If[
    debug == True,
    MapThread[
      printWrapper["constraint matrix: ", formatOutput[#1], " tuning: ", formatOutput[#2] , " damages: ", formatOutput[#3]]&,
      {vertexConstraints, candidateTunings, sortedDamagesByCandidateTuning}
    ]
  ];
  
  (* ignore the problems that are singular and therefore have no solution *)
  candidateTunings = Select[candidateTunings, !TrueQ[# == {"err", "map"}]&];
  sortedDamagesByCandidateTuning = Select[sortedDamagesByCandidateTuning, !TrueQ[# == "err"]&];
  sortedDamagesByCandidateTuning = Map[{{ReverseSort[First[getA[#]]]}, "map"}&, sortedDamagesByCandidateTuning];
  
  (*     
  here we're iterating by index of the targeted intervals, 
  repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  
  note that because the damages have all been sorted in descending order,
  these target "indices" do not actually correspond to an individual targeted interval.
  that's okay though because here it's not important which target each of these damages is for.
  all that matters is the size of the damages.
  once we find the tuning we want, we can easily compute its damages list sorted by target when we need it later; that info is not lost.
  
  and note that we don't iterate over *every* target "index".
  we only check as many targets as we could possibly nested-minimax by this point.
  that's why this method doesn't simply always return a unique nested-minimax tuning each time.
  this is also why the damages have been sorted in this way
  so first we compare each tuning's actual minimum damage,
  then we compare each tuning's second-closest-to-minimum damage,
  then compare each third-closest-to-minimum, etc.
  the count of target indices we iterate over is a running total; 
  each time it is increased, it goes up by the present generator count plus 1.
  why it increases by that amount is a bit of a mystery to me, but perhaps someone can figure it out and let me know.
  *)
  targetIndices = Range[Min[maxCountOfNestedMinimaxibleDamages + generatorCount + 1, targetCount]];
  Do[
    newCandidateTunings = {};
    newSortedDamagesByCandidateTuning = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target index *)
    nthmostMinDamage = Min[Map[Part[First[getA[#]], targetIndex]&, sortedDamagesByCandidateTuning]];
    Do[
      (* having found the minimum damage for this target index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[First[getA[sortedDamagesForThisCandidateTuning]], targetIndex] <= nthmostMinDamage + 0.000000001,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newSortedDamagesByCandidateTuning, sortedDamagesForThisCandidateTuning]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    sortedDamagesByCandidateTuning = newSortedDamagesByCandidateTuning,
    
    {targetIndex, targetIndices}
  ];
  
  (* if duplicates are not deleted, then when differences are checked between tunings,
  some will come out to all zeroes, and this causes a crash *)
  DeleteDuplicates[
    Map[{Transpose[getA[#]], "map"}&, candidateTunings],
    Function[{tuningA, tuningB}, AllTrue[MapThread[#1 == #2&, {First[getA[tuningA]], First[getA[tuningB]]}], TrueQ]]
  ]
];
fixUpZeros[l_] := Map[
  Function[
    {nestedList},
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, linearSolvePrecision]]&,
      nestedList
    ]
  ],
  l
];

getTuningMaxPolytopeVertexConstraints[generatorCount_, targetCount_] := Module[
  {vertexConstraintA, vertexConstraintAs, targetCombinations, directionPermutations},
  
  vertexConstraintAs = {};
  
  (* here we iterate over every combination of r + 1 (rank = generator count, in the basic case) targets 
  and for each of those combinations, looks at all permutations of their directions. 
  these are the vertices of the maximum damage tuning polytope. each is a generator tuning map. the minimum of these will be the minimax tuning.
  
  e.g. for target intervals 3/2, 5/4, and 5/3, with 1 generator, we'd look at three combinations (3/2, 5/4) (3/2, 5/3) (5/4, 5/3)
  and for the first combination, we'd look at both 3/2 \[Times] 5/4 = 15/8 and 3/2 \[Divide] 5/4 = 6/5.
  
  then what we do with each of those combo perm vertices is build a constraint matrix. 
  we'll apply this constraint matrix to a typical linear equation of the form Ax = b, 
  where A is a matrix, b is a vector, and x is another vector, the one we're solving for.
  in our case our matrix A is M, our mapping, b is our just tuning map j, and x is our generators tuning map g.
  
  e.g. when the targets are just the primes (and thus an identity matrix we can ignore),
  and the temperament we're tuning is 12-ET with M = [12 19 28] and standard interval basis so p = [log₂2 log₂3 log₂5],
  then we have [12 19 28][g₁] = [log₂2 log₂3 log₂5], or a system of three equations:
  
  12g₁ = log₂2
  19g₁ = log₂3
  28g₁ = log₂5
  
  Obviously not all of those can be true, but that's the whole point: we linear solve for the closest possible g₁ that satisfies all well.
  
  Now suppose we get the constraint matrix [1 1 0]. We multiply both sides of the setup by that:
  
  [1 1 0][12 19 28][g₁] = [1 1 0][log₂2 log₂3 log₂5]
  [31][g₁] = [log₂2 + log₂3]
  
  This leaves us with only a single equation:
  
  31g₁ = log₂6
  
  Or in other words, this tuning makes 6/1 pure, and divides it into 31 equal steps.
  If this temperament's mapping says it's 12 steps to 2/1 and 19 steps to 3/1, and it takes 31 steps to a pure 6/1,
  that implies that whatever damage there is on 2/1 is equal to whatever damage there is on 3/1, since they apparently cancel out.
  
  This constraint matrix [1 1 0] means that the target combo was 2/1 and 3/1, 
  because those are the targets corresponding to its nonzero elements.
  And both nonzero elements are +1 meaning that both targets are combined in the same direction.
  If the targeted intervals list had been [3/2, 4/3, 5/4, 8/5, 5/3, 6/5] instead, and the constraint matrix [1 0 0 0 -1 0],
  then that's 3/2 \[Divide] 5/3 = 5/2.
  
  The reason why we only need half of the permutations is because we only need relative direction permutations;
  they're anchored with the first targeted interval always in the super direction.
  *)
  targetCombinations = DeleteDuplicates[Map[Sort, Select[Tuples[Range[1, targetCount], generatorCount + 1], DuplicateFreeQ[#]&]]];
  
  If[debug == True, printWrapper["targetCombinations: ", formatOutput[targetCombinations]]];
  
  Do[
    (* note that these are only generatorCount, not generatorCount + 1, because whichever is the first one will always be +1 *)
    If[debug == True, printWrapper["  targetCombination: ", formatOutput[targetCombination]]];
    
    directionPermutations = Tuples[{1, -1}, generatorCount];
    If[debug == True, printWrapper["  directionPermutations: ", formatOutput[directionPermutations]]];
    
    Do[
      If[debug == True, printWrapper["    directionPermutation: ", formatOutput[directionPermutation]]];
      
      vertexConstraintA = Table[Table[0, targetCount], generatorCount];
      
      Do[
        vertexConstraintA[[generatorIndex, Part[targetCombination, 1]]] = 1;
        vertexConstraintA[[generatorIndex, Part[targetCombination, generatorIndex + 1]]] = Part[directionPermutation, generatorIndex],
        
        {generatorIndex, Range[generatorCount]}
      ];
      
      If[debug == True, printWrapper["      vertexConstraintA: ", formatOutput[vertexConstraintA]]];
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {directionPermutation, directionPermutations}
    ],
    
    {targetCombination, targetCombinations}
  ];
  
  (* if there's only one generator, we also need to consider each tuning where a target is pure 
  (rather than tied for damage with another target) *)
  If[
    generatorCount == 1,
    Do[
      vertexConstraintA = {Table[0, targetCount]};
      vertexConstraintA[[1, targetIndex]] = 1;
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {targetIndex, Range[targetCount]}
    ]
  ];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  Map[{#, "map"}&, vertexConstraintAs]
];


(* SOLUTIONS: OPTIMIZATION POWER = 1 (MINISUM) OR COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tuning schemes use this *)
(* an analytical solution *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where odd-diamond minimax-U "minimax" is described;
however, this computation method is in general actually a solution for minisum tuning schemes, not minimax tuning schemes. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond targeted interval set,
namely that the places where damage to targets are equal is the same where other targets are pure.
*)
sumPolytopeSolution[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := Module[
  {
    generatorCount,
    
    unchangedIntervalSetIndices,
    candidateUnchangedIntervalSets,
    normalizedCandidateUnchangedIntervalSets,
    filteredNormalizedCandidateUnchangedIntervalSets,
    candidateOptimumGenerators,
    candidateOptimumGeneratorsTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorsTuningMapIndices,
    optimumGeneratorsTuningMapIndex
  },
  
  generatorCount = First[Dimensions[getA[temperedSideMappingPartArg]]]; (* First[], not Last[], because it's not transposed here. *)
  
  unchangedIntervalSetIndices = Subsets[Range[First[Dimensions[getA[eitherSideIntervalsPartArg]]]], {generatorCount}];
  candidateUnchangedIntervalSets = Map[{Map[getA[eitherSideIntervalsPartArg][[#]]&, #], "vector"}&, unchangedIntervalSetIndices];
  normalizedCandidateUnchangedIntervalSets = Map[canonicalFormPrivate, candidateUnchangedIntervalSets];
  filteredNormalizedCandidateUnchangedIntervalSets = DeleteDuplicates[Select[normalizedCandidateUnchangedIntervalSets, MatrixRank[Transpose[getA[#]]] == generatorCount&]];
  candidateOptimumGenerators = Select[Map[
    getGeneratorsAFromUnchangedIntervals[temperedSideMappingPartArg, #]&,
    filteredNormalizedCandidateUnchangedIntervalSets
  ], Not[# === Null]&];
  candidateOptimumGeneratorsTuningMaps = Map[multiply[{justSideGeneratorsPartArg, #}, "map"]&, candidateOptimumGenerators];
  candidateOptimumGeneratorTuningMapAbsErrors = Map[
    Total[First[getA[getAbsErrors[{
      #, (* note: this is an override for temperedSideGeneratorsPartArg, and it's the only reason why these tuning method args need to be unpacked *)
      temperedSideMappingPartArg,
      justSideGeneratorsPartArg,
      justSideMappingPartArg,
      eitherSideIntervalsPartArg,
      eitherSideMultiplierPartArg,
      powerArg,
      unchangedIntervalsArg
    }]]]]&,
    candidateOptimumGeneratorsTuningMaps
  ];
  
  If[
    debug == True,
    printWrapper["candidateUnchangedIntervalSets: ", Map[formatOutput, candidateUnchangedIntervalSets]];
    printWrapper["normalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, normalizedCandidateUnchangedIntervalSets]];
    printWrapper["filteredNormalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, filteredNormalizedCandidateUnchangedIntervalSets]];
    printWrapper["candidateOptimumGenerators: ", Map[formatOutput, candidateOptimumGenerators]];
    printWrapper["candidateOptimumGeneratorsTuningMaps: ", Map[formatOutput, candidateOptimumGeneratorsTuningMaps]];
    printWrapper["candidateOptimumGeneratorTuningMapAbsErrors: ", Map[formatOutput, candidateOptimumGeneratorTuningMapAbsErrors]];
  ];
  
  optimumGeneratorsTuningMapIndices = Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]];
  If[
    Length[optimumGeneratorsTuningMapIndices] == 1,
    
    (* result is unique; done *)
    optimumGeneratorsTuningMapIndex = First[First[Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]]]];
    candidateOptimumGeneratorsTuningMaps[[optimumGeneratorsTuningMapIndex]],
    
    (* result is non-unique, will need to handle otherwise *)
    Null
  ]
];

getGeneratorsAFromUnchangedIntervals[m_, unchangedIntervalEigenvectors_] := Module[
  {mappedUnchangedIntervalEigenvectors},
  
  mappedUnchangedIntervalEigenvectors = multiply[{m, unchangedIntervalEigenvectors}, "vector"];
  
  If[
    Det[getA[mappedUnchangedIntervalEigenvectors]] == 0,
    Null,
    multiply[{unchangedIntervalEigenvectors, inverse[mappedUnchangedIntervalEigenvectors]}, "vector"]
  ]
];


(* SOLUTIONS: OPTIMIZATION POWER = 2 (MINISOS) OR COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical solution *)
(* covers odd-diamond minisos-U "least squares", minimax-ES "TE", pure-stretched-octave minimax-ES "POTE",
minimax-copfr-ES "Frobenius", minimax-lil-ES "WE", minimax-sopfr-ES "BE" *)
pseudoinverseSolution[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := Module[
  {temperedSideButWithoutGeneratorsPart, justSide},
  
  temperedSideButWithoutGeneratorsPart = multiply[{temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg}, "map"];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  
  If[
    debug == True,
    printWrapper["temperedSideButWithoutGeneratorsPart: ", formatOutput[temperedSideButWithoutGeneratorsPart]];
    printWrapper["temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]: ", formatOutput[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "map"]]];
    printWrapper["Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "map"]]]];
    printWrapper["Transpose[temperedSideButWithoutGeneratorsPart].Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[multiply[{temperedSideButWithoutGeneratorsPart, Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "map"]]}, "map"]]];
    printWrapper["justSide.Transpose[temperedSideButWithoutGeneratorsPart].Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[multiply[{justSide, temperedSideButWithoutGeneratorsPart, Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "map"]]}, "map"]]];
    printWrapper["justSide: ", formatOutput[justSide]];
  ];
  
  (* Technically the Aᵀ(AAᵀ)⁻¹ type of pseudoinverse is necessary. Wolfram's built-in will sometimes use other techniques, which do not give the correct answer. *)
  multiply[
    {
      justSide,
      multiply[
        {
          transpose[temperedSideButWithoutGeneratorsPart],
          inverse[multiply[
            {
              temperedSideButWithoutGeneratorsPart,
              transpose[temperedSideButWithoutGeneratorsPart]
            },
            "vector"
          ]]
        },
        "map"
      ]
    },
    "map"
  ]
];


(* SOLUTIONS: GENERAL OPTIMIZATION POWER (MINISOP) OR GENERAL COMPLEXITY NORM POWER (P-NORM) *)

(* a numerical solution *)
(* covers minimax-lol-ES "KE", unchanged-octave minimax-ES "CTE" *)
powerSumSolution[tuningMethodArgs_] := Module[
  {temperedSideGeneratorsPartArg, solution},
  
  temperedSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideGeneratorsPartArg"];
  
  solution = getPowerSumSolution[tuningMethodArgs];
  
  {{First[getA[temperedSideGeneratorsPartArg]] /. Last[solution]}, "map"}
];

(* no historically described tuning schemes use this *)
(* a numerical solution *)
(* this is the fallback for when sumPolytopeSolution fails to find a unique solution *)
powerSumLimitSolution[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := Module[
  {
    powerSumPowerLimit,
    powerSumPowerPower,
    powerSumPower,
    previousAbsErrorMagnitude,
    absErrorMagnitude,
    previousSolution,
    solution
  },
  
  powerSumPowerLimit = powerArg;
  powerSumPowerPower = 1;
  powerSumPower = Power[2, 1 / powerSumPowerPower];
  previousAbsErrorMagnitude = 1000001; (* this is just something really big, in order for initial conditions to work *)
  absErrorMagnitude = 1000000; (* this is just something really big, but not quite as big as previous *)
  
  While[
    powerSumPowerPower <= 6 && previousAbsErrorMagnitude - absErrorMagnitude > 0,
    previousAbsErrorMagnitude = absErrorMagnitude;
    previousSolution = solution;
    solution = getPowerSumSolution[{
      temperedSideGeneratorsPartArg,
      temperedSideMappingPartArg,
      justSideGeneratorsPartArg,
      justSideMappingPartArg,
      eitherSideIntervalsPartArg,
      eitherSideMultiplierPartArg,
      powerSumPower, (* note: this is different *)
      unchangedIntervalsArg
    }];
    absErrorMagnitude = First[solution];
    powerSumPowerPower = powerSumPowerPower += 1;
    powerSumPower = If[powerSumPowerLimit == 1, Power[2, 1 / powerSumPowerPower], Power[2, powerSumPowerPower]];
  ];
  
  {{First[getA[temperedSideGeneratorsPartArg]] /. Last[solution]}, "map"}
];

getPowerSumSolution[tuningMethodArgs_] := Module[
  {
    temperedSideGeneratorsPartArg,
    temperedSideMappingPartArg,
    justSideGeneratorsPartArg,
    justSideMappingPartArg,
    unchangedIntervalsArg,
    powerSum,
    minimizedPowerSum
  },
  
  temperedSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideGeneratorsPartArg"];
  temperedSideMappingPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideMappingPartArg"];
  justSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "justSideGeneratorsPartArg"];
  justSideMappingPartArg = tuningMethodArg[tuningMethodArgs, "justSideMappingPartArg"];
  unchangedIntervalsArg = tuningMethodArg[tuningMethodArgs, "unchangedIntervalsArg"];
  
  powerSum = getPowerSumAbsError[tuningMethodArgs];
  minimizedPowerSum = SetPrecision[
    If[
      ToString[unchangedIntervalsArg] == "Null",
      powerSum,
      {
        powerSum,
        SetPrecision[
          First[getA[multiply[{temperedSideGeneratorsPartArg, temperedSideMappingPartArg, unchangedIntervalsArg}, "map"]]] == First[getA[multiply[{justSideGeneratorsPartArg, justSideMappingPartArg, unchangedIntervalsArg}, "map"]]] /. {gAugmented -> 0},
          nMinimizePrecision
        ]
      }
    ],
    nMinimizePrecision
  ];
  
  NMinimize[minimizedPowerSum, First[getA[temperedSideGeneratorsPartArg]], WorkingPrecision -> nMinimizePrecision]
];

(* 
where the generators part is ¢1LG (tempered) or ¢1LGₚ (just), the mapping part is M (tempered) or Mₚ (just), 
the intervals part is T (non-all-interval) or Tₚ (all-interval), and
the multiplier part is W (non-all-interval) or X⁻¹ (all-interval), finds:
tempered non-all-interval: ¢ 1 L G M T W
tempered all-interval:     ¢ 1 L G M TₚX⁻¹
just non-all-interval:     ¢ 1 L GₚMₚT W 
just all-interval:         ¢ 1 L GₚMₚTₚX⁻¹
in the approximation ¢1LGMTW \[TildeTilde] ¢1LGₚMₚTW or ¢1LGMTₚX⁻¹ \[TildeTilde] ¢1LGₚMₚTₚX⁻¹
where Gₚ = Mₚ = Tₚ = I (identity matrix)
*)
getTemperedOrJustSide[
  temperedOrJustSideGeneratorsPart_,
  temperedOrJustSideMappingPart_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_
] := multiply[{temperedOrJustSideGeneratorsPart, temperedOrJustSideMappingPart, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg}, "map"];
