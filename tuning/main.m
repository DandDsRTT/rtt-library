(* OPTIMIZATION *)

optimizeGeneratorsTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorsTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorsTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    tPossiblyWithChangedIntervalBasis,
    targetedIntervals,
    unchangedIntervals,
    complexitySizeFactor,
    tuningSchemeIntervalBasis,
    pureStretchedInterval,
    logging,
    quick,
    
    useUnchangedIntervalMethod,
    
    tuningMethodArgs,
    powerArg,
    unchangedIntervalsArg,
    
    optimumGeneratorsTuningMap
  },
  
  forDamage = False;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tPossiblyWithChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  tuningSchemeIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "tuningSchemeIntervalBasis"]; (* trait 6 *)
  pureStretchedInterval = tuningSchemeProperty[tuningSchemeProperties, "pureStretchedInterval"]; (* trait 7 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  quick = tuningSchemeProperty[tuningSchemeProperties, "quick"];
  
  useUnchangedIntervalMethod = canUseUnchangedIntervalMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis];
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null" && !useUnchangedIntervalMethod,
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  unchangedIntervalsArg = tuningMethodArg[tuningMethodArgs, "unchangedIntervalsArg"];
  
  optimumGeneratorsTuningMap = TimeConstrained[
    If[
      quick == True,
      Null,
      If[
        ToString[unchangedIntervalsArg] != "Null",
        
        If[
          useUnchangedIntervalMethod,
          
          (* no historically described tuning schemes use this *)
          If[logging == True, printWrapper["\nTUNING METHOD\nunchanged interval"]];
          unchangedIntervalMethod[tuningMethodArgs],
          
          (* covers minimax-E-lol-S "KE", unchanged-octave minimax-E-S "CTE" *)
          If[logging == True, printWrapper["\nTUNING METHOD\npower solver"]];
          powerSumMethod[tuningMethodArgs]
        ],
        
        If[
          powerArg == 2,
          
          (* covers OLD miniRMS-U "least squares", 
          minimax-E-S "TE", minimax-E-copfr-S "Frobenius", pure-stretched-octave minimax-E-S "POTE", 
          minimax-E-lil-S "WE", minimax-E-sopfr-S "BE" *)
          If[logging == True, printWrapper["\nTUNING METHOD\npseudoinverse"]];
          pseudoinverseMethod[tuningMethodArgs],
          
          If[
            powerArg == \[Infinity],
            
            (* covers OLD minimax-U "minimax", 
            minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
            minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
            If[logging == True, printWrapper["\nTUNING METHOD\nmax polytope"]];
            maxPolytopeMethod[tuningMethodArgs],
            
            If[
              powerArg == 1,
              
              (* no historically described tuning schemes use this *)
              If[logging == True, printWrapper["\nTUNING METHOD\nsum polytope"]];
              sumPolytopeMethod[tuningMethodArgs],
              
              (* no historically described tuning schemes use this *)
              If[logging == True, printWrapper["\nTUNING METHOD\npower solver"]];
              powerSumMethod[tuningMethodArgs]
            ]
          ]
        ]
      ]
    ],
    50, (* just enough time to finish the job another way within Wolfram's 60 second window *)
    If[logging == True, printWrapper["aborted due to time constraints"]];
    Null
  ];
  
  (* this only happens if the sum polytope method fails to find a unique optimum generators tuning map, or if a computation takes too long *)
  If[
    optimumGeneratorsTuningMap == Null,
    If[logging == True, printWrapper["falling back to power limit solver"]];
    optimumGeneratorsTuningMap = powerSumLimitMethod[tuningMethodArgs]
  ];
  
  (* for e.g. minimax-lil "Weil" "WE" and minimax-lol "Kees" "KE" tunings, remove the junk final entry from the augmentation *)
  If[
    ToString[targetedIntervals] == "Null" && complexitySizeFactor != 0,
    optimumGeneratorsTuningMap = rowify[Drop[getL[optimumGeneratorsTuningMap], -1]]
  ];
  
  If[logging == True, printWrapper["\nSOLUTION FROM METHOD\n", formatOutput[optimumGeneratorsTuningMap]]];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningSchemeIntervalBasis == "primes",
    optimumGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis];
    If[logging == True, printWrapper["\nRESULT AFTER RETURNING TO PRIMES INTERVAL BASIS\n", formatOutput[optimumGeneratorsTuningMap]]];
  ];
  
  If[
    ToString[pureStretchedInterval] != "Null",
    optimumGeneratorsTuningMap = getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap, t, pureStretchedInterval];
    If[logging == True, printWrapper["\nRESULT AFTER PURE-STRETCHING\n", formatOutput[optimumGeneratorsTuningMap]]];
  ];
  
  If[logging == True, printWrapper[""]];
  
  optimumGeneratorsTuningMap
];

optimizeTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeTuningMapPrivate[t_, tuningSchemeSpec_] := multiplyToRows[optimizeGeneratorsTuningMapPrivate[t, tuningSchemeSpec], t];


(* MEAN DAMAGE *)

getGeneratorsTuningMapMeanDamage[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapMeanDamagePrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorsTuningMap, getM[t]];
  
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
  
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null",
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  formatNumber[getPowerMeanAbsError[tuningMethodArgs]]
];


(* DAMAGES *)

getGeneratorsTuningMapDamages[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapDamagesPrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorsTuningMap, getM[t]];
  
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
  
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  
  tuningMethodArgs = If[
    ToString[targetedIntervals] == "Null",
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  damages = formatNumberL[fixUpZeros[getL[N[getAbsErrors[tuningMethodArgs]]]]];
  targetedIntervals = Map[pcvToQuotient, getA[targetedIntervals]];
  
  MapThread[#1 -> #2&, {targetedIntervals, damages}]
];


(* TARGET SET SCHEMES *)

(* Odd-limit diamond *)
getOld[oddLimit_] := Module[
  {old},
  
  old = DeleteDuplicates[Flatten[Map[
    Function[
      {numerator},
      Map[
        Function[
          {denominator},
          numerator / denominator
        ],
        Range[1, oddLimit, 2]
      ]
    ],
    Range[1, oddLimit, 2]
  ]]];
  old = Select[old, # != 1&];
  old = Map[octaveReduce, old];
  PrependTo[old, 2 / 1];
  
  old
];
octaveReduce[quotient_] := Module[{localQuotient},
  localQuotient = quotient;
  While[localQuotient >= 2, localQuotient /= 2];
  While[localQuotient < 1, localQuotient *= 2];
  
  localQuotient
];

minSize = 15 / 13;
maxSize = 13 / 4;
getTilt[integerLimit_] := Module[
  {integerDiamond, maxComplexity},
  
  integerDiamond = DeleteDuplicates[Flatten[Map[
    Function[
      {numerator},
      Map[
        Function[
          {denominator},
          numerator / denominator
        ],
        Range[1, numerator - 1]
      ]
    ],
    Range[2, integerLimit]
  ]]];
  
  maxComplexity = integerLimit * 13;
  
  Select[
    integerDiamond,
    minSize <= # <= maxSize && Numerator[#] * Denominator[#] <= maxComplexity&
  ]
];

getOtonalChord[harmonicsL_] := DeleteDuplicates[Flatten[MapIndexed[
  Function[
    {denominator, index},
    Map[
      Function[
        {numerator},
        numerator / denominator
      ],
      Drop[harmonicsL, First[index]]
    ]
  ],
  Drop[harmonicsL, -1]
]]];

getComplexityLimit[maxComplexity_, tuningSchemeSpec_] := Module[
  {
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    
    numerator,
    interval,
    complexityLimit,
    intervalsForThisNumerator,
    index,
    intervalComplexity
  },
  
  (* lots of dummy stuff used in this block to let us leverage the processing stuff normally used for entire optimizations of temperaments, just to process the complexity; although actually I think you ought to do something completely different because this doens't even support lain old product ocplexity as far as I can tell... *)
  tuningSchemeOptions = processTuningSchemeSpec[Join[tuningSchemeSpec, {"targetedIntervals" -> "{1}", "damageWeightingSlope" -> "unweighted"}]];
  tuningSchemeProperties = processTuningSchemeOptions[{{{1}}, "map"}, False, tuningSchemeOptions];
  
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 5d *)
  
  numerator = 2;
  complexityLimit = {};
  
  While[
    intervalsForThisNumerator = getIntervalsForThisNumerator[numerator];
    intervalComplexity = getIntervalComplexity[
      First[intervalsForThisNumerator],
      complexityNormPower,
      complexityNegateLogPrimeCoordinator,
      complexityPrimePower,
      complexitySizeFactor,
      complexityMakeOdd
    ];
    intervalComplexity <= maxComplexity,
    
    index = 1;
    While[
      interval = If[index > Length[intervalsForThisNumerator], Null, Part[intervalsForThisNumerator, index]];
      ToString[interval] != "Null" && getIntervalComplexity[
        interval,
        complexityNormPower,
        complexityNegateLogPrimeCoordinator,
        complexityPrimePower,
        complexitySizeFactor,
        complexityMakeOdd
      ] <= maxComplexity,
      
      AppendTo[complexityLimit, interval];
      index++;
    ];
    numerator++;
  ];
  
  DeleteDuplicates[complexityLimit]
];
getIntervalsForThisNumerator[numerator_] := Map[numerator / #&, Range[1, numerator - 1]];
getIntervalComplexity[
  interval_,
  complexityNormPower_,
  complexityNegateLogPrimeCoordinator_,
  complexityPrimePower_,
  complexitySizeFactor_,
  complexityMakeOdd_
] := Module[
  {pcv},
  
  pcv = quotientToPcv[interval];
  
  getComplexity[
    colify[pcv],
    rowify[pcv], (* this is "t"; it just has to have the same dimensionality as the pcv so that the complexity multiplier gets done correctly *)
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd (* trait 5d *)
  ]
];


(* GRAPHING *)

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
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 3 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 5d *)
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      Abs[getL[subtractT[multiplyToRows[generatorsTuningMap, m, targetedIntervalPcv], multiplyToRows[primeCentsMap, targetedIntervalPcv]]]] / getComplexity[
        targetedIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        complexityNormPower, (* trait 4 *)
        complexityNegateLogPrimeCoordinator, (* trait 5a *)
        complexityPrimePower, (* trait 5b *)
        complexitySizeFactor, (* trait 5c *)
        complexityMakeOdd (* trait 5d *)
      ]
    ],
    breakByRowsOrCols[targetedIntervals]
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


(* CONVERSION *)

generatorsTuningMapFromTAndTuningMap[unparsedT_, unparsedTuningMap_] := formatOutput[generatorsTuningMapFromTAndTuningMapPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap]]];
generatorsTuningMapFromTAndTuningMapPrivate[t_, tuningMap_] := Module[
  {generatorsTuningMap, m, primeCentsMap, solution},
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  solution = NMinimize[Norm[getL[multiplyToRows[generatorsTuningMap, m]] - getL[tuningMap]], generatorsTuningMap];
  
  rowify[generatorsTuningMap /. Last[solution]]
];




(* ___ PRIVATE ___ *)



(* TUNING SCHEME OPTIONS *)

linearSolvePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

processTuningSchemeSpec[tuningSchemeSpec_] := If[
  StringQ[tuningSchemeSpec],
  If[
    StringMatchQ[tuningSchemeSpec, RegularExpression["(?:.* )?mini(?:max|RMS|mean|-\\d\\d*-mean)-(?:E-)?(?:\\w+-)?[UCS]"]],
    {"tuningSchemeSystematicName" -> tuningSchemeSpec},
    {"tuningSchemeOriginalName" -> tuningSchemeSpec}
  ],
  tuningSchemeSpec
];

tuningSchemeOptions = {
  "unchangedIntervals" -> Null, (* trait 0 *)
  "targetedIntervals" -> Null, (* trait 1 *)
  "optimizationPower" -> Null, (* trait 2: \[Infinity] = minimax, 2 = miniRMS, 1 = minimean *)
  "damageWeightingSlope" -> "", (* trait 3: unweighted, complexityWeighted, or simplicityWeighted *)
  "complexityNormPower" -> 1, (* trait 4: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "complexityNegateLogPrimeCoordinator" -> False, (* trait 5a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 5b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 5c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "complexityMakeOdd" -> False, (* trait 5d: False = do nothing, True = achieve odd limit from integer limit, etc. *)
  "tuningSchemeIntervalBasis" -> "primes", (* trait 6: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-E-S ("TE") tuning, but it can be used for any tuning *)
  "pureStretchedInterval" -> Null, (* trait 7 *)
  "tuningSchemeSystematicName" -> "",
  "tuningSchemeOriginalName" -> "",
  "damageSystematicName" -> "",
  "damageOriginalName" -> "",
  "complexitySystematicName" -> "",
  "complexityOriginalName" -> "",
  "logging" -> False,
  "quick" -> False
};
Options[processTuningSchemeOptions] = tuningSchemeOptions;
processTuningSchemeOptions[t_, forDamage_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait 0 *)
    targetedIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightingSlope, (* trait 3 *)
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    tuningSchemeIntervalBasis, (* trait 6 *)
    pureStretchedInterval, (* trait 7 *)
    tuningSchemeSystematicName,
    tuningSchemeOriginalName,
    damageSystematicName,
    damageOriginalName,
    complexitySystematicName,
    complexityOriginalName,
    logging,
    quick,
    tPossiblyWithChangedIntervalBasis,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 0 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 1 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 2 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 3 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = OptionValue["complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 5c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 5d *)
  tuningSchemeIntervalBasis = OptionValue["tuningSchemeIntervalBasis"]; (* trait 6 *)
  pureStretchedInterval = OptionValue["pureStretchedInterval"]; (* trait 7 *)
  tuningSchemeSystematicName = OptionValue["tuningSchemeSystematicName"];
  tuningSchemeOriginalName = OptionValue["tuningSchemeOriginalName"];
  damageSystematicName = OptionValue["damageSystematicName"];
  damageOriginalName = OptionValue["damageOriginalName"];
  complexitySystematicName = OptionValue["complexitySystematicName"];
  complexityOriginalName = OptionValue["complexityOriginalName"];
  logging = OptionValue["logging"];
  quick = OptionValue["quick"];
  
  If[
    tuningSchemeOriginalName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted"; targetedIntervals = "OLD"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted"; targetedIntervals = "OLD"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "TOP" || tuningSchemeOriginalName === "TIPTOP" || tuningSchemeOriginalName === "T1" || tuningSchemeOriginalName === "TOP-max" || tuningSchemeOriginalName === "Tenney",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    tuningSchemeOriginalName === "TE" || tuningSchemeOriginalName === "Tenney-Euclidean" || tuningSchemeOriginalName === "T2" || tuningSchemeOriginalName === "TOP-RMS",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "E";
  ];
  If[
    tuningSchemeOriginalName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "copfr-E";
  ];
  If[
    tuningSchemeOriginalName === "BOP" || tuningSchemeOriginalName === "Benedetti",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "sopfr";
  ];
  If[
    tuningSchemeOriginalName === "BE" || tuningSchemeOriginalName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  complexitySystematicName = "sopfr-E";
  ];
  If[
    tuningSchemeOriginalName === "Weil" || tuningSchemeOriginalName === "WOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";complexitySystematicName = "lil";
  ];
  If[
    tuningSchemeOriginalName === "WE" || tuningSchemeOriginalName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "lil-E";
  ];
  If[
    tuningSchemeOriginalName === "Kees" || tuningSchemeOriginalName === "KOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  complexitySystematicName = "lol";
  ];
  If[
    tuningSchemeOriginalName === "KE" || tuningSchemeOriginalName === "Kees-Euclidean",
    (* Note how this tuning scheme works by enforcing an unchanged octave via a solver constraint, rather than through the complexity units multiplier *)
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "lol-E"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTOP" || tuningSchemeOriginalName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; pureStretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "E"; pureStretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "CTE" || tuningSchemeOriginalName === "Constrained Tenney-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexitySystematicName = "E"; unchangedIntervals = "octave";
  ];
  
  If[
    damageOriginalName === "topDamage",
    damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    complexityOriginalName === "copfr" || complexityOriginalName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "sopfr" || complexityOriginalName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 1; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "integerLimit" || complexityOriginalName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "oddLimit" || complexityOriginalName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  If[
    complexityOriginalName === "logProduct" || complexityOriginalName === "tenneyHeight" || complexityOriginalName === "harmonicDistance",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "logIntegerLimit" || complexityOriginalName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "logOddLimit" || complexityOriginalName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    complexityOriginalName === "rososcopfr" || complexityOriginalName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "rosossopfr",
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
    complexityOriginalName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    complexityOriginalName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    complexityOriginalName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordinator = True; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
  (* trait 0 - unchanged intervals *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+.*"]],
    unchangedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["unchanged\\-(\\{[\\w\\s\\,\\/]+\\}|[\\w\\/]+)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 1 - targeted intervals *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*all-interval*"] || (StringMatchQ[tuningSchemeSystematicName, "*minimax*"] && StringMatchQ[tuningSchemeSystematicName, "*S*"]),
    targetedIntervals = {};
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*odd-limit-diamond*"],
    targetedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*odd-limit-diamond)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*OLD*"],
    targetedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*OLD)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*truncated-integer-limit-triangle*"],
    targetedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*truncated-integer-limit-triangle)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*TILT*"],
    targetedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*TILT)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*primes*"],
    targetedIntervals = "primes";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?\\{[\\d\\/\\,\\s]*\\}\\s+.*"]],
    targetedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?(\\{[\\d\\/\\,\\s]*\\})\\s+.*"] -> "$1"]],
  ];
  
  (* trait 2 - optimization power *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*minimax*"],
    optimizationPower = \[Infinity];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*miniRMS*"],
    optimizationPower = 2;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*minimean*"],
    optimizationPower = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression[".*mini-(\\d)-mean.*"]],
    optimizationPower = ToExpression[First[StringCases[tuningSchemeSystematicName, RegularExpression[".*mini-(\\d)-mean.*"] -> "$1"]]];
  ];
  
  (* trait 3 - damage weighting slope *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-S*"] || StringMatchQ[damageSystematicName, "*S-*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-C*"] || StringMatchQ[damageSystematicName, "*C-*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-U*"] || StringMatchQ[damageSystematicName, "*U-*"],
    damageWeightingSlope = "unweighted";
  ];
  
  (* trait 4 - interval complexity norm power *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-E-*"] || StringMatchQ[damageSystematicName, "*E-*"] || StringMatchQ[complexitySystematicName, "*E*"],
    complexityNormPower = 2;
  ];
  
  (* trait 5 - interval complexity coordinate change *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-copfr-*"] || StringMatchQ[damageSystematicName, "*copfr-*"] || StringMatchQ[complexitySystematicName, "*copfr*"],
    complexityNegateLogPrimeCoordinator = True;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-sopfr-*"] || StringMatchQ[damageSystematicName, "*sopfr-*"] || StringMatchQ[complexitySystematicName, "*sopfr*"],
    complexityNegateLogPrimeCoordinator = True; complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lil-*"] || StringMatchQ[damageSystematicName, "*lil-*"] || StringMatchQ[complexitySystematicName, "*lil*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lol-*"] || StringMatchQ[damageSystematicName, "*lol-*"] || StringMatchQ[complexitySystematicName, "*lol*"],
    complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  
  (* trait 6 - tuning scheme interval basis *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*formal-primes-basis*"],
    tuningSchemeIntervalBasis = "primes";
  ];
  
  (* trait 7 - pure-stretched interval *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["pure\\-stretched\\-\\S+\\s+.*"]],
    pureStretchedInterval = First[StringCases[tuningSchemeSystematicName, RegularExpression["pure\\-stretched\\-(\\S+)\\s+.*"] -> "$1"]];
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
    unchangedIntervals = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[unchangedIntervals, t]];
    targetedIntervals = rebase[intervalRebase, processTargetedIntervals[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage, unchangedIntervals]];
    pureStretchedInterval = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t]],
    
    tPossiblyWithChangedIntervalBasis = t;
    unchangedIntervals = processUnchangedOrPureStretchedIntervals[unchangedIntervals, t];
    targetedIntervals = processTargetedIntervals[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage, unchangedIntervals];
    pureStretchedInterval = processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t];
  ];
  
  If[
    logging == True,
    printWrapper["\nTUNING SCHEME OPTIONS"];
    printWrapper["tPossiblyWithChangedIntervalBasis: ", formatOutput[tPossiblyWithChangedIntervalBasis]];
    printWrapper["unchangedIntervals: ", formatOutput[unchangedIntervals]]; (* trait 0 *)
    printWrapper["targetedIntervals: ", formatOutput[targetedIntervals]]; (* trait 1 *)
    printWrapper["optimizationPower: ", formatOutput[optimizationPower]]; (* trait 2 *)
    printWrapper["damageWeightingSlope: ", formatOutput[damageWeightingSlope]]; (* trait 3 *)
    printWrapper["complexityNormPower: ", formatOutput[complexityNormPower]]; (* trait 4 *)
    printWrapper["complexityNegateLogPrimeCoordinator: ", formatOutput[complexityNegateLogPrimeCoordinator]]; (* trait 5a *)
    printWrapper["complexityPrimePower: ", formatOutput[complexityPrimePower]]; (* trait 5b *)
    printWrapper["complexitySizeFactor: ", formatOutput[complexitySizeFactor]]; (* trait 5c *)
    printWrapper["complexityMakeOdd: ", formatOutput[complexityMakeOdd]]; (* trait 5d *)
    printWrapper["tuningSchemeIntervalBasis: ", formatOutput[tuningSchemeIntervalBasis]]; (* trait 6 *)
    printWrapper["pureStretchedInterval: ", formatOutput[pureStretchedInterval]]; (* trait 7 *)
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
    Throw["It is not possible to optimize for minimean or miniRMS over all intervals, only minimax."]
  ];
  If[
    ToString[targetedIntervals] == "Null" && damageWeightingSlope != "simplicityWeighted" && !canUseUnchangedIntervalMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis],
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."]
  ];
  
  {
    tPossiblyWithChangedIntervalBasis,
    unchangedIntervals, (* trait 0 *)
    targetedIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightingSlope, (* trait 3 *)
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    tuningSchemeIntervalBasis, (* trait 6 *)
    pureStretchedInterval, (* trait 7 *)
    logging,
    quick
  }
];

tuningSchemePropertiesPartsByOptionName = <|
  "t" -> 1,
  "unchangedIntervals" -> 2, (* trait 0 *)
  "targetedIntervals" -> 3, (* trait 1 *)
  "optimizationPower" -> 4, (* trait 2 *)
  "damageWeightingSlope" -> 5, (* trait 3 *)
  "complexityNormPower" -> 6, (* trait 4 *)
  "complexityNegateLogPrimeCoordinator" -> 7, (* trait 5a *)
  "complexityPrimePower" -> 8, (* trait 5b *)
  "complexitySizeFactor" -> 9, (* trait 5c *)
  "complexityMakeOdd" -> 10, (* trait 5d *)
  "tuningSchemeIntervalBasis" -> 11, (* trait 6 *)
  "pureStretchedInterval" -> 12, (* trait 7 *)
  "logging" -> 13,
  "quick" -> 14
|>;
tuningSchemeProperty[tuningSchemeProperties_, optionName_] := Part[tuningSchemeProperties, tuningSchemePropertiesPartsByOptionName[optionName]];

processTargetedIntervals[targetedIntervals_, t_, tPossiblyWithChangedIntervalBasis_, forDamage_, unchangedIntervals_] := If[
  ToString[targetedIntervals] == "Null",
  If[
    canUseUnchangedIntervalMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis],
    Null,
    Throw["no targeted intervals"]
  ],
  If[
    ToString[targetedIntervals] == "{}",
    If[
      forDamage,
      getFormalPrimes[tPossiblyWithChangedIntervalBasis],
      Null
    ],
    If[
      StringQ[targetedIntervals] && (StringMatchQ[targetedIntervals, "*odd-limit-diamond*"] || StringMatchQ[targetedIntervals, "*OLD*"]),
      processOld[targetedIntervals, tPossiblyWithChangedIntervalBasis],
      If[
        StringQ[targetedIntervals] && (StringMatchQ[targetedIntervals, "*truncated-integer-limit-triangle*"] || StringMatchQ[targetedIntervals, "*TILT*"]),
        processTilt[targetedIntervals, tPossiblyWithChangedIntervalBasis],
        If[
          ToString[targetedIntervals] == "primes",
          colify[IdentityMatrix[getDPrivate[tPossiblyWithChangedIntervalBasis]]],
          colify[getA[parseQuotientL[targetedIntervals, t]]]
        ]
      ]
    ]
  ]
];

processOld[targetedIntervals_, tPossiblyWithChangedIntervalBasis_] := Module[
  {d, maybeOddLimit, old},
  
  d = getD[tPossiblyWithChangedIntervalBasis];
  
  maybeOddLimit = First[StringCases[StringReplace[targetedIntervals, "odd-limit-diamond" -> "OLD"], RegularExpression["(\\d*)-?OLD"] -> "$1"]];
  
  old = If[
    maybeOddLimit == "",
    getOld[Prime[d + 1] - 2], (* default to odd immediately before the prime that is the next prime after the temperament's prime limit *)
    getOld[ToExpression[maybeOddLimit]]
  ];
  
  colify[padVectorsWithZerosUpToD[
    Map[
      quotientToPcv,
      old
    ],
    d
  ]]
];

processTilt[targetedIntervals_, tPossiblyWithChangedIntervalBasis_] := Module[
  {d, maybeMaxInteger, tid},
  
  d = getD[tPossiblyWithChangedIntervalBasis];
  
  maybeMaxInteger = First[StringCases[StringReplace[targetedIntervals, "truncated-integer-limit-triangle" -> "TILT"], RegularExpression["(\\d*)-?TILT"] -> "$1"]];
  
  tid = If[
    maybeMaxInteger == "",
    getTilt[Prime[d + 1] - 1], (* default to integer immediately before the prime that is the next prime after the temperament's prime limit *)
    getTilt[ToExpression[maybeMaxInteger]]
  ];
  
  colify[padVectorsWithZerosUpToD[
    Map[
      quotientToPcv,
      tid
    ],
    d
  ]]
];

processUnchangedOrPureStretchedIntervals[unchangedOrPureStretchedIntervals_, t_] := If[
  ToString[unchangedOrPureStretchedIntervals] == "Null",
  Null,
  If[
    ToString[unchangedOrPureStretchedIntervals] == "octave",
    getOctave[t],
    parseQuotientL[unchangedOrPureStretchedIntervals, t]
  ]
];

rebase[intervalRebase_, t_] := If[t == Null, t, multiplyToRows[intervalRebase, t]];


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
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  temperedSideGeneratorsPartArg = generatorsTuningMap;
  temperedSideMappingPartArg = m;
  justSideGeneratorsPartArg = primeCentsMap;
  justSideMappingPartArg = getPrimesI[t];
  eitherSideIntervalsPartArg = targetedIntervals;
  eitherSideMultiplierPartArg = If[ToString[eitherSideIntervalsPartArg] == "Null", Null, getDamageWeights[tuningSchemeProperties]];
  powerArg = optimizationPower;
  unchangedIntervalsArg = unchangedIntervals;
  
  If[
    logging == True,
    printWrapper["\nTUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* T *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* W *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
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

getOctave[t_] := colify[Join[{1}, Table[0, getDPrivate[t] - 1]]];

getSummationMap[t_] := rowify[Table[1, getDPrivate[t]]];

getLogPrimeCoordinator[t_] := rowify[DiagonalMatrix[Log2[getIntervalBasis[t]]]];

getPrimeCentsMap[t_] := scale[
  multiplyToRows[
    getSummationMap[t],
    getLogPrimeCoordinator[t]
  ],
  1200
];

getPrimesI[t_] := rowify[IdentityMatrix[getDPrivate[t]]];

getTuningSchemeMappings[t_] := Module[
  {generatorsTuningMap, m, primeCentsMap},
  
  generatorsTuningMap = rowify[Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getRPrivate[t]}]];
  m = getM[t];
  primeCentsMap = getPrimeCentsMap[t];
  
  {generatorsTuningMap, m, primeCentsMap}
];

(* similar to pseudoinverse, but works for any tuning so far described *)
tuningInverse[damageWeightsOrComplexityMultiplier_] := rowify[MapThread[
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
]];


(* DAMAGE *)

(* compare with getDualMultiplier *)
getDamageWeights[tuningSchemeProperties_] := Module[
  {
    t,
    targetedIntervals, (* trait 1 *)
    damageWeightingSlope, (* trait 3 *)
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    
    damageWeights
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervals"]; (* trait 1 *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 3 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 5d *)
  
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    rowify[IdentityMatrix[Length[getA[targetedIntervals]]]],
    
    rowify[DiagonalMatrix[Map[
      Function[
        {targetedIntervalPcv},
        getComplexity[
          targetedIntervalPcv,
          t,
          complexityNormPower, (* trait 4 *)
          complexityNegateLogPrimeCoordinator, (* trait 5a *)
          complexityPrimePower, (* trait 5b *)
          complexitySizeFactor, (* trait 5c *)
          complexityMakeOdd (* trait 5d *)
        ]
      ],
      breakByRowsOrCols[targetedIntervals]
    ]]]
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
  Max[getL[getAbsErrors[tuningMethodArgs]]],
  
  Total[Power[
    getL[getAbsErrors[tuningMethodArgs]],
    tuningMethodArg[tuningMethodArgs, "powerArg"]
  ]]
];

getPowerMeanAbsError[tuningMethodArgs_] := Module[
  {absErrors, powerArg, targetedIntervalCount, result},
  
  absErrors = getAbsErrors[tuningMethodArgs];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  targetedIntervalCount = First[Dimensions[getA[tuningMethodArg[tuningMethodArgs, "eitherSideIntervalsPartArg"]]]]; (* k *)
  
  If[debug == True, printWrapper["absErrors: ", absErrors]];
  
  result = If[
    powerArg == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
    Max[getL[absErrors]],
    
    Power[
      Total[Power[
        getL[absErrors],
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
  
  absErrors = rowify[Abs[N[
    fixUpZeros[getL[temperedSide] - getL[justSide]],
    absoluteValuePrecision
  ]]];
  
  If[
    debug == True,
    printWrapper["temperedSide: ", formatOutput[temperedSide]];
    printWrapper["justSide: ", formatOutput[justSide]];
    printWrapper["absErrors: ", formatOutput[Map[If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, 4]]&, getL[absErrors]]]];
  ];
  
  absErrors
];

(* COMPLEXITY *)

(* returns complexities in weighted octaves *)
getComplexity[
  pcv_,
  t_,
  complexityNormPower_, (* trait 4 *)
  complexityNegateLogPrimeCoordinator_, (* trait 5a *)
  complexityPrimePower_, (* trait 5b *)
  complexitySizeFactor_, (* trait 5c *)
  complexityMakeOdd_ (* trait 5d *)
] := Module[
  {complexityMultiplierAndLogPrimeCoordinator},
  
  complexityMultiplierAndLogPrimeCoordinator = getComplexityMultiplierAndLogPrimeCoordinator[
    t,
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd (* trait 5d *)
  ];
  
  Norm[
    getL[multiplyToCols[
      complexityMultiplierAndLogPrimeCoordinator,
      pcv
    ]],
    complexityNormPower
  ] / (1 + complexitySizeFactor)
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
it covers any non-all-interval tuning scheme using this for its damage's complexity *)
getComplexityMultiplier[
  t_,
  complexityNegateLogPrimeCoordinator_, (* trait 5a *)
  complexityPrimePower_, (* trait 5b *)
  complexitySizeFactor_, (* trait 5c *)
  complexityMakeOdd_ (* trait 5d *)
] := Module[{complexityMultiplier},
  (* when used by getDualMultiplier in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-S ("TOP") and minimax-E-S ("TE") *)
  complexityMultiplier = rowify[IdentityMatrix[getDPrivate[t]]];
  
  If[
    (* when used by getDualMultiplier in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-copfr-S (the L1 version of "Frobenius") and minimax-E-copfr-S ("Frobenius") *)
    complexityNegateLogPrimeCoordinator == True,
    complexityMultiplier = multiplyToRows[
      complexityMultiplier,
      inverse[getLogPrimeCoordinator[t]]
    ]
  ];
  
  If[
    (* when used by getDualMultiplier in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-sopfr-S ("BOP") and minimax-E-sopfr-S ("BE") *)
    complexityPrimePower > 0,
    complexityMultiplier = multiplyToRows[
      complexityMultiplier,
      rowify[DiagonalMatrix[
        Power[
          getIntervalBasis[t],
          complexityPrimePower
        ]
      ]]
    ]
  ];
  
  If[
    (* when used by getDualMultiplier in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-lil-S ("Weil"), minimax-E-lil-S ("WE"), minimax-lol-S ("Kees"), and minimax-E-lol-S ("KE")
    (yes, surprisingly, when computing minimax-lol-S and minimax-E-lol-S tunings, we do not use the below, though user calls for odd-limit complexity do use it;
    the tuning calculations instead use only this size-sensitizer effect, and apply an unchanged octave constraint to achieve the oddness aspect) *)
    complexitySizeFactor > 0,
    complexityMultiplier = multiplyToRows[
      rowify[Join[
        getA[getPrimesI[t]],
        {Table[
          complexitySizeFactor,
          getDPrivate[t]
        ]}
      ]],
      complexityMultiplier
    ]
  ];
  
  If[
    (* When minimax-lol-S ("Kees") and minimax-E-lol-S ("KE") need their dual norms, they don't use this; see note above *)
    complexityMakeOdd == True,
    complexityMultiplier = multiplyToRows[
      complexityMultiplier,
      rowify[DiagonalMatrix[
        Join[
          {0},
          Table[1, getDPrivate[t] - 1]
        ]
      ]]
    ]
  ];
  
  complexityMultiplier
];

getComplexityMultiplierAndLogPrimeCoordinator[
  t_,
  complexityNegateLogPrimeCoordinator_, (* trait 5a *)
  complexityPrimePower_, (* trait 5b *)
  complexitySizeFactor_, (* trait 5c *)
  complexityMakeOdd_ (* trait 5d *)
] := multiplyToRows[
  getComplexityMultiplier[
    t,
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd (* trait 5d *)
  ],
  getLogPrimeCoordinator[t]
];


(* ALL-INTERVAL *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningSchemeProperties_] := Module[
  {
    t,
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    
    complexityMultiplierAndLogPrimeCoordinator
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 5d *)
  
  complexityMultiplierAndLogPrimeCoordinator = getComplexityMultiplierAndLogPrimeCoordinator[
    t,
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd (* trait 5d *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplierAndLogPrimeCoordinator]
];

(* compare with getTuningMethodArgs *)
getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    unchangedIntervals,
    complexityNormPower,
    complexitySizeFactor,
    logging,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    primesI,
    transposedPrimesI,
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
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  primesI = getPrimesI[t];
  transposedPrimesI = transpose[primesI];
  dualMultiplier = getDualMultiplier[tuningSchemeProperties];
  primesErrorMagnitudeNormPower = getDualPower[complexityNormPower];
  
  If[
    complexitySizeFactor != 0,
    
    temperedSideGeneratorsPartArg = augmentedTemperedSideGeneratorsPartArg[generatorsTuningMap];
    temperedSideMappingPartArg = augmentedTemperedSideMappingPartArg[m, complexitySizeFactor];
    justSideGeneratorsPartArg = augmentedJustSideGeneratorsPartArg[primeCentsMap];
    justSideMappingPartArg = augmentedJustSideMappingPartArg[primesI];
    eitherSideIntervalsPartArg = augmentedEitherSideIntervalsPartArg[transposedPrimesI];
    eitherSideMultiplierPartArg = augmentedEitherSideMultiplierPartArg[dualMultiplier];
    unchangedIntervalsArg = augmentedUnchangedIntervalsArg[unchangedIntervals];
    powerArg = primesErrorMagnitudeNormPower, (* doesn't make sense to augment a power *)
    
    temperedSideGeneratorsPartArg = generatorsTuningMap;
    temperedSideMappingPartArg = m;
    justSideGeneratorsPartArg = primeCentsMap;
    justSideMappingPartArg = primesI;
    eitherSideIntervalsPartArg = transposedPrimesI;
    eitherSideMultiplierPartArg = dualMultiplier;
    unchangedIntervalsArg = unchangedIntervals;
    powerArg = primesErrorMagnitudeNormPower;
  ];
  
  If[
    logging == True,
    printWrapper["\n(ALL-INTERVAL TUNING SCHEME) TUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* g *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* M *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* p *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* Mₚ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* Tₚ *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* X⁻¹ *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
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

augmentedTemperedSideGeneratorsPartArg[generatorsTuningMap_] := rowify[Join[
  getL[generatorsTuningMap],
  {Symbol["gAugmented"]}
]];

augmentedTemperedSideMappingPartArg[m_, complexitySizeFactor_] := Module[
  {d, temperedSideMappingPartArg, mappingAugmentation},
  
  d = getD[m];
  temperedSideMappingPartArg = rowify[Map[Join[#, {0}]&, getA[m]]];
  mappingAugmentation = {Join[
    getL[multiplyToRows[
      rowify[Table[complexitySizeFactor, d]],
      getLogPrimeCoordinator[m]
    ]],
    {-1}
  ]};
  
  rowify[Join[getA[temperedSideMappingPartArg], mappingAugmentation]]
];

augmentedJustSideGeneratorsPartArg[primeCentsMap_] := rowify[Join[
  getL[primeCentsMap],
  {0}
]];

augmentedJustSideMappingPartArg[primesI_] := Module[
  {a, augmentedA},
  
  a = getA[primesI];
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  rowify[augmentedA]
];

augmentedEitherSideIntervalsPartArg[transposedPrimesI_] := Module[
  {a, augmentedA},
  
  a = getA[transposedPrimesI];
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  colify[augmentedA]
];

augmentedEitherSideMultiplierPartArg[dualMultiplier_] := rowify[Join[
  getA[dualMultiplier],
  {Join[
    Table[
      0,
      Last[Dimensions[getA[dualMultiplier]]] - 1
    ],
    {1}
  ]}
]];

augmentedUnchangedIntervalsArg[unchangedIntervals_] := If[
  ToString[unchangedIntervals] == "Null",
  unchangedIntervals,
  colify[Map[
    Join[#, {0}]&,
    getA[unchangedIntervals]
  ]]
];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorsPreimageTransversal, f},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorsTuningMap, m];
  generatorsPreimageTransversal = getGeneratorsPreimageTransversalPrivate[originalT];
  f = getFormalPrimes[originalT];
  
  multiplyToRows[optimumTuningMap, f, generatorsPreimageTransversal]
];


(* PURE-STRETCHED INTERVAL *)

getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedInterval_] := Module[
  {
    generatorsTuningMap,
    m,
    primeCentsMap,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiplyToCols[primeCentsMap, pureStretchedInterval];
  temperedIntervalSize = multiplyToCols[optimumGeneratorsTuningMap, m, pureStretchedInterval];
  
  rowify[(justIntervalSize / temperedIntervalSize) * getL[optimumGeneratorsTuningMap]]
];


(* UNCHANGED INTERVALS *)

canUseUnchangedIntervalMethod[unchangedIntervals_, t_] := ToString[unchangedIntervals] != "Null" && Length[getA[unchangedIntervals]] == getR[t];


(* METHODS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers unchanged-octave OLD minimax-U "minimax", minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
(* a semi-analytical method *)
(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
maxPolytopeMethod[{
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
    undoMinimaxLocksForJustSide
  },
  
  (* the mapped and weighted targeted intervals on one side, and the just and weighted targeted intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
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
  undoMinimaxLocksForTemperedSide = rowify[IdentityMatrix[generatorCount]];
  undoMinimaxLocksForJustSide = rowify[Table[0, generatorCount]];
  
  While[
    (* a unique optimum has not yet been found *)
    Length[minimaxTunings] > 1,
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    minimaxLockForJustSide = First[minimaxTunings];
    (* list of differences between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one *)
    minimaxLockForTemperedSide = rowify[Map[
      getL[Part[minimaxTunings, #]] - getL[minimaxLockForJustSide]&,
      Range[2, Length[minimaxTunings]]
    ]];
    
    (* apply the minimax-damage-locking transformation to the just side, and track it to undo later *)
    justSide = subtractT[justSide, multiplyToRows[minimaxLockForJustSide, temperedSideButWithoutGeneratorsPart]];
    undoMinimaxLocksForJustSide = addT[
      undoMinimaxLocksForJustSide,
      multiplyToRows[minimaxLockForJustSide, undoMinimaxLocksForTemperedSide]
    ];
    
    (* apply the minimax-damage-locking transformation to the tempered side, and track it to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the justSide, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    temperedSideButWithoutGeneratorsPart = multiplyToRows[minimaxLockForTemperedSide, temperedSideButWithoutGeneratorsPart];
    undoMinimaxLocksForTemperedSide = multiplyToRows[minimaxLockForTemperedSide, undoMinimaxLocksForTemperedSide];
    
    (* search again, now in this transformed state *)
    minimaxTunings = findAllNestedMinimaxTuningsFromMaxPolytopeVertices[temperedSideButWithoutGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];
    maxCountOfNestedMinimaxibleDamages += generatorCount + 1;
  ];
  
  If[
    Length[minimaxTunings] == 1,
    addT[
      undoMinimaxLocksForJustSide,
      multiplyToRows[First[minimaxTunings], undoMinimaxLocksForTemperedSide] (* here's that left-multiplication mentioned earlier *)
    ],
    Null
  ]
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
      rowify[Quiet[Check[
        LinearSolve[
          N[
            getA[multiplyToRows[
              vertexConstraint,
              transpose[temperedSideButWithoutGeneratorsPart]
            ]],
            linearSolvePrecision
          ],
          N[
            getL[multiplyToRows[
              justSide,
              transpose[vertexConstraint]
            ]],
            linearSolvePrecision
          ]
        ],
        "err"
      ]]]
    ],
    {vertexConstraint, vertexConstraints}
  ];
  
  (* each damages list is sorted in descending order; 
  the list of lists itself is sorted corresponding to the candidate tunings *)
  sortedDamagesByCandidateTuning = Quiet[Map[
    Function[
      {candidateTuning},
      If[
        ToString[getL[candidateTuning]] == "err",
        "err",
        {Abs[fixUpZeros[getL[subtractT[
          multiplyToRows[candidateTuning, temperedSideButWithoutGeneratorsPart],
          justSide
        ]]]]}
      ]
    ],
    candidateTunings
  ]];
  
  If[
    debug == True,
    MapThread[
      printWrapper["constraint matrix: ", formatOutput[#1], " tuning: ", formatOutput[#2], " damages: ", formatOutput[#3]]&,
      {vertexConstraints, candidateTunings, sortedDamagesByCandidateTuning}
    ]
  ];
  
  (* ignore the problems that are singular and therefore have no solution *)
  candidateTunings = Select[candidateTunings, !TrueQ[# == rowify["err"]]&];
  sortedDamagesByCandidateTuning = Select[sortedDamagesByCandidateTuning, !TrueQ[# == "err"]&];
  sortedDamagesByCandidateTuning = Map[rowify[ReverseSort[getL[#]]]&, sortedDamagesByCandidateTuning];
  
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
    nthmostMinDamage = Min[Map[Part[getL[#], targetIndex]&, sortedDamagesByCandidateTuning]];
    Do[
      (* having found the minimum damage for this target index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[getL[sortedDamagesForThisCandidateTuning], targetIndex] <= nthmostMinDamage + 0.000000001,
        
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
    candidateTunings,
    Function[{tuningA, tuningB}, AllTrue[MapThread[#1 == #2&, {getL[tuningA], getL[tuningB]}], TrueQ]]
  ]
];

fixUpZeros[l_] := Map[
  If[Quiet[PossibleZeroQ[#]], 0, #]&,
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
  targetCombinations = Subsets[Range[1, targetCount], {generatorCount + 1}];
  targetCombinations = If[
    Length[targetCombinations] * Power[generatorCount, 2] * targetCount > 275000,
    If[debug == True, printWrapper["pre-emptively aborting the analytical solution because we estimate it will exceed the time limit"]];
    {},
    targetCombinations
  ]; (* anything above this is likely to exceed the time limit, so might as well save time *)
  
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
  Map[rowify, vertexConstraintAs]
];


(* METHODS: OPTIMIZATION POWER = 1 (MINIMEAN) OR COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tuning schemes use this *)
(* an analytical method *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where unchanged-octave OLD minimax-U "minimax" is described;
however, this computation method is in general actually for minimean tuning schemes, not minimax tuning schemes. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond targeted interval set,
namely that the places where damage to targets are equal is the same where other targets are pure.
*)
sumPolytopeMethod[{
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
    canonicalizedCandidateUnchangedIntervalSets,
    filteredCanonicalizedCandidateUnchangedIntervalSets,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets,
    candidateOptimumGenerators,
    candidateOptimumGeneratorsTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorsTuningMapIndices,
    optimumGeneratorsTuningMapIndex
  },
  
  generatorCount = First[Dimensions[getA[temperedSideMappingPartArg]]]; (* First[], not Last[], because it's not transposed here. *)
  
  unchangedIntervalSetIndices = Subsets[
    Range[First[Dimensions[getA[eitherSideIntervalsPartArg]]]],
    {generatorCount}
  ];
  candidateUnchangedIntervalSets = Map[
    colify[Map[
      getA[eitherSideIntervalsPartArg][[#]]&,
      #
    ]]&,
    unchangedIntervalSetIndices
  ];
  canonicalizedCandidateUnchangedIntervalSets = Map[canonicalFormPrivate, candidateUnchangedIntervalSets];
  filteredCanonicalizedCandidateUnchangedIntervalSets = Select[canonicalizedCandidateUnchangedIntervalSets, MatrixRank[Transpose[getA[#]]] == generatorCount&];
  dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets = DeleteDuplicates[filteredCanonicalizedCandidateUnchangedIntervalSets];
  candidateOptimumGenerators = Select[Map[
    getGeneratorsAFromUnchangedIntervals[temperedSideMappingPartArg, #]&,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets
  ], Not[# === Null]&];
  candidateOptimumGeneratorsTuningMaps = Map[multiplyToRows[justSideGeneratorsPartArg, #]&, candidateOptimumGenerators];
  candidateOptimumGeneratorTuningMapAbsErrors = Map[
    Total[getL[getAbsErrors[{
      #, (* note: this is an override for temperedSideGeneratorsPartArg, and it's the only reason why these tuning method args need to be unpacked *)
      temperedSideMappingPartArg,
      justSideGeneratorsPartArg,
      justSideMappingPartArg,
      eitherSideIntervalsPartArg,
      eitherSideMultiplierPartArg,
      powerArg,
      unchangedIntervalsArg
    }]]]&,
    candidateOptimumGeneratorsTuningMaps
  ];
  
  If[
    debug == True,
    printWrapper["candidateUnchangedIntervalSets: ", Map[formatOutput, candidateUnchangedIntervalSets]];
    printWrapper["canonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, canonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["filteredCanonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, filteredCanonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["candidateOptimumGenerators: ", Map[formatOutput, candidateOptimumGenerators]];
    printWrapper["candidateOptimumGeneratorsTuningMaps: ", Map[formatOutput, candidateOptimumGeneratorsTuningMaps]];
    printWrapper["candidateOptimumGeneratorTuningMapAbsErrors: ", Map[formatOutput, candidateOptimumGeneratorTuningMapAbsErrors]];
  ];
  
  optimumGeneratorsTuningMapIndices = Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]];
  If[
    Length[optimumGeneratorsTuningMapIndices] == 1,
    
    (* result is unique; done *)
    optimumGeneratorsTuningMapIndex = First[First[Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]]]];
    maybeRowify[candidateOptimumGeneratorsTuningMaps[[optimumGeneratorsTuningMapIndex]]],
    
    (* result is non-unique, will need to handle otherwise *)
    Null
  ]
];

getGeneratorsAFromUnchangedIntervals[m_, unchangedIntervalEigenvectors_] := Module[
  {mappedUnchangedIntervalEigenvectors},
  
  mappedUnchangedIntervalEigenvectors = multiplyToCols[m, unchangedIntervalEigenvectors];
  
  If[
    Det[getA[mappedUnchangedIntervalEigenvectors]] == 0,
    Null,
    multiplyToCols[unchangedIntervalEigenvectors, inverse[mappedUnchangedIntervalEigenvectors]]
  ]
];


(* METHODS: OPTIMIZATION POWER = 2 (MINIRMS) OR COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical method *)
(* covers unchanged-octave OLD miniRMS-U "least squares", minimax-E-S "TE", pure-stretched-octave minimax-E-S "POTE",
minimax-E-copfr-S "Frobenius", minimax-E-lil-S "WE", minimax-E-sopfr-S "BE" *)
pseudoinverseMethod[{
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
  
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  
  If[
    debug == True,
    printWrapper["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart];
    printWrapper["transpose[%]: ", transpose[temperedSideButWithoutGeneratorsPart]];
    printWrapper["multiplyToCols[%1, %2]: ", formatOutput[multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]]];
    printWrapper["inverse[%]: ", formatOutput[inverse[multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]]]];
    printWrapper["multiplyToRows[transpose[temperedSideButWithoutGeneratorsPart], %]: ", (*formatOutput[*)multiplyToRows[transpose[temperedSideButWithoutGeneratorsPart], inverse[multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]]](*]*)];
    printWrapper["multiplyToRows[justSide, %]: ", formatOutput[multiplyToRows[justSide, multiplyToRows[transpose[temperedSideButWithoutGeneratorsPart], inverse[multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]]]]]];
    printWrapper["justSide: ", formatOutput[justSide]];
  ];
  
  (* Technically the Aᵀ(AAᵀ)⁻¹ type of pseudoinverse is necessary. Wolfram's built-in will sometimes use other techniques, which do not give the correct answer. *)
  maybeRowify[multiplyToRows[
    justSide,
    multiplyToRows[
      transpose[temperedSideButWithoutGeneratorsPart],
      inverse[multiplyToCols[
        temperedSideButWithoutGeneratorsPart,
        transpose[temperedSideButWithoutGeneratorsPart]
      ]]
    ]
  ]]
];


(* METHODS: GENERAL OPTIMIZATION POWER (MINI-P-MEAN) OR GENERAL PRIMES ERROR MAGNITUDE NORM POWER (MINI-P-NORM) *)

(* a numerical method *)
(* covers minimax-E-lol-S "KE", unchanged-octave minimax-E-S "CTE" *)
powerSumMethod[tuningMethodArgs_] := Module[
  {temperedSideGeneratorsPartArg, solution},
  
  temperedSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideGeneratorsPartArg"];
  
  solution = getPowerSumSolution[tuningMethodArgs];
  
  rowify[getL[temperedSideGeneratorsPartArg] /. Last[solution]]
];

(* no historically described tuning schemes use this *)
(* a numerical method *)
(* this is the fallback for when sumPolytopeMethod fails to find a unique solution *)
powerSumLimitMethod[{
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
      powerSumPower, (* note: this is different than the usual `powerArg`, but derived from it *)
      unchangedIntervalsArg
    }];
    absErrorMagnitude = First[solution];
    powerSumPowerPower = powerSumPowerPower += 1;
    powerSumPower = If[powerSumPowerLimit == 1, Power[2, 1 / powerSumPowerPower], Power[2, powerSumPowerPower]];
  ];
  
  rowify[getL[temperedSideGeneratorsPartArg] /. Last[solution]]
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
          getL[multiplyToRows[temperedSideGeneratorsPartArg, temperedSideMappingPartArg, unchangedIntervalsArg]] == getL[multiplyToRows[justSideGeneratorsPartArg, justSideMappingPartArg, unchangedIntervalsArg]] /. {gAugmented -> 0},
          nMinimizePrecision
        ]
      }
    ],
    nMinimizePrecision
  ];
  
  NMinimize[minimizedPowerSum, getL[temperedSideGeneratorsPartArg], WorkingPrecision -> nMinimizePrecision]
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
] := multiplyToRows[temperedOrJustSideGeneratorsPart, temperedOrJustSideMappingPart, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];

(* no historically described tuning schemes use this *)
(* an analytical method *)
(* G = U(MU)⁻¹; g = pG *)
unchangedIntervalMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  unchangedIntervalsArg_
}] := multiplyToRows[justSideGeneratorsPartArg,
  multiplyToCols[
    unchangedIntervalsArg,
    inverse[
      multiplyToCols[temperedSideMappingPartArg, unchangedIntervalsArg]
    ]
  ]
];
