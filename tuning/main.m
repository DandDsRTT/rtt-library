(* OPTIMIZATION *)

(* every one of these user functions has a public and private version. 
the private is consumed by other methods. the public one parses input and formats output. *)
optimizeGeneratorTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    tPossiblyWithChangedIntervalBasis,
    targetIntervals,
    unchangedIntervals,
    intervalComplexityNormPrescalerSizeFactor,
    tuningSchemeIntervalBasis,
    pureStretchedInterval,
    logging,
    quick,
    
    useOnlyUnchangedIntervalsMethod,
    
    tuningMethodArgs,
    powerArg,
    unchangedIntervalsArg,
    
    optimumGeneratorTuningMap
  },
  
  forDamage = False; (* when True, processTargetIntervals sets an empty target interval set to the primes *)
  
  (* this is how it handles provision of the spec 
  either as a simple string (ID'ing it as either for an original scheme name or for a systematic scheme name) 
  or as an options object, either way converting it to an options object *)
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  (* then this converts that object into "properties", which is similar to "traits"
  but includes the t itself and options for the optimizer not the tuning (e.g. `logging` and `quick`) *)
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  (* mostly we then use the properties to compute args to the tuning method, but we do need several of them here too *)
  tPossiblyWithChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  intervalComplexityNormPrescalerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)
  tuningSchemeIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "tuningSchemeIntervalBasis"]; (* trait 7 *)
  pureStretchedInterval = tuningSchemeProperty[tuningSchemeProperties, "pureStretchedInterval"]; (* trait 6 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  quick = tuningSchemeProperty[tuningSchemeProperties, "quick"];
  
  (* if the count of target intervals k equals the count of generators (rank) r *)
  useOnlyUnchangedIntervalsMethod = canUseOnlyUnchangedIntervalsMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis];
  
  (* the final transformation of the user input, really, is to take the tuning scheme "properties"
  and convert those into args which are generic to whichever tuning method we end up choosing*)
  tuningMethodArgs = If[
    (* w/o target intervals, and not the case that we're relying exclusively on unchanged intervals to use, then it must be all-interval scheme *)
    ToString[targetIntervals] == "Null" && !useOnlyUnchangedIntervalsMethod,
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* generally prefer to wait to unpack these until into the tuning method function, but these two we need here *)
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  unchangedIntervalsArg = tuningMethodArg[tuningMethodArgs, "unchangedIntervalsArg"];
  
  optimumGeneratorTuningMap = TimeConstrained[
    If[
      quick == True,
      Null,
      If[
        useOnlyUnchangedIntervalsMethod,
        
        (* no historically described tuning schemes use this *)
        If[logging == True, printWrapper["\nTUNING METHOD\nunchanged interval"]];
        onlyUnchangedIntervalMethod[tuningMethodArgs],
        
        If[
          ToString[unchangedIntervalsArg] != "Null" && powerArg != 1 && powerArg != 2 && powerArg != \[Infinity],
          
          (* covers unchanged-octave minimax-E-lil-S "KE", unchanged-octave minimax-ES "CTE" *)
          If[logging == True, printWrapper["\nTUNING METHOD\npower solver"]];
          powerSumMethod[tuningMethodArgs],
          
          If[
            powerArg == 2,
            
            (* covers OLD miniRMS-U "least squares",
            minimax-ES "TE", minimax-E-copfr-S "Frobenius", pure-stretched-octave minimax-ES "POTE",
            minimax-E-lil-S "WE", minimax-E-sopfr-S "BE" *)
            If[logging == True, printWrapper["\nTUNING METHOD\npseudoinverse"]];
            pseudoinverseMethod[tuningMethodArgs],
            
            If[
              powerArg == \[Infinity],
              
              (* covers OLD minimax-U "minimax",
              minimax-S "TOP", pure-stretched-octave minimax-S "POTOP",
              minimax-sopfr-S "BOP", minimax-lil-S "Weil", unchanged-octave minimax-lil-S "Kees" *)
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
      ]
    ],
    50, (* just enough time to finish the job another way within Wolfram's 60 second window *)
    If[logging == True, printWrapper["aborted due to time constraints"]];
    Null
  ];
  
  (* this only happens if the sum polytope method fails to find a unique optimum generator tuning map, or if a computation takes too long *)
  If[
    optimumGeneratorTuningMap == Null,
    If[logging == True, printWrapper["falling back to power limit solver"]];
    optimumGeneratorTuningMap = powerSumLimitMethod[tuningMethodArgs]
  ];
  
  (* for e.g. minimax-lil "Weil" "WE" and unchanged-octave minimax-lil-S "Kees" "KE" tunings, remove the junk final entry from the augmentation; 
  I wish this didn't have to bleed up to this level, but better here maybe in one place than in each method individually? *)
  If[
    ToString[targetIntervals] == "Null" && intervalComplexityNormPrescalerSizeFactor != 0,
    optimumGeneratorTuningMap = rowify[Drop[getL[optimumGeneratorTuningMap], -1]]
  ];
  
  If[logging == True, printWrapper["\nSOLUTION FROM METHOD\n", formatOutput[optimumGeneratorTuningMap]]];
  
  (* handle trait 7 - non-standard interval basis *)
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningSchemeIntervalBasis == "primes",
    optimumGeneratorTuningMap = retrievePrimeIntervalBasisGeneratorTuningMap[optimumGeneratorTuningMap, t, tPossiblyWithChangedIntervalBasis];
    If[logging == True, printWrapper["\nRESULT AFTER RETURNING TO PRIMES INTERVAL BASIS\n", formatOutput[optimumGeneratorTuningMap]]];
  ];
  
  (* handle trait 6 - pure-stretched interval *)
  If[
    ToString[pureStretchedInterval] != "Null",
    optimumGeneratorTuningMap = getPureStretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap, t, pureStretchedInterval];
    If[logging == True, printWrapper["\nRESULT AFTER PURE-STRETCHING\n", formatOutput[optimumGeneratorTuningMap]]];
  ];
  
  If[logging == True, printWrapper[""]];
  
  optimumGeneratorTuningMap
];

optimizeTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeTuningMapPrivate[t_, tuningSchemeSpec_] := multiplyToRows[optimizeGeneratorTuningMapPrivate[t, tuningSchemeSpec], t];


(* MEAN DAMAGE *)

getGeneratorTuningMapMeanDamage[unparsedT_, unparsedGeneratorTuningMap_, tuningSchemeSpec_] := getGeneratorTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorTuningMap], tuningSchemeSpec];
getGeneratorTuningMapMeanDamagePrivate[t_, generatorTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorTuningMap, getM[t]];
  
  getTuningMapMeanDamagePrivate[t, tuningMap, tuningSchemeSpec]
];

getTuningMapMeanDamage[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapMeanDamagePrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetIntervals,
    tuningMethodArgs
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  
  tuningMethodArgs = If[
    ToString[targetIntervals] == "Null",
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

getGeneratorTuningMapDamages[unparsedT_, unparsedGeneratorTuningMap_, tuningSchemeSpec_] := getGeneratorTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorTuningMap], tuningSchemeSpec];
getGeneratorTuningMapDamagesPrivate[t_, generatorTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorTuningMap, getM[t]];
  
  getTuningMapDamagesPrivate[t, tuningMap, tuningSchemeSpec]
];

getTuningMapDamages[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapDamagesPrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetIntervals,
    tuningMethodArgs,
    damages
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  
  tuningMethodArgs = If[
    ToString[targetIntervals] == "Null",
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  damages = formatNumberL[fixUpZeros[getL[N[getAbsErrors[tuningMethodArgs]]]]];
  targetIntervals = Map[pcvToQuotient, getA[targetIntervals]];
  
  MapThread[#1 -> #2&, {targetIntervals, damages}]
];


(* TARGET INTERVAL SET SCHEMES *)

(* truncated integer limit triangle *)
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


(* CONVERSION *)

generatorTuningMapFromTAndTuningMap[unparsedT_, unparsedTuningMap_] := formatOutput[generatorTuningMapFromTAndTuningMapPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap]]];
generatorTuningMapFromTAndTuningMapPrivate[t_, tuningMap_] := Module[
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA, solution},
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA} = getTuningSchemeMappings[t];
  
  (* kind of bonkers, but if we want to reverse engineer g from t, 
  the best way for Wolfram to do it, though it seems like it should be an exact thing, is to minimize a norm *)
  solution = NMinimize[Norm[getL[multiplyToRows[generatorTuningMap, m]] - getL[tuningMap]], generatorTuningMap];
  
  rowify[generatorTuningMap /. Last[solution]]
];




(* ___ PRIVATE ___ *)



(* TUNING SCHEME OPTIONS *)

linearSolvePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

processTuningSchemeSpec[tuningSchemeSpec_] := If[
  StringQ[tuningSchemeSpec],
  If[
    StringMatchQ[tuningSchemeSpec, RegularExpression["(?:.* )?mini(?:max|RMS|mean|-\\d\\d*-mean)-(?:E|E-)?(?:\\w+-)?[UCS]"]],
    {"tuningSchemeSystematicName" -> tuningSchemeSpec},
    {"tuningSchemeOriginalName" -> tuningSchemeSpec}
  ],
  tuningSchemeSpec
];

tuningSchemeOptions = {
  "unchangedIntervals" -> Null, (* trait 0 *)
  "targetIntervals" -> Null, (* trait 1 *)
  "optimizationPower" -> Null, (* trait 2: \[Infinity] = minimax, 2 = miniRMS, 1 = minimean *)
  "damageWeightSlope" -> "", (* trait 3: unityWeight, complexityWeight, or simplicityWeight *)
  "intervalComplexityNormPower" -> 1, (* trait 4: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "intervalComplexityNormPrescalerLogPrimePower" -> 1, (* trait 5a: the power to raise the log-prime scaler to, as part of the interval complexity norm power; default 1 *)
  "intervalComplexityNormPrescalerPrimePower" -> 0, (* trait 5b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log-prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log-prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "intervalComplexityNormPrescalerSizeFactor" -> 0, (* trait 5c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "tuningSchemeIntervalBasis" -> "primes", (* trait 7: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-ES ("TE") tuning, but it can be used for any tuning *)
  "pureStretchedInterval" -> Null, (* trait 6 *)
  "tuningSchemeSystematicName" -> "",
  "tuningSchemeOriginalName" -> "",
  "damageSystematicName" -> "",
  "damageOriginalName" -> "",
  "intervalComplexitySystematicName" -> "",
  "intervalComplexityOriginalName" -> "",
  "logging" -> False,
  "quick" -> False
};
Options[processTuningSchemeOptions] = tuningSchemeOptions;
processTuningSchemeOptions[t_, forDamage_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait 0 *)
    targetIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightSlope, (* trait 3 *)
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor, (* trait 5c *)
    pureStretchedInterval, (* trait 6 *)
    tuningSchemeIntervalBasis, (* trait 7 *)
    tuningSchemeSystematicName,
    tuningSchemeOriginalName,
    damageSystematicName,
    damageOriginalName,
    intervalComplexitySystematicName,
    intervalComplexityOriginalName,
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
  targetIntervals = OptionValue["targetIntervals"]; (* trait 1 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 2 *)
  damageWeightSlope = OptionValue["damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = OptionValue["intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPrescalerLogPrimePower = OptionValue["intervalComplexityNormPrescalerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPrescalerPrimePower = OptionValue["intervalComplexityNormPrescalerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPrescalerSizeFactor = OptionValue["intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)
  pureStretchedInterval = OptionValue["pureStretchedInterval"]; (* trait 6 *)
  tuningSchemeIntervalBasis = OptionValue["tuningSchemeIntervalBasis"]; (* trait 7 *)
  tuningSchemeSystematicName = OptionValue["tuningSchemeSystematicName"];
  tuningSchemeOriginalName = OptionValue["tuningSchemeOriginalName"];
  damageSystematicName = OptionValue["damageSystematicName"];
  damageOriginalName = OptionValue["damageOriginalName"];
  intervalComplexitySystematicName = OptionValue["intervalComplexitySystematicName"];
  intervalComplexityOriginalName = OptionValue["intervalComplexityOriginalName"];
  logging = OptionValue["logging"];
  quick = OptionValue["quick"];
  
  (* tuning scheme original names *)
  If[
    tuningSchemeOriginalName === "minimax",
    optimizationPower = \[Infinity]; damageWeightSlope = "unityWeight"; targetIntervals = "OLD"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "least squares",
    optimizationPower = 2; damageWeightSlope = "unityWeight"; targetIntervals = "OLD"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "TOP" || tuningSchemeOriginalName === "TIPTOP" || tuningSchemeOriginalName === "T1" || tuningSchemeOriginalName === "TOP-max" || tuningSchemeOriginalName === "Tenney",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight";
  ];
  If[
    tuningSchemeOriginalName === "TE" || tuningSchemeOriginalName === "Tenney-Euclidean" || tuningSchemeOriginalName === "T2" || tuningSchemeOriginalName === "TOP-RMS",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "E";
  ];
  If[
    tuningSchemeOriginalName === "Frobenius",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "copfr-E";
  ];
  If[
    tuningSchemeOriginalName === "BOP" || tuningSchemeOriginalName === "Benedetti",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "sopfr";
  ];
  If[
    tuningSchemeOriginalName === "BE" || tuningSchemeOriginalName === "Benedetti-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "sopfr-E";
  ];
  If[
    tuningSchemeOriginalName === "Weil" || tuningSchemeOriginalName === "WOP",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lil";
  ];
  If[
    tuningSchemeOriginalName === "WE" || tuningSchemeOriginalName === "Weil-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lil-E";
  ];
  If[
    tuningSchemeOriginalName === "Kees" || tuningSchemeOriginalName === "KOP",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lil"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "KE" || tuningSchemeOriginalName === "Kees-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lil-E"; unchangedIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTOP" || tuningSchemeOriginalName === "POTT",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; pureStretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTE",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "E"; pureStretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "CTE" || tuningSchemeOriginalName === "Constrained Tenney-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "E"; unchangedIntervals = "octave";
  ];
  
  (* damage original name *)
  If[
    damageOriginalName === "topDamage",
    damageWeightSlope = "simplicityWeight"; intervalComplexityNormPrescalerLogPrimePower = 1;
  ];
  
  (* interval complexity original name *)
  If[
    intervalComplexityOriginalName === "copfr" || intervalComplexityOriginalName === "l1Norm",
    intervalComplexityNormPrescalerLogPrimePower = 0;
  ];
  (* product complexity is realized from a PC-vector as a product of terms, raised to the powers of the absolute values of the entries. 
  But RTT's use of linear algebra only multiplies entries and sums them. That's how complexity functions are put into vector form.
  Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    intervalComplexityOriginalName === "sopfr" || intervalComplexityOriginalName === "wilsonHeight",
    intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerPrimePower = 1;
  ];
  If[
    intervalComplexityOriginalName === "integerLimit" || intervalComplexityOriginalName === "weilHeight",
    intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "oddLimit" || intervalComplexityOriginalName === "keesHeight",
    intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerSizeFactor = 1; unchangedIntervals = "octave";
  ];
  If[
    intervalComplexityOriginalName === "logProduct" || intervalComplexityOriginalName === "tenneyHeight" || intervalComplexityOriginalName === "harmonicDistance",
    "" (* do nothing; default situation *)
  ];
  If[
    intervalComplexityOriginalName === "logIntegerLimit" || intervalComplexityOriginalName === "logarithmicWeilHeight",
    intervalComplexityNormPrescalerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "logOddLimit" || intervalComplexityOriginalName === "keesExpressibility",
    intervalComplexityNormPrescalerSizeFactor = 1; unchangedIntervals = "octave";
  ];
  If[
    intervalComplexityOriginalName === "rososcopfr" || intervalComplexityOriginalName === "l2Norm",
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerLogPrimePower = 0;
  ];
  If[
    intervalComplexityOriginalName === "rosossopfr",
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerPrimePower = 1;
  ];
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerSizeFactor = 1;
  ]; *)
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerSizeFactor = 1; unchangedIntervals = "octave";
  ]; *)
  If[
    intervalComplexityOriginalName === "tenneyEuclideanHeight",
    intervalComplexityNormPower = 2;
  ];
  If[
    intervalComplexityOriginalName === "weilEuclideanNorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "keesEuclideanSeminorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerSizeFactor = 1; unchangedIntervals = "octave";
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    intervalComplexityOriginalName === "carlsNorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerPrimePower = 2;
  ];
  
  (* trait 0 - unchanged intervals *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+.*"]],
    unchangedIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["unchanged\\-(\\{[\\w\\s\\,\\/]+\\}|[\\w\\/]+)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 1 - target intervals *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*all-interval*"] || (StringMatchQ[tuningSchemeSystematicName, "*minimax*"] && StringMatchQ[tuningSchemeSystematicName, "*S*"]),
    targetIntervals = {};
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*odd limit diamond*"],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*odd limit diamond)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*OLD*"],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*OLD)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*truncated integer limit triangle*"],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*truncated integer limit triangle)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*TILT*"],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["(\\d*-*TILT)"] -> "$1"]];
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*primes*"],
    targetIntervals = "primes";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?\\{[\\d\\/\\,\\s]*\\}\\s+.*"]],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?(\\{[\\d\\/\\,\\s]*\\})\\s+.*"] -> "$1"]];
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
  
  (* trait 3 - damage weight slope *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-S*"] || StringMatchQ[tuningSchemeSystematicName, "*-ES*"] || StringMatchQ[damageSystematicName, "*S-*"],
    damageWeightSlope = "simplicityWeight";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-C*"] || StringMatchQ[tuningSchemeSystematicName, "*-EC*"] || StringMatchQ[damageSystematicName, "*C-*"],
    damageWeightSlope = "complexityWeight";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-U*"] || StringMatchQ[damageSystematicName, "*U-*"],
    damageWeightSlope = "unityWeight";
  ];
  
  (* trait 4 - interval complexity norm power *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-E*"] || StringMatchQ[damageSystematicName, "*E*"] || StringMatchQ[intervalComplexitySystematicName, "*E*"],
    intervalComplexityNormPower = 2;
  ];
  
  (* trait 5 - interval complexity coordinate change *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-copfr-*"] || StringMatchQ[damageSystematicName, "*copfr-*"] || StringMatchQ[intervalComplexitySystematicName, "*copfr*"],
    intervalComplexityNormPrescalerLogPrimePower = 0;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-sopfr-*"] || StringMatchQ[damageSystematicName, "*sopfr-*"] || StringMatchQ[intervalComplexitySystematicName, "*sopfr*"],
    intervalComplexityNormPrescalerLogPrimePower = 0; intervalComplexityNormPrescalerPrimePower = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lil-*"] || StringMatchQ[damageSystematicName, "*lil-*"] || StringMatchQ[intervalComplexitySystematicName, "*lil*"],
    intervalComplexityNormPrescalerSizeFactor = 1;
  ];
  
  (* trait 6 - pure-stretched interval *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["pure\\-stretched\\-\\S+\\s+.*"]],
    pureStretchedInterval = First[StringCases[tuningSchemeSystematicName, RegularExpression["pure\\-stretched\\-(\\S+)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 7 - tuning scheme interval basis *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*formal-primes-basis*"],
    tuningSchemeIntervalBasis = "primes";
  ];
  (* This has to go below the systematic tuning scheme name gating, so that targetIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningSchemeIntervalBasis == "primes",
    
    (* handle non-standard interval basis *)
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasisPrivate[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    unchangedIntervals = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[unchangedIntervals, t]];
    targetIntervals = rebase[intervalRebase, processTargetIntervals[targetIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage, unchangedIntervals]];
    pureStretchedInterval = rebase[intervalRebase, processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t]],
    
    (* standard interval basis case *)
    tPossiblyWithChangedIntervalBasis = t;
    unchangedIntervals = processUnchangedOrPureStretchedIntervals[unchangedIntervals, t];
    targetIntervals = processTargetIntervals[targetIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage, unchangedIntervals];
    pureStretchedInterval = processUnchangedOrPureStretchedIntervals[pureStretchedInterval, t];
  ];
  
  If[
    logging == True,
    printWrapper["\nTUNING SCHEME OPTIONS"];
    printWrapper["tPossiblyWithChangedIntervalBasis: ", formatOutput[tPossiblyWithChangedIntervalBasis]];
    printWrapper["unchangedIntervals: ", formatOutput[unchangedIntervals]]; (* trait 0 *)
    printWrapper["targetIntervals: ", formatOutput[targetIntervals]]; (* trait 1 *)
    printWrapper["optimizationPower: ", formatOutput[optimizationPower]]; (* trait 2 *)
    printWrapper["damageWeightSlope: ", formatOutput[damageWeightSlope]]; (* trait 3 *)
    printWrapper["intervalComplexityNormPower: ", formatOutput[intervalComplexityNormPower]]; (* trait 4 *)
    printWrapper["intervalComplexityNormPrescalerLogPrimePower: ", formatOutput[intervalComplexityNormPrescalerLogPrimePower]]; (* trait 5a *)
    printWrapper["intervalComplexityNormPrescalerPrimePower: ", formatOutput[intervalComplexityNormPrescalerPrimePower]]; (* trait 5b *)
    printWrapper["intervalComplexityNormPrescalerSizeFactor: ", formatOutput[intervalComplexityNormPrescalerSizeFactor]]; (* trait 5c *)
    printWrapper["tuningSchemeIntervalBasis: ", formatOutput[tuningSchemeIntervalBasis]]; (* trait 7 *)
    printWrapper["pureStretchedInterval: ", formatOutput[pureStretchedInterval]]; (* trait 6 *)
  ];
  
  (* potential errors at this point *)
  If[
    !NumericQ[optimizationPower] && optimizationPower != \[Infinity],
    Throw["no optimization power"]
  ];
  If[
    damageWeightSlope == "",
    Throw["no damage weight slope"]
  ];
  If[
    ToString[targetIntervals] == "Null" && optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minimean or miniRMS over all intervals, only minimax."]
  ];
  If[
    ToString[targetIntervals] == "Null" && damageWeightSlope != "simplicityWeight" && !canUseOnlyUnchangedIntervalsMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis],
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weight damage."]
  ];
  
  {
    tPossiblyWithChangedIntervalBasis,
    unchangedIntervals, (* trait 0 *)
    targetIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightSlope, (* trait 3 *)
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor, (* trait 5c *)
    pureStretchedInterval, (* trait 6 *)
    tuningSchemeIntervalBasis, (* trait 7 *)
    logging,
    quick
  }
];

tuningSchemePropertiesPartsByOptionName = <|
  "t" -> 1,
  "unchangedIntervals" -> 2, (* trait 0 *)
  "targetIntervals" -> 3, (* trait 1 *)
  "optimizationPower" -> 4, (* trait 2 *)
  "damageWeightSlope" -> 5, (* trait 3 *)
  "intervalComplexityNormPower" -> 6, (* trait 4 *)
  "intervalComplexityNormPrescalerLogPrimePower" -> 7, (* trait 5a *)
  "intervalComplexityNormPrescalerPrimePower" -> 8, (* trait 5b *)
  "intervalComplexityNormPrescalerSizeFactor" -> 9, (* trait 5c *)
  "pureStretchedInterval" -> 10, (* trait 6 *)
  "tuningSchemeIntervalBasis" -> 11, (* trait 7 *)
  "logging" -> 12,
  "quick" -> 13
|>;
tuningSchemeProperty[tuningSchemeProperties_, optionName_] := Part[tuningSchemeProperties, tuningSchemePropertiesPartsByOptionName[optionName]];

(* depending on whether asked for them by target interval set scheme name, or manual listing *)
processTargetIntervals[targetIntervals_, t_, tPossiblyWithChangedIntervalBasis_, forDamage_, unchangedIntervals_] := If[
  ToString[targetIntervals] == "Null",
  If[
    canUseOnlyUnchangedIntervalsMethod[unchangedIntervals, tPossiblyWithChangedIntervalBasis],
    Null,
    Throw["no target intervals"]
  ],
  If[
    ToString[targetIntervals] == "{}",
    If[
      forDamage,
      colify[IdentityMatrix[getDPrivate[tPossiblyWithChangedIntervalBasis]]],
      Null
    ],
    If[
      StringQ[targetIntervals] && (StringMatchQ[targetIntervals, "*truncated integer limit triangle*"] || StringMatchQ[targetIntervals, "*TILT*"]),
      processTilt[targetIntervals, tPossiblyWithChangedIntervalBasis],
      If[
        ToString[targetIntervals] == "primes",
        colify[IdentityMatrix[getDPrivate[tPossiblyWithChangedIntervalBasis]]],
        If[
          StringQ[targetIntervals] && (StringMatchQ[targetIntervals, "*odd limit diamond*"] || StringMatchQ[targetIntervals, "*OLD*"]),
          processOld[targetIntervals, tPossiblyWithChangedIntervalBasis],
          colify[getA[parseQuotientL[targetIntervals, t]]]
        ]
      ]
    ]
  ]
];

processTilt[targetIntervals_, tPossiblyWithChangedIntervalBasis_] := Module[
  {d, maybeMaxInteger, tid},
  
  d = getDPrivate[tPossiblyWithChangedIntervalBasis];
  
  maybeMaxInteger = First[StringCases[StringReplace[targetIntervals, "truncated integer limit triangle" -> "TILT"], RegularExpression["(\\d*)-?TILT"] -> "$1"]];
  
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


(* PARTS *)

getTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    targetIntervals,
    unchangedIntervals,
    optimizationPower,
    logging,
    
    generatorTuningMap,
    m,
    centsConversionAndSummationMapAndLogPrimeA,
    
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
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA} = getTuningSchemeMappings[t];
  
  temperedSideGeneratorsPartArg = generatorTuningMap;
  temperedSideMappingPartArg = m;
  justSideGeneratorsPartArg = centsConversionAndSummationMapAndLogPrimeA;
  justSideMappingPartArg = getPrimesI[t];
  eitherSideIntervalsPartArg = targetIntervals;
  eitherSideMultiplierPartArg = If[ToString[eitherSideIntervalsPartArg] == "Null", Null, getDamageWeights[tuningSchemeProperties]];
  powerArg = optimizationPower;
  unchangedIntervalsArg = unchangedIntervals;
  
  If[
    logging == True,
    printWrapper["\nTUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* 𝒈 *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* 𝑀 *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* 𝒋 *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* 𝑀ⱼ *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* T *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* 𝑊 *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["unchangedIntervalsArg: ", formatOutput[unchangedIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* 𝒈 *)
    temperedSideMappingPartArg, (* 𝑀 *)
    justSideGeneratorsPartArg, (* 𝒋 *)
    justSideMappingPartArg, (* 𝑀ⱼ *)
    eitherSideIntervalsPartArg, (* T *)
    eitherSideMultiplierPartArg, (* 𝑊 *)
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

getCentsConversionAndSummationMap[t_] := rowify[Table[1200, getDPrivate[t]]];

getLogPrimeA[t_] := rowify[DiagonalMatrix[Log2[getIntervalBasis[t]]]];

(* Note: "prime cents map" is avoided in articles because it's likely to get confused with "just (primes) tuning map" 
Which it is identical to, but conceptually different, because it hasn't had a generators and mapping matrix combined with it. *)
getCentsConversionAndSummationMapAndLogPrimeA[t_] := multiplyToRows[
  getCentsConversionAndSummationMap[t],
  getLogPrimeA[t] (* in this context, the log-prime matrix is the primes-to-octaves converter *)
];

getPrimesI[t_] := rowify[IdentityMatrix[getDPrivate[t]]];

getTuningSchemeMappings[t_] := Module[
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA},
  
  generatorTuningMap = rowify[Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getRPrivate[t]}]];
  m = getM[t];
  centsConversionAndSummationMapAndLogPrimeA = getCentsConversionAndSummationMapAndLogPrimeA[t];
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA}
];

(* similar to pseudoinverse, but works for any tuning so far described *)
tuningInverse[damageWeightOrComplexityPrescaler_] := rowify[MapThread[
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
    Inverse[
      getA[damageWeightOrComplexityPrescaler][[1 ;; Last[Dimensions[getA[damageWeightOrComplexityPrescaler]]]]]
    ],
    Table[
      Table[
        0,
        First[Dimensions[getA[damageWeightOrComplexityPrescaler]]]
      ],
      Last[Dimensions[getA[damageWeightOrComplexityPrescaler]]]
    ]
  }
]];


(* DAMAGE *)

getDamageWeights[tuningSchemeProperties_] := Module[
  {
    t,
    targetIntervals, (* trait 1 *)
    damageWeightSlope, (* trait 3 *)
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor, (* trait 5c *)
    
    damageWeights
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  damageWeightSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPrescalerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPrescalerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPrescalerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)
  
  damageWeights = If[
    damageWeightSlope == "unityWeight",
    
    rowify[IdentityMatrix[Length[getA[targetIntervals]]]],
    
    rowify[DiagonalMatrix[Map[
      Function[
        {targetIntervalPcv},
        getComplexity[
          targetIntervalPcv,
          t,
          intervalComplexityNormPower, (* trait 4 *)
          intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
          intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
          intervalComplexityNormPrescalerSizeFactor (* trait 5c *)
        ]
      ],
      breakByRowsOrCols[targetIntervals]
    ]]]
  ];
  
  If[
    damageWeightSlope == "simplicityWeight",
    
    tuningInverse[damageWeights],
    
    damageWeights
  ]
];


(* ERROR *)

(* used by getPowerSumSolution *)
getPowerSumAbsError[tuningMethodArgs_] := If[
  tuningMethodArg[tuningMethodArgs, "powerArg"] == \[Infinity],
  
  (* I thought it would be fine, but apparently Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
  Max[getL[getAbsErrors[tuningMethodArgs]]],
  
  Total[Power[
    getL[getAbsErrors[tuningMethodArgs]],
    tuningMethodArg[tuningMethodArgs, "powerArg"]
  ]]
];

(* used by getTuningMapDamages and getTuningMapMeanDamage *)
getPowerMeanAbsError[tuningMethodArgs_] := Module[
  {absErrors, powerArg, targetIntervalCount, result},
  
  absErrors = getAbsErrors[tuningMethodArgs];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  targetIntervalCount = First[Dimensions[getA[tuningMethodArg[tuningMethodArgs, "eitherSideIntervalsPartArg"]]]]; (* k *)
  
  If[debug == True, printWrapper["absErrors: ", absErrors]];
  
  result = If[
    powerArg == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
    Max[getL[absErrors]],
    
    Power[
      Total[Power[
        getL[absErrors],
        powerArg
      ]] / targetIntervalCount,
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

getComplexity[
  pcv_,
  t_,
  intervalComplexityNormPower_, (* trait 4 *)
  intervalComplexityNormPrescalerLogPrimePower_, (* trait 5a *)
  intervalComplexityNormPrescalerPrimePower_, (* trait 5b *)
  intervalComplexityNormPrescalerSizeFactor_ (* trait 5c *)
] := Module[
  {complexityPrescaler},
  
  complexityPrescaler = getComplexityPrescaler[
    t,
    intervalComplexityNormPrescalerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPrescalerPrimePower, (* trait 5b *)
    intervalComplexityNormPrescalerSizeFactor (* trait 5c *)
  ];
  
  Norm[
    getL[multiplyToCols[
      complexityPrescaler,
      pcv
    ]],
    intervalComplexityNormPower
  ] / (1 + intervalComplexityNormPrescalerSizeFactor)
];

(* This is different than getDamageWeights, this is nested within it;
this is to weight the quantities of the PC-vector entries before taking their norm to get an interval complexity, 
and these complexities are then gathered for each interval and applied 
(or their reciprocals applied, in the case of simplicity-weighting) as damageWeights;
when this method is used by getDamageWeights in getTuningMethodArgs, 
it covers any non-all-interval tuning scheme using this for its damage's interval complexity *)
getComplexityPrescaler[ (* TODO: wait shit this cna't be called this, we're outside of all-interval-land. this is generic to both cases *)
  t_,
  intervalComplexityNormPrescalerLogPrimePower_, (* trait 5a *)
  intervalComplexityNormPrescalerPrimePower_, (* trait 5b *)
  intervalComplexityNormPrescalerSizeFactor_ (* trait 5c *)
] := Module[{complexityPrescaler},
  (* when used by getSimplicityPrescaler in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-copfr-S (the L1 version of "Frobenius") and minimax-E-copfr-S ("Frobenius") *)
  complexityPrescaler = rowify[IdentityMatrix[getDPrivate[t]]];
  
  If[
    (* when used by getSimplicityPrescaler in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-S ("TOP") and minimax-ES ("TE") *)
    intervalComplexityNormPrescalerLogPrimePower > 0,
    complexityPrescaler = multiplyToRows[
      complexityPrescaler,
      rowify[DiagonalMatrix[
        Power[
          Log2[getIntervalBasis[t]],
          intervalComplexityNormPrescalerLogPrimePower
        ]
      ]]
    ]
  ];
  
  If[
    (* when used by getSimplicityPrescaler in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-sopfr-S ("BOP") and minimax-E-sopfr-S ("BE") *)
    intervalComplexityNormPrescalerPrimePower > 0,
    complexityPrescaler = multiplyToRows[
      complexityPrescaler,
      rowify[DiagonalMatrix[
        Power[
          getIntervalBasis[t],
          intervalComplexityNormPrescalerPrimePower
        ]
      ]]
    ]
  ];
  
  If[
    (* when used by getSimplicityPrescaler in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-lil-S ("Weil"), minimax-E-lil-S ("WE"), unchanged-octave minimax-lil-S ("Kees"), and unchanged-octave minimax-E-lil-S ("KE") *)
    intervalComplexityNormPrescalerSizeFactor > 0,
    complexityPrescaler = multiplyToRows[
      rowify[Join[
        getA[getPrimesI[t]],
        {Table[
          intervalComplexityNormPrescalerSizeFactor,
          getDPrivate[t]
        ]}
      ]],
      complexityPrescaler
    ]
  ];
  
  complexityPrescaler
];


(* UNCHANGED INTERVALS *)

canUseOnlyUnchangedIntervalsMethod[unchangedIntervals_, t_] := ToString[unchangedIntervals] != "Null" && Length[getA[unchangedIntervals]] == getRPrivate[t];



(* METHODS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR INTERVAL COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers unchanged-octave OLD minimax-U "minimax", minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
minimax-sopfr-S "BOP", minimax-lil-S "Weil", unchanged-octave minimax-lil-S "Kees" *)
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
    unchangedIntervalCount,
    
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    
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
  
  (* if there are any unchanged intervals, we append them to the end of the target intervals in this method, with weights of 1,
  so that they can participate in the system of equations our constraint matrices represent. *)
  unchangedIntervalCount = If[ToString[unchangedIntervalsArg] == "Null", 0, First[Dimensions[getA[unchangedIntervalsArg]]]];
  eitherSideIntervalsPart = maybeAugmentIntervalsForUnchangedIntervals[eitherSideIntervalsPartArg, unchangedIntervalsArg];
  eitherSideMultiplierPart = maybeAugmentMultiplierForUnchangedIntervals[eitherSideMultiplierPartArg, unchangedIntervalCount];
  
  (* the mapped and weighted target intervals on one side, and the just and weighted target intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPart, eitherSideMultiplierPart];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPart, eitherSideMultiplierPart];
  
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
  once we've done that, though, our result isn't in the form of a generator tuning map yet. it's still distorted.
  well, with each iteration, we've been keeping track of the distortion applied, so that in the end we could undo them all.
  after undoing those, voilà, we're done! *)
  
  (* the same as rank here, but named this for correlation with elsewhere in this code *)
  generatorCount = getRPrivate[temperedSideButWithoutGeneratorsPart];
  
  (* this is too complicated to be explained here and will be explained later *)
  maxCountOfNestedMinimaxibleDamages = 0;
  
  (* the candidate generator tuning maps which minimax damage to the target intervals *)
  minimaxTunings = findAllNestedMinimaxTuningsFromMaxPolytopeVertices[
    temperedSideButWithoutGeneratorsPart,
    justSide,
    maxCountOfNestedMinimaxibleDamages,
    unchangedIntervalCount
  ];
  
  (* make sure to make room for the rows of the constraint matrices for enforcing unchanged intervals,
  which the generators will otherwise not be able to realize as tied damages *)
  maxCountOfNestedMinimaxibleDamages = generatorCount + 1 - unchangedIntervalCount;
  
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
    minimaxTunings = findAllNestedMinimaxTuningsFromMaxPolytopeVertices[
      temperedSideButWithoutGeneratorsPart,
      justSide,
      maxCountOfNestedMinimaxibleDamages,
      unchangedIntervalCount
    ];
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

(* simply include the unchanged intervals, if any, with the target intervals *)
maybeAugmentIntervalsForUnchangedIntervals[eitherSideIntervalsPartArg_, unchangedIntervalsArg_] := If[
  ToString[unchangedIntervalsArg] == "Null",
  eitherSideIntervalsPartArg,
  colify[Join[
    getA[eitherSideIntervalsPartArg],
    getA[unchangedIntervalsArg]
  ]]
];

(* simply add a weight of 1 for each unchanged interval that has been appended to the end of the target intervals *)
maybeAugmentMultiplierForUnchangedIntervals[eitherSideMultiplierPartArg_, unchangedIntervalCount_] := Module[
  {multiplierA},
  
  If[
    unchangedIntervalCount == 0,
    
    eitherSideMultiplierPartArg,
    
    multiplierA = Transpose[getA[eitherSideMultiplierPartArg]];
    rowify[Join[
      joinColumnwise[
        multiplierA,
        zeroMatrix[
          First[Dimensions[multiplierA]],
          unchangedIntervalCount
        ]
      ],
      joinColumnwise[
        zeroMatrix[
          unchangedIntervalCount,
          Last[Dimensions[multiplierA]]
        ],
        identityMatrix[unchangedIntervalCount]
      ]
    ]]
  ]
];

findAllNestedMinimaxTuningsFromMaxPolytopeVertices[
  temperedSideButWithoutGeneratorsPart_,
  justSide_,
  maxCountOfNestedMinimaxibleDamages_,
  unchangedIntervalCount_
] := Module[
  {
    targetIntervalCount,
    generatorCount,
    nthmostMinDamage,
    vertexConstraints,
    targetIntervalIndices,
    candidateTunings,
    sortedDamagesByCandidateTuning,
    candidateTuning,
    sortedDamagesForThisCandidateTuning,
    newCandidateTunings,
    newSortedDamagesByCandidateTuning
  },
  
  (* in the basic case where no minimax-damage-locking transformations have been applied, 
  these will be the same as the count of original target intervals and the rank of the temperament, respectively *)
  targetIntervalCount = getDPrivate[temperedSideButWithoutGeneratorsPart];
  generatorCount = getRPrivate[temperedSideButWithoutGeneratorsPart];
  
  (* here's the meat of it: solving a linear problem (Ax = b) for each vertex of the of tuning polytope;
  more details on this in the constraint matrix gathering function's comments below *)
  candidateTunings = {};
  vertexConstraints = getTuningMaxPolytopeVertexConstraints[generatorCount, targetIntervalCount, unchangedIntervalCount];
  Do[
    AppendTo[
      candidateTunings,
      (* solving for x *)
      rowify[Quiet[Check[
        LinearSolve[
          (* this is the A *)
          N[
            getA[multiplyToRows[
              vertexConstraint,
              transpose[temperedSideButWithoutGeneratorsPart]
            ]],
            linearSolvePrecision
          ],
          (* this is the b *)
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
  
  (* each damage list is sorted in descending order;
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
  here we're iterating by index of the target intervals, 
  repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  
  note that because the damages have all been sorted in descending order,
  these target "indices" do not actually correspond to an individual target interval.
  that's okay though because here it's not important which target interval each of these damages is for.
  all that matters is the amount of the damages.
  once we find the tuning we want, we can easily compute its damage list sorted by target interval when we need it later; that info is not lost.
  
  and note that we don't iterate over *every* target interval "index".
  we only check as many target intervals as we could possibly nested-minimax by this point.
  that's why this method doesn't simply always return a unique nested-minimax tuning each time.
  this is also why the damages have been sorted in this way
  so first we compare each tuning's actual minimum damage,
  then we compare each tuning's second-closest-to-minimum damage,
  then compare each third-closest-to-minimum, etc.
  the count of target interval indices we iterate over is a running total; 
  each time it is increased, it goes up by the present generator count plus 1.
  why it increases by that amount is a bit of a mystery to me, but perhaps someone can figure it out and let me know.
  *)
  targetIntervalIndices = Range[Min[maxCountOfNestedMinimaxibleDamages + generatorCount + 1, targetIntervalCount]];
  
  Do[
    newCandidateTunings = {};
    newSortedDamagesByCandidateTuning = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target interval index *)
    nthmostMinDamage = Min[Map[Part[getL[#], targetIntervalIndex]&, sortedDamagesByCandidateTuning]];
    Do[
      (* having found the minimum damage for this target interval index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[getL[sortedDamagesForThisCandidateTuning], targetIntervalIndex] <= nthmostMinDamage + 0.000000001,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newSortedDamagesByCandidateTuning, sortedDamagesForThisCandidateTuning]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    sortedDamagesByCandidateTuning = newSortedDamagesByCandidateTuning,
    
    {targetIntervalIndex, targetIntervalIndices}
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

getTuningMaxPolytopeVertexConstraints[generatorCount_, targetIntervalCount_, unchangedIntervalCount_] := Module[
  {vertexConstraintA, vertexConstraintAs, targetIntervalCombinations, directionPermutations},
  
  vertexConstraintAs = {};
  
  (* here we iterate over every combination of r + 1 (rank = generator count, in the basic case) target intervals 
  and for each of those combinations, looks at all permutations of their directions. 
  these are the vertices of the maximum damage tuning polytope. each is a generator tuning map. the minimum of these will be the minimax tuning.
  
  e.g. for target intervals 3/2, 5/4, and 5/3, with 1 generator, we'd look at three combinations (3/2, 5/4) (3/2, 5/3) (5/4, 5/3)
  and for the first combination, we'd look at both 3/2 \[Times] 5/4 = 15/8 and 3/2 \[Divide] 5/4 = 6/5.
  
  then what we do with each of those combo perm vertices is build a constraint matrix. 
  we'll apply this constraint matrix to a typical linear equation of the form Ax = b, 
  where A is a matrix, b is a vector, and x is another vector, the one we're solving for.
  in our case our matrix A is M, our mapping, b is our just tuning map j, and x is our generator tuning map g.
  
  e.g. when the target intervals are just the primes (and thus an identity matrix we can ignore),
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
  
  This constraint matrix [1 1 0] means that the target interval combo was 2/1 and 3/1, 
  because those are the target intervals corresponding to its nonzero elements.
  And both nonzero elements are +1 meaning that both target intervals are combined in the same direction.
  If the target intervals list had been [3/2, 4/3, 5/4, 8/5, 5/3, 6/5] instead, and the constraint matrix [1 0 0 0 -1 0],
  then that's 3/2 \[Divide] 5/3 = 5/2.
  
  The reason why we only need half of the permutations is because we only need relative direction permutations;
  they're anchored with the first target interval always in the super direction.
  *)
  targetIntervalCombinations = Subsets[Range[1, targetIntervalCount], {generatorCount + 1}];
  targetIntervalCombinations = If[
    Length[targetIntervalCombinations] * Power[generatorCount, 2] * targetIntervalCount > 275000,
    If[debug == True, printWrapper["pre-emptively aborting the analytical solution because we estimate it will exceed the time limit"]];
    {},
    targetIntervalCombinations
  ]; (* anything above this is likely to exceed the time limit, so might as well save time *)
  
  If[debug == True, printWrapper["targetIntervalCombinations: ", formatOutput[targetIntervalCombinations]]];
  
  Do[
    (* note that these are only generatorCount, not generatorCount + 1, because whichever is the first one will always be +1 *)
    If[debug == True, printWrapper["  targetCombination: ", formatOutput[targetCombination]]];
    
    directionPermutations = Tuples[{1, -1}, generatorCount];
    If[debug == True, printWrapper["  directionPermutations: ", formatOutput[directionPermutations]]];
    
    Do[
      If[debug == True, printWrapper["    directionPermutation: ", formatOutput[directionPermutation]]];
      
      vertexConstraintA = Table[Table[0, targetIntervalCount], generatorCount];
      
      Do[
        vertexConstraintA[[generatorIndex, Part[targetCombination, 1]]] = 1;
        vertexConstraintA[[generatorIndex, Part[targetCombination, generatorIndex + 1]]] = Part[directionPermutation, generatorIndex],
        
        {generatorIndex, Range[generatorCount]}
      ];
      
      If[debug == True, printWrapper["      vertexConstraintA: ", formatOutput[vertexConstraintA]]];
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {directionPermutation, directionPermutations}
    ],
    
    {targetCombination, targetIntervalCombinations}
  ];
  
  (* if there's only one generator, we also need to consider each tuning where a target interval is tuned pure 
  (rather than tied for damage with another target interval) *)
  If[
    generatorCount == 1,
    Do[
      vertexConstraintA = {Table[0, targetIntervalCount]};
      vertexConstraintA[[1, targetIntervalIndex]] = 1;
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {targetIntervalIndex, Range[targetIntervalCount]}
    ]
  ];
  
  (* augment the constraint matrix to account for unchanged intervals *)
  vertexConstraintAs = Map[augmentVertexConstraintAForUnchangedIntervals[#, unchangedIntervalCount]&, vertexConstraintAs];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  Map[rowify, vertexConstraintAs]
];

(* for each unchanged interval, add a row that is all zeros except for a one in the col corresponding to it *)
augmentVertexConstraintAForUnchangedIntervals[vertexConstraintA_, unchangedIntervalCount_] := Join[
  vertexConstraintA,
  joinColumnwise[
    zeroMatrix[unchangedIntervalCount, Last[Dimensions[vertexConstraintA]] - unchangedIntervalCount],
    identityMatrix[unchangedIntervalCount]
  ]
];

joinColumnwise[a1_, a2_] := Transpose[Join[Transpose[a1], Transpose[a2]]];
zeroMatrix[r_, c_] := ConstantArray[0, {r, c}];
identityMatrix[n_] := If[n == 0, {}, IdentityMatrix[n]];


(* METHODS: OPTIMIZATION POWER = 1 (MINIMEAN) OR INTERVAL COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tuning schemes use this *)
(* an analytical method *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where unchanged-octave OLD minimax-U "minimax" is described;
however, this computation method is in general actually for minimean tuning schemes, not minimax tuning schemes. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond target interval set,
namely that the places where damage to target intervals are equal is the same where other targets are pure.
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
    unchangedIntervalCount,
    
    unchangedIntervalSetIndices,
    candidateUnchangedIntervalSets,
    canonicalizedCandidateUnchangedIntervalSets,
    filteredCanonicalizedCandidateUnchangedIntervalSets,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets,
    candidateOptimumGenerators,
    candidateOptimumGeneratorTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorTuningMapIndices,
    optimumGeneratorTuningMapIndex
  },
  
  generatorCount = First[Dimensions[getA[temperedSideMappingPartArg]]];
  unchangedIntervalCount = If[ToString[unchangedIntervalsArg] == "Null", 0, First[Dimensions[getA[unchangedIntervalsArg]]]];
  
  unchangedIntervalSetIndices = Subsets[
    Range[First[Dimensions[getA[eitherSideIntervalsPartArg]]]],
    {generatorCount - unchangedIntervalCount}
  ];
  candidateUnchangedIntervalSets = Map[
    colify[
      Join[
        Map[
          getA[eitherSideIntervalsPartArg][[#]]&,
          #
        ],
        If[ToString[unchangedIntervalsArg] == "Null", {}, getA[unchangedIntervalsArg]]
      ]
    ]&,
    unchangedIntervalSetIndices
  ];
  canonicalizedCandidateUnchangedIntervalSets = Map[canonicalFormPrivate, candidateUnchangedIntervalSets];
  filteredCanonicalizedCandidateUnchangedIntervalSets = Select[canonicalizedCandidateUnchangedIntervalSets, MatrixRank[Transpose[getA[#]]] == generatorCount&];
  dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets = DeleteDuplicates[filteredCanonicalizedCandidateUnchangedIntervalSets];
  candidateOptimumGenerators = Select[Map[
    getGeneratorBasisFromUnchangedIntervals[temperedSideMappingPartArg, #]&,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets
  ], Not[# === Null]&];
  candidateOptimumGeneratorTuningMaps = Map[multiplyToRows[justSideGeneratorsPartArg, #]&, candidateOptimumGenerators];
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
    candidateOptimumGeneratorTuningMaps
  ];
  
  If[
    debug == True,
    printWrapper["candidateUnchangedIntervalSets: ", Map[formatOutput, candidateUnchangedIntervalSets]];
    printWrapper["canonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, canonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["filteredCanonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, filteredCanonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, dedupedFilteredCanonicalizedCandidateUnchangedIntervalSets]];
    printWrapper["candidateOptimumGenerators: ", Map[formatOutput, candidateOptimumGenerators]];
    printWrapper["candidateOptimumGeneratorTuningMaps: ", Map[formatOutput, candidateOptimumGeneratorTuningMaps]];
    printWrapper["candidateOptimumGeneratorTuningMapAbsErrors: ", Map[formatOutput, candidateOptimumGeneratorTuningMapAbsErrors]];
  ];
  
  optimumGeneratorTuningMapIndices = Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]];
  If[
    Length[optimumGeneratorTuningMapIndices] == 1,
    
    (* result is unique; done *)
    optimumGeneratorTuningMapIndex = First[First[Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]]]];
    maybeRowify[candidateOptimumGeneratorTuningMaps[[optimumGeneratorTuningMapIndex]]],
    
    (* result is non-unique, will need to handle otherwise *)
    Null
  ]
];

getGeneratorBasisFromUnchangedIntervals[m_, unchangedIntervalEigenvectors_] := Module[
  {mappedUnchangedIntervalEigenvectors},
  
  mappedUnchangedIntervalEigenvectors = multiplyToCols[m, unchangedIntervalEigenvectors];
  
  If[
    Det[getA[mappedUnchangedIntervalEigenvectors]] == 0,
    Null,
    multiplyToCols[unchangedIntervalEigenvectors, inverse[mappedUnchangedIntervalEigenvectors]]
  ]
];


(* METHODS: OPTIMIZATION POWER = 2 (MINIRMS) OR INTERVAL COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical method *)
(* covers unchanged-octave OLD miniRMS-U "least squares", minimax-ES "TE", pure-stretched-octave minimax-ES "POTE",
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
  {justSide, temperedSideButWithoutGeneratorsPart, nextToInverted, toBeInverted, rank, augmentedNextToInverted, augmentedToBeInverted},
  
  justSide = multiplyToRows[justSideGeneratorsPartArg, justSideMappingPartArg]; (* j *)
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg]; (* MTW *)
  nextToInverted = multiplyToCols[eitherSideIntervalsPartArg, eitherSideMultiplierPartArg, transpose[temperedSideButWithoutGeneratorsPart]]; (* TW(MTW)ᵀ *)
  toBeInverted = multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]; (* MTW(MTW)ᵀ *)
  
  (* Technically the Aᵀ(AAᵀ)⁻¹ type of pseudoinverse is necessary. 
  Wolfram's built-in will sometimes use other techniques, which do not give the correct answer.
  Also it's good to break it down to show the parallelism between the simpler case and the unchanged interval case. *)
  
  If[
    ToString[unchangedIntervalsArg] == "Null",
    
    (* jTW(MTW)ᵀ(MTW(MTW)ᵀ)⁻¹, so it's the pseudoinverse of MTW left-multiplied by jTW *)
    maybeRowify[multiplyToRows[
      justSide,
      nextToInverted,
      inverse[
        toBeInverted
      ]
    ]],
    
    (* same as above, but we augment matrices with the unchanged intervals and mapped versions thereof *)
    rank = Last[Dimensions[getA[temperedSideGeneratorsPartArg]]];
    augmentedNextToInverted = augmentNextToInvertedForUnchangedIntervals[nextToInverted, unchangedIntervalsArg];
    augmentedToBeInverted = augmentToBeInvertedForUnchangedIntervals[toBeInverted, unchangedIntervalsArg, temperedSideMappingPartArg];
    rowify[Take[getL[maybeRowify[multiplyToRows[
      justSide,
      augmentedNextToInverted,
      inverse[
        augmentedToBeInverted
      ]
    ]]], rank]]
  ]
];

augmentNextToInvertedForUnchangedIntervals[nextToInverted_, unchangedIntervalsArg_] := colify[Join[
  getA[nextToInverted],
  getA[unchangedIntervalsArg]
]];

augmentToBeInvertedForUnchangedIntervals[toBeInverted_, unchangedIntervalsArg_, temperedSideMappingPartArg_] := Module[
  {unchangedIntervalCount, mappedUnchangedIntervals, zeros},
  
  unchangedIntervalCount = First[Dimensions[getA[unchangedIntervalsArg]]];
  mappedUnchangedIntervals = multiplyToRows[temperedSideMappingPartArg, unchangedIntervalsArg]; (* MU *)
  zeros = zeroMatrix[unchangedIntervalCount, unchangedIntervalCount];
  
  colify[Join[
    getA[rowify[joinColumnwise[
      getA[toBeInverted],
      getA[mappedUnchangedIntervals]
    ]]],
    getA[rowify[joinColumnwise[
      Transpose[getA[mappedUnchangedIntervals]],
      zeros
    ]]]
  ]]
];


(* METHODS: GENERAL OPTIMIZATION POWER (MINI-P-MEAN) OR GENERAL PRIME ERROR MAGNITUDE NORM POWER (MINI-P-NORM) *)

(* a numerical method *)
(* covers unchanged-octave minimax-E-lil-S "KE", unchanged-octave minimax-ES "CTE" *)
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
  powerSumPower = Power[2, 1 / powerSumPowerPower]; (* could just set it to 2, since this is 2^(1/1), but just hinting at how it works coming up *)
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
    powerSumPower = If[
      powerSumPowerLimit == 1,
      Power[2, 1 / powerSumPowerPower], (* we are moving from starting power of 2 gradually down toward 1 *)
      Power[2, powerSumPowerPower] (* we are moving from starting power of 2 gradually up toward \[Infinity] *)
    ];
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
        (* this is how we enforce that the unchanged intervals are unchanged. note that if augmented, we have to zero out their augmentation. *)
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
where the generators part is 1200×𝟏𝐿𝐺 (tempered) or 1200×𝟏𝐿𝐺ⱼ (just), the mapping part is 𝑀 (tempered) or 𝑀ⱼ (just), 
the intervals part is T (non-all-interval) or Tₚ (all-interval), and
the multiplier part is 𝑊 (non-all-interval) or 𝑆ₚ (all-interval), finds:
tempered non-all-interval: 1200×𝟏𝐿 𝐺 𝑀 T 𝑊
tempered all-interval:     1200×𝟏𝐿 𝐺 𝑀 Tₚ𝑆ₚ
just non-all-interval:     1200×𝟏𝐿 𝐺ⱼ𝑀ⱼT 𝑊 
just all-interval:         1200×𝟏𝐿 𝐺ⱼ𝑀ⱼTₚ𝑆ₚ
in the approximation 1200×𝟏𝐿𝐺𝑀T𝑊 \[TildeTilde] 1200×𝟏𝐿𝐺ⱼ𝑀ⱼT𝑊 or 1200×𝟏𝐿𝐺𝑀Tₚ𝑆ₚ \[TildeTilde] 1200×𝟏𝐿𝐺ⱼ𝑀ⱼTₚ𝑆ₚ
where Gⱼ = 𝑀ⱼ = Tₚ = 𝐼 (identity matrix)
*)
getTemperedOrJustSide[
  temperedOrJustSideGeneratorsPart_,
  temperedOrJustSideMappingPart_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_
] := multiplyToRows[temperedOrJustSideGeneratorsPart, temperedOrJustSideMappingPart, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];

(* no historically described tuning schemes use this *)
(* an analytical method *)
(* 𝐺 = U(𝑀U)⁻¹; 𝒈 = 𝒋𝐺 *)
onlyUnchangedIntervalMethod[{
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
