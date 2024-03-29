(* OPTIMIZATION *)

(* every one of these user functions has a public and private version. 
the private is consumed by other methods. the public one parses input and formats output. *)
optimizeGeneratorTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    tPossiblyWithChangedDomainBasis,
    targetIntervals,
    heldIntervals,
    intervalComplexityNormPreTransformerSizeFactor,
    nonprimeBasisApproach,
    destretchedInterval,
    logging,
    quick,
    
    useOnlyHeldIntervalsMethod,
    
    tuningMethodArgs,
    powerArg,
    heldIntervalsArg,
    
    optimumGeneratorTuningMap
  },
  
  forDamage = False; (* when True, processTargetIntervals sets an empty target-interval set to the primes *)
  
  (* this is how it handles provision of the spec 
  either as a simple string (ID'ing it as either for an original scheme name or for a systematic scheme name) 
  or as an options object, either way converting it to an options object *)
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  (* then this converts that object into "properties", which is similar to "traits"
  but includes the t itself and options for the optimizer not the tuning (e.g. `logging` and `quick`) *)
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  (* mostly we then use the properties to compute args to the tuning method, but we do need several of them here too *)
  tPossiblyWithChangedDomainBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  heldIntervals = tuningSchemeProperty[tuningSchemeProperties, "heldIntervals"]; (* trait 0 *)
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  destretchedInterval = tuningSchemeProperty[tuningSchemeProperties, "destretchedInterval"]; (* trait 6 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  quick = tuningSchemeProperty[tuningSchemeProperties, "quick"];
  
  (* if the count of target-intervals k equals the count of generators (rank) r *)
  useOnlyHeldIntervalsMethod = canUseOnlyHeldIntervalsMethod[heldIntervals, tPossiblyWithChangedDomainBasis];
  
  (* the final transformation of the user input, really, is to take the tuning scheme "properties"
  and convert those into args which are generic to whichever tuning method we end up choosing*)
  tuningMethodArgs = If[
    (* w/o target-intervals, and not the case that we're relying exclusively on held-intervals to use, then it must be all-interval scheme *)
    ToString[targetIntervals] == "Null" && !useOnlyHeldIntervalsMethod,
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* generally prefer to wait to unpack these until into the tuning method function, but these two we need here *)
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  heldIntervalsArg = tuningMethodArg[tuningMethodArgs, "heldIntervalsArg"];
  
  optimumGeneratorTuningMap = TimeConstrained[
    If[
      quick == True,
      Null,
      If[
        useOnlyHeldIntervalsMethod,
        
        (* no historically described tuning schemes use this *)
        If[logging == True, printWrapper["\nTUNING METHOD\nonly held-intervals method"]];
        onlyHeldIntervalMethod[tuningMethodArgs],
        
        If[
          powerArg == 2,
          
          (* covers OLD miniRMS-U "least squares",
          minimax-ES "TE", minimax-E-copfr-S "Frobenius", destretched-octave minimax-ES "POTE",
          minimax-E-lils-S "WE", minimax-E-sopfr-S "BE",
          held-octave minimax-E-lils-S "KE", held-octave minimax-ES "CTE" *)
          If[logging == True, printWrapper["\nTUNING METHOD\npseudoinverse method"]];
          pseudoinverseMethod[tuningMethodArgs],
          
          If[
            powerArg == \[Infinity],
            
            (* covers OLD minimax-U "minimax",
            minimax-S "TOP", destretched-octave minimax-S "POTOP",
            minimax-sopfr-S "BOP", minimax-lils-S "Weil", destretched-octave minimax-lils-S "Kees" *)
            If[logging == True, printWrapper["\nTUNING METHOD\ncoinciding-damage method"]];
            coincidingDamageMethod[tuningMethodArgs],
            
            If[
              powerArg == 1,
              
              (* no historically described tuning schemes use this *)
              If[logging == True, printWrapper["\nTUNING METHOD\nzero-damage method"]];
              zeroDamageMethod[tuningMethodArgs],
              
              (* no historically described tuning schemes use this *)
              If[logging == True, printWrapper["\nTUNING METHOD\npower sum method"]];
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
  
  (* this only happens if the zero-damage method fails to find a unique optimum generator tuning map, or if a computation takes too long *)
  If[
    optimumGeneratorTuningMap == Null,
    If[logging == True, printWrapper["falling back to power limit solver"]];
    optimumGeneratorTuningMap = powerSumLimitMethod[tuningMethodArgs]
  ];
  
  (* for e.g. minimax-lils "Weil" "WE" and pure-stretched-octave minimax-lils-S "Kees" "KE" tunings, remove the junk final entry from the augmentation; 
  I wish this didn't have to bleed up to this level, but better here maybe in one place than in each method individually? *)
  If[
    ToString[targetIntervals] == "Null" && intervalComplexityNormPreTransformerSizeFactor != 0,
    optimumGeneratorTuningMap = rowify[Drop[getL[optimumGeneratorTuningMap], -1]]
  ];
  
  If[logging == True, printWrapper["\nSOLUTION FROM METHOD\n", formatOutput[optimumGeneratorTuningMap]]];
  
  (* handle trait 7 - nonprime basis *)
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[t]] && nonprimeBasisApproach == "prime-based",
    optimumGeneratorTuningMap = retrievePrimeDomainBasisGeneratorTuningMap[optimumGeneratorTuningMap, t, tPossiblyWithChangedDomainBasis];
    If[logging == True, printWrapper["\nRESULT AFTER RETURNING TO PRIMES DOMAIN BASIS\n", formatOutput[optimumGeneratorTuningMap]]];
  ];
  
  (* handle trait 6 - destretched interval *)
  If[
    ToString[destretchedInterval] != "Null",
    optimumGeneratorTuningMap = getDestretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap, t, destretchedInterval];
    If[logging == True, printWrapper["\nRESULT AFTER DESTRETCHING\n", formatOutput[optimumGeneratorTuningMap]]];
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
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsMultipliedErrors needs it, 
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
  (* set the temperedSideGeneratorsPartArg to the input tuningMap, in octaves, in the structure getAbsMultipliedErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  tuningMethodArgs[[1]] = tuningMap;
  (* override the other half of the temperedSideMappingPartArg too, since we have the whole tuning map already *)
  tuningMethodArgs[[2]] = getPrimesI[t];
  
  damages = formatNumberL[fixUpZeros[N[getAbsMultipliedErrors[tuningMethodArgs]]]];
  targetIntervals = Map[pcvToQuotient, getA[targetIntervals]];
  
  MapThread[#1 -> #2&, {targetIntervals, damages}]
];


(* TARGET-INTERVAL SET SCHEMES *)

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
  {generatorTuningMap, m, justTuningMap},
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  (* the pseudoinverse is relied upon here to give a valid right-inverse to the mapping M, and since the tuning map 𝒕 = 𝒈𝑀, 𝒈𝑀𝑀⁺ gives 𝒈 *)
  rowify[getL[tuningMap].PseudoInverse[getA[m]]]
];




(* ___ PRIVATE ___ *)



(* TUNING SCHEME OPTIONS *)

coincidingDamageMethodTieAdjuster = 0.000000001;

coincidingDamageMethodTiePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

processTuningSchemeSpec[tuningSchemeSpec_] := If[
  StringQ[tuningSchemeSpec],
  If[
    StringMatchQ[tuningSchemeSpec, RegularExpression["(?:.* )?mini(?:max|RMS|average|-\\d\\d*-mean)-(?:odd-)?(?:E|E-)?(?:\\w+-)?(?:limit-)?[UCS]"]],
    {"tuningSchemeSystematicName" -> tuningSchemeSpec},
    {"tuningSchemeOriginalName" -> tuningSchemeSpec}
  ],
  tuningSchemeSpec
];

tuningSchemeOptions = {
  "heldIntervals" -> Null, (* trait 0 *)
  "targetIntervals" -> Null, (* trait 1 *)
  "optimizationPower" -> Null, (* trait 2: \[Infinity] = minimax, 2 = miniRMS, 1 = miniaverage *)
  "damageWeightSlope" -> "", (* trait 3: unityWeight, complexityWeight, or simplicityWeight *)
  "intervalComplexityNormPower" -> 1, (* trait 4: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "intervalComplexityNormPreTransformerLogPrimePower" -> 1, (* trait 5a: the power to raise the log-prime prescaler to, as part of the interval complexity norm power; default 1 *)
  "intervalComplexityNormPreTransformerPrimePower" -> 0, (* trait 5b: the power to raise the prime prescaler to, as part of the interval complexity norm power; what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when otherwise defaults; 1 = product complexity, equiv to sopfr when otherwise defaults; >1 = pth power of those; default 0 *)
  "intervalComplexityNormPreTransformerSizeFactor" -> 0, (* trait 5c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "nonprimeBasisApproach" -> "", (* trait 7: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-ES ("TE") tuning, but it can be used for any tuning, and it is also possible to do neither approach *)
  "destretchedInterval" -> Null, (* trait 6 *)
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
    heldIntervals, (* trait 0 *)
    targetIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightSlope, (* trait 3 *)
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    destretchedInterval, (* trait 6 *)
    nonprimeBasisApproach, (* trait 7 *)
    tuningSchemeSystematicName,
    tuningSchemeOriginalName,
    damageSystematicName,
    damageOriginalName,
    intervalComplexitySystematicName,
    intervalComplexityOriginalName,
    logging,
    quick,
    tPossiblyWithChangedDomainBasis,
    commaBasisInNonstandardDomainBasis,
    simplestPrimeOnlyBasis,
    commaBasisInSimplestPrimeOnlyBasis,
    mappingInSimplestPrimeOnlyBasis,
    domainBasis,
    domainBasisChange
  },
  
  heldIntervals = OptionValue["heldIntervals"]; (* trait 0 *)
  targetIntervals = OptionValue["targetIntervals"]; (* trait 1 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 2 *)
  damageWeightSlope = OptionValue["damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = OptionValue["intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = OptionValue["intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = OptionValue["intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = OptionValue["intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  destretchedInterval = OptionValue["destretchedInterval"]; (* trait 6 *)
  nonprimeBasisApproach = OptionValue["nonprimeBasisApproach"]; (* trait 7 *)
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
    optimizationPower = \[Infinity]; damageWeightSlope = "unityWeight"; targetIntervals = "OLD"; heldIntervals = "octave";
  ];
  If[
    tuningSchemeOriginalName === "least squares",
    optimizationPower = 2; damageWeightSlope = "unityWeight"; targetIntervals = "OLD"; heldIntervals = "octave";
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
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lils";
  ];
  If[
    tuningSchemeOriginalName === "WE" || tuningSchemeOriginalName === "Weil-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lils-E";
  ];
  If[
    tuningSchemeOriginalName === "Kees" || tuningSchemeOriginalName === "KOP",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lils"; destretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "KE" || tuningSchemeOriginalName === "Kees-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "lils-E"; destretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTOP" || tuningSchemeOriginalName === "POTT",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; destretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "POTE",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "E"; destretchedInterval = "octave";
  ];
  If[
    tuningSchemeOriginalName === "CTE" || tuningSchemeOriginalName === "Constrained Tenney-Euclidean",
    targetIntervals = {}; optimizationPower = \[Infinity]; damageWeightSlope = "simplicityWeight"; intervalComplexitySystematicName = "E"; heldIntervals = "octave";
  ];
  
  (* damage original name *)
  If[
    damageOriginalName === "topDamage",
    damageWeightSlope = "simplicityWeight"; intervalComplexityNormPreTransformerLogPrimePower = 1;
  ];
  
  (* interval complexity original name *)
  If[
    intervalComplexityOriginalName === "copfr" || intervalComplexityOriginalName === "l1Norm",
    intervalComplexityNormPreTransformerLogPrimePower = 0;
  ];
  (* product complexity is realized from a PC-vector as a product of terms, raised to the powers of the absolute values of the entries. 
  But RTT's use of linear algebra only multiplies entries and sums them. That's how complexity functions are put into vector form.
  Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    intervalComplexityOriginalName === "sopfr" || intervalComplexityOriginalName === "wilsonHeight",
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 1;
  ];
  If[
    intervalComplexityOriginalName === "integerLimit" || intervalComplexityOriginalName === "weilHeight",
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "oddLimit" || intervalComplexityOriginalName === "keesHeight",
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerSizeFactor = 1; heldIntervals = "octave";
  ];
  If[
    intervalComplexityOriginalName === "logProduct" || intervalComplexityOriginalName === "tenneyHeight" || intervalComplexityOriginalName === "harmonicDistance",
    "" (* do nothing; default situation *)
  ];
  If[
    intervalComplexityOriginalName === "logIntegerLimit" || intervalComplexityOriginalName === "logarithmicWeilHeight",
    intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "logOddLimit" || intervalComplexityOriginalName === "keesExpressibility",
    intervalComplexityNormPreTransformerSizeFactor = 1; heldIntervals = "octave";
  ];
  If[
    intervalComplexityOriginalName === "rososcopfr" || intervalComplexityOriginalName === "l2Norm",
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerLogPrimePower = 0;
  ];
  If[
    intervalComplexityOriginalName === "rosossopfr",
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 1;
  ];
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerSizeFactor = 1;
  ]; *)
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerSizeFactor = 1; heldIntervals = "octave";
  ]; *)
  If[
    intervalComplexityOriginalName === "tenneyEuclideanHeight",
    intervalComplexityNormPower = 2;
  ];
  If[
    intervalComplexityOriginalName === "weilEuclideanNorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    intervalComplexityOriginalName === "keesEuclideanSeminorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerSizeFactor = 1; heldIntervals = "octave";
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    intervalComplexityOriginalName === "carlsNorm",
    intervalComplexityNormPower = 2; intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 2;
  ];
  
  (* trait 0 - held-intervals *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["held\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+.*"]],
    heldIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["held\\-(\\{[\\w\\s\\,\\/]+\\}|[\\w\\/]+)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 1 - target-intervals *)
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
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["^(?:held\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:destretched\\-\\S+\\s+)?\\{[\\d\\/\\,\\s]*\\}\\s+.*"]],
    targetIntervals = First[StringCases[tuningSchemeSystematicName, RegularExpression["^(?:held\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:destretched\\-\\S+\\s+)?(\\{[\\d\\/\\,\\s]*\\})\\s+.*"] -> "$1"]];
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
    StringMatchQ[tuningSchemeSystematicName, "*miniaverage*"],
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
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 0;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lopfr-*"] || StringMatchQ[damageSystematicName, "*lopfr-*"] || StringMatchQ[intervalComplexitySystematicName, "*lopfr*"] ||
        StringMatchQ[tuningSchemeSystematicName, "*-lp-*"] || StringMatchQ[damageSystematicName, "*lp-*"] || StringMatchQ[intervalComplexitySystematicName, "*lp*"],
    intervalComplexityNormPreTransformerLogPrimePower = 1; intervalComplexityNormPreTransformerPrimePower = 0;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-sopfr-*"] || StringMatchQ[damageSystematicName, "*sopfr-*"] || StringMatchQ[intervalComplexitySystematicName, "*sopfr*"] ||
        StringMatchQ[tuningSchemeSystematicName, "*-prod-*"] || StringMatchQ[damageSystematicName, "*prod-*"] || StringMatchQ[intervalComplexitySystematicName, "*prod*"],
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-ils-*"] || StringMatchQ[damageSystematicName, "*ils-*"] || StringMatchQ[intervalComplexitySystematicName, "*ils*"],
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 1;
    intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-ols-*"] || StringMatchQ[damageSystematicName, "*ols-*"] || StringMatchQ[intervalComplexitySystematicName, "*ols*"],
    intervalComplexityNormPreTransformerLogPrimePower = 0; intervalComplexityNormPreTransformerPrimePower = 1;
    intervalComplexityNormPreTransformerSizeFactor = 1;
    heldIntervals = "octave";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lils-*"] || StringMatchQ[damageSystematicName, "*lils-*"] || StringMatchQ[intervalComplexitySystematicName, "*lils*"],
    intervalComplexityNormPreTransformerLogPrimePower = 1; intervalComplexityNormPreTransformerPrimePower = 0;
    intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-limit-*"] || StringMatchQ[damageSystematicName, "*limit-*"] || StringMatchQ[intervalComplexitySystematicName, "*limit*"],
    intervalComplexityNormPreTransformerSizeFactor = 1;
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-lols-*"] || StringMatchQ[damageSystematicName, "*lols-*"] || StringMatchQ[intervalComplexitySystematicName, "*lols*"],
    intervalComplexityNormPreTransformerSizeFactor = 1;
    heldIntervals = "octave";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*-odd-*"] || StringMatchQ[damageSystematicName, "*odd-*"] || StringMatchQ[intervalComplexitySystematicName, "*odd*"],
    heldIntervals = "octave";
  ];
  
  (* trait 6 - destretched interval *)
  If[
    StringMatchQ[tuningSchemeSystematicName, RegularExpression["destretched\\-\\S+\\s+.*"]],
    destretchedInterval = First[StringCases[tuningSchemeSystematicName, RegularExpression["destretched\\-(\\S+)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 7 - nonprime basis *)
  If[
    StringMatchQ[tuningSchemeSystematicName, "*prime-based*"],
    nonprimeBasisApproach = "prime-based";
  ];
  If[
    StringMatchQ[tuningSchemeSystematicName, "*nonprime-based*"], (* important this comes 2nd so it overrides the above! *)
    nonprimeBasisApproach = "nonprime-based";
  ];
  (* This has to go below the systematic tuning scheme name gating, so that targetIntervals has a change to be set to {} *)
  domainBasis = getDomainBasis[t];
  If[
    ToString[nonprimeBasisApproach] == "prime-based",
    
    (* handle prime-based approach to nonprime bases *)
    commaBasisInNonstandardDomainBasis = getC[t];
    simplestPrimeOnlyBasis = getSimplestPrimeOnlyBasis[domainBasis];
    commaBasisInSimplestPrimeOnlyBasis = changeDomainBasisPrivate[commaBasisInNonstandardDomainBasis, simplestPrimeOnlyBasis];
    domainBasisChange = colify[getDomainBasisChangeForC[domainBasis, simplestPrimeOnlyBasis]];
    mappingInSimplestPrimeOnlyBasis = getM[commaBasisInSimplestPrimeOnlyBasis];
    tPossiblyWithChangedDomainBasis = mappingInSimplestPrimeOnlyBasis;
    If[
      debug == True,
      printWrapper["commaBasisInNonstandardDomainBasis: ", formatOutput[commaBasisInNonstandardDomainBasis]];
      printWrapper["simplestPrimeOnlyBasis: ", formatOutput[simplestPrimeOnlyBasis]];
      printWrapper["commaBasisInSimplestPrimeOnlyBasis: ", formatOutput[commaBasisInSimplestPrimeOnlyBasis]];
      printWrapper["domainBasisChange: ", formatOutput[domainBasisChange]];
      printWrapper["mappingInSimplestPrimeOnlyBasis: ", formatOutput[mappingInSimplestPrimeOnlyBasis]];
      printWrapper["tPossiblyWithChangedDomainBasis: ", formatOutput[tPossiblyWithChangedDomainBasis]];
    ],
    
    (* handle prime-only bases, or nonprime-based or neutral approaches to nonprime bases *)
    tPossiblyWithChangedDomainBasis = t
  ];
  heldIntervals = processHeldOrDestretchedIntervals[heldIntervals, tPossiblyWithChangedDomainBasis];
  targetIntervals = processTargetIntervals[targetIntervals, t, tPossiblyWithChangedDomainBasis, forDamage, heldIntervals];
  destretchedInterval = processHeldOrDestretchedIntervals[destretchedInterval, tPossiblyWithChangedDomainBasis];
  
  If[
    logging == True,
    printWrapper["\nTUNING SCHEME OPTIONS"];
    printWrapper["tPossiblyWithChangedDomainBasis: ", formatOutput[tPossiblyWithChangedDomainBasis]];
    printWrapper["heldIntervals: ", formatOutput[heldIntervals]]; (* trait 0 *)
    printWrapper["targetIntervals: ", formatOutput[targetIntervals]]; (* trait 1 *)
    printWrapper["optimizationPower: ", formatOutput[optimizationPower]]; (* trait 2 *)
    printWrapper["damageWeightSlope: ", formatOutput[damageWeightSlope]]; (* trait 3 *)
    printWrapper["intervalComplexityNormPower: ", formatOutput[intervalComplexityNormPower]]; (* trait 4 *)
    printWrapper["intervalComplexityNormPreTransformerLogPrimePower: ", formatOutput[intervalComplexityNormPreTransformerLogPrimePower]]; (* trait 5a *)
    printWrapper["intervalComplexityNormPreTransformerPrimePower: ", formatOutput[intervalComplexityNormPreTransformerPrimePower]]; (* trait 5b *)
    printWrapper["intervalComplexityNormPreTransformerSizeFactor: ", formatOutput[intervalComplexityNormPreTransformerSizeFactor]]; (* trait 5c *)
    printWrapper["nonprimeBasisApproach: ", formatOutput[nonprimeBasisApproach]]; (* trait 7 *)
    printWrapper["destretchedInterval: ", formatOutput[destretchedInterval]]; (* trait 6 *)
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
    Throw["It is not possible to optimize for miniaverage or miniRMS over all intervals, only minimax."]
  ];
  If[
    ToString[targetIntervals] == "Null" && damageWeightSlope != "simplicityWeight" && !canUseOnlyHeldIntervalsMethod[heldIntervals, tPossiblyWithChangedDomainBasis],
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weight damage."]
  ];
  
  {
    tPossiblyWithChangedDomainBasis,
    heldIntervals, (* trait 0 *)
    targetIntervals, (* trait 1 *)
    optimizationPower, (* trait 2 *)
    damageWeightSlope, (* trait 3 *)
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    destretchedInterval, (* trait 6 *)
    nonprimeBasisApproach, (* trait 7 *)
    logging,
    quick
  }
];

tuningSchemePropertiesPartsByOptionName = <|
  "t" -> 1,
  "heldIntervals" -> 2, (* trait 0 *)
  "targetIntervals" -> 3, (* trait 1 *)
  "optimizationPower" -> 4, (* trait 2 *)
  "damageWeightSlope" -> 5, (* trait 3 *)
  "intervalComplexityNormPower" -> 6, (* trait 4 *)
  "intervalComplexityNormPreTransformerLogPrimePower" -> 7, (* trait 5a *)
  "intervalComplexityNormPreTransformerPrimePower" -> 8, (* trait 5b *)
  "intervalComplexityNormPreTransformerSizeFactor" -> 9, (* trait 5c *)
  "destretchedInterval" -> 10, (* trait 6 *)
  "nonprimeBasisApproach" -> 11, (* trait 7 *)
  "logging" -> 12,
  "quick" -> 13
|>;
tuningSchemeProperty[tuningSchemeProperties_, optionName_] := Part[tuningSchemeProperties, tuningSchemePropertiesPartsByOptionName[optionName]];

(* depending on whether asked for them by target-interval set scheme name, or manual listing *)
processTargetIntervals[targetIntervals_, t_, tPossiblyWithChangedDomainBasis_, forDamage_, heldIntervals_] := If[
  ToString[targetIntervals] == "Null",
  If[
    canUseOnlyHeldIntervalsMethod[heldIntervals, tPossiblyWithChangedDomainBasis],
    Null,
    Throw["no target-intervals"]
  ],
  If[
    ToString[targetIntervals] == "{}",
    If[
      forDamage,
      colify[IdentityMatrix[getDPrivate[tPossiblyWithChangedDomainBasis]]],
      Null
    ],
    If[
      StringQ[targetIntervals] && (StringMatchQ[targetIntervals, "*truncated integer limit triangle*"] || StringMatchQ[targetIntervals, "*TILT*"]),
      processTilt[targetIntervals, tPossiblyWithChangedDomainBasis],
      If[
        ToString[targetIntervals] == "primes",
        colify[IdentityMatrix[getDPrivate[tPossiblyWithChangedDomainBasis]]],
        If[
          StringQ[targetIntervals] && (StringMatchQ[targetIntervals, "*odd limit diamond*"] || StringMatchQ[targetIntervals, "*OLD*"]),
          processOld[targetIntervals, tPossiblyWithChangedDomainBasis],
          If[
            isTemperamentData[targetIntervals],
            parseTemperamentData[targetIntervals], (* only in this case do we take your word for it, if you put them right into vectors, you had better get the right basis *)
            parseQuotientLAndMaybeChangeBasis[targetIntervals, t, tPossiblyWithChangedDomainBasis]
          ]
        ]
      ]
    ]
  ]
];

parseQuotientLAndMaybeChangeBasis[targetIntervals_, t_, tPossiblyWithChangedDomainBasis_] := Module[
  {parsedQuotients, basisChange},
  
  parsedQuotients = getA[parseQuotientL[targetIntervals, t]];
  
  (* TODO: would be good to DRY this with the other place where we have to linear solve *)
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[tPossiblyWithChangedDomainBasis]],
    
    basisChange = Transpose[getDomainBasisChangeForC[
      getDomainBasis[tPossiblyWithChangedDomainBasis],
      getPrimes[ Length[ First[ getA[ parsedQuotients ] ] ] ]
    ]];
    
    parsedQuotients = Map[
      LinearSolve[basisChange, #]&,
      parsedQuotients
    ]
  ];
  
  colify[parsedQuotients]
];

processTilt[targetIntervals_, tPossiblyWithChangedDomainBasis_] := Module[
  {greatestInteger, nextPrime, maybeMaxInteger, tilt, basisChange},
  
  maybeMaxInteger = First[StringCases[StringReplace[targetIntervals, "truncated integer limit triangle" -> "TILT"], RegularExpression["(\\d*)-?TILT"] -> "$1"]];
  tilt = If[
    maybeMaxInteger == "",
    
    greatestInteger = Max[Map[Numerator, getDomainBasis[tPossiblyWithChangedDomainBasis]]];
    nextPrime = Prime[PrimePi[greatestInteger] + 1];
    getTilt[nextPrime - 1], (* default to integer immediately before the prime that is the next prime after the temperament's greatest prime *)
    
    getTilt[ToExpression[maybeMaxInteger]]
  ];
  
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[tPossiblyWithChangedDomainBasis]],
    tilt = filterTargetIntervalsForNonstandardDomainBasis[tilt, tPossiblyWithChangedDomainBasis]
  ];
  
  tilt = Map[quotientToPcv, tilt];
  
  (* TODO: not great that we go in and out of quotient and vector form so much; same with processOld[] *)
  tilt = padVectorsWithZerosUpToD[
    tilt,
    Max[Map[Length, tilt]]
  ];
  
  (* TODO: this probably needs to be done for OLD as well *)
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[tPossiblyWithChangedDomainBasis]],
    
    basisChange = Transpose[getDomainBasisChangeForC[
      getDomainBasis[tPossiblyWithChangedDomainBasis],
      getPrimes[ Length[ First[ getA[ tilt ] ] ] ]
    ]];
    
    tilt = Map[
      LinearSolve[basisChange, #]&,
      tilt
    ]
  ];
  
  colify[tilt]
];

processHeldOrDestretchedIntervals[heldOrDestretchedIntervals_, t_] := If[
  ToString[heldOrDestretchedIntervals] == "Null",
  Null,
  If[
    ToString[heldOrDestretchedIntervals] == "octave",
    getOctave[t],
    If[
      isTemperamentData[heldOrDestretchedIntervals],
      parseTemperamentData[heldOrDestretchedIntervals],
      parseQuotientL[heldOrDestretchedIntervals, t]
    ]
  ]
];


(* PARTS *)

getTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    targetIntervals,
    heldIntervals,
    optimizationPower,
    logging,
    
    generatorTuningMap,
    m,
    justTuningMap,
    
    temperedSideGeneratorsPartArg,
    temperedSideMappingPartArg,
    justSideGeneratorsPartArg,
    justSideMappingPartArg,
    eitherSideIntervalsPartArg,
    eitherSideMultiplierPartArg,
    powerArg,
    heldIntervalsArg
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  heldIntervals = tuningSchemeProperty[tuningSchemeProperties, "heldIntervals"]; (* trait 0 *)
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  temperedSideGeneratorsPartArg = generatorTuningMap;
  temperedSideMappingPartArg = m;
  justSideGeneratorsPartArg = justTuningMap;
  justSideMappingPartArg = getPrimesI[t];
  eitherSideIntervalsPartArg = targetIntervals;
  eitherSideMultiplierPartArg = If[ToString[eitherSideIntervalsPartArg] == "Null", Null, getDamageWeights[tuningSchemeProperties]];
  powerArg = optimizationPower;
  heldIntervalsArg = heldIntervals;
  
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
    printWrapper["heldIntervalsArg: ", formatOutput[heldIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* 𝒈 *)
    temperedSideMappingPartArg, (* 𝑀 *)
    justSideGeneratorsPartArg, (* 𝒋 *)
    justSideMappingPartArg, (* 𝑀ⱼ *)
    eitherSideIntervalsPartArg, (* T *)
    eitherSideMultiplierPartArg, (* 𝑊 *)
    powerArg,
    heldIntervalsArg
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
  "heldIntervalsArg" -> 8
|>;
tuningMethodArg[tuningMethodArgs_, partName_] := Part[tuningMethodArgs, tuningMethodArgsByName[partName]];


(* SHARED *)

getOctave[t_] := colify[Join[{1}, Table[0, getDPrivate[t] - 1]]];

getLogPrimeA[t_] := rowify[DiagonalMatrix[Log2[getDomainBasis[t]]]];

getJustTuningMap[t_] := multiplyToRows[
  rowify[Table[1200, getDPrivate[t]]],
  getLogPrimeA[t] (* in this context, the log-prime matrix is the primes-to-octaves converter, units of oct/p *)
];

getPrimesI[t_] := rowify[IdentityMatrix[getDPrivate[t]]];

getTuningSchemeMappings[t_] := Module[
  {generatorTuningMap, m, justTuningMap},
  
  generatorTuningMap = rowify[Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getRPrivate[t]}]];
  m = getM[t];
  justTuningMap = getJustTuningMap[t];
  
  {generatorTuningMap, m, justTuningMap}
];

(* similar to pseudoinverse, but works for any tuning so far described *)
tuningInverse[damageWeightOrComplexityPreTransformer_] := rowify[MapThread[
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
      getA[damageWeightOrComplexityPreTransformer][[1 ;; Last[Dimensions[getA[damageWeightOrComplexityPreTransformer]]]]]
    ],
    Table[
      Table[
        0,
        First[Dimensions[getA[damageWeightOrComplexityPreTransformer]]]
      ],
      Last[Dimensions[getA[damageWeightOrComplexityPreTransformer]]]
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
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    nonprimeBasisApproach, (* trait 7 *)
    
    damageWeights
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  damageWeightSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  
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
          intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
          intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
          intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
          nonprimeBasisApproach (* trait 7 *)
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

(* used by getTuningMapDamages and getTuningMapMeanDamage *)
getPowerMeanAbsError[tuningMethodArgs_] := Module[
  {absErrors, powerArg, targetIntervalCount, result},
  
  absErrors = getAbsMultipliedErrors[tuningMethodArgs];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  targetIntervalCount = First[Dimensions[getA[tuningMethodArg[tuningMethodArgs, "eitherSideIntervalsPartArg"]]]]; (* k *)
  
  If[debug == True, printWrapper["absErrors: ", absErrors]];
  
  result = If[
    powerArg == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
    Max[absErrors],
    
    Power[
      Total[Power[
        absErrors,
        powerArg
      ]] / targetIntervalCount,
      1 / powerArg
    ]
  ];
  
  result
];

(* returns errors in octaves *)
getMultipliedErrors[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
}] := Module[
  {temperedSide, justSide, errors},
  
  temperedSide = getTemperedOrJustSide[temperedSideGeneratorsPartArg, temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  justSide = getTemperedOrJustSide[justSideGeneratorsPartArg, justSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];
  
  errors = N[
    fixUpZeros[getL[temperedSide] - getL[justSide]],
    absoluteValuePrecision
  ];
  
  If[
    debug == True,
    printWrapper["temperedSide: ", formatOutput[temperedSide]];
    printWrapper["justSide: ", formatOutput[justSide]];
    printWrapper["errors: ", formatOutput[Map[If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, 4]]&, errors]]];
  ];
  
  errors
];

getAbsMultipliedErrors[args_] := Abs[getMultipliedErrors[args]];


(* COMPLEXITY *)

getComplexity[
  pcv_,
  t_,
  intervalComplexityNormPower_, (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower_, (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower_, (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor_, (* trait 5c *)
  nonprimeBasisApproach_ (* trait 7 *)
] := Module[
  {complexityPreTransformer},
  
  complexityPreTransformer = getComplexityPreTransformer[
    t,
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    nonprimeBasisApproach (* trait 7 *)
  ];
  
  Norm[
    getL[multiplyToCols[
      complexityPreTransformer,
      pcv
    ]],
    intervalComplexityNormPower
  ] / (1 + intervalComplexityNormPreTransformerSizeFactor)
];

(* This is different than getDamageWeights, this is nested within it;
this is to weight the quantities of the PC-vector entries before taking their norm to get an interval complexity, 
and these complexities are then gathered for each interval and applied 
(or their reciprocals applied, in the case of simplicity-weighting) as damageWeights;
when this method is used by getDamageWeights in getTuningMethodArgs, 
it covers any non-all-interval tuning scheme using this for its damage's interval complexity.
Note that complexity pre-transformers are relevant in ordinary (non-all-interval tuning schemes)
while simplicity pre-transformers are not. *)
getComplexityPreTransformer[
  t_,
  intervalComplexityNormPreTransformerLogPrimePower_, (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower_, (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor_, (* trait 5c *)
  nonprimeBasisApproach_ (* trait 7 *)
] := Module[{complexityPreTransformer},
  (* when used by getSimplicityPreTransformer in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-copfr-S (the L1 version of "Frobenius") and minimax-E-copfr-S ("Frobenius") *)
  complexityPreTransformer = rowify[IdentityMatrix[getDPrivate[t]]];
  
  If[
    (* when used by getSimplicityPreTransformer in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-S ("TOP") and minimax-ES ("TE") *)
    intervalComplexityNormPreTransformerLogPrimePower > 0,
    complexityPreTransformer = multiplyToRows[
      complexityPreTransformer,
      rowify[DiagonalMatrix[
        Power[
          If[
            nonprimeBasisApproach == "nonprime-based",
            Log2[getDomainBasis[t]], (* treat them as primes, regardless whether they actually are or not *)
            Log2[Map[Numerator[#] * Denominator[#]&, getDomainBasis[t]]]
          ],
          intervalComplexityNormPreTransformerLogPrimePower
        ]
      ]]
    ]
  ];
  (* this technically doesn't use getLogPrimeA[] because of the Power[] call in the middle, 
  but this is the other place where L gets used, but doesn't have units of oct/p, instead, has annotation only: (C) *)
  
  If[
    (* when used by getSimplicityPreTransformer in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-sopfr-S ("BOP") and minimax-E-sopfr-S ("BE") *)
    intervalComplexityNormPreTransformerPrimePower > 0,
    complexityPreTransformer = multiplyToRows[
      complexityPreTransformer,
      rowify[DiagonalMatrix[
        Power[
          If[
            nonprimeBasisApproach == "nonprime-based",
            getDomainBasis[t], (* treat them as primes, regardless whether they actually are or not *)
            Map[Numerator[#] * Denominator[#]&, getDomainBasis[t]]
          ],
          intervalComplexityNormPreTransformerPrimePower
        ]
      ]]
    ]
  ];
  
  If[
    (* when used by getSimplicityPreTransformer in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-lils-S ("Weil"), minimax-E-lils-S ("WE"), pure-stretched-octave minimax-lils-S ("Kees"), and pure-stretched-octave minimax-E-lils-S ("KE") *)
    intervalComplexityNormPreTransformerSizeFactor > 0,
    complexityPreTransformer = multiplyToRows[
      rowify[Join[
        getA[getPrimesI[t]],
        {Table[
          intervalComplexityNormPreTransformerSizeFactor,
          getDPrivate[t]
        ]}
      ]],
      complexityPreTransformer
    ]
  ];
  
  complexityPreTransformer
];


(* HELD-INTERVALS *)

canUseOnlyHeldIntervalsMethod[heldIntervals_, t_] := ToString[heldIntervals] != "Null" && Length[getA[heldIntervals]] == getRPrivate[t];



(* METHODS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR INTERVAL COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers held-octave OLD minimax-U "minimax", minimax-S "TOP", destretched-octave minimax-S "POTOP", 
minimax-sopfr-S "BOP", minimax-lils-S "Weil", destretched-octave minimax-lils-S "Kees" *)
(* a semi-analytical method *)
(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
coincidingDamageMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
}] := Module[
  {
    justTuningMap,
    mapping,
    eitherSideIntervalsAndMultipliersPart,
    targetIntervalCount,
    heldIntervalCount,
    minimaxTunings
  },
  
  (* if there are any held-intervals, we append them to the end of the target-intervals in this method, with weights of 1,
  so that they can participate in the system of equations our constraint matrices represent. *)
  heldIntervalCount = If[ToString[heldIntervalsArg] == "Null", 0, First[Dimensions[getA[heldIntervalsArg]]]];
  
  (* the mapped and weighted target-intervals on one side, and the just and weighted target-intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  justTuningMap = justSideGeneratorsPartArg;
  mapping = temperedSideMappingPartArg;
  eitherSideIntervalsAndMultipliersPart = multiplyToRows[
    maybeAugmentIntervalsForHeldIntervals[eitherSideIntervalsPartArg, heldIntervalsArg],
    maybeAugmentMultiplierForHeldIntervals[eitherSideMultiplierPartArg, heldIntervalCount]
  ];
  
  targetIntervalCount = Last[Dimensions[getA[eitherSideIntervalsAndMultipliersPart]]] - heldIntervalCount;
  
  (* our goal is to find the generator tuning map not merely with minimaxed damage, 
  but where the next-highest damage is minimaxed as well, and in fact every next-highest damage is minimaxed, all the way down.
  the tuning which has all damages minimaxed within minimaxed all the way down like this we can call a "nested-minimax".
  it's the only sensible optimum given a desire for minimax damage, so in general we can simply still call it "minimax".
  though people have sometimes distinguished this tuning scheme from the range of minimax tuning schemes with a prefix, 
  such as "TIPTOP tuning" versus "TOP tunings", although there is no value in "TOP tunings" given the existence of "TIPTOP",
  so you may as well just keep calling it "TOP" and refine its definition. anyway... *)
  
  (* the candidate generator tuning maps which nestedly minimaxes damage to as many target-intervals as is possible at this time.
  sometimes even that's not enough, and we need to scope our search space down to a specific region, and do another iteration of tie-breaking. 
  see `findFurtherNestedMinimaxTuningsByBlendingTiedMinimaxTunings`. *)
  minimaxTunings = findNestedMinimaxTuningsFromCoincidingDamagePoints[
    justTuningMap,
    mapping,
    eitherSideIntervalsAndMultipliersPart,
    targetIntervalCount,
    heldIntervalCount
  ];
  
  If[
    Length[minimaxTunings] > 1,
    
    (* TODO: there may be a way to refactor this to be much cleaner, how we need to not have the held-intervals anymore in this case? *)
    eitherSideIntervalsAndMultipliersPart = multiplyToRows[
      eitherSideIntervalsPartArg,
      eitherSideMultiplierPartArg
    ];
    
    minimaxTunings = findFurtherNestedMinimaxTuningsByBlendingTiedMinimaxTunings[
      minimaxTunings,
      justTuningMap,
      mapping,
      eitherSideIntervalsAndMultipliersPart,
      targetIntervalCount,
      heldIntervalCount
    ]
  ];
  
  If[
    Length[minimaxTunings] == 0,
    Null,
    First[minimaxTunings]
  ]
];

(* the clever way we continue our quest for a nested-minimax uses the same coinciding-damage point searching method used for that first pass,
  but now with a twist. so in the basic case, this method finds the points of coinciding damage.
  so now, instead of identifying coinciding damages throughout all of tuning damage space, we search only in a specific region,
  the region which can be described as a blend of the tied tunings from the previous iteration.
  
  we repeatedly do this until we eventually find a unique, nested-minimax optimum, even if we need blends of blends of tunings.
  
  once we've done that, though, our result isn't in the form of a generator tuning map yet. it's still in the form of a blend thereof.
  but with each iteration, we've been keeping track of the distortion applied, so that in the end we can undo them all.
  after undoing those, voilà, we're done! *)
findFurtherNestedMinimaxTuningsByBlendingTiedMinimaxTunings[
  inputMinimaxTunings_,
  inputJustTuningMap_,
  inputMapping_,
  eitherSideIntervalsAndMultipliersPart_,
  targetIntervalCount_,
  heldIntervalCount_
] := Module[
  {
    minimaxTunings,
    justTuningMapEquivalent,
    mappingEquivalent,
    
    generatorCount,
    
    freeGeneratorCount,
    dimensionOfTuningDamageSpace,
    
    countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing,
    
    deltas,
    anchorTuning,
    
    undoAllMultiplicativeIterationTransforms,
    undoAllAdditiveIterationTransforms
  },
  
  minimaxTunings = inputMinimaxTunings;
  (* to avoid complicating and over-abstracting `findNestedMinimaxTuningsFromCoincidingDamagePoints`, we're going to
  treat these as justTuningMap and mapping inside there, but here at least, we can recognize that these aren't really a
  just tuning map and a mapping. in the 2nd iteration of tie-breaking (i.e. 1st one involving this function) the 
  justTuningMapEquivalent will be a negative retuning map, and the mapping equivalent will be the mapping * deltas. *)
  justTuningMapEquivalent = inputJustTuningMap;
  mappingEquivalent = inputMapping;
  
  generatorCount = First[Dimensions[getA[mappingEquivalent]]];
  
  (* yes, these were both calculated inside `findNestedMinimaxTuningsFromCoincidingDamagePoints` but we only need them 
  outside it whenever repeat iterations are required in here, so we just re-calculate them now. *)
  (* first dimension is used instead of rank because of edge case with prime-based tuning of nonstandard domain bases
  where it is possible to get a row of all zeroes which would count as not full-rank *)
  freeGeneratorCount = generatorCount - heldIntervalCount;
  dimensionOfTuningDamageSpace = freeGeneratorCount + 1;
  
  (* initial state for our blend transformations: 
  identities per their respective operations of matrix multiplication and addition *)
  undoAllMultiplicativeIterationTransforms = rowify[IdentityMatrix[generatorCount]];
  undoAllAdditiveIterationTransforms = rowify[Table[0, generatorCount]];
  
  countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing = 0;
  
  While[
    (* if we're in this function at all, we're going to do at least our 2nd iteration of tie-breaking.
    but we may need 3, 4, or more iterations. *)
    Length[minimaxTunings] > 1,
    
    countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing += dimensionOfTuningDamageSpace;
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    anchorTuning = First[minimaxTunings];
    (* list of deltas between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one.
    so we're searching a space relative to the arbitrarily chosen tuning, and a blend of the differences between it and the others.
    so essentially where before we were checking damage graph intersections everywhere, now we only check the points 
    where the maximum count of damage graphs intersect while also satisfying the constraint of 
    being within the plane these tunings make together. (ideally it'd be within their convex hull, but it doesn't do that.)
    
    the stuff about normalizing and de-duping is not from Keenan's code. 
    this was found to be necessary upon implementing Dave's improvement, i.e. using Inverse[] rather than LinearSolve[].
    essentially, when a set of tied minimax tunings come back which represent the same essential deviation from the 
    arbitrarily chosen first tuning (e.g. there's three of them but they fall on a line instead of forming a triangle) 
    all of the matrices to invert will be singular. 
    for example, the TILT minimax-U tuning of blackwood temperament's first pass comes back with three tied minimax tunings:
    ⟨240.000 2786.314], ⟨240.000 2795.337], and ⟨240.000 2804.359], all three of which tie for the abbreviated 
    descending-sorted list of damages [18.0450 18.0450 18.0450]. the problem is that the matrix below would come out to
    [[0 18.0450] [0 9.0225]] otherwise, unless we rationalize (to be able to use HNF to reduce it), then reduce it, and
    remove all-zero rows so that it comes out to be full-rank. *)
    deltas = Map[
      getL[Part[minimaxTunings, #]] - getL[anchorTuning]&,
      Range[2, Length[minimaxTunings]]
    ];
    deltas = rowify[
      DeleteDuplicates[
        Map[Normalize, deltas],
        Function[{deltaA, deltaB}, deltaA == deltaB || deltaA == -deltaB]
      ]
    ];
    
    (* transform the just side to match that we're solving for tuning blends now, and track this additive part of the transform to undo later *)
    (* the right half of this is the primes tuning map, so this makes it a *negative* retuning map (𝒋-𝒕 rather than the typical 𝒕-𝒋) *)
    justTuningMapEquivalent = subtractT[justTuningMapEquivalent, multiplyToRows[anchorTuning, mappingEquivalent]];
    (* this seems complicated, but on the first pass, since undoAllMultiplicativeIterationTransforms is an identity matrix, and 
    undoAllAdditiveIterationTransforms starts out as a zeros matrix, this just sets it to anchorTuning.
    in other words, for the additive transforms, so far, the undo is the same as the do *)
    undoAllAdditiveIterationTransforms = addT[
      undoAllAdditiveIterationTransforms,
      multiplyToRows[anchorTuning, undoAllMultiplicativeIterationTransforms]
    ];
    
    (* include the deltas with the mapping, and track this multiplicative part of the transform to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the additive part of the transformation, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    mappingEquivalent = multiplyToRows[deltas, mappingEquivalent];
    (* again this seems complicated, but on the first pass, since undoAllMultiplicativeIterationTransforms starts off as an identity matrix, 
    this just sets undoAllMultiplicativeIterationTransforms to deltas. in other words, just like the additive transforms,
    the undo is the same as the do *)
    undoAllMultiplicativeIterationTransforms = multiplyToRows[deltas, undoAllMultiplicativeIterationTransforms];
    
    (* search again, now in this transformed state *)
    minimaxTunings = findNestedMinimaxTuningsFromCoincidingDamagePoints[
      justTuningMapEquivalent,
      mappingEquivalent,
      eitherSideIntervalsAndMultipliersPart,
      targetIntervalCount,
      heldIntervalCount,
      countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing
    ];
  ];
  
  If[
    Length[minimaxTunings] == 1,
    {addT[
      undoAllAdditiveIterationTransforms,
      multiplyToRows[First[minimaxTunings], undoAllMultiplicativeIterationTransforms] (* here's that left-multiplication mentioned earlier *)
    ]},
    {}
  ]
];

(* simply include the held-intervals, if any, with the target-intervals *)
maybeAugmentIntervalsForHeldIntervals[eitherSideIntervalsPartArg_, heldIntervalsArg_] := If[
  ToString[heldIntervalsArg] == "Null",
  eitherSideIntervalsPartArg,
  colify[Join[
    getA[eitherSideIntervalsPartArg],
    getA[heldIntervalsArg]
  ]]
];

(* simply add a weight of 1 for each held-interval that has been appended to the end of the target-intervals *)
maybeAugmentMultiplierForHeldIntervals[eitherSideMultiplierPartArg_, heldIntervalCount_] := Module[
  {multiplierA},
  
  If[
    heldIntervalCount == 0,
    
    eitherSideMultiplierPartArg,
    
    multiplierA = Transpose[getA[eitherSideMultiplierPartArg]];
    rowify[Join[
      joinColumnwise[
        multiplierA,
        zeroMatrix[
          First[Dimensions[multiplierA]],
          heldIntervalCount
        ]
      ],
      joinColumnwise[
        zeroMatrix[
          heldIntervalCount,
          Last[Dimensions[multiplierA]]
        ],
        identityMatrix[heldIntervalCount]
      ]
    ]]
  ]
];

findNestedMinimaxTuningsFromCoincidingDamagePoints[
  justTuningMap_,
  mapping_,
  eitherSideIntervalsAndMultipliersPart_,
  targetIntervalCount_,
  heldIntervalCount_,
  countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing_ : 0
] := Module[
  {
    justTuningMapA,
    eitherSideIntervalsAndMultipliersPartA,
    mappingSideA,
    justSideA,
    
    isAdvancedTieBreakingIteration,
    
    freeGeneratorCount,
    dimensionOfTuningDamageSpace,
    
    candidateTuning,
    candidateEmbedding,
    candidateAbbreviatedDescendingSortedListOfDamage,
    
    nthmostMinDamage,
    pointConstraints,
    maxCountOfDamagesThatCanBeMinimaxedAtThisTime,
    
    candidatePointConstraints,
    candidateEmbeddings,
    candidateTunings,
    candidateDamageLists,
    candidateAbbreviatedDescendingSortedListsOfDamage,
    
    newCandidateTunings,
    newCandidateEmbeddings,
    newCandidateAbbreviatedDescendingSortedListsOfDamage
  },
  
  justTuningMapA = getA[justTuningMap];
  eitherSideIntervalsAndMultipliersPartA = getA[eitherSideIntervalsAndMultipliersPart];
  mappingSideA = getA[multiplyToRows[mapping, eitherSideIntervalsAndMultipliersPart]];
  justSideA = getA[multiplyToRows[justTuningMap, eitherSideIntervalsAndMultipliersPart]];
  
  isAdvancedTieBreakingIteration = countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing > 0;
  
  (* in the basic case where no transforms have been applied, 
  these will be the same as the count of original target-intervals and the rank of the temperament, respectively; 
  otherwise the free generator count is actually the count of ties from the previous iteration minus 1 *)
  freeGeneratorCount = First[Dimensions[mappingSideA]] - If[isAdvancedTieBreakingIteration, 0, heldIntervalCount];
  dimensionOfTuningDamageSpace = freeGeneratorCount + 1;
  
  (* here's the meat of it: for each constrained linear system of equations, we isolate the generator embedding
  by doing a matrix inverse of everything else on its side. *)
  candidateEmbeddings = {};
  candidatePointConstraints = {};
  pointConstraints = getCoincidingDamagePointConstraints[
    freeGeneratorCount,
    targetIntervalCount,
    heldIntervalCount,
    dimensionOfTuningDamageSpace,
    isAdvancedTieBreakingIteration
  ];
  
  Do[
    candidateEmbedding = Quiet[Check[
      eitherSideIntervalsAndMultipliersPartA.pointConstraint.Inverse[mappingSideA.pointConstraint],
      "err"
    ]];
    If[
      (* don't keep ones where the matrices were singular (had no inverse), or ones containing Indeterminate or ComplexInfinity entries *)
      !StringQ[candidateEmbedding] && AllTrue[Map[NumericQ, N[Flatten[candidateEmbedding]]], TrueQ],
      AppendTo[candidateEmbeddings, candidateEmbedding];
      AppendTo[candidatePointConstraints, pointConstraint];
    ],
    {pointConstraint, pointConstraints}
  ];
  
  candidateTunings = Quiet[Map[justTuningMapA.#&, candidateEmbeddings]];
  
  (* each damage list is sorted in descending order;
  the list of lists itself is sorted corresponding to the candidate tunings *)
  candidateDamageLists = Quiet[Map[
    Function[
      {candidateTuning},
      N[Abs[First[candidateTuning.mappingSideA] - First[justSideA]], coincidingDamageMethodTiePrecision]
    ],
    candidateTunings
  ]];
  
  (* debugging: just all the reasonable vertical lines on the tuning damage graph *)
  If[
    debug == True,
    printWrapper["\nall coinciding damage points:"];
    printWrapper[Grid[N[Transpose[{
      Map[MatrixForm, Map[Transpose, candidatePointConstraints]],
      Map[MatrixForm, candidateTunings],
      Map[MatrixForm, candidateEmbeddings],
      Map[MatrixForm, Map[{#}&, candidateDamageLists]]
    }]], Frame -> All]]
  ];
  
  (* we need another version of this list of damage lists, where each damage list is sorted in descending order;
  so it loses its correspondence with the target-intervals, but all that matters is the amount of the damages.
  because first we're going to compare each tuning's actual maximum damage,
  then we compare each tuning's second-closest-to-maximum damage,
  then compare each third-closest-to-maximum, etc.
  *)
  candidateAbbreviatedDescendingSortedListsOfDamage = Map[ReverseSort, candidateDamageLists];
  (* and note that we don't iterate over *every* target-interval "index".
  we only check as many target-intervals as we could possibly nested-minimax by this point.
  we don't want to check any further than that, i.e. we don't want to check to make sure the damage lists coincide all
  the way down to the bottom. because if we did that, we'd leave some of the area of the region we need to check
  with the While[] loop in the parent function out of scope! *)
  maxCountOfDamagesThatCanBeMinimaxedAtThisTime = Min[
    countOfDamagesAlreadyAccountedForByPreviousIterationMinimaxing + dimensionOfTuningDamageSpace,
    targetIntervalCount
  ];
  candidateAbbreviatedDescendingSortedListsOfDamage = Map[Take[#, maxCountOfDamagesThatCanBeMinimaxedAtThisTime]&, candidateAbbreviatedDescendingSortedListsOfDamage];
  
  (*     
  here we work through the abbreviated, reverse-sorted tunings, repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  *)
  Do[
    newCandidateTunings = {};
    newCandidateEmbeddings = {};
    newCandidateAbbreviatedDescendingSortedListsOfDamage = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target-interval index *)
    nthmostMinDamage = Min[Map[Part[#, candidateAbbreviatedDescendingSortedListOfDamageIndex]&, candidateAbbreviatedDescendingSortedListsOfDamage]];
    Do[
      (* having found the minimum damage for this target-interval index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      candidateEmbedding = Part[candidateEmbeddings, minimaxTuningIndex];
      candidateAbbreviatedDescendingSortedListOfDamage = Part[candidateAbbreviatedDescendingSortedListsOfDamage, minimaxTuningIndex];
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[candidateAbbreviatedDescendingSortedListOfDamage, candidateAbbreviatedDescendingSortedListOfDamageIndex] <= nthmostMinDamage + coincidingDamageMethodTieAdjuster,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newCandidateEmbeddings, candidateEmbedding];
        AppendTo[newCandidateAbbreviatedDescendingSortedListsOfDamage, candidateAbbreviatedDescendingSortedListOfDamage]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    candidateEmbeddings = newCandidateEmbeddings;
    candidateAbbreviatedDescendingSortedListsOfDamage = newCandidateAbbreviatedDescendingSortedListsOfDamage,
    
    {candidateAbbreviatedDescendingSortedListOfDamageIndex, Range[maxCountOfDamagesThatCanBeMinimaxedAtThisTime]}
  ];
  
  (* debugging: all the tunings that were able to be minimaxed at this point (hopefully just one of them!) *)
  If[
    debug == True,
    printWrapper["\nminimax tunings:"];
    printWrapper[Grid[N[Transpose[{
      Map[MatrixForm, candidateTunings],
      Map[MatrixForm, candidateEmbeddings],
      Map[MatrixForm, Map[{#}&, candidateAbbreviatedDescendingSortedListsOfDamage]]
    }]], Frame -> All]]
  ];
  
  (* if duplicates are not deleted, then when differences are checked between tunings,
  some will come out to all zeroes, and this causes a crash *)
  Map[rowify, DeleteDuplicates[
    Map[First, candidateTunings],
    Function[{tuningA, tuningB}, AllTrue[MapThread[Abs[N[#1] - N[#2]] < 0.001&, {tuningA, tuningB}], TrueQ]]
  ]]
];

fixUpZeros[l_] := Map[
  If[Quiet[PossibleZeroQ[#]], 0, #]&,
  l
];

getCoincidingDamagePointConstraints[
  freeGeneratorCount_,
  targetIntervalCount_,
  heldIntervalCount_,
  dimensionOfTuningDamageSpace_,
  isAdvancedTieBreakingIteration_
] := Module[
  {pointConstraintA, pointConstraintAs, targetIntervalCombinations, directionPermutations, debugString},
  
  pointConstraintAs = {};
  
  (* here we iterate over every combination of r + 1 (rank = generator count, in the basic case) target-intervals 
  and for each of those combinations, looks at all permutations of their directions. 
  these make the coinciding-damage point set. each is a generator tuning map. the minimum of these will be the minimax tuning.
  
  e.g. for target-intervals 3/2, 5/4, and 5/3, with 1 generator, we'd look at three combinations (3/2, 5/4) (3/2, 5/3) (5/4, 5/3)
  and for the first combination, we'd look at both 3/2 \[Times] 5/4 = 15/8 and 3/2 \[Divide] 5/4 = 6/5.
  
  then what we do with each of those ReDPOTICs (relative-direction permutations of target-interval combinations) 
  is build a constraint matrix. we'll use this to transform our temperament's approximation of JI into an equality,
  where only a select few intervals will be held.
  
  e.g. when the target-intervals are just the primes (and thus an identity matrix we can ignore),
  and the temperament we're tuning is 12-ET with M = [12 19 28] and standard basis so p = [log₂2 log₂3 log₂5],
  then we have [12 19 28][g₁] = [log₂2 log₂3 log₂5], or a system of three equations:
  
  12g₁ = log₂2
  19g₁ = log₂3
  28g₁ = log₂5
  
  Obviously not all of those can be true, but that's the whole point.
  
  Now suppose we get the constraint matrix [1 1 0]. We multiply both sides of the setup by that:
  
  [1 1 0][12 19 28][g₁] = [1 1 0][log₂2 log₂3 log₂5]
  [31][g₁] = [log₂2 + log₂3]
  
  This leaves us with only a single equation:
  
  31g₁ = log₂6
  
  Or in other words, this tuning makes 6/1 pure, and divides it into 31 equal steps.
  If this temperament's mapping says it's 12 steps to 2/1 and 19 steps to 3/1, and it takes 31 steps to a pure 6/1,
  that implies that whatever damage there is on 2/1 is equal to whatever damage there is on 3/1, since they apparently cancel out.
  
  This constraint matrix [1 1 0] means that the target-interval combo was 2/1 and 3/1, 
  because those are the target-intervals corresponding to its nonzero elements.
  And both nonzero elements are +1 meaning that both target-intervals are combined in the same direction.
  If the target-intervals list had been [3/2, 4/3, 5/4, 8/5, 5/3, 6/5] instead, and the constraint matrix [1 0 0 0 -1 0],
  then that's 3/2 \[Divide] 5/3 = 5/2.
  
  The reason why we only need half of the permutations is because we only need relative direction permutations;
  they're anchored with the first target-interval always in the super direction.
  *)
  debugString = "";
  targetIntervalCombinations = Subsets[Range[1, targetIntervalCount], {dimensionOfTuningDamageSpace}];
  targetIntervalCombinations = If[
    Length[targetIntervalCombinations] * Power[freeGeneratorCount, 2] * targetIntervalCount > 275000,
    If[debug == True, debugString = debugString <> "pre-emptively aborting the analytical solution because we estimate it will exceed the time limit"];
    {},
    targetIntervalCombinations
  ]; (* anything above this is likely to exceed the time limit, so might as well save time *)
  
  If[debug == True, debugString = debugString <> "\ntargetIntervalCombinations: " <> ToString[targetIntervalCombinations]];
  
  Do[
    (* note that these are only generatorCount, not generatorCount + 1, because whichever is the first one will always be +1 *)
    If[debug == True, debugString = debugString <> "\n  targetIntervalCombination: " <> ToString[targetIntervalCombination]];
    
    directionPermutations = Tuples[{1, -1}, freeGeneratorCount];
    If[debug == True, debugString = debugString <> "\n  directionPermutations: " <> ToString[directionPermutations]];
    
    Do[
      If[debug == True, debugString = debugString <> "\n    directionPermutation: " <> ToString[directionPermutation]];
      
      pointConstraintA = Table[Table[0, targetIntervalCount], freeGeneratorCount];
      
      Do[
        pointConstraintA[[freeGeneratorIndex, Part[targetIntervalCombination, 1]]] = 1;
        pointConstraintA[[freeGeneratorIndex, Part[targetIntervalCombination, freeGeneratorIndex + 1]]] = Part[directionPermutation, freeGeneratorIndex],
        
        {freeGeneratorIndex, Range[freeGeneratorCount]}
      ];
      
      If[debug == True, debugString = debugString <> "\n      pointConstraintA: " <> ToString[pointConstraintA]];
      AppendTo[pointConstraintAs, pointConstraintA],
      
      {directionPermutation, directionPermutations}
    ],
    
    {targetIntervalCombination, targetIntervalCombinations}
  ];
  
  (* Also need to include coinciding-zero-damage points, i.e. the same ones that would be included in the zero-damage method 
  instead of r + 1 - h (the tuning damage space dimension) per combo, one less than that, r - h, the free generator count
  and then we don't need to worry about directional permutations because the errors are zero *)
  targetIntervalCombinations = Subsets[Range[1, targetIntervalCount], {freeGeneratorCount}];
  Do[
    pointConstraintA = Table[Table[0, targetIntervalCount], freeGeneratorCount];
    MapIndexed[
      Function[
        {targetIntervalIndex, freeGeneratorIndex},
        pointConstraintA[[freeGeneratorIndex, targetIntervalIndex]] = 1;
      ],
      targetIntervalCombination
    ];
    If[debug == True, debugString = debugString <> "\nunchanged-target-interval pointConstraintA: " <> ToString[pointConstraintA]];
    AppendTo[pointConstraintAs, pointConstraintA],
    {targetIntervalCombination, targetIntervalCombinations}
  ];
  
  If[debug == True, printWrapper[debugString]];
  
  (* augment the constraint matrix to account for held-intervals *)
  If[
    !isAdvancedTieBreakingIteration && heldIntervalCount > 0,
    pointConstraintAs = Map[augmentPointConstraintAForHeldIntervals[#, heldIntervalCount]&, pointConstraintAs]
  ];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  Map[Transpose, pointConstraintAs]
];

(* for each held-interval, add a row that is all zeros except for a one in the col corresponding to it and add the zeros in columns above it *)
augmentPointConstraintAForHeldIntervals[pointConstraintA_, heldIntervalCount_] := Join[
  joinColumnwise[
    pointConstraintA,
    zeroMatrix[First[Dimensions[pointConstraintA]], heldIntervalCount]
  ],
  joinColumnwise[
    zeroMatrix[heldIntervalCount, Last[Dimensions[pointConstraintA]]],
    identityMatrix[heldIntervalCount]
  ]
];

joinColumnwise[a1_, a2_] := Transpose[Join[Transpose[a1], Transpose[a2]]];
zeroMatrix[r_, c_] := ConstantArray[0, {r, c}];
identityMatrix[n_] := If[n == 0, {}, IdentityMatrix[n]];


(* METHODS: OPTIMIZATION POWER = 1 (MINIAVERAGE) OR INTERVAL COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tuning schemes use this *)
(* an analytical method *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where held-octave OLD minimax-U "minimax" is described;
however, this computation method is in general actually for miniaverage tuning schemes, not minimax tuning schemes. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond target-interval set,
namely that the places where damage to target-intervals are equal is the same where other targets are pure.
*)
zeroDamageMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
}] := Module[
  {
    generatorCount,
    heldIntervalCount,
    
    unchangedIntervalBasisIndices,
    candidateUnchangedIntervalBases,
    canonicalizedCandidateUnchangedIntervalBases,
    filteredCanonicalizedCandidateUnchangedIntervalBases,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalBases,
    candidateOptimumGenerators,
    candidateOptimumGeneratorTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorTuningMapIndices,
    optimumGeneratorTuningMapIndex
  },
  
  generatorCount = First[Dimensions[getA[temperedSideMappingPartArg]]];
  heldIntervalCount = If[ToString[heldIntervalsArg] == "Null", 0, First[Dimensions[getA[heldIntervalsArg]]]];
  
  unchangedIntervalBasisIndices = Subsets[
    Range[First[Dimensions[getA[eitherSideIntervalsPartArg]]]],
    {generatorCount - heldIntervalCount}
  ];
  candidateUnchangedIntervalBases = Map[
    colify[
      Join[
        Map[
          getA[eitherSideIntervalsPartArg][[#]]&,
          #
        ],
        If[ToString[heldIntervalsArg] == "Null", {}, getA[heldIntervalsArg]]
      ]
    ]&,
    unchangedIntervalBasisIndices
  ];
  canonicalizedCandidateUnchangedIntervalBases = Map[canonicalFormPrivate, candidateUnchangedIntervalBases];
  filteredCanonicalizedCandidateUnchangedIntervalBases = Select[canonicalizedCandidateUnchangedIntervalBases, MatrixRank[Transpose[getA[#]]] == generatorCount&];
  dedupedFilteredCanonicalizedCandidateUnchangedIntervalBases = DeleteDuplicates[filteredCanonicalizedCandidateUnchangedIntervalBases];
  candidateOptimumGenerators = Select[Map[
    getGeneratorEmbeddingFromUnchangedIntervalBasis[temperedSideMappingPartArg, #]&,
    dedupedFilteredCanonicalizedCandidateUnchangedIntervalBases
  ], Not[# === Null]&];
  candidateOptimumGeneratorTuningMaps = Map[multiplyToRows[justSideGeneratorsPartArg, #]&, candidateOptimumGenerators];
  candidateOptimumGeneratorTuningMapAbsErrors = Map[
    Total[getAbsMultipliedErrors[{
      #, (* note: this is an override for temperedSideGeneratorsPartArg, and it's the only reason why these tuning method args need to be unpacked *)
      temperedSideMappingPartArg,
      justSideGeneratorsPartArg,
      justSideMappingPartArg,
      eitherSideIntervalsPartArg,
      eitherSideMultiplierPartArg,
      powerArg,
      heldIntervalsArg
    }]]&,
    candidateOptimumGeneratorTuningMaps
  ];
  
  If[
    debug == True,
    printWrapper["candidateUnchangedIntervalBases: ", Map[formatOutput, candidateUnchangedIntervalBases]];
    printWrapper["canonicalizedCandidateUnchangedIntervalBases: ", Map[formatOutput, canonicalizedCandidateUnchangedIntervalBases]];
    printWrapper["filteredCanonicalizedCandidateUnchangedIntervalBases: ", Map[formatOutput, filteredCanonicalizedCandidateUnchangedIntervalBases]];
    printWrapper["dedupedFilteredCanonicalizedCandidateUnchangedIntervalBases: ", Map[formatOutput, dedupedFilteredCanonicalizedCandidateUnchangedIntervalBases]];
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

(* 𝐺 = U(𝑀U)⁻¹ *)
getGeneratorEmbeddingFromUnchangedIntervalBasis[m_, unchangedIntervals_] := Module[
  {mappedUnchangedIntervals},
  
  mappedUnchangedIntervals = multiplyToCols[m, unchangedIntervals];
  
  If[
    Det[getA[mappedUnchangedIntervals]] == 0,
    Null,
    multiplyToCols[unchangedIntervals, inverse[mappedUnchangedIntervals]]
  ]
];


(* METHODS: OPTIMIZATION POWER = 2 (MINIRMS) OR INTERVAL COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical method *)
(* covers held-octave OLD miniRMS-U "least squares", minimax-ES "TE", destretched-octave minimax-ES "POTE",
minimax-E-copfr-S "Frobenius", minimax-E-lils-S "WE", minimax-E-sopfr-S "BE" *)
pseudoinverseMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
}] := Module[
  {justSide, temperedSideButWithoutGeneratorsPart, nextToInverted, toBeInverted, rank, augmentedNextToInverted, augmentedToBeInverted},
  
  justSide = multiplyToRows[justSideGeneratorsPartArg, justSideMappingPartArg]; (* j *)
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg]; (* MTW, or MTₚSₚ *)
  nextToInverted = multiplyToCols[eitherSideIntervalsPartArg, eitherSideMultiplierPartArg, transpose[temperedSideButWithoutGeneratorsPart]]; (* TW(MTW)ᵀ, or TₚS(MTₚSₚ) *)
  toBeInverted = multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]; (* MTW(MTW)ᵀ, or MTₚSₚ(MTₚSₚ)ᵀ *)
  
  (* Technically the Aᵀ(AAᵀ)⁻¹ type of pseudoinverse is necessary. 
  Wolfram's built-in will sometimes use other techniques, which do not give the correct answer.
  Also it's good to break it down to show the parallelism between the simpler case and the held-interval case. *)
  
  If[
    ToString[heldIntervalsArg] == "Null",
    
    (* jTW(MTW)ᵀ(MTW(MTW)ᵀ)⁻¹, so it's the pseudoinverse of MTW left-multiplied by jTW *)
    (* or jTₚSₚ(MTₚSₚ)ᵀ(MTₚSₚ(MTₚSₚ)ᵀ)⁻¹, so it's the pseudoinverse of MTₚSₚ left-multiplied by jTₚSₚ *)
    maybeRowify[multiplyToRows[
      justSide,
      nextToInverted,
      inverse[
        toBeInverted
      ]
    ]],
    
    (* same as above, but we augment matrices with the held-intervals and mapped versions thereof *)
    rank = Last[Dimensions[getA[temperedSideGeneratorsPartArg]]];
    augmentedNextToInverted = augmentNextToInvertedForHeldIntervals[nextToInverted, heldIntervalsArg];
    augmentedToBeInverted = augmentToBeInvertedForHeldIntervals[toBeInverted, heldIntervalsArg, temperedSideMappingPartArg];
    rowify[Take[getL[maybeRowify[multiplyToRows[
      justSide,
      augmentedNextToInverted,
      inverse[
        augmentedToBeInverted
      ]
    ]]], rank]]
  ]
];

augmentNextToInvertedForHeldIntervals[nextToInverted_, heldIntervalsArg_] := colify[Join[
  getA[nextToInverted],
  getA[heldIntervalsArg]
]];

augmentToBeInvertedForHeldIntervals[toBeInverted_, heldIntervalsArg_, temperedSideMappingPartArg_] := Module[
  {heldIntervalCount, mappedHeldIntervals, zeros},
  
  heldIntervalCount = First[Dimensions[getA[heldIntervalsArg]]];
  mappedHeldIntervals = multiplyToRows[temperedSideMappingPartArg, heldIntervalsArg]; (* MH *)
  zeros = zeroMatrix[heldIntervalCount, heldIntervalCount];
  
  colify[Join[
    getA[rowify[joinColumnwise[
      getA[toBeInverted],
      getA[mappedHeldIntervals]
    ]]],
    getA[rowify[joinColumnwise[
      Transpose[getA[mappedHeldIntervals]],
      zeros
    ]]]
  ]]
];


(* METHODS: GENERAL OPTIMIZATION POWER (MINI-P-MEAN) OR GENERAL PRIME ERROR MAGNITUDE NORM POWER (MINI-P-NORM) *)

(* no historically described tuning schemes use this *)
(* a numerical method *)
(* this is for when the optimization power is not 1, 2, or \[Infinity] *)
powerSumMethod[tuningMethodArgs_] := Module[
  {temperedSideGeneratorsPartArg, solution},
  
  temperedSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideGeneratorsPartArg"];
  
  solution = getPowerSumSolution[tuningMethodArgs];
  
  rowify[getL[temperedSideGeneratorsPartArg] /. Last[solution]]
];

(* no historically described tuning schemes use this *)
(* a numerical method *)
(* this is the fallback for when zeroDamageMethod fails to find a unique solution *)
powerSumLimitMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
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
      heldIntervalsArg
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
    heldIntervalsArg,
    powerArg,
    minimizedPowerSum
  },
  
  temperedSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideGeneratorsPartArg"];
  temperedSideMappingPartArg = tuningMethodArg[tuningMethodArgs, "temperedSideMappingPartArg"];
  justSideGeneratorsPartArg = tuningMethodArg[tuningMethodArgs, "justSideGeneratorsPartArg"];
  justSideMappingPartArg = tuningMethodArg[tuningMethodArgs, "justSideMappingPartArg"];
  heldIntervalsArg = tuningMethodArg[tuningMethodArgs, "heldIntervalsArg"];
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  
  If[
    powerArg == \[Infinity],
    
    (* I thought it would be fine, but apparently Wolfram Language thinks the infinitieth-power-sum is "indeterminate" *)
    minimizedPowerSum = SetPrecision[Max[getAbsMultipliedErrors[tuningMethodArgs]], nMinimizePrecision],
    
    If[
      EvenQ[powerArg],
      
      minimizedPowerSum = SetPrecision[Total[Power[
        getMultipliedErrors[tuningMethodArgs],
        powerArg
      ]], nMinimizePrecision],
      
      minimizedPowerSum = SetPrecision[Total[Power[
        getAbsMultipliedErrors[tuningMethodArgs],
        powerArg
      ]], nMinimizePrecision]
    ]
  ];
  
  If[
    ToString[heldIntervalsArg] != "Null",
    minimizedPowerSum = {
      minimizedPowerSum,
      (* this is how we enforce the held-intervals. note that if augmented, we have to zero out their augmentation. *)
      SetPrecision[
        getL[multiplyToRows[temperedSideGeneratorsPartArg, temperedSideMappingPartArg, heldIntervalsArg]] == getL[multiplyToRows[justSideGeneratorsPartArg, justSideMappingPartArg, heldIntervalsArg]] /. {gAugmented -> 0},
        nMinimizePrecision
      ]
    }
  ];
  
  NMinimize[minimizedPowerSum, getL[temperedSideGeneratorsPartArg], WorkingPrecision -> nMinimizePrecision]
];

(* 
where the generators part is 1200\[Times]𝟏𝐿𝐺 (tempered) or 1200\[Times]𝟏𝐿𝐺ⱼ (just), the mapping part is 𝑀 (tempered) or 𝑀ⱼ (just), 
the intervals part is T (non-all-interval) or Tₚ (all-interval), and
the multiplier part is 𝑊 (non-all-interval) or 𝑆ₚ (all-interval), finds:
tempered non-all-interval: 1200\[Times]𝟏𝐿 𝐺 𝑀 T 𝑊
tempered all-interval:     1200\[Times]𝟏𝐿 𝐺 𝑀 Tₚ𝑆ₚ
just non-all-interval:     1200\[Times]𝟏𝐿 𝐺ⱼ𝑀ⱼT 𝑊 
just all-interval:         1200\[Times]𝟏𝐿 𝐺ⱼ𝑀ⱼTₚ𝑆ₚ
in the approximation 1200\[Times]𝟏𝐿𝐺𝑀T𝑊 \[TildeTilde] 1200\[Times]𝟏𝐿𝐺ⱼ𝑀ⱼT𝑊 or 1200\[Times]𝟏𝐿𝐺𝑀Tₚ𝑆ₚ \[TildeTilde] 1200\[Times]𝟏𝐿𝐺ⱼ𝑀ⱼTₚ𝑆ₚ
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
onlyHeldIntervalMethod[{
  temperedSideGeneratorsPartArg_,
  temperedSideMappingPartArg_,
  justSideGeneratorsPartArg_,
  justSideMappingPartArg_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_,
  powerArg_,
  heldIntervalsArg_
}] := multiplyToRows[justSideGeneratorsPartArg,
  multiplyToCols[
    heldIntervalsArg,
    inverse[
      multiplyToCols[temperedSideMappingPartArg, heldIntervalsArg]
    ]
  ]
];
