(* GRAPHING *)

powerMean[l_, power_] := If[
  power == \[Infinity],
  Max[l],
  Power[Mean[Power[l, power]], 1 / power]
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
    intervalComplexityNormPower,
    intervalComplexityNormMultiplierLogPrimePower,
    intervalComplexityNormMultiplierPrimePower,
    intervalComplexityNormMultiplierSizeFactor,
    
    tWithPossiblyChangedIntervalBasis,
    targetedIntervals,
    
    generatorsTuningMap,
    m,
    centsSummationMapAndLogPrimeMultiplier,
    
    meanPower,
    meanGraph,
    
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
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormMultiplierLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormMultiplierPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierPrimePower"]; (* trait 5b *)
  intervalComplexityNormMultiplierSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormMultiplierSizeFactor"]; (* trait 5c *)
  
  {generatorsTuningMap, m, centsSummationMapAndLogPrimeMultiplier} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      complexity = getComplexity[
        targetedIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        intervalComplexityNormPower, (* trait 4 *)
        intervalComplexityNormMultiplierLogPrimePower, (* trait 5a *)
        intervalComplexityNormMultiplierPrimePower, (* trait 5b *)
        intervalComplexityNormMultiplierSizeFactor (* trait 5c *)
      ];
      weighting = If[
        damageWeightingSlope == "unweighted",
        1,
        If[
          damageWeightingSlope == "complexityWeighted",
          complexity,
          1 / complexity
        ]
      ];
      error = getL[subtractT[
        multiplyToRows[generatorsTuningMap, m, targetedIntervalPcv],
        multiplyToRows[centsSummationMapAndLogPrimeMultiplier, targetedIntervalPcv]
      ]];
      damage = Abs[error] * weighting;
      
      damage
    ],
    breakByRowsOrCols[targetedIntervals]
  ];
  
  meanPower = If[
    optimizationPower == \[Infinity] && damageWeightingSlope == "simplicityWeighted" && ToString[targetedIntervals] == "Null",
    getDualPower[intervalComplexityNormPower],
    optimizationPower
  ];
  meanGraph = powerMean[targetedIntervalGraphs, meanPower] + 0.0001;
  
  AppendTo[plotArgs, {targetedIntervalGraphs, meanGraph}];
  
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
  MapIndexed[
    Function[
      {optimumGeneratorsTuningMapEntry, index},
      
      AppendTo[
        plotArgs,
        (* this is where we give it \[PlusMinus]2 ¢ around the exact tuning map *)
        {Part[getL[generatorsTuningMap], First[index]], optimumGeneratorsTuningMapEntry - 2, optimumGeneratorsTuningMapEntry + 2}
      ]
    ],
    
    getL[ optimumGeneratorsTuningMap]
  ];
  
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
