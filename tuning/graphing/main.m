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
    optimumGeneratorTuningMap,
    
    tuningSchemeProperties,
    
    optimizationPower,
    damageWeightSlope,
    intervalComplexityNormPower,
    intervalComplexityNormPreTransformerLogPrimePower,
    intervalComplexityNormPreTransformerPrimePower,
    intervalComplexityNormPreTransformerSizeFactor,
    nonprimeBasisApproach,
    
    tWithPossiblyChangedDomainBasis,
    targetIntervals,
    
    generatorTuningMap,
    m,
    justTuningMap,
    
    meanPower,
    meanGraph,
    
    plotArgs,
    targetIntervalGraphs,
    r,
    plotStyle,
    image
  },
  
  t = parseTemperamentData[unparsedT];
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  optimumGeneratorTuningMap = optimizeGeneratorTuningMapPrivate[t, tuningSchemeOptions];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tWithPossiblyChangedDomainBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  damageWeightSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetIntervalGraphs = Map[
    Function[
      {targetIntervalPcv},
      
      complexity = getComplexity[
        targetIntervalPcv,
        tWithPossiblyChangedDomainBasis,
        intervalComplexityNormPower, (* trait 4 *)
        intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
        intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
        intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
        nonprimeBasisApproach (* trait 7 *)
      ];
      weighting = If[
        damageWeightSlope == "unityWeight",
        1,
        If[
          damageWeightSlope == "complexityWeight",
          complexity,
          1 / complexity
        ]
      ];
      error = getL[subtractT[
        multiplyToRows[generatorTuningMap, m, targetIntervalPcv],
        multiplyToRows[justTuningMap, targetIntervalPcv]
      ]];
      damage = Abs[error] * weighting;
      
      damage
    ],
    breakByRowsOrCols[targetIntervals]
  ];
  
  meanPower = If[
    optimizationPower == \[Infinity] && damageWeightSlope == "simplicityWeight" && ToString[targetIntervals] == "Null",
    getDualPower[intervalComplexityNormPower],
    optimizationPower
  ];
  meanGraph = powerMean[targetIntervalGraphs, meanPower] + 0.0001;
  
  AppendTo[plotArgs, {targetIntervalGraphs, meanGraph}];
  
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
  plotStyle = Join[Table[{Auto, Opacity[0.5]}, Length[targetIntervalGraphs]], {If[r == 1, {Black, Dashed}, {Texture[image]}]}];
  
  If[debug == True, printWrapper[plotStyle]];
  
  (* range *)
  MapIndexed[
    Function[
      {optimumGeneratorTuningMapEntry, index},
      
      AppendTo[
        plotArgs,
        (* this is where we give it \[PlusMinus]2 ¢ around the exact tuning map *)
        {Part[getL[generatorTuningMap], First[index]], optimumGeneratorTuningMapEntry - 2, optimumGeneratorTuningMapEntry + 2}
      ]
    ],
    
    getL[ optimumGeneratorTuningMap]
  ];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  AppendTo[plotArgs, PlotStyle -> plotStyle];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
  (* plot type *)
  r = getRPrivate[tWithPossiblyChangedDomainBasis];
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
