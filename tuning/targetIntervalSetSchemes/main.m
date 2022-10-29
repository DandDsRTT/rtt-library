(* TARGET INTERVAL SET SCHEMES *)

(* odd limit diamond *)
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


(* PROCESSING *)

processOld[targetIntervals_, tPossiblyWithChangedIntervalBasis_] := Module[
  {d, maybeOddLimit, old},
  
  d = getDPrivate[tPossiblyWithChangedIntervalBasis];
  
  maybeOddLimit = First[StringCases[StringReplace[targetIntervals, "odd limit diamond" -> "OLD"], RegularExpression["(\\d*)-?OLD"] -> "$1"]];
  
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
