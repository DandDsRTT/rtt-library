(* ::Package:: *)

(* ::Title:: *)
(*RTT Library in Wolfram Language*)


(* ::Subtitle:: *)
(*by Douglas Blumeyer and Dave Keenan*)


SetOptions[
  EvaluationNotebook[],
  StyleDefinitions -> Notebook[{
    Cell[StyleData[StyleDefinitions -> "Default.nb"]],
    Cell[StyleData["Chapter"], ShowGroupOpener -> True, Selectable -> False, Editable -> False, Evaluatable -> False, CellMargins -> {{0, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Section"], ShowGroupOpener -> True, Selectable -> False, Editable -> False, Evaluatable -> False, CellMargins -> {{40, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Subsection"], ShowGroupOpener -> True, Selectable -> False, Editable -> False, Evaluatable -> False, CellMargins -> {{80, Inherited}, {Inherited, Inherited}}],
    Cell[StyleData["Subsubsection"], ShowGroupOpener -> True, Selectable -> False, Editable -> False, Evaluatable -> False, CellMargins -> {{120, Inherited}, {Inherited, Inherited}}]
  }]
]
defaultFontFamily = CurrentValue[{StyleDefinitions, "Text", FontFamily}];
defaultFontSize = CurrentValue[{StyleDefinitions, "Text", FontSize}];
closeSetupCell[] := SetOptions[EvaluationCell[], Background -> None, CellOpen -> False];
textBlock[enhancedTextChunks_, tag_] := Module[{},
  CellPrint[TextCell[Row[enhancedTextChunks], "Text", Evaluatable -> False, Editable -> False, CellTags -> tag]];
  closeSetupCell[];
];
hyperlink[text_, url_] := Hyperlink[Style[text, FontFamily -> defaultFontFamily, FontSize -> defaultFontSize, FontColor -> Blue], url];
inlineCode[text_] := Style[text, FontFamily -> "Courier", Background -> GrayLevel[0.95]]
br[] := "\n\n";
title[text_] := Style[text, Bold];
codeBlock[name_, textLines_] := Module[{},
  CellPrint[Cell[name, "CodeText", Selectable -> False, Evaluatable -> False, Editable -> False]];
  CellPrint[Cell[StringJoin[Riffle[textLines, "\n"]], "Input", Evaluatable -> False, Editable -> False, FontFamily -> "Courier", FontWeight -> "Plain", Background -> GrayLevel[0.95]]];
  closeSetupCell[];
];
blankSpace[] := closeSetupCell[];
image[url_] := Module[{},
  closeSetupCell[];
  Import[url]
];
bullet[] := "\n   • ";
nestedBullet[] := "\n       • ";
closeSetupCell[];


textBlock[{
  "This library contains Regular Temperament Theory (RTT) utilities implemented in ",
  hyperlink["Wolfram Language", "https://www.wolfram.com/language/"],
  ", a popular and capable programming language for working with math."
}, "top"]


blankSpace[]


textBlock[{
  title["how to use"],
  br[],
  "If you have access to Mathematica, the Wolfram Language desktop application (a paid option), ",
  " open ", inlineCode["notebook.nb"], ".",
  "Otherwise, the best option is to create a free account on ",
  hyperlink["Wolfram Cloud", "https://www.wolframcloud.com)"], 
  ", where you can use these functions for free right on the web ",
  "without downloading or setting anything up on your computer. ",
  "Just sign up for an account, upload ", inlineCode["rtt-library.nb"],
  " and evaluate the notebook; you'll be computing temperaments and such in no time. ",
  "FYI, any notebook you create has a lifespan of 60 days before Wolfram will recycle it, ",
  "so you'll have to copy and paste them to new notebooks or wherever if you don't want to lose your work.",
  br[],
  "WIP: Functionality from this library can also be explored from this ",
  hyperlink["web app", "https://danddsrtt.github.io"],
  "."
}, "how to use"]


blankSpace[]


textBlock[{
  title["output"],
  br[],
  "You can set the global variable ",
  inlineCode["format"],
  " to one of three things: ",
  "\n",
  bullet[], inlineCode["display"], " (default): ",
  "results displayed using Wolfram Language's ",
  inlineCode["MatrixForm"],
  ", as numbers arranged in rows and columns",
  bullet[], inlineCode["EBK"], ": results will be printed as ",
  hyperlink["Extended Bra-Ket notated", "https://en.xen.wiki/w/Extended_bra-ket_notation"],
  " strings, in our preferred style ",
  inlineCode["[⟨1 0 -4] ⟨0 1 4]}"],
  bullet[], inlineCode["Wolfram"], ": results will be displayed in our underlying data structure, e.g. ",
  inlineCode["{{{1, 0, -4}, {0, 1, 4}}, \"row\"}"]
}, "top output"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "IO",
  nestedBullet[], "quotient sets",
  nestedBullet[], "units",
  bullet[], "new sections",
  nestedBullet[], "scales & lattices",
  nestedBullet[], "temperament complexity & badness",
  nestedBullet[], "timbre",
  nestedBullet[], "notation",
  nestedBullet[], "temperament classification",
  nestedBullet[], "chords",
  br[],
  "Please report any bugs you find, and we'll be happy to investigate ASAP. Pull requests are also welcome."
}, "top roadmap"]


blankSpace[]


textBlock[{
  title["credits"],
  br[],
  "These implementations were developed by ",
  hyperlink["Dave Keenan", "https://en.xen.wiki/w/Dave_Keenan"],
  " and ",
  hyperlink["Douglas Blumeyer", "https://en.xen.wiki/w/Douglas_Blumeyer"],
  " from 2021 - 2023. "
}, "credits"]


blankSpace[]


blankSpace[]


(* ::Chapter:: *)
(*public*)


blankSpace[]


(* ::Section::Closed:: *)
(*temperament*)


textBlock[{
  "This section contains temperament exploration functions such as:",
  "\n",
  bullet[], inlineCode["getD"],
  bullet[], inlineCode["getR"],
  bullet[], inlineCode["getN"],
  bullet[], inlineCode["canonicalForm"],
  bullet[], inlineCode["dual"],
  bullet[], inlineCode["mapMerge"],
  bullet[], inlineCode["commaMerge"],
  bullet[], inlineCode["getGeneratorDetempering"],
  br[],
  "It is based on material from the following article series:",
  "\n",
  bullet[], hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: exploring temperaments", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_exploring_temperaments"],
  bullet[], hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: mappings", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_mappings"],
  bullet[], hyperlink["Defactoring algorithms", "https://en.xen.wiki/w/Defactoring_algorithms"],
  bullet[], hyperlink["Temperament merging", "https://en.xen.wiki/w/Temperament_merging"]
}, "temperament"]


blankSpace[]


textBlock[{
  title["data structures"],
  br[],
  "Temperament representations, such as mappings and comma bases, may be input like this:",
  "\n",
  bullet[], "12-ET's map: ", inlineCode["⟨12 19 28]"],
  bullet[], "meantone's mapping: ", inlineCode["[⟨1 0 -4] ⟨0 1 4]}"],
  bullet[], "meantone's comma: ", inlineCode["[4 -4 1⟩"],
  bullet[], "12-ET's comma basis: ", inlineCode["[[4 -4 1⟩ [-7 0 3⟩]"],
  bullet[], "quarter-comma meantone's tuning map: ", inlineCode["⟨1200.000 696.578]"],
  br[],
  "Those are left ", inlineCode["⟨"], " and right ", inlineCode["⟩"], 
  " angle braces there, but if these are not easy for you to type, less than ", inlineCode["<"], 
  " or greater than ", inlineCode[">"], " signs can be used instead.",
  br[],
  "Any amount of space is allowed, e.g. ", inlineCode["[ ⟨ 1 0 -4] ⟨ 0 1 4 ] ⟩"], ".",
  br[], 
  "Commas are also allowed, e.g. ", inlineCode["⟨12, 19, 28]"], ".",
  br[],
  "You can use outer brackets on the (co)vectors if preferred, e.g. ", inlineCode["[⟨12 19 28]}"], " or ", inlineCode["[[4 -4 1⟩]"], ".",
  br[],
  "For outer brackets, it's acceptable to use square brackets on both sides, so long as variance is indicated by the interior (co)vectors, e.g. ",
  inlineCode["[⟨1 0 -4] ⟨0 1 4]]"], ".",
  br[],
  "It is also acceptable to input things directly into this library's internal data structure, which is based on how",
  "Wolfram Language treats matrices as nested lists, e.g. ", inlineCode["{{{1, 0, -4}, {0, 1, 4}}, \"row\"}"],
  " or ", inlineCode["{{{4, -4, 1}, {-7, 0, 3}}, \"comma basis\"}"], ". These structures open with three braces (", inlineCode["{"], "), which Wolfram Language ",
  "uses for lists. The outermost list is an ordered pair of a matrix and a variance. The matrix in turn is a list of lists, ",
  "so that accounts for the other two braces. The variance is a string which tells whether the inner lists of the matrix ",
  "are vectors or covectors. Recognized variance strings for covariant matrices:",
  "\n",
  bullet[], inlineCode["map"],
  bullet[], inlineCode["maps"],
  bullet[], inlineCode["co"],
  bullet[], inlineCode["covector"],
  bullet[], inlineCode["covectors"],
  bullet[], inlineCode["covariant"],
  bullet[], inlineCode["m"],
  bullet[], inlineCode["mapping"],
  bullet[], inlineCode["et"],
  bullet[], inlineCode["ets"],
  bullet[], inlineCode["edo"],
  bullet[], inlineCode["edos"],
  bullet[], inlineCode["edomapping"],
  bullet[], inlineCode["edomappings"],
  bullet[], inlineCode["val"],
  bullet[], inlineCode["vals"],
  bullet[], inlineCode["with"],
  bullet[], inlineCode["row"],
  bullet[], inlineCode["rows"],
  bullet[], inlineCode["row-major order"],
  bullet[], inlineCode["row-major"],
  bullet[], inlineCode["row order"],
  br[],
  "Recognized variance strings for contravariant matrices:",
  "\n",
  bullet[], inlineCode["vector"],
  bullet[], inlineCode["vectors"],
  bullet[], inlineCode["contra"],
  bullet[], inlineCode["contravector"],
  bullet[], inlineCode["contravectors"],
  bullet[], inlineCode["contravariant"],
  bullet[], inlineCode["v"],
  bullet[], inlineCode["c"],
  bullet[], inlineCode["comma"],
  bullet[], inlineCode["commas"],
  bullet[], inlineCode["comma basis"],
  bullet[], inlineCode["comma-basis"],
  bullet[], inlineCode["commaBasis"],
  bullet[], inlineCode["comma_basis"],
  bullet[], inlineCode["i"],
  bullet[], inlineCode["interval"],
  bullet[], inlineCode["intervals"],
  bullet[], inlineCode["g"],
  bullet[], inlineCode["generator"],
  bullet[], inlineCode["generators"],
  bullet[], inlineCode["pcv"],
  bullet[], inlineCode["gcv"],
  bullet[], inlineCode["monzo"],
  bullet[], inlineCode["monzos"],
  bullet[], inlineCode["against"],
  bullet[], inlineCode["col"],
  bullet[], inlineCode["cols"],
  bullet[], inlineCode["column-major order"],
  bullet[], inlineCode["column-major"],
  bullet[], inlineCode["column order"],
  bullet[], inlineCode["col-major order"],
  bullet[], inlineCode["col-major"],
  bullet[], inlineCode["col order"]
}, "temperament data structures"]


blankSpace[]


textBlock[{
  title["edge cases"],
  br[],
  "For 0-rank mappings or 0-nullity comma bases, the temperament's dimensionality ", inlineCode["d"], " is encoded by a single row of ", inlineCode["d"],
  " zeros. For example, the mapping ", inlineCode["{{{0, 0, 0, 0}}, \"row\"}"], " indicates the 7-limit because it is 4D."
}, "temperament edge cases"]


blankSpace[]


textBlock[{
  title["conventional single-letter variable names"],
  br[],
  "For basic data structures: ",
  "\n",
  bullet[], inlineCode["l"], ": list (e.g. vector, covector)",
  bullet[], inlineCode["a"], ": matrix",
  br[],
  "For temperaments: ",
  "\n",
  bullet[], inlineCode["t = {a, variance}"], ": temperament, represented as a mapping or comma basis",
  bullet[], inlineCode["m = {a, variance}"], ": temperament, represented as a mapping",
  bullet[], inlineCode["c = {a, variance}"], ": temperament, represented as a comma basis",
  br[],
  "For properties of temperaments: ",
  "\n",
  bullet[], inlineCode["d"], ": dimensionality",
  bullet[], inlineCode["r"], ": rank",
  bullet[], inlineCode["n"], ": nullity",
  br[],
  "This library is designed such that every public method returns its result ",
  "in ", hyperlink["canonical form", "https://en.xen.wiki/w/canonical_form"], ". This is for convenience, and supported by the fact that in EA ",
  "the dual function was defined to automatically canonicalize."
}, "temperament vars"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "error handling",
  nestedBullet[], "enfactored temperaments",
  nestedBullet[], "temperament merging across different dimensionalities",
  bullet[], "additional features",
  nestedBullet[], "generator size manipulation (mingen form, etc.)",
  nestedBullet[], "*simplest* generator detempering",
  nestedBullet[], "unreduce mappings to merged ETs"
}, "temperament roadmap"]


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*utilities*)


getM[t_] := If[isRows[t] == True, t, dualPrivate[t]];


getC[t_] := If[isCols[t] == True, t, dualPrivate[t]];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*dimensions*)


textBlock[{
  title["get dimensionality"],
  br[],
  "Given a representation of a temperament as a mapping or comma basis, returns the dimensionality."
}, "getD"]


codeBlock[
  "Example 1",
  {
    "In    meantoneM = \"[\:27e81 0 -4] \:27e80 1 4]}\";",
    "      getD[meantoneM]",
    "",
    "Out   3"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneC = \"[4 -4 1\:27e9\";",
    "      getD[meantoneM]",
    "",
    "Out   3"
  }
]


blankSpace[]


blankSpace[]


getD[unparsedT_] := getDPrivate[parseTemperamentData[unparsedT]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["get rank"],
  br[],
  "Given a representation of a temperament as a mapping or comma basis, returns the rank."
}, "getR"]


codeBlock[
  "Example 1",
  {
    "In    meantoneM = \"[\:27e81 0 -4] \:27e80 1 4]}\";",
    "      getR[meantoneM]",
    "",
    "Out   2"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneC = \"[4 -4 1\:27e9\";",
    "      getR[meantoneM]",
    "",
    "Out   2"
  }
]


blankSpace[]


blankSpace[]


getR[unparsedT_] := getRPrivate[parseTemperamentData[unparsedT]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["get nullity"],
  br[],
  "Given a representation of a temperament as a mapping or comma basis, returns the nullity."
}, "getN"]


codeBlock[
  "Example 1",
  {
    "In    meantoneM = \"[\:27e81 0 -4] \:27e80 1 4]}\";",
    "      getN[meantoneM]",
    "",
    "Out   1"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneC = \"[4 -4 1\:27e9\";",
    "      getN[meantoneM]",
    "",
    "Out   1"
  }
]


blankSpace[]


blankSpace[]


getN[unparsedT_] := getNPrivate[parseTemperamentData[unparsedT]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


getDPrivate[t_] := innerLLength[getA[t]];


getRPrivate[t_] := If[
  isRows[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];


getNPrivate[t_] := If[
  isCols[t],
  If[
    hasA[t],
    MatrixRank[getA[t]],
    1
  ],
  getDPrivate[t] - MatrixRank[getA[t]]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*canonicalization*)


textBlock[{
  title["canonical form"],
  br[],
  "Returns the given temperament representation — whether mapping or comma basis — ",
  "in canonical form: defactored, then put into Hermite Normal Form."
}, "canonicalForm"]


codeBlock[
  "Example 1",
  {
    "In    someMeantoneM = \"[⟨5 8 12] ⟨7 11 16]}\";",
    "      canonicalForm[someMeantoneM]",
    "",
    "Out   \"[⟨1 0 -4] ⟨0 1 4]}\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    someMeantoneC = \"[-8 8 -2⟩\";",
    "      canonicalForm[someMeantoneC]",
    "",
    "Out   \"[4 -4 1⟩\""
  }
]


blankSpace[]


blankSpace[]


canonicalForm[unparsedT_] := formatOutput[canonicalFormPrivate[parseTemperamentData[unparsedT]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


canonicalFormPrivate[t_] := Module[{domainBasis, canonicalT},
  canonicalT = If[
    isCols[t],
    {canonicalCa[getA[t]], getVariance[t]},
    {canonicalMa[getA[t]], getVariance[t]}
  ];
  domainBasis = getDomainBasis[t];
  
  If[
    isStandardPrimeLimitDomainBasis[domainBasis],
    canonicalT,
    Join[canonicalT, {domainBasis}]
  ]
];


dhf[a_] := removeUnneededZeroLists[If[
  allZeros[a],
  a,
  hnf[colHermiteDefactor[a]]
]];


canonicalMa[ma_] := dhf[ma];


(* 
  The `ca` is the raw matrix extracted from the comma basis temperament object, which also contained variance information,
  and so the first call to `rotate180[]` is essentially accomplishing an antitranspose. That's because Wolfram's handling 
  of nested lists corresponds to the way that we write matrices row-first. The second `rotate180` followed by a `colify`
  (or equivalently adding "col" as the variance when rehydrating into a full temperament object) is the other antitranspose.
  Thus we complete the "antitranspose sandwich" as we describe in the Guide.
*)
canonicalCa[ca_] := rotate180[dhf[rotate180[ca]]]; 


hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];


colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*dual*)


textBlock[{
  title["dual"],
  br[],
  "Returns the dual for the given temperament representation ", 
  "(if given a mapping, the comma basis, or vice-versa)."
}, "dual"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 0 -4] ⟨0 1 4]}\";",
    "      dual[meantoneM]",
    "",
    "Out   \"[4 -4 1⟩\""
  }
]


blankSpace[]


blankSpace[]


dual[unparsedT_] := formatOutput[dualPrivate[parseTemperamentData[unparsedT]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


dualPrivate[t_] := Module[{dualA, domainBasis},
  dualA = If[
    isCols[t],
    canonicalMa[NullSpace[getA[t]]],
    canonicalCa[NullSpace[getA[t]]]
  ];

  dualA = If[
    dualA == {{}},
    {Table[0, getDPrivate[t]]},
    dualA
  ];

  domainBasis = getDomainBasis[t];
  If[
    isStandardPrimeLimitDomainBasis[domainBasis],
    {dualA, dualVariance[t]},
    {dualA, dualVariance[t], domainBasis}
  ]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*generator detempering*)


textBlock[{
  title["get generator detempering"],
  br[],
  "Given a representation of a temperament as a mapping or comma basis, ",
  "returns a generator detempering: for each generator, one JI interval that maps (tempers) to it."
}, "getGeneratorDetempering"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      getGeneratorDetempering[meantoneM]",
    "",
    "Out   \"[[1 0 0⟩ [-1 1 0⟩]\""
  }
]


blankSpace[]


blankSpace[]


getGeneratorDetempering[unparsedT_] := formatOutput[getGeneratorDetemperingPrivate[parseTemperamentData[unparsedT]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


getGeneratorDetemperingPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorDetempering},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorDetempering = right . Transpose[snf] . left;
  
  colify[Transpose[generatorDetempering]]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*merging*)


textBlock[{
  title["map merge"],
  br[],
  "Merges the given temperaments' maps:",
  "concatenates their mappings",
  "and puts the result into canonical form.",
  br[],
  "Can accept any number of temperament representations,",
  "as any combination of mappings or comma bases,",
  "but returns the temperament as a mapping."
}, "mapMerge"]


codeBlock[
  "Example 1",
  {
    "In    et5M = \"⟨5 8 12]\";",
    "      et7M = \"⟨7 11 16]\";",
    "      mapMerge[et5M, et7M]",
    "",
    "Out   \"[⟨1 0 -4] ⟨0 1 4]}\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    et7dM = \"⟨7 11 16 19]\";",
    "      et12M = \"⟨12 19 28 34]\";",
    "      et22M = \"⟨22 35 51 62]\";",
    "      mapMerge[et7dM, et12M, et22M]",
    "",
    "Out   \"[⟨1 0 0 -5] ⟨0 1 0 2] ⟨0 0 1 2]}\""
  }
]


blankSpace[]


blankSpace[]


mapMerge[unparsedTl___] := formatOutput[Apply[mapMergePrivate, Map[parseTemperamentData, {unparsedTl}]]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["comma merge"],
  br[],
  "Merges the given temperaments' comma bases:",
  "concatenates their comma bases",
  "and puts the result into canonical form.",
  br[],
  "Can accept any number of temperament representations,",
  "as any combination of mappings or comma bases,",
  "but returns the temperament as a comma basis."
}, "commaMerge"]


codeBlock[
  "Example 1",
  {
    "In    meantoneC = \"[4 -4 1⟩\";",
    "      porcupineC = \"[1 -5 3⟩\";",
    "      commaMerge[meantoneC, porcupineC]",
    "",
    "Out   \"[[-11 7 0⟩ [-7 3 1⟩]\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    mintC = \"[2 2 -1 -1⟩\";",
    "      meantoneC = \"[4 -4 1 0⟩\";",
    "      negriC = \"[-14 3 4 0⟩\";",
    "      commaMerge[mintC, meantoneC, negriC]",
    "",
    "Out   \"[[30 19 0 0⟩ [-26 15 1 0⟩ [-6 2 0 1⟩]\""
  }
]


blankSpace[]


blankSpace[]


commaMerge[unparsedTl___] := formatOutput[Apply[commaMergePrivate, Map[parseTemperamentData, {unparsedTl}]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


mapMergePrivate[tl___] := Module[{ml, domainBasisL, intersectedDomainBasis, tlWithIntersectedDomainBasis},
  ml = Map[If[isCols[#], dualPrivate[#], #]&, {tl}];
  domainBasisL = Map[getDomainBasis, {tl}];
  intersectedDomainBasis = Apply[domainBasisIntersection, domainBasisL];
  tlWithIntersectedDomainBasis = Map[changeDomainBasisForM[#, intersectedDomainBasis]&, ml];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getM, tlWithIntersectedDomainBasis]]], "row", intersectedDomainBasis}]
];


commaMergePrivate[tl___] := Module[{cl, domainBasisL, mergedDomainBasis, tlWithMergedDomainBasis},
  cl = Map[If[isCols[#], #, dualPrivate[#]]&, {tl}];
  domainBasisL = Map[getDomainBasis, {tl}];
  mergedDomainBasis = Apply[domainBasisMerge, domainBasisL];
  tlWithMergedDomainBasis = Map[changeDomainBasisForC[#, mergedDomainBasis]&, cl];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getC, tlWithMergedDomainBasis]]], "col", mergedDomainBasis}]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*domain basis*)


textBlock[{
  "This section is based on material from: ", hyperlink["Temperament merging across interval bases#Changing domain basis", "https://en.xen.wiki/w/Temperament_merging_across_interval_bases#Changing_basis"], "."
}, "domain basis"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "error handling",
  nestedBullet[], "impossible domain basis changes",
  bullet[], "additional features",
  nestedBullet[], "irrational interval bases"
}, "domain basis roadmap"]


blankSpace[]


blankSpace[]


textBlock[{
  title["canonical domain basis"],
  br[],
  "Gets the canonical form of the given domain basis."
}, "canonicalDomainBasis"]


codeBlock[
  "Example 1",
  {
    "In    domainBasis = \"2.7.9\";",
    "      canonicalDomainBasis[domainBasis]",
    "",
    "Out   \"2.9.7\""
  }
]


canonicalDomainBasis[unparsedDomainBasis_] := formatOutput[canonicalDomainBasisPrivate[parseDomainBasis[unparsedDomainBasis]]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["change domain basis"],
  br[],
  "Changes the domain basis for the given temperament.",
  br[],
  "If the target domain basis is not possible (such as a *super*space for a mapping, or a *sub*space for a comma basis), the function will error."
}, "changeDomainBasis"]


codeBlock[
  "Example 1",
  {
    "In    meantoneC = \"[4 -4 1⟩\";",
    "      targetDomainBasis = \"2.3.5.7\";",
    "      changeDomainBasis[meantoneC, targetDomainBasis]",
    "",
    "Out   \"[4 -4 1 0⟩\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneM = \"[⟨1 0 -4] ⟨0 1 4]}\";",
    "      targetDomainBasis = \"2.3\";",
    "      changeDomainBasis[meantoneM, targetDomainBasis]",
    "",
    "Out   \"[⟨1 0] ⟨0 1]⟩\""
  }
]


blankSpace[]


blankSpace[]


changeDomainBasis[unparsedT_, unparsedTargetDomainBasis_] := formatOutput[changeDomainBasisPrivate[parseTemperamentData[unparsedT], parseDomainBasis[unparsedTargetDomainBasis]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


canonicalDomainBasisPrivate[domainBasis_] := Module[{basisChangeA, canonicalBasisChangeA},
  basisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis], getDomainBasisDimension[domainBasis]];
  canonicalBasisChangeA = rotate180[removeAllZeroLists[hnf[rotate180[basisChangeA]]]];
  
  If[
    Length[canonicalBasisChangeA] == 0,
    {1},
    Map[super, Map[pcvToQuotient, canonicalBasisChangeA]]
  ]
];


getBasisA[t_] := Module[{domainBasis},
  domainBasis = getDomainBasis[t];
  
  colify[padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis], getDomainBasisDimension[domainBasis]]]
];


changeDomainBasisPrivate[t_, targetDomainBasis_] := If[
  isCols[t],
  changeDomainBasisForC[t, targetDomainBasis],
  changeDomainBasisForM[t, targetDomainBasis]
];


getStandardPrimeLimitDomainBasis[t_] := getPrimes[getDPrivate[t]];


isStandardPrimeLimitDomainBasis[domainBasis_] := canonicalDomainBasisPrivate[domainBasis] == getPrimes[Length[domainBasis]];


getDomainBasis[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitDomainBasis[t]
];


getDomainBasisDimension[domainBasis_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, domainBasis]]]]]];


domainBasisMerge[domainBasisL___] := Module[{concattedDomainBasis, concattedDomainBasisA},
  concattedDomainBasis = Apply[Join, {domainBasisL}];
  concattedDomainBasisA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedDomainBasis], getDomainBasisDimension[concattedDomainBasis]];
  
  canonicalDomainBasisPrivate[Map[pcvToQuotient, concattedDomainBasisA]]
];


domainBasisIntersectionBinary[domainBasis1_, domainBasis2_] := Module[{domainBasisDimension, basisChangeA1, basisChangeA2, allZerosFillerBasisChangeA, blockA, intersectedBasisChangeA, blockLHalf1, blockLHalf2},
  domainBasisDimension = Max[getDomainBasisDimension[domainBasis1], getDomainBasisDimension[domainBasis2]];
  basisChangeA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis1], domainBasisDimension];
  basisChangeA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, domainBasis2], domainBasisDimension];
  
  allZerosFillerBasisChangeA = Table[Table[0, Length[First[basisChangeA2]]], Length[basisChangeA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {basisChangeA1, basisChangeA1},
      {basisChangeA2, allZerosFillerBasisChangeA}
    }
  ]];
  
  intersectedBasisChangeA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedBasisChangeA = Join[intersectedBasisChangeA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedBasisChangeA = If[Length[intersectedBasisChangeA] == 0, {0}, intersectedBasisChangeA];
  
  canonicalDomainBasisPrivate[Map[pcvToQuotient, intersectedBasisChangeA]]
];


domainBasisIntersection[domainBasisL___] := Module[{intersectedDomainBasis},
  intersectedDomainBasis = First[{domainBasisL}];
  
  Do[
    intersectedDomainBasis = domainBasisIntersectionBinary[intersectedDomainBasis, domainBasis],
    {domainBasis, Drop[{domainBasisL}, 1]}
  ];
  
  canonicalDomainBasisPrivate[intersectedDomainBasis]
];


isSubspaceOf[candidateSubspaceDomainBasis_, candidateSuperspaceDomainBasis_] :=
    domainBasisMerge[candidateSubspaceDomainBasis, candidateSuperspaceDomainBasis] == candidateSuperspaceDomainBasis;
    
    
    changeDomainBasisForM[m_, targetSubspaceDomainBasis_] := If[
  getDomainBasis[m] == targetSubspaceDomainBasis,
  m,
  If[
    isSubspaceOf[getDomainBasis[m], targetSubspaceDomainBasis],
    Error,
    canonicalFormPrivate[{getA[m] . Transpose[getDomainBasisChangeForM[getDomainBasis[m], targetSubspaceDomainBasis]], "row", targetSubspaceDomainBasis}]
  ]
];


changeDomainBasisForC[c_, targetSuperspaceDomainBasis_] := If[
  getDomainBasis[c] == targetSuperspaceDomainBasis,
  c,
  If[
    isSubspaceOf[getDomainBasis[c], targetSuperspaceDomainBasis],
    canonicalFormPrivate[{Transpose[Transpose[getDomainBasisChangeForC[getDomainBasis[c], targetSuperspaceDomainBasis]] . Transpose[getA[c]]], "col", targetSuperspaceDomainBasis}],
    Error
  ]
];


(* express the target domain basis elements in terms of the origin domain basis elements *)
getDomainBasisChangeForM[originalSuperspaceDomainBasis_, targetSubspaceDomainBasis_] := Module[
  {
    domainBasisDimension,
    targetSubspaceBasisChangeA,
    originalSuperspaceBasisChangeA,
    domainBasisChange,
    domainBasisChangeCol,
    domainBasisChangeColEntry,
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry
  },
  
  domainBasisDimension = getDomainBasisDimension[Join[originalSuperspaceDomainBasis, targetSubspaceDomainBasis]];
  targetSubspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceDomainBasis], domainBasisDimension];
  originalSuperspaceBasisChangeA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceDomainBasis], domainBasisDimension];
  
  domainBasisChange = {};
  
  Do[
    domainBasisChangeCol = {};
    remainingToBeFactorizedTargetSubspaceBasisChangeAEntry = targetSubspaceBasisChangeAEntry;
    Do[
      domainBasisChangeColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        domainBasisChangeColEntry += 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry -= originalSuperspaceBasisChangeAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceBasisChangeAEntry, originalSuperspaceBasisChangeAEntry],
        domainBasisChangeColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceBasisChangeAEntry += originalSuperspaceBasisChangeAEntry
      ];
      
      domainBasisChangeCol = Join[domainBasisChangeCol, {domainBasisChangeColEntry}],
      {originalSuperspaceBasisChangeAEntry, originalSuperspaceBasisChangeA}
    ];
    domainBasisChange = Join[domainBasisChange, {domainBasisChangeCol}],
    {targetSubspaceBasisChangeAEntry, targetSubspaceBasisChangeA}
  ];
  
  domainBasisChange
];


(* yes, just swapping initial and target, that's all! *)
getDomainBasisChangeForC[originalSubspaceDomainBasis_, targetSuperspaceDomainBasis_] := getDomainBasisChangeForM[targetSuperspaceDomainBasis, originalSubspaceDomainBasis];


signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];


factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];


isNumeratorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry - superspaceFEntry}
], False];


isDenominatorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry + superspaceFEntry}
], False];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*addition*)


textBlock[{
  "This section contains the following functions:",
  "\n",
  bullet[], inlineCode["sum"],
  bullet[], inlineCode["diff"],
  br[],
  "It is based on material from ", hyperlink["Temperament addition", "https://en.xen.wiki/w/Temperament_addition"], "."
}, "addition"]


blankSpace[]


textBlock[{
  title["sum"],
  br[],
  "Sums the given temperaments: if they have the same dimensions ",
  "(same dimensionality, rank (and nullity)), ",
  "and are addable (can be put into a form where ",
  "they are identical except for a single basis vector (or covector, if covariant)), ",
  "entry-wise sums this pair of linearly independent basis (co)vectors, ",
  "recombines them with identical vectors (their linear-dependence basis), ",
  "corrects for negativity, then canonicalizes the result, ",
  "returning a single new temperament with the same dimensions as the inputs.",
  br[],
  "If the given temperaments are not the same dimensions and addable, ",
  "it will error.",
  br[],
  "Can accept temperament representations of different variances, ",
  "but it will return a temperament with the same variance ",
  "as the first given temperament representation."
}, "sum"]


codeBlock[
  "Example 1",
  {
    "In    meantoneC = \"[4 -4 1⟩\";",
    "      porcupineC = \"[1 -5 3⟩\";",
    "      sum[meantoneC, porcupineC]",
    "",
    "Out   \"[5 -9 4⟩\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneM = \"[⟨1 0 -4] ⟨0 1 4]}\";",
    "      porcupineM = \"[⟨1 2 3] ⟨0 3 5]}\";",
    "      sum[meantoneM, porcupineM]",
    "",
    "Out   \"[⟨1 1 1] ⟨0 4 9]}\""
  }
]


blankSpace[]


blankSpace[]


sum[unparsedT1_, unparsedT2_] := formatOutput[sumPrivate[parseTemperamentData[unparsedT1], parseTemperamentData[unparsedT2]]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["diff"],
  br[],
  "Diffs the given temperaments: if they have the same dimensions ",
  "(same dimensionality, rank (and nullity)), ",
  "and are addable (can be put into a form where ",
  "they are identical except for a single basis vector (or basis covector, if covariant)), ",
  "entry-wise diffs this pair of linearly independent basis (co)vectors, ",
  "recombines them with identical vectors (their linear-dependence basis), ",
  "corrects for negativity, then canonicalizes the result, ",
  "returning a single new temperament with the same dimensions as the inputs.",
  br[],
  "If the given temperaments are not the same dimensions and addable, ",
  "it will error.",
  br[],
  "Can accept temperament representations of different variances, ",
  "but it will return a temperament with the same variance ",
  "as the first given temperament representation."
}, "diff"]


codeBlock[
  "Example 1",
  {
    "In    meantoneC = \"[4 -4 1⟩\";",
    "      porcupineC = \"[1 -5 3⟩\";",
    "      diff[meantoneC, porcupineC]",
    "",
    "Out   \"[-3 -1 2⟩\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneM = \"[⟨1 0 -4] ⟨0 1 4]}\";",
    "      porcupineM = \"[⟨1 2 3] ⟨0 3 5]}\";",
    "      diff[meantoneM, porcupineM]",
    "",
    "Out   \"[⟨1 1 2] ⟨0 2 1]}\""
  }
]


blankSpace[]


blankSpace[]


diff[unparsedT1_, unparsedT2_] := formatOutput[diffPrivate[parseTemperamentData[unparsedT1], parseTemperamentData[unparsedT2]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


sumPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    t1,
    addition[t1, t2, True]
  ]
];


diffPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    Error,
    addition[t1, t2, False]
  ]
];


addition[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2] || intervalBasesDoNotMatch[t1, t2],
  Error,
  Module[{linearDependenceBasis},
    linearDependenceBasis = getLinearDependenceBasis[t1, t2];
    
    If[
      linearDependenceBasis === Error, (* not addable *)
      Error,
      addableAddition[t1, t2, linearDependenceBasis, isSum]
    ]
  ]
];


addableAddition[t1_, t2_, linearDependenceBasis_, isSum_] := Module[
  {
    t1LinearIndependenceBasisVector,
    t2LinearIndependenceBasisVector,
    t1t2LinearIndependenceBasisVector
  },
  
  t1LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t1, linearDependenceBasis];
  t2LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t2, linearDependenceBasis];
  
  t1t2LinearIndependenceBasisVector = If[
    isSum,
    t1LinearIndependenceBasisVector + t2LinearIndependenceBasisVector,
    t1LinearIndependenceBasisVector - t2LinearIndependenceBasisVector
  ];
  
  canonicalFormPrivate[{Join[linearDependenceBasis, {t1t2LinearIndependenceBasisVector}], getVariance[t1]}]
];


getLinearIndependenceBasisVector[t_, linearDependenceBasis_] := Module[{a, linearIndependenceBasisVector},
  a = addabilizationDefactor[t, linearDependenceBasis];
  linearIndependenceBasisVector = Last[a];
  If[isNegative[a, isCols[t]], linearIndependenceBasisVector = -linearIndependenceBasisVector];
  
  linearIndependenceBasisVector
];


addabilizationDefactor[t_, linearDependenceBasis_] := Module[
  {
    grade,
    explicitLinearDependenceBasisFormOfA
  },
  
  grade = getGrade[t];
  explicitLinearDependenceBasisFormOfA = getInitialExplicitLinearDependenceBasisFormOfA[t, linearDependenceBasis, grade];
  
  If[
    isLinearlyDependent[linearDependenceBasis],
    addabilizationDefactorWithNonemptyLinearDependenceBasis[t, linearDependenceBasis, grade, explicitLinearDependenceBasisFormOfA],
    explicitLinearDependenceBasisFormOfA
  ]
];


addabilizationDefactorWithNonemptyLinearDependenceBasis[t_, linearDependenceBasis_, grade_, explicitLdbFormOfAInput_] := Module[
  {
    explicitLinearDependenceBasisFormOfA,
    d,
    linearDependence,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  
  explicitLinearDependenceBasisFormOfA = explicitLdbFormOfAInput;
  d = getDPrivate[t];
  linearDependence = getLinearDependence[linearDependenceBasis];
  enfactoring = getGreatestFactor[explicitLinearDependenceBasisFormOfA];
  
  multiples = Table[Subscript[x, index], {index, linearDependence}];
  equations = Map[
    Function[
      dIndex,
      Mod[explicitLinearDependenceBasisFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * linearDependenceBasis[[multiplesIndex]][[dIndex]]],
        Range[linearDependence]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  explicitLinearDependenceBasisFormOfA[[grade]] = divideOutGcd[explicitLinearDependenceBasisFormOfA[[grade]] + getLinearDependenceBasisLinearCombination[linearDependenceBasis, result]];
  
  explicitLinearDependenceBasisFormOfA
];


variancesMatch[t1_, t2_] := getVariance[t1] == getVariance[t2];


getLinearDependenceBasis[t1_, t2_] := Module[{linearDependenceBasis},
  linearDependenceBasis = removeAllZeroLists[getA[dualPrivate[
    If[
      isCols[t1],
      mapMergePrivate[t1, t2],
      commaMergePrivate[t1, t2]
    ]
  ]]];
  
  If[
    isAddable[linearDependenceBasis, t1],
    linearDependenceBasis,
    Error
  ]
];


isAddable[linearDependenceBasis_, t_] := getLinearDependence[linearDependenceBasis] === getGrade[t] - 1;


getLinearDependence[linearDependenceBasis_] := Length[linearDependenceBasis];


dimensionsDoNotMatch[t1_, t2_] := getRPrivate[t1] != getRPrivate[t2] || getDPrivate[t1] != getDPrivate[t2];


intervalBasesDoNotMatch[t1_, t2_] := getDomainBasis[t1] != getDomainBasis[t2];


getGrade[t_] := If[isCols[t], getNPrivate[t], getRPrivate[t]];


isLinearlyDependent[linearDependenceBasis_] := getLinearDependence[linearDependenceBasis] > 0;


getInitialExplicitLinearDependenceBasisFormOfA[t_, linearDependenceBasis_, grade_] := Module[
  {
    linearIndependenceBasisSource,
    explicitLinearDependenceBasisFormOfA
  },
  
  linearIndependenceBasisSource = getA[If[isCols[t], getC[t], getM[t]]];
  explicitLinearDependenceBasisFormOfA = linearDependenceBasis;
  
  Do[
    candidate = hnf[Join[linearDependenceBasis, {candidateLinearIndependenceBasisVector}]];
    If[
      Length[explicitLinearDependenceBasisFormOfA] < grade && MatrixRank[candidate] > Length[linearDependenceBasis],
      explicitLinearDependenceBasisFormOfA = Join[explicitLinearDependenceBasisFormOfA, {candidateLinearIndependenceBasisVector}]
    ],
    {candidateLinearIndependenceBasisVector, linearIndependenceBasisSource}
  ];
  Take[explicitLinearDependenceBasisFormOfA, grade]
];


getGreatestFactor[a_] := Det[getGreatestFactorA[a]];


getGreatestFactorA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];


getLinearDependenceBasisLinearCombination[linearDependenceBasis_, linearDependenceBasisMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {linearDependenceBasis, linearDependenceBasisMultiplePermutation}
]];


isNegative[a_, isContravariant_] := Module[{largestMinorsL, entryFn, normalizingEntry},
  largestMinorsL = getLargestMinorsL[a];
  entryFn = If[isContravariant, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[largestMinorsL];
  
  normalizingEntry < 0
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*exterior algebra*)


textBlock[{
  hyperlink["EA", "https://en.xen.wiki/w/Intro_to_exterior_algebra_for_RTT"], " utiltiies.",
  br[],
  "This section contains the following functions: ",
  "\n",
  bullet[], inlineCode["eaGetD"],
  bullet[], inlineCode["eaGetR"],
  bullet[], inlineCode["eaGetN"],
  bullet[], inlineCode["eaCanonicalForm"],
  bullet[], inlineCode["eaDual"],
  bullet[], inlineCode["multivectorToMatrix"],
  bullet[], inlineCode["matrixToMultivector"],
  bullet[], inlineCode["progressiveProduct"],
  bullet[], inlineCode["regressiveProduct"],
  bullet[], inlineCode["interiorProduct"],
  br[],
  "It is based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to EA for RTT", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_EA_for_RTT"], ".",
  br[],
  "Several of these algorithms were adapted from or inspired by ones described by ", hyperlink["Gene Ward Smith", "https://en.xen.wiki/w/Gene_Ward_Smith"], "."
}, "EA"]


blankSpace[]


textBlock[{
  title["data structures"],
  br[],
  "Multivectors are implemented in this library as ordered triplets:",
  "\n    1. the list of largest-minors",
  "\n    2. the grade (the count of brackets)",
  "\n    3. the variance (whether the brackets point to the left or the right)",
  br[],
  "In the case of nilovectors, a fourth entry is required in order to fully specify the temperament: the dimensionality.",
  br[],
  "All multivectors in this library are varianced. So \"multivector\" refers to multivectors that may be of either variance,",
  "and \"contravariant multivector\" and \"covariant multivector\" are used for the specific variances.",
  br[],
  "Examples:",
  "\n",
  bullet[], "meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as ", inlineCode["{{1, 4, 4}, 2, \"co\"}"],
  bullet[], "meantone's multicomma [4 -4 1⟩ is input as ", inlineCode["{{4, -4, 1}, 1, \"contra\"}"],
  br[],
  "Recognized variance strings for covariant multivectors:",
  "\n",
  bullet[], inlineCode["\"co\""],
  bullet[], inlineCode["\"covector\""],
  bullet[], inlineCode["\"covectors\""],
  bullet[], inlineCode["\"multicovector\""],
  bullet[], inlineCode["\"covariant\""],
  bullet[], inlineCode["\"m\""],
  bullet[], inlineCode["\"map\""],
  bullet[], inlineCode["\"maps\""],
  bullet[], inlineCode["\"multimap\""],
  bullet[], inlineCode["\"val\""],
  bullet[], inlineCode["\"vals\""],
  bullet[], inlineCode["\"multival\""],
  bullet[], inlineCode["\"with\""],
  bullet[], inlineCode["\"mm\""],
  bullet[], inlineCode["\"row\""],
  bullet[], inlineCode["\"rows\""],
  br[],
  "Recognized variance strings for contravariant multivectors:",
  "\n",
  bullet[], inlineCode["\"contra\""],
  bullet[], inlineCode["\"contravector\""],
  bullet[], inlineCode["\"contravectors\""],
  bullet[], inlineCode["\"multicontravector\""],
  bullet[], inlineCode["\"contravariant\""],
  bullet[], inlineCode["\"v\""],
  bullet[], inlineCode["\"vector\""],
  bullet[], inlineCode["\"vectors\""],
  bullet[], inlineCode["\"c\""],
  bullet[], inlineCode["\"comma\""],
  bullet[], inlineCode["\"commas\""],
  bullet[], inlineCode["\"multicomma\""],
  bullet[], inlineCode["\"i\""],
  bullet[], inlineCode["\"interval\""],
  bullet[], inlineCode["\"intervals\""],
  bullet[], inlineCode["\"multinterval\""],
  bullet[], inlineCode["\"multiinterval\""],
  bullet[], inlineCode["\"monzo\""],
  bullet[], inlineCode["\"monzos\""],
  bullet[], inlineCode["\"multimonzo\""],
  bullet[], inlineCode["\"against\""],
  bullet[], inlineCode["\"wedgie\""],
  bullet[], inlineCode["\"mc\""],
  bullet[], inlineCode["\"col\""],
  bullet[], inlineCode["\"cols\""]
}, "EA data structures"]


blankSpace[]


textBlock[{
  title["edge cases"],
  br[],
  "Note that while nilovectors are essentially scalars, their first entry is still technically a largestMinorsL *list*,",
  "albeit one with a single entry. So for example, the scalar ", inlineCode["5"], " is input as ", inlineCode["{{5}, 0, v, d}"], 
  ". This indicates the number 5 nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has brackets.",
}, "EA edge cases"]


blankSpace[]


textBlock[{
  title["conventional single-character (or double-character) variable names"],
  "\n",
  bullet[], inlineCode["u = {largestMinorsL, variance, grade, d}"], ": temperament, represented as a multivector",
  bullet[], inlineCode["mm"], ": multimap, a covariant ", inlineCode["u"],
  bullet[], inlineCode["mc"], ": multicomma, a contravariant ", inlineCode["u"]
}, "EA vars"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "IO",
  nestedBullet[], "EBK notation",
  nestedBullet[], "matrix display",
  bullet[], "error handling",
  nestedBullet[], "progressive product across different dimensionalities",
  nestedBullet[], "minors lists not matching grade"
}, "EA roadmap"]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*dimensions*)


textBlock[{
  title["EA get dimensionality"],
  br[],
  "Given a representation of a temperament as a multivector, returns the dimensionality."
}, "eaGetD"]


codeBlock[
  "Example 1",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      eaGetD[meantoneMm]",
    "",
    "Out   3"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneMc = {{4, -4, 1}, 1, \"col\"};",
    "      eaGetD[meantoneMc]",
    "",
    "Out   3"
  }
]


blankSpace[]


blankSpace[]


eaGetD[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableD[u]
];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["EA get rank"],
  br[],
  "Given a representation of a temperament as a multivector, returns the rank."
}, "eaGetR"]


codeBlock[
  "Example 1",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      eaGetR[meantoneMm]",
    "",
    "Out   2"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneMc = {{4, -4, 1}, 1, \"col\"};",
    "      eaGetR[meantoneMc]",
    "",
    "Out   2"
  }
]


blankSpace[]


blankSpace[]


eaGetR[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableR[u]
];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["EA get nullity"],
  br[],
  "Given a representation of a temperament as a multivector, returns the nullity."
}, "eaGetN"]


codeBlock[
  "Example 1",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      eaGetN[meantoneMm]",
    "",
    "Out   1"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneMc = {{4, -4, 1}, 1, \"col\"};",
    "      eaGetN[meantoneMc]",
    "",
    "Out   1"
  }
]


blankSpace[]


blankSpace[]


eaGetN[u_] := If[
  isNondecomposable[u],
  Error,
  eaGetDecomposableN[u]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*canonicalization*)


textBlock[{
  title["EA canonical form"],
  br[],
  "Returns the given multivector in canonical form.",
  br[],
  "If a multimap, the GCD is extracted, ",
  "and the leading entry is normalized to positive. ",
  "If a multicomma, the GCD is extracted, ",
  "and the trailing entry is normalized to positive. "
}, "eaCanonicalForm"]


codeBlock[
  "Example 1",
  {
    "In    enfactoredMeantoneMm = {{2, 8, 8}, 2, \"row\"};",
    "      eaCanonicalForm[enfactoredMeantoneMm]",
    "",
    "Out   {{1, 4, 4}, 2, \"row\"}"
  }
]


codeBlock[
  "Example 2",
  {
    "In    wrongSignMeantoneMc = {{-4, 4, -1}, 1, \"col\"};",
    "      eaCanonicalForm[wrongSignMeantoneMc]",
    "",
    "Out   {{4, -4, 1}, 1, \"col\"}"
  }
]


blankSpace[]


blankSpace[]


eaCanonicalForm[u_] := If[
  allZerosL[eaGetLargestMinorsL[u]],
  u,
  If[
    isNondecomposable[u],
    Error,
    decomposableEaCanonicalForm[u]
  ]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*dual*)


textBlock[{
  title["EA dual"],
  br[],
  "Given a multivector, returns its dual in canonical form."
}, "eaDual"]


codeBlock[
  "Example 1",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      eaDual[meantoneMm]",
    "",
    "Out   {{4, -4, 1}, 1, \"col\"}"
  }
]


codeBlock[
  "Example 2",
  {
    "In    nilovector = {{1}, 0, \"col\"};",
    "      d = 3",
    "      eaDual[nilovector, d]",
    "",
    "Out   {{1}, 0, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


eaDual[u_] := If[
  isNondecomposable[u],
  Error,
  decomposableEaDual[u]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*conversion to and from matrix*)


textBlock[{
  title["multivector to matrix"],
  br[],
  "Given a temperament represented as a multivector, ",
  "returns the corresponding mapping or comma basis ",
  "(given a multimap, returns the corresponding mapping, or ",
  "given a multicomma, returns the corresponding comma basis). ",
  "The matrix is returned in canonical form."
}, "multivectorToMatrix"]


codeBlock[
  "Example",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      multivectorToMatrix[meantoneMm]",
    "",
    "Out   {{{1, 0, -4}, {0, 1, 4}}, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


multivectorToMatrix[u_] := Module[{grade, t},
  grade = eaGetGrade[u];
  t = If[
    grade == 0,
    nilovectorToA[u],
    If[
      grade == 1,
      monovectorToA[u],
      If[
        eaIsCols[u],
        mcToC[u],
        mmToM[u]
      ]
    ]
  ];
  
  If[t === Error, Error, canonicalFormPrivate[t]]
];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["matrix to multivector"],
  br[],
  "Given a temperament represented as a mapping or comma basis, ",
  "returns the corresponding multivector ",
  "(for a mapping, returns a multimap, or ",
  "for a comma basis, returns a multicomma). ",
  "The multivector is returned in canonical form. "
}, "matrixToMultivector"]


codeBlock[
  "Example",
  {
    "In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, \"row\"};",
    "      eaGetD[meantoneMm]",
    "",
    "Out   {{1, 4, 4}, 2, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


matrixToMultivector[t_] := eaCanonicalForm[
  If[
    isCols[t],
    {getLargestMinorsL[getA[t]], getNPrivate[t], getVariance[t], getDPrivate[t]},
    {getLargestMinorsL[getA[t]], getRPrivate[t], getVariance[t], getDPrivate[t]}
  ]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*merging*)


textBlock[{
  title["progressive product"],
  br[],
  "Given two multivectors, returns the multivector result for their progressive product.",
  br[],
  "Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.",
  br[],
  "Also known as the wedge product or the exterior product."
}, "progressiveProduct"]


codeBlock[
  "Example",
  {
    "In    et5 = {{5, 8, 12}, 1, \"row\"};",
    "      et7 = {{7, 11, 16}, 1, \"row\"};",
    "      progressiveProduct[et5, et7]",
    "",
    "Out   {{1, 4, 4}, 2, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


progressiveProduct[u1_, u2_] := Module[{grade1, grade2, grade, d, variance1, variance2, variance},
  grade1 = eaGetGrade[u1];
  grade2 = eaGetGrade[u2];
  grade = grade1 + grade2;
  d = eaGetD[u1];
  variance1 = eaGetVariance[u1];
  variance2 = eaGetVariance[u2];
  variance = If[variance1 != variance2, Error, variance1];
  
  If[
    variance === Error || grade > d,
    Error,
    eaCanonicalForm[
      tensorToU[
        TensorWedge[uToTensor[u1], uToTensor[u2]],
        grade,
        variance1,
        d
      ]
    ]
  ]
];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["regressive product"],
  br[],
  "Given two multivectors, returns the multivector result for their regressive product.",
  br[],
  "Works for any two multimaps, or any two multicommas, but multimaps and multicommas cannot be mixed.",
  br[],
  "Also known as the vee product."
}, "regressiveProduct"]


codeBlock[
  "Example",
  {
    "In    comma1 = {{44, -30, 19}, 2, \"col\"};",
    "      comma2 = {{28, -19, 12}, 2, \"col\"};",
    "      regressiveProduct[comma1, comma2]",
    "",
    "Out   {{4, -4, 1}, 1, \"col\"}"
  }
]


blankSpace[]


blankSpace[]


regressiveProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[eaDual[u1], eaDual[u2]];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["interior product"],
  br[],
  "Given two multivectors, returns the multivector result for their symmetric interior product. ",
  "By \"symmetric\", it is meant that it chooses either the right or left interior product ",
  "depending on the grades of the input multivectors."
}, "interiorProduct"]


codeBlock[
  "Example",
  {
    "In    u1 = {{1, 2, -3, -2, 1, -4, -5, 12, 9, -19}, 3, \"row\"};",
    "      u2 = {{-3, 2, -1, 2, -1}, 1, \"col\"};",
    "      interiorProduct[u1, u2]",
    "",
    "Out   {{6, -7, -2, 15, -25, -20, 3, 15, 59, 49}, 2, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


interiorProduct[u1_, u2_] := If[
  eaGetGrade[u1] >= eaGetGrade[u2],
  rightInteriorProduct[u1, u2],
  leftInteriorProduct[u1, u2]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*addition*)


textBlock[{
  "This section contains the following functions: ",
  "\n",
  bullet[], inlineCode["eaSum"],
  bullet[], inlineCode["eaDiff"],
  br[],
  "This section is based on material from ", hyperlink["Douglas Blumeyer and Dave Keenan's Intro to exterior algebra for RTT#Temperament addition", "https://en.xen.wiki/w/Douglas_Blumeyer_and_Dave_Keenan%27s_Intro_to_exterior_algebra_for_RTT#Temperament_addition"], "."
}, "EA addition"]


textBlock[{
  title["EA sum"],
  br[],
  "Sums the given multivectors: if they have the same dimensions ",
  "(same dimensionality, rank (and nullity)), ",
  "and are addable (can be decomposed into a set of vectors ",
  "that are identical except for a single vector (or covector, if covariant)), ",
  "entry-wise sums the multivectors, then canonicalizes the result, ",
  "returning a single new multivector with the same dimensions as the inputs. ",
  br[],
  "If the given multivectors are not the same dimensions and addable, ",
  "it will error. ",
  br[],
  "Can accept multivectors of different variances, ",
  "but it will return a multivector with the same variance ",
  "as the first given multivector. "
}, "eaSum"]


blankSpace[]


blankSpace[]


codeBlock[
  "Example 1",
  {
    "In    meantoneMc = {{4, -4, 1}, 1, \"col\"};",
    "      porcupineMc = {{1, -5, 3}, 1, \"col\"};",
    "      eaSum[meantoneMc, porcupineMc]",
    "",
    "Out   {{{5, -9, 4}}, \"col\"}"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      porcupineMm = {{3, 5, 1}, 2, \"row\"};",
    "      eaSum[meantoneMm, porcupineMm]",
    "",
    "Out   {{{1, 1, 1}, {0, 4, 9}}, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


eaSum[u1_, u2_] := eaAddition[u1, u2, True];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["EA diff"],
  br[],
  "Diffs the given multivectors: if they have the same dimensions ",
  "(same dimensionality, rank (and nullity)), ",
  "and are addable (can be decomposed into a set of vectors ",
  "that are identical except for a single vector (or covector, if covariant)), ",
  "entry-wise diffs the multivectors, then canonicalizes the result, ",
  "returning a single new multivector with the same dimensions as the inputs. ",
  br[],
  "If the given multivectors are not the same dimensions and addable, ",
  "it will error. ",
  br[],
  "Can accept multivectors of different variances, ",
  "but it will return a multivector with the same variance ",
  "as the first given multivector. "
}, "eaDiff"]


codeBlock[
  "Example 1",
  {
    "In    meantoneMc = {{4, -4, 1}, 1, \"col\"};",
    "      porcupineMc = {{1, -5, 3}, 1, \"col\"};",
    "      eaDiff[meantoneMc, porcupineMc]",
    "",
    "Out   {{-3, -1, 2}, 1, \"col\"}"
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneMm = {{1, 4, 4}, 2, \"row\"};",
    "      porcupineMm = {{3, 5, 1}, 2, \"row\"};",
    "      eaDiff[meantoneMm, porcupineMm]",
    "",
    "Out   {{2, 1, -3}, 2, \"row\"}"
  }
]


blankSpace[]


blankSpace[]


eaDiff[u1_, u2_] := eaAddition[u1, u2, False];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


(* ::Text:: *)
(*multivector utilities*)


eaIsCols[u_] := MemberQ[{
  "contra",
  "contravector",
  "contravectors",
  "multicontravector",
  "contravariant",
  "v",
  "vector",
  "vectors",
  "c",
  "comma",
  "commas",
  "multicomma",
  "i",
  "interval",
  "intervals",
  "multinterval",
  "multiinterval",
  "monzo",
  "monzos",
  "multimonzo",
  "against",
  "mc",
  "col",
  "cols"
}, eaGetVariance[u]];


eaIsRows[u_] := MemberQ[{
  "co",
  "covector",
  "covectors",
  "multicovector",
  "covariant",
  "m",
  "map",
  "maps",
  "multimap",
  "val",
  "vals",
  "multival",
  "with",
  "wedgie",
  "mm",
  "row",
  "rows"
}, eaGetVariance[u]];


eaGetDecomposableD[u_] := If[
  Length[u] == 4,
  Part[u, 4],
  Module[{largestMinorsL, grade, d},
    largestMinorsL = eaGetLargestMinorsL[u];
    grade = eaGetGrade[u];
    
    First[Association[Solve[
      Binomial[d, grade] == Length[largestMinorsL] && d >= 0,
      d,
      Integers
    ]]]
  ]
];


eaGetDecomposableR[u_] := If[
  eaIsRows[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableN[u]
];


eaGetDecomposableN[u_] := If[
  eaIsCols[u],
  eaGetGrade[u],
  eaGetDecomposableD[u] - eaGetDecomposableR[u]
];


eaIndices[d_, grade_] := Subsets[Range[d], {grade}];


isNondecomposable[variance_] := multivectorToMatrix[variance] === Error;


eaGetLargestMinorsL[u_] := Part[u, 1];


eaGetGrade[u_] := Part[u, 2];


eaGetVariance[u_] := Part[u, 3];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*canonicalization*)


decomposableEaCanonicalForm[u_] := Module[{largestMinorsL, grade, variance, normalizer},
  grade = eaGetGrade[u];
  variance = eaGetVariance[u];
  largestMinorsL = divideOutGcd[eaGetLargestMinorsL[u]];
  normalizer = If[
    (eaIsRows[u] && leadingEntry[largestMinorsL] < 0) || (eaIsCols[u] && trailingEntry[largestMinorsL] < 0),
    -1,
    1
  ];
  
  If[
    grade == 0,
    {normalizer * largestMinorsL, grade, variance, eaGetD[u]},
    {normalizer * largestMinorsL, grade, variance}
  ]
];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*dual*)


getDualV[u_] := If[
  eaIsRows[u],
  "col",
  "row"
];


decomposableEaDual[u_] := Module[{dualV, d, grade},
  dualV = getDualV[u];
  d = eaGetDecomposableD[u];
  grade = eaGetGrade[u];
  
  If[
    grade == 0,
    {{1}, d, dualV},
    If[
      grade == d,
      {{1}, 0, dualV, d},
      Module[{dualGrade, tensor, dualTensor, dualU},
        dualGrade = d - grade;
        tensor = uToTensor[u];
        dualTensor = HodgeDual[tensor];
        dualU = tensorToU[dualTensor, dualGrade, dualV, d];
        
        decomposableEaCanonicalForm[dualU]
      ]
    ]
  ]
];


uToTensor[u_] := Module[{d, grade, largestMinorsL},
  d = eaGetDecomposableD[u];
  grade = eaGetGrade[u];
  largestMinorsL = eaGetLargestMinorsL[u];
  
  SymmetrizedArray[
    MapThread[Rule[#1, #2]&, {eaIndices[d, grade], largestMinorsL}],
    ConstantArray[d, grade],
    Antisymmetric[All]
  ]
];


tensorToU[tensor_, grade_, variance_, d_] := Module[{rules, assoc, signTweak, largestMinorsL},
  rules = SymmetrizedArrayRules[tensor];
  
  If[
    allZerosL[Map[Last, rules]],
    {Table[0, Binomial[d, grade]], grade, variance},
    assoc = Association[rules];
    signTweak = If[eaIsRows[{{}, variance, grade, d}] && Mod[grade(d - grade), 2] == 1, -1, 1];
    largestMinorsL = signTweak * Map[If[KeyExistsQ[assoc, #], assoc[#], 0]&, eaIndices[d, grade]];
    
    {largestMinorsL, grade, variance}
  ]
];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*conversion to and from matrix*)


nilovectorToA[{largestMinorsL_, grade_, variance_, d_}] := {{Table[0, d]}, variance};


monovectorToA[u_] := {{eaGetLargestMinorsL[u]}, eaGetVariance[u]};


mmToM[mm_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mm];
  flattenedTensorA = hnf[Flatten[uToTensor[mm], grade - 2]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {Take[flattenedTensorA, grade], eaGetVariance[mm]}
  ]
];


mcToC[mc_] := Module[{grade, flattenedTensorA},
  grade = eaGetGrade[mc];
  flattenedTensorA = hnf[reverseInnerL[Flatten[uToTensor[mc], grade - 2]]];
  
  If[
    MatrixRank[flattenedTensorA] != grade,
    Error,
    {rotate180[Take[flattenedTensorA, grade]], eaGetVariance[mc]}
  ]
];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*merging*)


rightInteriorProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[eaDual[u1], u2];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];


leftInteriorProduct[u1_, u2_] := Module[{dualU},
  dualU = progressiveProduct[u1, eaDual[u2]];
  
  If[
    dualU === Error,
    Error,
    eaDual[dualU]
  ]
];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*addition*)


eaAddition[u1input_, u2input_, isSum_] := Module[{u1, u2},
  u1 = eaCanonicalForm[u1input];
  u2 = If[eaGetVariance[u2input] != eaGetVariance[u1], eaDual[u2input], eaCanonicalForm[u2input]];
  
  If[
    eaGetR[u1] != eaGetR[u2] || eaGetD[u1] != eaGetD[u2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetLargestMinorsL[u1] + eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}],
      eaCanonicalForm[{eaGetLargestMinorsL[u1] - eaGetLargestMinorsL[u2], eaGetGrade[u1], eaGetVariance[u1]}]
    ]
  ]
];


blankSpace[]


blankSpace[]


(* ::Section::Closed:: *)
(*tuning*)


textBlock[{
  "This section contains functions related to temperament tunings, and in particular, schemes for optimizing generator tunings:",
  "\n",
  bullet[], inlineCode["optimizeGeneratorTuningMap"],
  bullet[], inlineCode["getGeneratorTuningMapMeanDamage"],
  bullet[], inlineCode["getGeneratorTuningMapDamages"],
  bullet[], inlineCode["optimizeTuningMap"],
  bullet[], inlineCode["getTuningMapMeanDamage"],
  bullet[], inlineCode["getTuningMapDamages"],
  bullet[], inlineCode["graphTuningDamage"],
  bullet[], inlineCode["generatorTuningMapFromTAndTuningMap"],
  br[],
  "This article is based on material from the following articles:",
  "\n",
  bullet[], hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals"],
  bullet[], hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning computation", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_computation"],
  br[],
  "In all cases, tuning schemes may be specified by original name (e.g. ", inlineCode["\"TOP\""], "), systematic name (", inlineCode["\"minimax-S\""], "), or by individual parameters.",
  br[],
  "Note that anywhere a mapping is called for, a comma basis representation of a temperament will also work."
}, "tuning"]


blankSpace[]


textBlock[{
  title["traits"],
  br[],
  "You may notice that a numbered system of tuning scheme traits is used in the code. This is not necessarily advocated for",
  "general use; it's just something we found helpful when organizing our thoughts around the problem ourselves."
}, "tuning traits"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "tradeoff and monotone tuning ranges",
  bullet[], "projection and generators matrices",
  bullet[], "\"TOCTE\" tuning and possibly other new tunings invented recently by Flora Canou",
  bullet[], "custom precision/accuracy",
  bullet[], inlineCode["getComplexity"], " should support original complexity names",
  bullet[], "exact results (not decimals)"
}, "tuning roadmap"]


blankSpace[]


blankSpace[]


outputAccuracy = 3;

coincidingDamageMethodTieAdjuster = 0.000000001;

coincidingDamageMethodTiePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*optimization*)


textBlock[{
  title["optimize generator tuning map"],
  br[],
  "Given a mapping and tuning scheme, returns the optimum generator tuning map."
}, "optimizeGeneratorTuningMap"]


codeBlock[
  "Example 1",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeGeneratorTuningMap[",
    "          meantoneM, ",
    "          {",
    "              \"optimizationPower\" -> \[Infinity], ",
    "              \"damageWeightSlope\" -> \"simplicityWeight\"",
    "          }",
    "      ]",
    "",
    "Out   \"⟨1201.69 697.563]\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeGeneratorTuningMap[meantoneM, \"TOP\"]",
    "",
    "Out   \"⟨1201.70 697.563]\""
  }
]


codeBlock[
  "Example 3",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeGeneratorTuningMap[meantoneM, \"tilt miniRMS-copfr-EC\"]",
    "",
    "Out   \"⟨1200.522 1897.112]\""
  }
]


blankSpace[]


blankSpace[]


optimizeGeneratorTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["optimize tuning map"],
  br[],
  "Given a mapping and tuning scheme, returns the optimum tuning map."
}, "optimizeTuningMap"]


codeBlock[
  "Example 1",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeTuningMap[",
    "          meantoneM, ",
    "          {",
    "              \"optimizationPower\" -> \[Infinity], ",
    "              \"damageWeightSlope\" -> \"simplicityWeight\"",
    "          }",
    "      ]",
    "",
    "Out   \"⟨1201.69 1899.26 2790.25]\""
  }
]


codeBlock[
  "Example 2",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeTuningMap[meantoneM, \"TOP\"]",
    "",
    "Out   \"⟨1201.70 1899.26 2790.25]\""
  }
]


codeBlock[
  "Example 3",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      optimizeTuningMap[meantoneM, \"tilt miniRMS-copfr-EC\"]",
    "",
    "Out   \"⟨1200.522 1897.112 2786.363]\""
  }
]


blankSpace[]


blankSpace[]


optimizeTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


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
          held-octave minimax-E-lils-S "KE"/"CWE", held-octave minimax-ES "CTE" *)
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
  
  (* for e.g. minimax-lils-S "Weil" "WE" and variants, remove the junk final entry from the augmentation; 
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


optimizeTuningMapPrivate[t_, tuningSchemeSpec_] := multiplyToRows[optimizeGeneratorTuningMapPrivate[t, tuningSchemeSpec], t];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*mean damage*)


textBlock[{
  title["get generator tuning map mean damage"],
  br[],
  "Given a mapping, generator tuning map, and tuning scheme, returns how much damage this generator tuning map causes this temperament using this tuning scheme."
}, "getGeneratorTuningMapMeanDamage"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      quarterCommaGeneratorTuningMap = \"⟨1200.000 696.578]\";",    
    "      getGeneratorTuningMapMeanDamage[meantoneM, quarterCommaGeneratorTuningMap, \"minimax-S\"]",
    "",
    "Out   3.39251"
  }
]


blankSpace[]


blankSpace[]


getGeneratorTuningMapMeanDamage[unparsedT_, unparsedGeneratorTuningMap_, tuningSchemeSpec_] := getGeneratorTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorTuningMap], tuningSchemeSpec];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["get tuning map mean damage"],
  br[],
  "Given a mapping, tuning map, and tuning scheme, returns how much damage this tuning map causes this temperament using this tuning scheme."
}, "getTuningMapMeanDamage"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      quarterCommaTuningMap = \"⟨1200.000 1896.578 2786.314]\";",    
    "      getTuningMapMeanDamage[meantoneM, quarterCommaTuningMap, \"minimax-S\"]",
    "",
    "Out   3.39236"
  }
]


blankSpace[]


blankSpace[]


getTuningMapMeanDamage[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


getGeneratorTuningMapMeanDamagePrivate[t_, generatorTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorTuningMap, getM[t]];
  
  getTuningMapMeanDamagePrivate[t, tuningMap, tuningSchemeSpec]
];


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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*conversion*)


textBlock[{
  title["generator tuning map from temperament and tuning map"],
  br[],
  "Given a mapping and tuning map, returns the generator tuning map."
}, "generatorTuningMapFromTAndTuningMap"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      quarterCommaTuningMap = \"⟨1200.000 1896.578 2786.314]\";",    
    "      generatorTuningMapFromTAndTuningMap[meantoneM, quarterCommaTuningMap]",
    "",
    "Out   \"⟨1200.000 696.578]\""
  }
]


blankSpace[]


blankSpace[]


generatorTuningMapFromTAndTuningMap[unparsedT_, unparsedTuningMap_] := formatOutput[generatorTuningMapFromTAndTuningMapPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap]]];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


generatorTuningMapFromTAndTuningMapPrivate[t_, tuningMap_] := Module[
  {generatorTuningMap, m, justTuningMap},
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  (* the pseudoinverse is relied upon here to give a valid right-inverse to the mapping M, and since the tuning map \|01d495 = \|01d488\|01d440, \|01d488\|01d440\|01d440\:207a gives \|01d488 *)
  rowify[getL[tuningMap] . PseudoInverse[getA[m]]]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*damages*)


textBlock[{
  title["generator tuning map damages"],
  br[],
  "Given a mapping, generator tuning map, and tuning scheme, returns the damages to each of the target-intervals.",
  br[],
  "Note: for all-interval tuning schemes, it is impossible to return an infinitely-long list of damages to all intervals. Instead, the damages to the primes will be returned."
}, "getGeneratorTuningMapDamages"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      quarterCommaGeneratorTuningMap = \"⟨1200.000 696.578]\";",    
    "      getGeneratorTuningMapDamages[meantoneM, quarterCommaGeneratorTuningMap, \"minimax-S\"]",
    "",
    "Out   {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}"
  }
]


blankSpace[]


blankSpace[]


getGeneratorTuningMapDamages[unparsedT_, unparsedGeneratorTuningMap_, tuningSchemeSpec_] := getGeneratorTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorTuningMap], tuningSchemeSpec];


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["tuning map damages"],
  br[],
  "Given a mapping, tuning map, and tuning scheme, returns the damages to each of the target-intervals.",
  br[],
  "Note: for all-interval tuning schemes, it is impossible to return an infinitely-long list of damages to all intervals. Instead, the damages to the primes will be returned."
}, "getTuningMapDamages"]


codeBlock[
  "Example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\";",
    "      quarterCommaTuningMap = \"⟨1200.000 1896.578 2786.314]\";",    
    "      getTuningMapDamages[meantoneM, quarterCommaTuningMap, \"minimax-S\"]",
    "",
    "Out   {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}"
  }
]


blankSpace[]


blankSpace[]


getTuningMapDamages[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


getGeneratorTuningMapDamagesPrivate[t_, generatorTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = multiplyToRows[generatorTuningMap, getM[t]];
  
  getTuningMapDamagesPrivate[t, tuningMap, tuningSchemeSpec]
];


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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*target-interval set schemes*)


textBlock[{
  "This section contains functions which produce target-interval sets according to predefined schemes.",
  "\n",
  bullet[], inlineCode["getTilt"],
  bullet[], inlineCode["getOld"],
  bullet[], inlineCode["getOtonalChord"],
  br[],
  "This section is based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals#Target-interval set schemes", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals#Target-interval_set_schemes)"], "."
}, "target-interval set schemes"]


blankSpace[]


blankSpace[]


textBlock[{
  title["truncated integer limit triangle (TILT)"],
  br[],
  "Given a maximum integer, returns a list of quotients with numerator greater than the demoninator, numerator less than or ",
  "equal to the maximum integer, the size of the quotient between 15/13 and 13/4 (inclusive), and the numerator times the ",
  "denominator being less than the maximum integer times 13. "
}, "getTilt"]


codeBlock[
  "Example",
  {
    "In      getTilt[6]",
    "",
    "Out     {2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}"
  }
]


blankSpace[]


blankSpace[]


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


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["odd limit diamond (OLD)"],
  "Given a maximum odd number, returns a list of quotients following the pattern established for the historical tunings",
  "called \"Minimax\" and \"Least squares\", per the ",
  hyperlink["Target tunings page of the Xen wiki", "https://en.xen.wiki/w/Target_tuning"],
  " (essentially Partch's tonality",
  "diamond, but with 2/1 instead of 1/1, which is not useful as a tuning target)."
}, "getOld"]


codeBlock[
  "Example",
  {
    "In      getOld[5]",
    "",
    "Out     {2/1, 3/2, 4/3, 5/4, 8/5, 5/3, 6/5}"
  }
]


blankSpace[]


blankSpace[]


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


blankSpace[]


blankSpace[]


blankSpace[]


textBlock[{
  title["otonal chord"],
  br[],
  "Given a list of harmonics, returns a list of intervals between each of those harmonics."
}, "getOtonalChord"]


codeBlock[
  "Example",
  {
    "In      getOtonalChord[{4, 5, 6, 7}]",
    "",
    "Out     {5/4, 3/2, 7/4, 6/5, 7/5, 7/6}"
  }
]


blankSpace[]


blankSpace[]


getOtonalChord[harmonicsL_] :=
  DeleteDuplicates[
    Flatten[
      MapIndexed[
        Function[{denominator, index},
          Map[
            Function[{numerator},
              numerator / denominator
            ]
            ,
            Drop[harmonicsL, First[index]]
          ]
        ]
        ,
        Drop[harmonicsL, -1]
      ]
    ]
  ];
  
  
blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*all-interval tuning schemes*)


textBlock[{
  title["all-interval tuning schemes"],
  br[],
  "This section contains functions supporting to all-interval tuning schemes.",
  br[],
  "It is based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: all-interval tuning schemes", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_all-interval_tuning_schemes"], "."
}, "all-interval tuning schemes"]


blankSpace[]


blankSpace[]


getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


(* compare with getDamageWeights *)
getSimplicityPreTransformer[tuningSchemeProperties_] := Module[
  {
    t,
    intervalComplexityNormPower, (* trait 4 *)
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    nonprimeBasisApproach, (* trait 7 *)
    
    complexityPreTransformer
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  
  complexityPreTransformer = getComplexityPreTransformer[
    t,
    intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
    intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
    intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
    nonprimeBasisApproach (* trait 7 *)
  ];
  
  (* always essentially simplicity-weight *)
  tuningInverse[complexityPreTransformer]
];


(* compare with getTuningMethodArgs *)
getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties_] := Module[
  {
    t,
    heldIntervals,
    intervalComplexityNormPower,
    intervalComplexityNormPreTransformerSizeFactor,
    logging,
    
    generatorTuningMap,
    m,
    justTuningMap,
    primesI,
    transposedPrimesI,
    simplicityPreTransformer,
    retuningMagnitudeNormPower,
    
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
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  primesI = getPrimesI[t];
  transposedPrimesI = transpose[primesI];
  simplicityPreTransformer = getSimplicityPreTransformer[tuningSchemeProperties];
  retuningMagnitudeNormPower = getDualPower[intervalComplexityNormPower];
  
  If[
    (* handle tuning schemes like minimax-lils-S "Weil", minimax-E-lils-S "WE", held-octave minimax-lils-S "Kees", held-octave minimax-E-lils-S "KE"/"CWE" *)
    intervalComplexityNormPreTransformerSizeFactor != 0,
    
    (* augmentation of args *)
    temperedSideGeneratorsPartArg = augmentedTemperedSideGeneratorsPartArg[generatorTuningMap];
    temperedSideMappingPartArg = augmentedTemperedSideMappingPartArg[m, intervalComplexityNormPreTransformerSizeFactor];
    justSideGeneratorsPartArg = augmentedJustSideGeneratorsPartArg[justTuningMap];
    justSideMappingPartArg = augmentedJustSideMappingPartArg[primesI];
    eitherSideIntervalsPartArg = augmentedEitherSideIntervalsPartArg[transposedPrimesI];
    eitherSideMultiplierPartArg = augmentedEitherSideMultiplierPartArg[simplicityPreTransformer];
    heldIntervalsArg = augmentedHeldIntervalsArg[heldIntervals];
    powerArg = retuningMagnitudeNormPower, (* doesn't make sense to augment a power *)
    
    (* same thing as above, but no need to augment them *)
    temperedSideGeneratorsPartArg = generatorTuningMap;
    temperedSideMappingPartArg = m;
    justSideGeneratorsPartArg = justTuningMap;
    justSideMappingPartArg = primesI;
    eitherSideIntervalsPartArg = transposedPrimesI;
    eitherSideMultiplierPartArg = simplicityPreTransformer;
    heldIntervalsArg = heldIntervals;
    powerArg = retuningMagnitudeNormPower;
  ];
  
  If[
    logging == True,
    printWrapper["\n(ALL-INTERVAL TUNING SCHEME) TUNING METHOD ARGS"];
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* \|01d488 *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* \|01d440 *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* \|01d48b *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* \|01d440\:2c7c *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* T\:209a *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* \|01d446\:209a *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["heldIntervalsArg: ", formatOutput[heldIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* \|01d488 *)
    temperedSideMappingPartArg, (* \|01d440 *)
    justSideGeneratorsPartArg, (* \|01d48b *)
    justSideMappingPartArg, (* \|01d440\:2c7c *)
    eitherSideIntervalsPartArg, (* T\:209a *)
    eitherSideMultiplierPartArg, (* \|01d446\:209a *)
    powerArg,
    heldIntervalsArg
  }
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*destretching*)


textBlock[{
  title["destretched intervals"],
  br[],
  "This section contains material involved in tuning schemes that destretch intervals, including all-interval tuning schemes such as POTE and POTOP.",
  br[],
  "It is based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning fundamentals#Destretching vs. holding", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_fundamentals#Destretching_vs._holding"], "."
}, "destretched intervals"]


blankSpace[]


blankSpace[]


getDestretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap_, t_, destretchedInterval_] := Module[
  {
    generatorTuningMap,
    m,
    justTuningMap,
    justIntervalTuning,
    temperedIntervalTuning
  },
  
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[t];
  
  justIntervalTuning = multiplyToCols[justTuningMap, destretchedInterval];
  temperedIntervalTuning = multiplyToCols[optimumGeneratorTuningMap, m, destretchedInterval];
  
  (* take the ratio of the just version of the interval to stretch to, 
  and stretch everything by the factor it differs from the tempered result of tuning method *)
  rowify[(justIntervalTuning / temperedIntervalTuning) * getL[optimumGeneratorTuningMap]]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*domain basis*)


textBlock[{
  "Support for tunings of temperaments with nonstandard domain bases.",
  br[],
  "Based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning in nonstandard domains", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_in_nonstandard_domains"], "."
}, "tuning domain basis"]


blankSpace[]


blankSpace[]


changeBasis[domainBasisChange_, t_] := If[ToString[t] == "Null", t, multiplyToRows[domainBasisChange, t]];

getSimplestPrimeOnlyBasis[domainBasis_] := Module[
  {unsorted},
  
  unsorted = {};
  Do[
    unsorted = Join[unsorted, FactorInteger[domainBasisElement]],
    {domainBasisElement, domainBasis}
  ];
  
  DeleteDuplicates[Sort[Map[First, unsorted]]]
];


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


retrievePrimeDomainBasisGeneratorTuningMap[optimumGeneratorTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorDetempering, basisChange},
  
  m = getM[t];
  optimumTuningMap = multiplyToRows[optimumGeneratorTuningMap, m];
  generatorDetempering = getGeneratorDetemperingPrivate[originalT];
  basisChange = colify[getDomainBasisChangeForM[getDomainBasis[t], getDomainBasis[originalT]]];
  
  If[
    debug == True,
    printWrapper["optimumTuningMap: ", optimumTuningMap];
    printWrapper["basisChange: ", basisChange];
    printWrapper["generatorDetempering: ", generatorDetempering];
  ];
  
  multiplyToRows[optimumTuningMap, basisChange, generatorDetempering]
];


blankSpace[]


blankSpace[]


(* ::Text:: *)
(*target-interval set schemes*)


textBlock[{
  "Support for target-interval set schemes for temperaments with nonstandard domain bases.",
  br[],
  "Based on material from ", hyperlink["Dave Keenan & Douglas Blumeyer's guide to RTT: tuning in nonstandard domains", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_tuning_in_nonstandard_domains"], "."
}, "nonstandard domain basis target-interval set schemes"]


filterTargetIntervalsForNonstandardDomainBasis[targetIntervalL_, tWithNonstandardDomainBasis_] := Module[
  {pcvs, basis, maxPrimeD, possibleTargetIntervalL, basisWithPcv},
  
  pcvs = Map[quotientToPcv, targetIntervalL];
  basis = Map[quotientToPcv, getDomainBasis[tWithNonstandardDomainBasis]];
  maxPrimeD = Max[
    Join[
      Map[Length, pcvs],
      Map[Length, basis]
    ]
  ];
  pcvs = padVectorsWithZerosUpToD[pcvs, maxPrimeD];
  basis = padVectorsWithZerosUpToD[basis, maxPrimeD];
  
  possibleTargetIntervalL = {};
  Do[
    basisWithPcv = Join[basis, {pcv}];
    If[
      removeAllZeroLists[hnf[basisWithPcv]] == hnf[basis], (* the canonical forms of the bases must match *)
      possibleTargetIntervalL = Join[possibleTargetIntervalL, {pcv}]
    ],
    {pcv, pcvs}
  ];
  
  Map[pcvToQuotient, possibleTargetIntervalL]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*graphing*)


textBlock[{
  "This section contains functions related to graphing tuning damage, both 2D and 3D.",
  br[],
  title["graph tuning damage"],
  br[],
  "Given a representation of a temperament as a mapping or comma basis, and a tuning scheme, ",
  "graphs the damage to the target-intervals within a close range around the optimum tuning. ",
  "Graphs in 2D for a rank-1 temperament, 3D for a rank-2 temperament, and errors otherwise."
}, "graphing"]


blankSpace[]


textBlock[{
  title["roadmap"],
  br[],
  "The following features are planned:",
  "\n",
  bullet[], "visualize the solution",
  bullet[], "user-controlled zoom",
  bullet[], "3D graph checkerboard to translucent black continuum per optimization power",
  bullet[], "ability to specify which norms are included",
  bullet[], "titles and other info",
  bullet[], "contour-style topographic graphs for 3D",
  bullet[], "test failures automatically graph",
  bullet[], "opacity configuration",
  bullet[], "ability to explode out into a separate graph for each target-interval"
}, "graphing roadmap"]


blankSpace[]


blankSpace[]


codeBlock[
  "2D example",
  {
    "In    12etM = \"⟨12 19 28]\";",
    "      graphTuningDamage[12etM, \"miniRMS-copfr-EC\"]",
    "",
    "Out   (2D graph)"
  }
]


(* ::Text:: *)
(*Here's an example of a 2D tuning graph:*)


image["https://raw.githubusercontent.com/DandDsRTT/rtt-library/main/2D%20graph%20example.jpg"]


blankSpace[]


blankSpace[]


codeBlock[
  "3D example",
  {
    "In    meantoneM = \"[⟨1 1 0] ⟨0 1 4]}\"",
    "      graphTuningDamage[meantoneM, \"miniRMS-copfr-EC\"]",
    "",
    "Out   (3D graph)"
  }
]


(* ::Text:: *)
(*Here's an example of a 3D tuning graph:*)


image["https://raw.githubusercontent.com/DandDsRTT/rtt-library/main/3D%20graph%20example.jpg"]


blankSpace[]


blankSpace[]


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
        (* this is where we give it \[PlusMinus]2\:202f\[Cent] around the exact tuning map *)
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


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*private*)


powerMean[l_, power_] := If[
  power == \[Infinity],
  Max[l],
  Power[Mean[Power[l, power]], 1 / power]
];


blankSpace[]


blankSpace[]


(* ::Chapter::Closed:: *)
(*private*)


blankSpace[]


(* ::Section::Closed:: *)
(*main*)


blankSpace[]


(* ::Subsection::Closed:: *)
(*music utilities*)


octaveReduce[quotient_] := Module[{localQuotient},
  localQuotient = quotient;
  While[localQuotient >= 2, localQuotient /= 2];
  While[localQuotient < 1, localQuotient *= 2];
  
  localQuotient
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*math utilities*)


getPrimes[count_] := Map[Prime, Range[count]];


quotientToPcv[quotient_] := Module[{factorization, greatestPrime, count, primes, pcv, currentPrimeIndex},
  factorization = FactorInteger[quotient];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  pcv = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      pcv[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    pcv
  ]
];


pcvToQuotient[pcv_] := Module[{quotient, primeIndex},
  quotient = 1;
  primeIndex = 1;
  Do[
    quotient = quotient * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, pcv}
  ];
  
  quotient
];


super[quotient_] := If[quotient < 1, Denominator[quotient] / Numerator[quotient], quotient];


padVectorsWithZerosUpToD[a_, d_] := Map[PadRight[#, d]&, a];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*parsing utilities*)


parseTemperamentData[temperamentData_] := Module[
  {ebk, domainBasis, variance, ebkVectors, aOrL},
  
  If[
    StringMatchQ[ToString[temperamentData], RegularExpression[".*[\\[\\]\:27e8\:27e9<>]+.*"]], (* {} not included because those ID Wolfram-format stuff too! *)
    
    ebk = supportMathInEntries[temperamentData];
    
    If[
      StringMatchQ[ebk, RegularExpression["^[\\d\\.\\/]+\\s.*"]],
      
      domainBasis = First[StringCases[ebk, RegularExpression["^([\\d\\.\\/]+)\\s.*"] -> "$1"]];
      ebk = First[StringCases[ebk, RegularExpression["^[\\d\\.\\/]+\\s(.*)"] -> "$1"]],
      
      domainBasis = Null;
      ebk = ebk;
    ];
    
    variance = If[isCovariantEBK[ebk], "row", "col"];
    
    ebkVectors = If[
      isRows[{{}, variance}], (* use a dummy t *)
      StringCases[ebk, RegularExpression["[\:27e8{<]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[\\]\\|]\\s*"] -> "$1"],
      StringCases[ebk, RegularExpression["[\\[\\|]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[}\:27e9>]\\s*"] -> "$1"]
    ];
    
    aOrL = Map[parseEBKVector, ebkVectors];
    aOrL = If[Length[aOrL] == 1, First[aOrL], aOrL]; (* reduce from {{x}} to {x} if possible *)
    
    If[
      ToString[domainBasis] == "Null",
      {aOrL, variance},
      {aOrL, variance, parseDomainBasis[domainBasis]}
    ],
    
    temperamentData
  ]
];


supportMathInEntries[ebk_] := StringReplace[
  StringReplace[
    StringReplace[
      StringReplace[
        ebk,
        RegularExpression["\\s*\\*\\s*"] -> "*"
      ],
      RegularExpression["\\s*\\/\\s*"] -> "/"
    ],
    RegularExpression["\\s*\\+\\s+"] -> "+"
  ],
  RegularExpression["\\s*\\-\\s+"] -> "-"
];


isCovariantEBK[ebk_] := StringMatchQ[ebk, RegularExpression["^[\\[]?\\s*[<\:27e8\\{][^\\[]*"]];


parseEBKVector[ebkVector_] := Map[ToExpression, StringSplit[ebkVector, RegularExpression["(?:\\s*\\,\\s*)|\\s+"]]];


parseDomainBasis[domainBasisString_] := Map[ToExpression, StringSplit[domainBasisString, "."]];


toEBK[t_] := If[
  hasAOrL[t],
  If[
    isCols[t],
    If[
      Length[getA[t]] == 1,
      vectorToEBK[First[getA[t]], t],
      If[
        Length[getA[t]] == getDPrivate[t],
        ToString[StringForm["\:27e8``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]],
        If[
          Length[getA[t]] == getRPrivate[t],
          ToString[StringForm["{``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]],
          ToString[StringForm["[``]", StringRiffle[Map[vectorToEBK[#, t]&, getA[t]]]]]
        ]
      ]
    ],
    If[
      Length[getA[t]] == 1,
      covectorToEBK[First[getA[t]], t],
      If[
        Length[getA[t]] == getDPrivate[t],
        ToString[StringForm["[``\:27e9", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]],
        If[
          Length[getA[t]] == getRPrivate[t],
          ToString[StringForm["[``}", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]],
          ToString[StringForm["[``]", StringRiffle[Map[covectorToEBK[#, t]&, getA[t]]]]]
        ]
      ]
    ]
  ],
  t
];


hasAOrL[maybeT_] := ListQ[maybeT] && Length[maybeT] > 1 && (isRows[{{}, getVariance[maybeT]}] || isCols[{{}, getVariance[maybeT]}]);


vectorToEBK[vector_, t_] := If[
  Length[vector] == getDPrivate[t],
  ToString[StringForm["[``\:27e9", StringRiffle[Map[formatNumber, vector]]]],
  If[
    Length[vector] == getRPrivate[t],
    ToString[StringForm["[``}", StringRiffle[Map[formatNumber, vector]]]],
    ToString[StringForm["[``]", StringRiffle[Map[formatNumber, vector]]]]
  ]
];


covectorToEBK[covector_, t_] := If[
  Length[covector] == getDPrivate[t],
  ToString[StringForm["\:27e8``]", StringRiffle[Map[formatNumber, covector]]]],
  If[
    Length[covector] == getRPrivate[t],
    ToString[StringForm["{``]", StringRiffle[Map[formatNumber, covector]]]],
    ToString[StringForm["[``]", StringRiffle[Map[formatNumber, covector]]]]
  ]
];


formatNumber[entry_] := If[
  IntegerQ[entry],
  entry,
  NumberForm[
    N[entry],
    {\[Infinity], outputAccuracy}, (* as many digits as needed to the left of the decimal point, 3 to the right *)
    ScientificNotationThreshold -> {-Infinity, Infinity} (* never lapse into scientific notation, e.g. 1\[Times]10\:207b\:2074 *)
  ]
];


formatNumberL[l_] := Map[formatNumber, l];


toDisplay[t_] := If[
  hasAOrL[t],
  MatrixForm[Map[
    formatNumberL,
    If[isCols[t], Transpose[getA[t]], getA[t]]
  ]],
  t
];


formatOutput[output_] := If[
  ToString[output] == "err",
  "err",
  If[
    format == "EBK",
    toEBK[output],
    If[
      format == "display",
      toDisplay[output],
      output
    ]
  ]
];


printWrapper[string___] := Apply[Print, {string}];


isTemperamentData[maybeString_] := StringQ[maybeString] && StringMatchQ[maybeString, RegularExpression[".*[\:27e8\:27e9\\[\\]].*"]];


parseQuotientL[quotientLMaybeString_, t_] := Module[
  {quotientLString, quotientL},
  
  quotientLString = If[
    StringQ[quotientLMaybeString],
    quotientLMaybeString,
    quotientLToString[quotientLMaybeString]
  ];
  quotientLString = If[
    StringMatchQ[quotientLString, RegularExpression["^\\{.*\\}$"]],
    quotientLString,
    "{" <> quotientLString <> "}"
  ];
  
  quotientL = Map[ToExpression, StringCases[quotientLString, RegularExpression["([\\d\\/]+)[\\,\\s\\}]+"] -> "$1"]];
  
  colify[padVectorsWithZerosUpToD[
    Map[quotientToPcv, quotientL],
    getDomainBasisDimension[getDomainBasis[t]]
  ]]
];


quotientLToString[quotientL_] := ToString[Map[
  ToString[Numerator[#]] <> "/" <> ToString[Denominator[#]]&,
  quotientL
]];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*list utilities*)


getGcd[l_] := Apply[GCD, l];


divideOutGcd[l_] := Module[{gcd}, gcd = getGcd[l]; If[gcd == 0, l, l / gcd]];


multByLcd[l_] := Apply[LCM, Denominator[l]] * l;


leadingEntry[l_] := First[Select[l, # != 0&, 1]];


trailingEntry[l_] := leadingEntry[Reverse[l]];


allZerosL[l_] := AllTrue[l, # == 0&];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*matrix utilities*)


reverseInnerL[a_] := Reverse[a, 2];


reverseOuterL[a_] := Reverse[a];


rotate180[a_] := reverseInnerL[reverseOuterL[a]];


innerLLength[a_] := Last[Dimensions[a]];


hnf[a_] := Last[HermiteDecomposition[a]];


getLargestMinorsL[a_] := divideOutGcd[First[Minors[a, MatrixRank[a]]]];


allZeros[a_] := AllTrue[a, # == 0&, 2];


removeAllZeroLists[a_] := Select[a, FreeQ[#, {0 ..}] &];


removeUnneededZeroLists[a_] := If[
  allZeros[a],
  {Table[0, innerLLength[a]]},
  removeAllZeroLists[a]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*variance utilities*)


getAOrLOrS[t_] := If[
  ListQ[t],
  Part[t, 1],
  t
];


hasA[t_] := If[
  ListQ[t],
  ListQ[First[getAOrLOrS[t]]],
  False
];


hasL[t_] := If[
  ListQ[t],
  !hasA[t],
  False
];


getA[t_] := If[
  hasA[t],
  getAOrLOrS[t],
  If[
    hasL[t],
    {getAOrLOrS[t]},
    {{t}}
  ]
];


getL[t_] := If[
  hasL[t],
  getAOrLOrS[t],
  If[
    hasA[t],
    Error, (* you probably didn't mean to ask for the first (co)vector of a list *)
    {t}
  ]
];


breakByRowsOrCols[t_] := If[
  hasA[t],
  Map[
    {#, getVariance[t]}&,
    getA[t]
  ],
  If[
    hasL[t],
    {t},
    Error
  ]
];


scale[t_, scalar_] := If[
  hasA[t] || hasL[t],
  {scalar * getAOrLOrS[t], getVariance[t]},
  scalar * t
];


(* currently assumes matching variance and data type *)
addT[t1_, t2_] := If[
  hasA[t1],
  {getA[t1] + getA[t2], getVariance[t1]},
  If[
    hasL[t1],
    {getL[t1] + getL[t2], getVariance[t1]},
    t1 + t2
  ]
];


(* currently assumes matching variance and data type *)
subtractT[t1_, t2_] := If[
  hasA[t1],
  {getA[t1] - getA[t2], getVariance[t1]},
  If[
    hasL[t1],
    {getL[t1] - getL[t2], getVariance[t1]},
    t1 - t2
  ]
];


rowify[aOrL_] := {aOrL, "row"};


colify[aOrL_] := {aOrL, "col"};


maybeRowify[t_] := If[hasL[t], t, rowify[{t}]];


getVariance[t_] := Part[t, 2];


dualVariance[t_] := If[isCols[t], "row", "col"];


isCols[t_] := MemberQ[{
  "vector",
  "vectors",
  "contra",
  "contravector",
  "contravectors",
  "contravariant",
  "v",
  "c",
  "comma",
  "commas",
  "comma basis",
  "comma-basis",
  "commaBasis",
  "comma_basis",
  "i",
  "interval",
  "intervals",
  "g",
  "generator",
  "generators",
  "pcv",
  "gcv",
  "monzo",
  "monzos",
  "against",
  "col",
  "cols",
  "column-major order",
  "column-major",
  "column order",
  "col-major order",
  "col-major",
  "col order"
}, getVariance[t]];


isRows[t_] := MemberQ[{
  "map",
  "maps",
  "co",
  "covector",
  "covectors",
  "covariant",
  "m",
  "mapping",
  "et",
  "ets",
  "edo",
  "edos",
  "edomapping",
  "edomappings",
  "val",
  "vals",
  "with",
  "row",
  "rows",
  "row-major order",
  "row-major",
  "row order"
}, getVariance[t]];


multiply[tl_, variance_] := Module[
  {a, aOrL},
  
  a = Apply[Dot, Map[If[hasAOrL[#] && isCols[#], Transpose[getA[#]], getA[#]]&, tl]];
  
  aOrL = If[Length[a] == 1, First[a], a]; (* reduce from {{x}} to {x} if possible *)
  
  If[
    Length[aOrL] == 1, (* it's a scalar! *)
    
    First[aOrL], (* return without any variance; it's irrelevant *)
    
    If[
      isRows[{{}, variance}], (* create dummy t to check variance *)
      {aOrL, variance},
      {
        If[
          Length[Transpose[aOrL]] == 1, (* post transposing, again, reduce from {{x}} to {x} if possible *)
          First[Transpose[aOrL]],
          Transpose[aOrL]
        ],
        variance
      }
    ]
  ]
];


multiplyToRows[tl___] := multiply[{tl}, "row"];


multiplyToCols[tl___] := multiply[{tl}, "col"];


inverse[t_] := If[
  hasA[t],
  {Inverse[getA[t]], getVariance[t]},
  If[
    hasL[t],
    {1 / getL[t], getVariance[t]},
    1 / t
  ]
];


transpose[t_] := If[
  hasA[t],
  {getA[t], If[isRows[t], "col", "row"]},
  If[
    hasL[t],
    {getL[t], If[isRows[t], "col", "row"]},
    Error (* you probably don't mean to be transposing a scalar *)
  ]
];


blankSpace[]


(* ::Section::Closed:: *)
(*tuning*)


blankSpace[]


(* ::Subsection::Closed:: *)
(*alternative complexities*)


textBlock[{
  "This section contains functions supporting tuning schemes with alternative (other than log-product ",
  "complexity) interval complexities, in particular all-interval tuning schemes such as Benedetti, Weil, Kees, and their Euclideanized variants.",
  br[],
  "Based on material from ", hyperlink["[Dave Keenan & Douglas Blumeyer's guide to RTT: alternative complexities", "https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_RTT:_alternative_complexities)"], "."
}, "alternative complexities"]


blankSpace[]


blankSpace[]


augmentedTemperedSideGeneratorsPartArg[generatorTuningMap_] := rowify[Join[
  getL[generatorTuningMap],
  {Symbol["gAugmented"]}
]];


augmentedTemperedSideMappingPartArg[m_, intervalComplexityNormPreTransformerSizeFactor_] := Module[
  {d, temperedSideMappingPartArg, mappingAugmentation},
  
  d = getDPrivate[m];
  temperedSideMappingPartArg = rowify[Map[Join[#, {0}]&, getA[m]]];
  mappingAugmentation = {Join[
    getL[multiplyToRows[
      rowify[Table[intervalComplexityNormPreTransformerSizeFactor, d]],
      getLogPrimeA[m]
    ]],
    {-1}
  ]};
  
  rowify[Join[getA[temperedSideMappingPartArg], mappingAugmentation]]
];


augmentedJustSideGeneratorsPartArg[justTuningMap_] := rowify[Join[
  getL[justTuningMap],
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


augmentedEitherSideMultiplierPartArg[simplicityPreTransformer_] := rowify[Join[
  getA[simplicityPreTransformer],
  {Join[
    Table[
      0,
      Last[Dimensions[getA[simplicityPreTransformer]]] - 1
    ],
    {1}
  ]}
]];


augmentedHeldIntervalsArg[heldIntervals_] := If[
  ToString[heldIntervals] == "Null",
  heldIntervals,
  colify[Map[
    Join[#, {1}]&,
    getA[heldIntervals]
  ]]
];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*tuning scheme options*)


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
    tuningSchemeOriginalName === "KE" || tuningSchemeOriginalName === "Kees-Euclidean" || tuningSchemeOriginalName === "CWE" || tuningSchemeOriginalName === "constrained Weil-Euclidean",
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
  
  tilt = padVectorsWithZerosUpToD[
    tilt,
    Max[Map[Length, tilt]]
  ];
  
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


processOld[targetIntervals_, tPossiblyWithChangedDomainBasis_] := Module[
  {greatestOdd, nextPrime, maybeOddLimit, old},
  
  maybeOddLimit = First[StringCases[StringReplace[targetIntervals, "odd limit diamond" -> "OLD"], RegularExpression["(\\d*)-?OLD"] -> "$1"]];
  old = If[
    maybeOddLimit == "",
    
    greatestOdd = Max[Map[oddify[Numerator[#]]&, getDomainBasis[tPossiblyWithChangedDomainBasis]]];
    nextPrime = nextPrime = Prime[PrimePi[greatestOdd] + 1];
    getOld[nextPrime - 2], (* default to odd immediately before the prime that is the next prime after the temperament's prime limit *)
    
    getOld[ToExpression[maybeOddLimit]]
  ];
  
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[tPossiblyWithChangedDomainBasis]],
    old = filterTargetIntervalsForNonstandardDomainBasis[old, tPossiblyWithChangedDomainBasis]
  ];
  
  old = Map[quotientToPcv, old];
  colify[padVectorsWithZerosUpToD[
    old,
    Max[Map[Length, old]]
  ]]
];


oddify[integer_] := Module[{localInteger},
  localInteger = integer;
  
  While[EvenQ[localInteger], localInteger /= 2];
  
  localInteger
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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*parts*)


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
    printWrapper["temperedSideGeneratorsPartArg: ", formatOutput[temperedSideGeneratorsPartArg]]; (* \|01d488 *)
    printWrapper["temperedSideMappingPartArg: ", formatOutput[temperedSideMappingPartArg]]; (* \|01d440 *)
    printWrapper["justSideGeneratorsPartArg: ", formatOutput[justSideGeneratorsPartArg]]; (* \|01d48b *)
    printWrapper["justSideMappingPartArg: ", formatOutput[justSideMappingPartArg]]; (* \|01d440\:2c7c *)
    printWrapper["eitherSideIntervalsPartArg: ", formatOutput[eitherSideIntervalsPartArg]]; (* T *)
    printWrapper["eitherSideMultiplierPartArg: ", formatOutput[eitherSideMultiplierPartArg]]; (* \|01d44a *)
    printWrapper["powerArg: ", formatOutput[powerArg]];
    printWrapper["heldIntervalsArg: ", formatOutput[heldIntervalsArg]];
  ];
  
  {
    temperedSideGeneratorsPartArg, (* \|01d488 *)
    temperedSideMappingPartArg, (* \|01d440 *)
    justSideGeneratorsPartArg, (* \|01d48b *)
    justSideMappingPartArg, (* \|01d440\:2c7c *)
    eitherSideIntervalsPartArg, (* T *)
    eitherSideMultiplierPartArg, (* \|01d44a *)
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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*shared*)


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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*error*)


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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*complexity*)


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
    (* when used by getSimplicityPreTransformer in getAllIntervalTuningSchemeTuningMethodArgs, covers minimax-lils-S ("Weil"), minimax-E-lils-S ("WE"), held-octave minimax-lils-S ("Kees"), and held-octave minimax-E-lils-S ("KE") *)
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


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*held-intervals*)


canUseOnlyHeldIntervalsMethod[heldIntervals_, t_] := ToString[heldIntervals] != "Null" && Length[getA[heldIntervals]] == getRPrivate[t];


blankSpace[]


blankSpace[]


(* ::Subsection::Closed:: *)
(*methods, by optimization or dual norm power*)


(* ::Subsubsection::Closed:: *)
(*\[Infinity] *)


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
  after undoing those, voil\[AGrave], we're done! *)
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
    \:27e8240.000 2786.314], \:27e8240.000 2795.337], and \:27e8240.000 2804.359], all three of which tie for the abbreviated 
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
    (* the right half of this is the primes tuning map, so this makes it a *negative* retuning map (\|01d48b-\|01d495 rather than the typical \|01d495-\|01d48b) *)
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
      eitherSideIntervalsAndMultipliersPartA . pointConstraint . Inverse[mappingSideA . pointConstraint],
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
  
  candidateTunings = Quiet[Map[justTuningMapA . #&, candidateEmbeddings]];
  
  (* each damage list is sorted in descending order;
  the list of lists itself is sorted corresponding to the candidate tunings *)
  candidateDamageLists = Quiet[Map[
    Function[
      {candidateTuning},
      N[Abs[First[candidateTuning . mappingSideA] - First[justSideA]], coincidingDamageMethodTiePrecision]
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
  and the temperament we're tuning is 12-ET with M = [12 19 28] and standard basis so p = [log\:20822 log\:20823 log\:20825],
  then we have [12 19 28][g\:2081] = [log\:20822 log\:20823 log\:20825], or a system of three equations:
  
  12g\:2081 = log\:20822
  19g\:2081 = log\:20823
  28g\:2081 = log\:20825
  
  Obviously not all of those can be true, but that's the whole point.
  
  Now suppose we get the constraint matrix [1 1 0]. We multiply both sides of the setup by that:
  
  [1 1 0][12 19 28][g\:2081] = [1 1 0][log\:20822 log\:20823 log\:20825]
  [31][g\:2081] = [log\:20822 + log\:20823]
  
  This leaves us with only a single equation:
  
  31g\:2081 = log\:20826
  
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


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*1*)


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


(* \|01d43a = U(\|01d440U)\:207b\.b9 *)
getGeneratorEmbeddingFromUnchangedIntervalBasis[m_, unchangedIntervals_] := Module[
  {mappedUnchangedIntervals},
  
  mappedUnchangedIntervals = multiplyToCols[m, unchangedIntervals];
  
  If[
    Det[getA[mappedUnchangedIntervals]] == 0,
    Null,
    multiplyToCols[unchangedIntervals, inverse[mappedUnchangedIntervals]]
  ]
];


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*2*)


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
  temperedSideButWithoutGeneratorsPart = multiplyToRows[temperedSideMappingPartArg, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg]; (* MTW, or MT\:209aS\:209a *)
  nextToInverted = multiplyToCols[eitherSideIntervalsPartArg, eitherSideMultiplierPartArg, transpose[temperedSideButWithoutGeneratorsPart]]; (* TW(MTW)\:1d40, or T\:209aS(MT\:209aS\:209a) *)
  toBeInverted = multiplyToCols[temperedSideButWithoutGeneratorsPart, transpose[temperedSideButWithoutGeneratorsPart]]; (* MTW(MTW)\:1d40, or MT\:209aS\:209a(MT\:209aS\:209a)\:1d40 *)
  
  (* Technically the A\:1d40(AA\:1d40)\:207b\.b9 type of pseudoinverse is necessary. 
  Wolfram's built-in will sometimes use other techniques, which do not give the correct answer.
  Also it's good to break it down to show the parallelism between the simpler case and the held-interval case. *)
  
  If[
    ToString[heldIntervalsArg] == "Null",
    
    (* jTW(MTW)\:1d40(MTW(MTW)\:1d40)\:207b\.b9, so it's the pseudoinverse of MTW left-multiplied by jTW *)
    (* or jT\:209aS\:209a(MT\:209aS\:209a)\:1d40(MT\:209aS\:209a(MT\:209aS\:209a)\:1d40)\:207b\.b9, so it's the pseudoinverse of MT\:209aS\:209a left-multiplied by jT\:209aS\:209a *)
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


blankSpace[]


blankSpace[]


(* ::Subsubsection::Closed:: *)
(*general*)


(* METHODS: GENERAL OPTIMIZATION POWER (MINI-P-MEAN) OR GENERAL DUAL NORM POWER (MINI-P-NORM) *)


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
where the generators part is 1200\[Times]\|01d7cf\|01d43f\|01d43a (tempered) or 1200\[Times]\|01d7cf\|01d43f\|01d43a\:2c7c (just), the mapping part is \|01d440 (tempered) or \|01d440\:2c7c (just), 
the intervals part is T (non-all-interval) or T\:209a (all-interval), and
the multiplier part is \|01d44a (non-all-interval) or \|01d446\:209a (all-interval), finds:
tempered non-all-interval: 1200\[Times]\|01d7cf\|01d43f \|01d43a \|01d440 T \|01d44a
tempered all-interval:     1200\[Times]\|01d7cf\|01d43f \|01d43a \|01d440 T\:209a\|01d446\:209a
just non-all-interval:     1200\[Times]\|01d7cf\|01d43f \|01d43a\:2c7c\|01d440\:2c7cT \|01d44a 
just all-interval:         1200\[Times]\|01d7cf\|01d43f \|01d43a\:2c7c\|01d440\:2c7cT\:209a\|01d446\:209a
in the approximation 1200\[Times]\|01d7cf\|01d43f\|01d43a\|01d440T\|01d44a \[TildeTilde] 1200\[Times]\|01d7cf\|01d43f\|01d43a\:2c7c\|01d440\:2c7cT\|01d44a or 1200\[Times]\|01d7cf\|01d43f\|01d43a\|01d440T\:209a\|01d446\:209a \[TildeTilde] 1200\[Times]\|01d7cf\|01d43f\|01d43a\:2c7c\|01d440\:2c7cT\:209a\|01d446\:209a
where G\:2c7c = \|01d440\:2c7c = T\:209a = \|01d43c (identity matrix)
*)
getTemperedOrJustSide[
  temperedOrJustSideGeneratorsPart_,
  temperedOrJustSideMappingPart_,
  eitherSideIntervalsPartArg_,
  eitherSideMultiplierPartArg_
] := multiplyToRows[temperedOrJustSideGeneratorsPart, temperedOrJustSideMappingPart, eitherSideIntervalsPartArg, eitherSideMultiplierPartArg];


(* no historically described tuning schemes use this *)
(* an analytical method *)
(* \|01d43a = U(\|01d440U)\:207b\.b9; \|01d488 = \|01d48b\|01d43a *)
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


blankSpace[]


blankSpace[]


blankSpace[]


(* ::Chapter:: *)
(*workspace*)


(* put your own work here! *)
debug = False;

(* format = "EBK"; *)
format = "display";
(* format = "Wolfram"; *)

dual["[\:27e81 1 0] \:27e80 1 4]}"]

