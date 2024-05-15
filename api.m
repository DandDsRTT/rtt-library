(* TEMPERAMENT *)

(* dimensions *)
CloudDeploy[APIFunction[{"getD"}, getD[#unparsedT]&], "getD"]
CloudDeploy[APIFunction[{"getR"}, getR[#unparsedT]&], "getR"]
CloudDeploy[APIFunction[{"getN"}, getN[#unparsedT]&], "getN"]

(* canonicalization *)
CloudDeploy[APIFunction[{"canonicalForm"}, canonicalForm[#unparsedT]&], "canonicalForm"]

(* dual *)
CloudDeploy[APIFunction[{"dual"}, dual[#unparsedT]&], "dual"]

(* generator detempering *)
CloudDeploy[APIFunction[{"getGeneratorDetempering"}, getGeneratorDetempering[#unparsedT]&], "getGeneratorDetempering"]

(* merging *)
CloudDeploy[APIFunction[{"mapMerge"}, mapMerge[#unparsedTl]&], "mapMerge"]
CloudDeploy[APIFunction[{"commaMerge"}, commaMerge[#unparsedTl]&], "commaMerge"]

(* domain basis *)
CloudDeploy[APIFunction[{"canonicalDomainBasis"}, canonicalDomainBasis[#unparsedDomainBasis]&], "canonicalDomainBasis"]
CloudDeploy[APIFunction[{"changeDomainBasis"}, changeDomainBasis[#unparsedT, #unparsedTargetDomainBasis]&], "changeDomainBasis"]

(* addition *)
CloudDeploy[APIFunction[{"sum"}, sum[#unparsedT1, #unparsedT2]&], "sum"]
CloudDeploy[APIFunction[{"diff"}, diff[#unparsedT1, #unparsedT2]&], "diff"]


(* EA (note: not yet EBK-parseable) *)

(* dimensions *)
CloudDeploy[APIFunction[{"eaGetD"}, eaGetD[#u]&], "eaGetD"]
CloudDeploy[APIFunction[{"eaGetR"}, eaGetR[#u]&], "eaGetR"]
CloudDeploy[APIFunction[{"eaGetN"}, eaGetN[#u]&], "eaGetN"]

(* canonicalization *)
CloudDeploy[APIFunction[{"eaCanonicalForm"}, eaCanonicalForm[#u]&], "eaCanonicalForm"]

(* dual *)
CloudDeploy[APIFunction[{"eaDual"}, eaDual[#u]&], "eaDual"]

(* conversion to and from matrix *)
CloudDeploy[APIFunction[{"multivectorToMatrix"}, multivectorToMatrix[#u]&], "multivectorToMatrix"]
CloudDeploy[APIFunction[{"matrixToMultivector"}, matrixToMultivector[#t]&], "matrixToMultivector"]

(* merging *)
CloudDeploy[APIFunction[{"progressiveProduct"}, progressiveProduct[#u1, #u2]&], "progressiveProduct"]
CloudDeploy[APIFunction[{"regressiveProduct"}, regressiveProduct[#u1, #u2]&], "regressiveProduct"]
CloudDeploy[APIFunction[{"interiorProduct"}, interiorProduct[#u1, #u2]&], "interiorProduct"]

(* addition *)
CloudDeploy[APIFunction[{"eaSum"}, eaSum[#u1, #u2]&], "eaSum"]
CloudDeploy[APIFunction[{"eaDiff"}, eaDiff[#u1, #u2]&], "eaDiff"]


(* TUNING *)

(* optimization *)
CloudDeploy[APIFunction[{"optimizeGeneratorTuningMap"}, optimizeGeneratorTuningMap[#unparsedT, #tuningSchemeSpec]&], "optimizeGeneratorTuningMap"]
CloudDeploy[APIFunction[{"optimizeTuningMap"}, optimizeTuningMap[#unparsedT, #tuningSchemeSpec]&], "optimizeTuningMap"]

(* mean damage *)
CloudDeploy[APIFunction[{"getGeneratorTuningMapMeanDamage"}, getGeneratorTuningMapMeanDamage[#unparsedT, #unparsedGeneratorTuningMap, #tuningSchemeSpec]&], "getGeneratorTuningMapMeanDamage"]
CloudDeploy[APIFunction[{"getTuningMapMeanDamage"}, getTuningMapMeanDamage[#unparsedT, #unparsedTuningMap, #tuningSchemeSpec]&], "getTuningMapMeanDamage"]

(* conversion *)
CloudDeploy[APIFunction[{"generatorTuningMapFromTAndTuningMap"}, generatorTuningMapFromTAndTuningMap[#unparsedT, #unparsedTuningMap]&], "generatorTuningMapFromTAndTuningMap"]

(* damages *)
CloudDeploy[APIFunction[{"getGeneratorTuningMapDamages"}, getGeneratorTuningMapDamages[#unparsedT, #unparsedGeneratorTuningMap, #tuningSchemeSpec]&], "getGeneratorTuningMapDamages"]
CloudDeploy[APIFunction[{"getTuningMapDamages"}, getTuningMapDamages[#unparsedT, #unparsedTuningMap, #tuningSchemeSpec]&], "getTuningMapDamages"]

(* target-interval set schemes *)
CloudDeploy[APIFunction[{"getTilt"}, getTilt[#integerLimit]&], "getTilt"]
CloudDeploy[APIFunction[{"getOld"}, getOld[#oddLimit]&], "getOld"]
CloudDeploy[APIFunction[{"getOtonalChord"}, getOtonalChord[#harmonicsL]&], "getOtonalChord"]

(* all-interval tuning schemes *)
CloudDeploy[APIFunction[{"getDualPower"}, getDualPower[#power]&], "getDualPower"]
