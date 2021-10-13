jip[d_] := Map[Log2, Map[Prime, Range[d]]];
tenneyWeights[m_] := DiagonalMatrix[1 / jip[getD[m]]];
unweightedG[m_] := PseudoInverse[m];
weightedG[m_] := tenneyWeights[m].PseudoInverse[m.tenneyWeights[m]];
octaves[vOrVList_] := jip[Length[vOrVList]].vOrVList;
absOctaves[vOrVList_] := jip[Length[vOrVList]].Abs[vOrVList];
cents[vOrVList_] := 1200.octaves[vOrVList];
