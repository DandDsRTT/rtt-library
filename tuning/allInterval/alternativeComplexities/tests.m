failures = 0;
passes = 0;

format = "EBK";


testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0}, "⟨1202.390 697.176]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormMultiplierLogPrimePower" -> 0, "intervalComplexityNormPower" -> 2}, "⟨1202.607 696.741]"];
testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-copfr-S"}, "⟨597.119 103.293]"];
testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-E-copfr-S"}, "⟨598.345 106.693]"];

(* minimax-E-copfr-S = "Frobenius" *)
(* could double-check with Scala, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-E-copfr-S", "⟨1202.6068 1899.3482 2786.9654]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-copfr-S", "⟨1191.8899 1907.0238 2786.3137]"]; (* [4], though Flora's code does have a bug where prime 5 comes out as a 0 *)
testClose[optimizeTuningMap, dicot, "minimax-E-copfr-S", "⟨1215.1441 1907.0030 2776.2177]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-copfr-S", "⟨1195.0446 1901.9550 2788.4374]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-copfr-S", "⟨1210.9365 1897.2679 2784.7514]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-copfr-S", "⟨1198.5953 1908.9787 2782.0995]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-copfr-S", "⟨1198.4746 1902.5097 2786.5911]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-copfr-S", "⟨1200.5015 1902.3729 2785.8122]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-copfr-S", "⟨1202.3503 1902.1900 2785.1386]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-copfr-S", "⟨1203.2384 1901.2611 2785.3885]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-copfr-S", "⟨1198.8664 1903.9955 2785.4068]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-copfr-S", "⟨1201.3440 1898.5615 2788.8699 3368.1428]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-copfr-S", "⟨1202.0285 1904.1849 2784.8940 3368.0151]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-copfr-S", "⟨1196.6908 1901.7292 2778.3407 3376.6861]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-copfr-S", "⟨1195.2617 1901.4887 2788.9439 3368.5928]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-copfr-S", "⟨1198.2677 1904.0314 2790.4025 3364.8772]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-copfr-S", "⟨1200.0000 1904.3201 2785.8407 3367.8799]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "Frobenius", optimizeGeneratorsTuningMap[meantone, "minimax-E-copfr-S"]];


(* minimax-sopfr-S = "BOP", "Benedetti OPtimal" *)
testClose[optimizeTuningMap, meantone, "minimax-sopfr-S", "⟨1201.7205 1899.3742 2790.6150]"];  (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-sopfr-S", "⟨1194.179 1910.686 2786.314]"];  (* [4] has ⟨820.9516 1313.5225 0.0000] due to a bug *)
testClose[optimizeTuningMap, dicot, "minimax-sopfr-S", "⟨1207.4392 1913.1138 2767.7157]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-sopfr-S", "⟨1197.1684 1901.9550 2793.3928]"];  (* [4] has ⟨1197.1684 1898.1244 2793.3928] which has the same damage, but prime 3 might as well be tuned pure *)
testClose[optimizeTuningMap, mavila, "minimax-sopfr-S", "⟨1206.5842 1892.0787 2769.8533]"];  (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-sopfr-S", "⟨1196.9271 1906.5643 2778.6315]"];  (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-sopfr-S", "⟨1199.1112 1903.2881 2788.5356]"];  (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-sopfr-S", "⟨1200.2845 1902.3817 2785.6025]"];  (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-sopfr-S", "⟨1201.2339 1903.8058 2783.2290]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-sopfr-S", "⟨1201.7937 1899.2645 2781.8295]"]; (* [4] *)
accuracy = 2;
testClose[optimizeTuningMap, tetracot, "minimax-sopfr-S", "⟨1199.0293 1903.4111 2783.8883]"];  (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, meantone7, "minimax-sopfr-S", "⟨1201.721 1899.374 2790.615 3371.376]"]; (* [4] has ⟨1201.7494 1899.4211 2790.6871 3371.4697], but that has 0.875 damage and mine has 0.860 damage *)
accuracy = 2;
testClose[optimizeTuningMap, magic7, "minimax-sopfr-S", "⟨1201.2340 1903.8044 2783.2288 3367.8966]"];  (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, pajara, "minimax-sopfr-S", "⟨1197.3094 1902.8073 2779.5873 3378.2420]"];  (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-sopfr-S", "⟨1197.168 1904.326 2793.393 3374.358]"];  (* [4] has ⟨1197.1684 1902.1518 2793.3928 3378.7064] which has the same damage, but it can be visualized with graphTuningDamage[augene, "minimax-sopfr-S"] that mine does a nested minimax, minimizing the maximum damage between primes 3 and 7 underneath the minimax boundary between primes 2 and 5 *)
testClose[optimizeTuningMap, sensi, "minimax-sopfr-S", "⟨1198.5891 1903.5233 2789.8411 3363.8876]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-sopfr-S", "⟨1200.0000 1903.2071 2784.2269 3365.9043]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "BOP", optimizeGeneratorsTuningMap[meantone, "minimax-sopfr-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Benedetti", optimizeGeneratorsTuningMap[meantone, "minimax-sopfr-S"]];

(* minimax-E-sopfr-S = "BE", "Benedetti-Euclidean" *)
testClose[optimizeTuningMap, meantone, "minimax-E-sopfr-S", "⟨1201.4768 1898.6321 2788.6213]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-sopfr-S", "⟨1193.9975 1910.3960 2786.3137]"]; (* [4] has ⟨1193.9975 1910.3960 0.0000] due to a bug *)
testClose[optimizeTuningMap, dicot, "minimax-E-sopfr-S", "⟨1205.8488 1906.3416 2761.9439]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-sopfr-S", "⟨1197.2692 1901.9550 2793.6282]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-sopfr-S", "⟨1208.5464 1893.7139 2778.683]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-sopfr-S", "⟨1199.5668 1906.8283 2778.1916]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-sopfr-S", "⟨1198.8183 1902.9219 2787.6566]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-sopfr-S", "⟨1200.1533 1902.2425 2785.3554]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-sopfr-S", "⟨1201.1456 1902.2128 2782.7337]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-sopfr-S", "⟨1202.2630 1900.8639 2782.2726]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-sopfr-S", "⟨1199.5499 1903.7780 2784.0631]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-sopfr-S", "⟨1201.3847 1898.6480 2789.0531 3368.4787]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-sopfr-S", "⟨1200.9990 1903.1832 2782.6345 3366.6407]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-sopfr-S", "⟨1197.9072 1903.2635 2781.9626 3380.9162]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-sopfr-S", "⟨1196.4076 1903.1641 2791.6178 3372.1175]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-sopfr-S", "⟨1199.7904 1902.7978 2789.2516 3362.3687]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-sopfr-S", "⟨1200.0000 1903.3868 2785.5183 3365.7078]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "BE", optimizeGeneratorsTuningMap[meantone, "minimax-E-sopfr-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Benedetti-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-E-sopfr-S"]];

(* minimax-lil-S = "Weil" *)
(* could maybe double-check w/ Flora's app but we're aware at this time that her implementation uses the pseudoinverse
of the Weil interval complexity norm multiplier which doesn't work correctly *)
testClose[optimizeTuningMap, meantone, "minimax-lil-S", "⟨1200.000 1896.578 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, blackwood, "minimax-lil-S", "⟨1188.722 1901.955 2773.22]"]; (* [2a] *)
testClose[optimizeTuningMap, dicot, "minimax-lil-S", "⟨1200.000 1901.955 2750.978]"]; (* [2a] *)
testClose[optimizeTuningMap, augmented, "minimax-lil-S", "⟨1194.134 1897.307 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, mavila, "minimax-lil-S", "⟨1200.000 1881.31 2756.07]"]; (* [2a] *)
testClose[optimizeTuningMap, porcupine, "minimax-lil-S", "⟨1193.828 1901.955 2771.982]"]; (* [2a] *)
testClose[optimizeTuningMap, srutal, "minimax-lil-S", "⟨1198.222 1901.955 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, hanson, "minimax-lil-S", "⟨1200.000 1901.955 2784.963]"]; (* [2a] *)
testClose[optimizeTuningMap, magic, "minimax-lil-S", "⟨1200.000 1901.955 2780.391]"]; (* [2a] *)
testClose[optimizeTuningMap, negri, "minimax-lil-S", "⟨1200.000 1896.185 2777.861]"]; (* [2a] *)
testClose[optimizeTuningMap, tetracot, "minimax-lil-S", "⟨1198.064 1901.955 2781.819]"]; (* [2a] *)
testClose[optimizeTuningMap, meantone7, "minimax-lil-S", "⟨1200.000 1896.578 2786.314 3365.784]"]; (* [2a] *)
testClose[optimizeTuningMap, magic7, "minimax-lil-S", "⟨1200.000 1901.955 2780.391 3364.692]"]; (* [2a] *)
testClose[optimizeTuningMap, pajara, "minimax-lil-S", "⟨1193.803 1896.996 2771.924 3368.826]"]; (* [2a] *)
testClose[optimizeTuningMap, augene, "minimax-lil-S", "⟨1194.134 1899.852 2786.314 3365.102]"]; (* [2a] *)
testClose[optimizeTuningMap, sensi, "minimax-lil-S", "⟨1196.783 1901.181 2786.314 3359.796]"]; (* [2a] *)
(* sensamagic - no examples to work off of*)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "Weil", optimizeGeneratorsTuningMap[meantone, "minimax-lil-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "WOP", optimizeGeneratorsTuningMap[meantone, "minimax-lil-S"]];

(* minimax-E-lil-S = "WE", "Weil-Euclidean" *)
(* could maybe double check w/ Sintel's app; what he calls Weil is actually Weil-Euclidean, according to Tom here: [10a] and I think he's right 
but unfortunately it's not easily discernible from his code at this time *)
testClose[optimizeTuningMap, meantone, "minimax-E-lil-S", "⟨1201.3906 1898.4361 2788.1819]"]; (* [4] and [1a] has ⟨1201.391 1898.436 2788.182] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-lil-S", "⟨1194.2544 1910.8071 2786.1895]"]; (* [1a] has ⟨1194.254 1910.807 2786.189]; [4] has a bug with this *)
testClose[optimizeTuningMap, dicot, "minimax-E-lil-S", "⟨1206.2832 1907.1223 2762.9860]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-E-lil-S", "⟨1197.0385 1901.9322 2793.0898]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-E-lil-S", "⟨1208.2873 1892.7881 2779.6466]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-lil-S", "⟨1199.5444 1907.4244 2779.1926]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-E-lil-S", "⟨1198.8214 1903.0273 2787.4633]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-E-lil-S", "⟨1200.1659 1902.3024 2785.4179]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-E-lil-S", "⟨1201.2449 1902.2636 2782.9425]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-E-lil-S", "⟨1202.3403 1900.6800 2782.6811]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-lil-S", "⟨1199.5586 1903.9387 2784.4138]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-lil-S", "⟨1201.2358, 1898.4479, 2788.8486, 3368.4143]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-E-lil-S", "⟨1201.0786, 1903.4695, 2782.8510, 3367.2482]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-E-lil-S", "⟨1197.6967, 1903.3872, 2780.5573, 3379.4056]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-E-lil-S", "⟨1196.2383, 1903.2719, 2791.2228, 3370.8863]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-E-lil-S", "⟨1199.7081, 1903.2158, 2789.7655, 3363.1568]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-lil-S", "⟨1199.9983 1903.7398 2785.5426 3366.5781]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "WE", optimizeGeneratorsTuningMap[meantone, "minimax-E-lil-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Weil-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-E-lil-S"]];

(* unchanged-octave minimax-lil-S = "Kees" *)
(* could maybe double-check with Flora's app, but per comment above about her implementation of Weil, we know it won't match now *)
(* this is the only actual example of a Kees tuning ever stated publicly by a human *)
accuracy = 1;
testClose[optimizeTuningMap, "[⟨1 3 0 0 3] ⟨0 -3 5 6 1]⟩", "unchanged-octave minimax-lil-S", "⟨1200.00 1915.93 2806.79 3368.14 4161.36]"]; (* [1b] *)
accuracy = 3;
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "Kees", optimizeGeneratorsTuningMap[meantone, "unchanged-octave minimax-lil-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "KOP", optimizeGeneratorsTuningMap[meantone, "unchanged-octave minimax-lil-S"]];

(* unchanged-octave minimax-E-lil-S = "KE", "Kees-Euclidean" *)
(* may be able double-check w/ Sintel's app *)
testClose[optimizeTuningMap, meantone, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1896.6512 2786.605]"]; (* [4]; [1a] has ⟨1200.000 1896.651 2786.605] *)
testClose[optimizeTuningMap, blackwood, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1920.0000 2795.1253]"]; (* [1a] has ⟨1200.000 1920.000 2795.126]; [4] has a bug with this one *)
testClose[optimizeTuningMap, dicot, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1902.1712 2751.0856]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1905.0691 2800.0000]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1879.1114 2762.6658]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1907.8138 2779.6896]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1904.9585 2790.0830]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1902.1850 2785.1542]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1901.0972 2780.2194]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1897.3560 2776.9830]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1904.3859 2784.8683]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1896.6562 2786.6248 3366.5620]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1902.2878 2780.4576 3365.4906]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1907.3438 2785.3124 3385.3124]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1909.3248 2800.0000 3381.3503]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1903.4449 2790.1435 3363.5406]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "unchanged-octave minimax-E-lil-S", "⟨1200.0000 1903.7411 2785.5446 3366.5805]"]; (* [4] *)
(* original name *)
testClose[optimizeGeneratorsTuningMap, meantone, "KE", optimizeGeneratorsTuningMap[meantone, "unchanged-octave minimax-E-lil-S"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Kees-Euclidean", optimizeGeneratorsTuningMap[meantone, "unchanged-octave minimax-E-lil-S"]];


(* confirming the relationship between tuning schemes using log-integer-limit and log-product as their interval complexities, for various targeted interval sets and optimization powers *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimax-lil-S"}, "⟨1201.191 697.405]"];                           (* lil     / non-all / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimean-lil-S"}, "⟨1200.000 696.578]"];                          (* lil     / non-all / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniRMS-lil-S"}, "⟨1201.648 697.183]"];                            (* lil     / non-all / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " mini-3-mean-lil-S"}, "⟨1201.621 697.326]"];                       (* lil     / non-all / sop *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lil-S"}, "⟨1200.000 696.578]"];                                       (* lil     / all     / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lil-S", "intervalComplexityNormPower" -> \[Infinity]}, "⟨1200.000 696.578]"];           (* lil     / all     / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-E-lil-S"}, "⟨1201.391 697.045]"];                                     (* lil     / all     / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-lil-S", "intervalComplexityNormPower" -> 3}, "⟨1201.038 696.782]"];           (* lil     / all     / sop *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimax-S"}, "⟨1201.699 697.564]"];                               (* non-lil / non-all / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " minimean-S"}, "⟨1200.000 696.578]"];                              (* non-lil / non-all / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " miniRMS-S"}, "⟨1201.617 697.379]"];                                (* non-lil / non-all / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> sixTilt <> " mini-3-mean-S", "optimizationPower" -> 3}, "⟨1201.603 697.601]"]; (* non-lil / non-all / sop*)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S"}, "⟨1201.699 697.564]"];                                           (* non-lil / all     / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPower" -> \[Infinity]}, "⟨1200.000 696.578]"];               (* non-lil / all     / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S"}, "⟨1201.699 697.564]"];                                           (* non-lil / all     / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormPower" -> 3}, "⟨1201.039 696.782]"];               (* non-lil / all     / sop *)


(* continuum between minimax-S (Mike's k = 0) and minimax-lil-S (Mike's k = 1) as well as beyond (k > 1) *)
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormMultiplierSizeFactor" -> 0.00}, "⟨1201.699 1899.263 2790.258]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormMultiplierSizeFactor" -> 0.25}, "⟨1201.273 1898.591 2789.271]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormMultiplierSizeFactor" -> 0.50}, "⟨1200.849 1897.920 2788.284]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormMultiplierSizeFactor" -> 1.00}, "⟨1200.000 1896.578 2786.314]"];
testClose[optimizeTuningMap, meantone, {"tuningSchemeSystematicName" -> "minimax-S", "intervalComplexityNormMultiplierSizeFactor" -> 2.00}, "⟨1198.306 1893.902 2782.381]"];


(* proving that minimax-E-copfr-S = primes miniRMS-U *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[meantone, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[blackwood, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[dicot, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[augmented, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[mavila, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[porcupine, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[srutal, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[hanson, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[magic, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[negri, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[tetracot, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[meantone7, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[magic7, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[pajara, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[augene, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[sensi, "primes miniRMS-U"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-E-copfr-S", optimizeGeneratorsTuningMap[sensamagic, "primes miniRMS-U"]];

(* proving that minimax-copfr-S = primes minimax-U *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-copfr-S", optimizeGeneratorsTuningMap[meantone, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-copfr-S", optimizeGeneratorsTuningMap[blackwood, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-copfr-S", optimizeGeneratorsTuningMap[dicot, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-copfr-S", optimizeGeneratorsTuningMap[augmented, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-copfr-S", optimizeGeneratorsTuningMap[mavila, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-copfr-S", optimizeGeneratorsTuningMap[porcupine, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-copfr-S", optimizeGeneratorsTuningMap[srutal, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-copfr-S", optimizeGeneratorsTuningMap[hanson, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-copfr-S", optimizeGeneratorsTuningMap[magic, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-copfr-S", optimizeGeneratorsTuningMap[negri, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-copfr-S", optimizeGeneratorsTuningMap[tetracot, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-copfr-S", optimizeGeneratorsTuningMap[meantone7, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-copfr-S", optimizeGeneratorsTuningMap[magic7, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-copfr-S", optimizeGeneratorsTuningMap[pajara, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-copfr-S", optimizeGeneratorsTuningMap[augene, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-copfr-S", optimizeGeneratorsTuningMap[sensi, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-copfr-S", optimizeGeneratorsTuningMap[sensamagic, "primes minimax-U"]];


(* augmentedTemperedSideGeneratorsPartArg *)
test[
  augmentedTemperedSideGeneratorsPartArg,
  {{g1, g2}, "row"},
  {{g1, g2, gAugmented}, "row"}
];

(* augmentedTemperedSideMappingPartArg *)
test[
  augmentedTemperedSideMappingPartArg,
  {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "row"},
  2,
  {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {2 * Log2[2], 2 * Log2[3], 2 * Log2[5], 2 * Log2[7], -1}}, "row"}
];

(* augmentedJustSideGeneratorsPartArg *)
test[
  augmentedJustSideGeneratorsPartArg,
  {{Log2[2], Log2[3], Log2[5], Log2[7]}, "row"},
  {{Log2[2], Log2[3], Log2[5], Log2[7], 0}, "row"}
];

(* augmentedJustSideMappingPartArg *)
test[
  augmentedJustSideMappingPartArg,
  {IdentityMatrix[4], "row"},
  {IdentityMatrix[5], "row"}
];

(* augmentedEitherSideIntervalsPartArg *)
test[
  augmentedEitherSideIntervalsPartArg,
  {IdentityMatrix[4], "col"},
  {IdentityMatrix[5], "col"}
];

(* augmentedEitherSideMultiplierPartArg *)
test[
  augmentedEitherSideMultiplierPartArg,
  {{{1 / Log2[2], 0, 0, 0, 0}, {0, 1 / Log2[3], 0, 0, 0}, {0, 0, 1 / Log2[5], 0, 0}, {0, 0, 0, 1 / Log2[7], 0}}, "row"}, (* already partially augmented per getComplexityA *)
  {{{1 / Log2[2], 0, 0, 0, 0}, {0, 1 / Log2[3], 0, 0, 0}, {0, 0, 1 / Log2[5], 0, 0}, {0, 0, 0, 1 / Log2[7], 0}, {0, 0, 0, 0, 1}}, "row"}
];

(* augmentedUnchangedIntervalsArg *)
test[augmentedUnchangedIntervalsArg, Null, Null];
test[
  augmentedUnchangedIntervalsArg,
  {{{1, 0, 0, 0}}, "col"},
  {{{1, 0, 0, 0, 0}}, "col"}
];




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
