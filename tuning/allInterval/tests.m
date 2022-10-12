failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";


testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1201.699 697.564]"];
testClose[optimizeGeneratorTuningMap, meantone, {"targetIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "intervalComplexityNormPower" -> 2}, "⟨1201.397 697.049]"];

testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-S"}, "⟨598.447 106.567]"];
testClose[optimizeGeneratorTuningMap, pajara, {"targetIntervals" -> {}, "tuningSchemeSystematicName" -> "minimax-E-S"}, "⟨598.859 106.844]"];


(* minimax-S = "TOP", "T1", "TOP-max", "TIPTOP", "Tenney OPtimal", "Tiebreaker-In-Polytope Tenney-OPtimal" *)
(* I had to fudge the factors to make mapping forms match in some places, due to rounding errors those matching factors introduced *)
(* could double-check with Scala, Xen wiki, Flora's app but it has incorrect results for TOP at this time *)
accuracy = 2;
testClose[optimizeGeneratorTuningMap, meantone, "minimax-S", "⟨1201.70, 1201.70 - 504.13]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-S", "⟨238.87, 238.86 * 11.0003 + 158.78]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, dicot, "minimax-S", "⟨1207.66 353.22]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, augmented, "minimax-S", "⟨399.02, 399.018 * 5.00005 - 93.15]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, mavila, "minimax-S", "⟨1206.55, 1206.55 + 685.03]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-S", "⟨1196.91, 1034.59 - 1196.91]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, srutal, "minimax-S", "⟨599.56, 599.56 * 3.99999 - 494.86]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, hanson, "minimax-S", "⟨1200.29 317.07]"];(* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, magic, "minimax-S", "⟨1201.28 380.80]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, negri, "minimax-S", "⟨1201.82, 1201.82 - 1075.68]"]; (* [5] as "negripent" (Table 1) *)
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-S", "⟨1199.03 176.11]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-S", "⟨1201.70, 1201.70 * 2 - 504.13]"]; (* [5](Table 2) *)
testClose[optimizeGeneratorTuningMap, magic7, "minimax-S", "⟨1201.28 380.80]"]; (* [5] (Table 3) *)
testClose[optimizeGeneratorTuningMap, pajara, "minimax-S", "⟨598.45, 598.45 - 491.88]"];  (* [5](Table 2) *)
testClose[optimizeGeneratorTuningMap, augene, "minimax-S", "⟨399.02, 399.02 * 5 - 90.59]"]; (* [5] (Table 2) *)
testClose[optimizeGeneratorTuningMap, sensi, "minimax-S", "⟨1198.39, 1198.39 - 755.23]"]; (* [5] as "sensisept" (Table 2) *)
(* original name *)
testClose[optimizeTuningMap, meantone, "TOP", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "T1", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TOP-max", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TIPTOP", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "Tenney", optimizeTuningMap[meantone, "minimax-S"]];
accuracy = 3;

(* minimax-E-S = "TE", "T2", "TOP-RMS", "Tenney-Euclidean" *)
(* could double-check with Scala, Sintel's app, Flora's app, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-E-S", "⟨1201.397 1898.446 2788.196]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "minimax-E-S", "⟨1194.308 1910.892 2786.314]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "minimax-E-S", "⟨1206.410 1907.322 2763.276]"]; (* [3a] *)
testClose[optimizeTuningMap, augmented, "minimax-E-S", "⟨1197.053 1901.955 2793.123]"]; (* [3b] *)
testClose[optimizeTuningMap, mavila, "minimax-E-S", "⟨1208.380 1892.933 2779.860]"]; (* [3c] *)
testClose[optimizeTuningMap, porcupine, "minimax-E-S", "⟨1199.562 1907.453 2779.234]"]; (* [3d] *)
testClose[optimizeTuningMap, srutal, "minimax-E-S", "⟨1198.823 1903.030 2787.467]"]; (* [3e] *)
testClose[optimizeTuningMap, hanson, "minimax-E-S", "⟨1200.166 1902.303 2785.418]"]; (* [3f] *)
testClose[optimizeTuningMap, magic, "minimax-E-S", "⟨1201.248 1902.269 2782.950]"]; (* [3g] *)
testClose[optimizeTuningMap, negri, "minimax-E-S", "⟨1202.347 1900.691 2782.698]"]; (* [3h] *)
testClose[optimizeTuningMap, tetracot, "minimax-E-S", "⟨1199.561 1903.942 2784.419]"]; (* [3i] *)
testClose[optimizeTuningMap, meantone7, "minimax-E-S", "⟨1201.242 1898.458 2788.863 3368.432]"]; (* [3j] *)
testClose[optimizeTuningMap, magic7, "minimax-E-S", "⟨1201.082 1903.476 2782.860 3367.259]"]; (* [3k] *)
testClose[optimizeTuningMap, pajara, "minimax-E-S", "⟨1197.719 1903.422 2780.608 3379.468]"]; (* [3l] *)
testClose[optimizeTuningMap, augene, "minimax-E-S", "⟨1196.255 1903.298 2791.261 3370.933]"]; (* [3m] *)
testClose[optimizeTuningMap, sensi, "minimax-E-S", "⟨1199.714 1903.225 2789.779 3363.173]"]; (* [3n] *)
testClose[optimizeTuningMap, sensamagic, "minimax-E-S", "⟨1200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3o] *)
(* original name *)
testClose[optimizeGeneratorTuningMap, meantone, "TE", optimizeGeneratorTuningMap[meantone, "minimax-E-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "T2", optimizeGeneratorTuningMap[meantone, "minimax-E-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "TOP-RMS", optimizeGeneratorTuningMap[meantone, "minimax-E-S"]];
testClose[optimizeGeneratorTuningMap, meantone, "Tenney-Euclidean", optimizeGeneratorTuningMap[meantone, "minimax-E-S"]];

(* unchanged-octave minimax-E-S = "CTE", "Constrained Tenney-Euclidean" *)
testClose[optimizeGeneratorTuningMap, meantone, "unchanged-octave minimax-E-S", "⟨1200.000 697.214]"]; (* [8a] *)
testClose[optimizeGeneratorTuningMap, blackwood, "unchanged-octave minimax-E-S", "⟨240.000, 1200.000 * 2 + 386.314]"]; (* [8b] *)
testClose[optimizeGeneratorTuningMap, dicot, "unchanged-octave minimax-E-S", "⟨1200.000 354.664]"]; (* [8c] *)
testClose[optimizeGeneratorTuningMap, augmented, "unchanged-octave minimax-E-S", "⟨400.000, 1200.000 + 701.955]"]; (* [8d] *)
testClose[optimizeGeneratorTuningMap, mavila, "unchanged-octave minimax-E-S", "⟨1200.000, 1200.000 + 677.145]"]; (* [8e] *)
testClose[optimizeGeneratorTuningMap, porcupine, "unchanged-octave minimax-E-S", "⟨1200.000 -164.166]"]; (* [8f] *)
testClose[optimizeGeneratorTuningMap, srutal, "unchanged-octave minimax-E-S", "⟨600.000, 1200.000 + 705.136]"]; (* [8g] *)
testClose[optimizeGeneratorTuningMap, hanson, "unchanged-octave minimax-E-S", "⟨1200.000 317.059]"]; (* [8h] *)
testClose[optimizeGeneratorTuningMap, magic, "unchanged-octave minimax-E-S", "⟨1200.000 380.499]"]; (* [8i] *)
testClose[optimizeGeneratorTuningMap, negri, "unchanged-octave minimax-E-S", "⟨1200.000 125.396]"]; (* [8j] *)
testClose[optimizeGeneratorTuningMap, tetracot, "unchanged-octave minimax-E-S", "⟨1200.000 176.028]"]; (* [8k] *)
testClose[optimizeGeneratorTuningMap, meantone7, "unchanged-octave minimax-E-S", "⟨1200.000, 1200.000 + 696.952]"]; (* [8l] *)
testClose[optimizeGeneratorTuningMap, magic7, "unchanged-octave minimax-E-S", "⟨1200.000 380.651]"]; (* [8m] *)
testClose[optimizeGeneratorTuningMap, pajara, "unchanged-octave minimax-E-S", "⟨600.000, 600.000 * -1 + 708.356]"]; (* [8n] *)
testClose[optimizeGeneratorTuningMap, augene, "unchanged-octave minimax-E-S", "⟨400.000, 1200.000 + 709.595]"]; (* [8o] *)
testClose[optimizeGeneratorTuningMap, sensi, "unchanged-octave minimax-E-S", "⟨1200.000, 1200.000 - 756.683]"]; (* [8p] *)
testClose[optimizeGeneratorTuningMap, sensamagic, "unchanged-octave minimax-E-S", "⟨1200.000, 1200.000 + 703.742, 440.902]"]; (* [8q] *)
testClose[optimizeTuningMap, meantone, "CTE", optimizeTuningMap[meantone, "unchanged-octave minimax-E-S"]];
testClose[optimizeTuningMap, meantone, "Constrained Tenney-Euclidean", optimizeTuningMap[meantone, "unchanged-octave minimax-E-S"]];


(* proving that minimax-E-S = primes miniRMS-S *)
testClose[optimizeGeneratorTuningMap, meantone, "minimax-E-S", optimizeGeneratorTuningMap[meantone, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-E-S", optimizeGeneratorTuningMap[blackwood, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-E-S", optimizeGeneratorTuningMap[dicot, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-E-S", optimizeGeneratorTuningMap[augmented, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-E-S", optimizeGeneratorTuningMap[mavila, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-E-S", optimizeGeneratorTuningMap[porcupine, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-E-S", optimizeGeneratorTuningMap[srutal, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-E-S", optimizeGeneratorTuningMap[hanson, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-E-S", optimizeGeneratorTuningMap[magic, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-E-S", optimizeGeneratorTuningMap[negri, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-E-S", optimizeGeneratorTuningMap[tetracot, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-E-S", optimizeGeneratorTuningMap[meantone7, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-E-S", optimizeGeneratorTuningMap[magic7, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-E-S", optimizeGeneratorTuningMap[pajara, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-E-S", optimizeGeneratorTuningMap[augene, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-E-S", optimizeGeneratorTuningMap[sensi, "primes miniRMS-S"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-E-S", optimizeGeneratorTuningMap[sensamagic, "primes miniRMS-S"]];

(* proving that minimax-S = primes minimax-S *)
testClose[optimizeGeneratorTuningMap, meantone, "minimax-S", optimizeGeneratorTuningMap[meantone, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, blackwood, "minimax-S", optimizeGeneratorTuningMap[blackwood, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, dicot, "minimax-S", optimizeGeneratorTuningMap[dicot, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, augmented, "minimax-S", optimizeGeneratorTuningMap[augmented, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, mavila, "minimax-S", optimizeGeneratorTuningMap[mavila, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, porcupine, "minimax-S", optimizeGeneratorTuningMap[porcupine, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, srutal, "minimax-S", optimizeGeneratorTuningMap[srutal, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, hanson, "minimax-S", optimizeGeneratorTuningMap[hanson, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, magic, "minimax-S", optimizeGeneratorTuningMap[magic, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, negri, "minimax-S", optimizeGeneratorTuningMap[negri, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, tetracot, "minimax-S", optimizeGeneratorTuningMap[tetracot, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, meantone7, "minimax-S", optimizeGeneratorTuningMap[meantone7, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, magic7, "minimax-S", optimizeGeneratorTuningMap[magic7, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, pajara, "minimax-S", optimizeGeneratorTuningMap[pajara, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, augene, "minimax-S", optimizeGeneratorTuningMap[augene, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, sensi, "minimax-S", optimizeGeneratorTuningMap[sensi, "primes minimax-S"]];
testClose[optimizeGeneratorTuningMap, sensamagic, "minimax-S", optimizeGeneratorTuningMap[sensamagic, "primes minimax-S"]];


(* getDualPower *)
test[getDualPower, 1, \[Infinity]];
test[getDualPower, 2, 2];
test[getDualPower, \[Infinity], 1];



(*
sources:
[1] Facebook https://www.facebook.com
[1a] https://www.facebook.com/groups/xenharmonicmath/posts/2363908480416027/?comment_id=2363994823740726
[1b] https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/
[1c] https://www.facebook.com/groups/xenharmonicmath/posts/1035558283251060/?comment_id=1041634519310103&reply_comment_id=1041649585975263
[1d] https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064
[1e] https://www.facebook.com/groups/xenharmonicmath/posts/738498989623659/?comment_id=738515309622027
[1f] (link lost, sorry) "The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents."
[2] Yahoo posts https://yahootuninggroupsultimatebackup.github.io
[2a] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029
[2b] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_15819
[3] Graham's temperament app http://x31eq.com/temper/
[3a] http://x31eq.com/cgi-bin/rt.cgi?ets=3_7&limit=5
[3b] http://x31eq.com/cgi-bin/rt.cgi?ets=12_3&limit=5
[3c] http://x31eq.com/cgi-bin/rt.cgi?ets=7_2p&limit=5
[3d] http://x31eq.com/cgi-bin/rt.cgi?ets=7_15&limit=5
[3e] http://x31eq.com/cgi-bin/rt.cgi?ets=12_34&limit=5
[3f] http://x31eq.com/cgi-bin/rt.cgi?ets=53_19&limit=5
[3g] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=5
[3h] http://x31eq.com/cgi-bin/rt.cgi?ets=19_10&limit=5
[3i] http://x31eq.com/cgi-bin/rt.cgi?ets=7_34&limit=5
[3j] http://x31eq.com/cgi-bin/rt.cgi?ets=12_19&limit=7
[3k] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=7
[3l] http://x31eq.com/cgi-bin/rt.cgi?ets=12_10&limit=7
[3m] http://x31eq.com/cgi-bin/rt.cgi?ets=12_15&limit=7
[3n] http://x31eq.com/cgi-bin/rt.cgi?ets=19_27&limit=7
[3o] http://x31eq.com/cgi-bin/rt.cgi?ets=27_19_22&limit=7
[3p] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=3_7&tuning=po
[3q] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_3&tuning=po
[3r] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_2p&tuning=po
[3s] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_15&tuning=po
[3t] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_34&tuning=po
[3u] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=53_19&tuning=po
[3v] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_22&tuning=po
[3w] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_10&tuning=po
[3x] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_34&tuning=po
[3y] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_19&tuning=po
[3z] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_22&tuning=po
[3aa] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_10&tuning=po
[3ab] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_15&tuning=po
[3ac] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_27&tuning=po
[3ad] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=27_19_22&tuning=po
[4] Flora's temperament app https://github.com/FloraCanou/te_temperament_measures
[5] Paul's papers 
[5a] 
[6] Graham's papers http://x31eq.com/tuning.htm
[6a] 
[7] Xen wiki https://en.xen.wiki
[7a] https://en.xen.wiki/w/Target_tunings#Example
[7b] https://en.xen.wiki/w/Augene
[7c] https://en.xen.wiki/w/Porcupine
[7d] https://en.xen.wiki/w/Magic
[7e] https://en.xen.wiki/w/Tetracot_family#Tetracot
[7f] https://en.xen.wiki/w/Meantone
[7g] https://en.xen.wiki/w/Sensipent_family#Septimal_sensi
[7h] https://en.xen.wiki/w/Sensamagic_family#Sensamagic
[7i] https://en.xen.wiki/w/Myna#Tuning_spectrum
[7j] https://en.xen.wiki/w/Pajara#Tuning_spectrum
[7k] https://en.xen.wiki/w/Chromatic_pairs#Voltage
[8] Sintel's app https://github.com/Sin-tel/temper
[8a] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=81%2F80&submit_comma=submit
[8b] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=256%2F243&submit_comma=submit
[8c] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=25%2F24&submit_comma=submit
[8d] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=128%2F125&submit_comma=submit
[8e] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=135%2F128&submit_comma=submit
[8f] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=250%2F243&submit_comma=submit
[8g] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=2048%2F2025&submit_comma=submit
[8h] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=15625%2F15552&submit_comma=submit
[8i] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=3125%2F3072&submit_comma=submit
[8j] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=16875%2F16384&submit_comma=submit
[8k] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=20000%2F19683&submit_comma=submit
[8l] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=81%2F80%2C+126%2F125&submit_comma=submit
[8m] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=225%2F224%2C+245%2F243&submit_comma=submit
[8n] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=50%2F49%2C+64%2F63&submit_comma=submit
[8o] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=64%2F63%2C+126%2F125&submit_comma=submit
[8p] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=126%2F125%2C+245%2F243&submit_comma=submit
[8q] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=245%2F243&submit_comma=submit
[9] Scala
[10] Discord history https://discord.com/channels/332357996569034752
[10a] https://discord.com/channels/332357996569034752/859884647337033738/969259730839171123
[11] Keenan Pepper's tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/KeenanPepper/tiptop.py
[12] Mike Battaglia's tipweil.py variation on tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/MikeBattaglia/tipweil.py
*)


(* MEAN DAMAGE *)

(* getGeneratorTuningMapMeanDamage *)
testDamageMeanOrComplexity[getGeneratorTuningMapMeanDamage, meantone, "⟨1201.70 697.564]", "minimax-S", 1.700];


(* stress tests *)

optimizeGeneratorTuningMap["[⟨53 84 123]⟩", "minimax-S"]; (* 5-limit *)
optimizeGeneratorTuningMap["[⟨1 1 3 3] ⟨0 6 -7 -2]⟩", "minimax-S"]; (* 7-limit *)
optimizeGeneratorTuningMap["[⟨1 0 0 -5 12] ⟨0 1 0 2 -1] ⟨0 0 1 2 -3]⟩", "minimax-S"]; (* 11-limit *)
optimizeGeneratorTuningMap["[⟨1 0 0 0 4 -1] ⟨0 2 0 0 -3 3] ⟨0 0 1 0 2 1] ⟨0 0 0 1 -1 0]⟩", "minimax-S"]; (* 13-limit *)
optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1] ⟨0 1 0 1 2 0 0] ⟨0 0 1 0 -1 0 0] ⟨0 0 0 2 1 0 -1] ⟨0 0 0 0 0 1 1]⟩", "minimax-S"]; (* 17-limit *)
optimizeGeneratorTuningMap["[⟨1 0 0 0 2 0 1 0] ⟨0 1 0 1 2 0 0 0] ⟨0 0 1 0 -1 0 0 0] ⟨0 0 0 2 1 0 -1 0] ⟨0 0 0 0 0 1 1 0] ⟨0 0 0 0 0 0 0 1]⟩", "minimax-S"]; (* 19-limit *)

(* ... *)
(* optimizeGeneratorTuningMap["[⟨1 0 0 0 0 0 -1 0 0 0 0 0] ⟨0 1 0 0 0 0 -1 0 0 0 0 0] ⟨0 0 1 0 0 0 1 0 0 0 0 0] ⟨0 0 0 1 0 0 -1 0 0 0 0 0] ⟨0 0 0 0 1 0 1 0 0 0 0 0] ⟨0 0 0 0 0 1 1 0 0 0 0 0] ⟨0 0 0 0 0 0 0 1 0 0 0 0] ⟨0 0 0 0 0 0 0 0 1 0 0 0] ⟨0 0 0 0 0 0 0 0 0 1 0 0] ⟨0 0 0 0 0 0 0 0 0 0 1 0] ⟨0 0 0 0 0 0 0 0 0 0 0 1]⟩", "minimax-S"]; *) (* 37-limit, 40-TILT; makes it to the power limit solver, but fails to converge there and times out *)




printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
