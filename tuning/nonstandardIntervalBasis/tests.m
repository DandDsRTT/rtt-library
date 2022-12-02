failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";

test[getMinimumStandardIntervalBasis, {2, 5 / 3, 9 / 7}, {2, 3, 5, 7}];



printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];
