mcn.test = function(v1, v2)
{
    M11 = sum(v1 == "TP" & v2 == "TP") +
          sum(v1 == "TN" & v2 == "TN")

    M01 = sum(v1 == "FP" & v2 == "TP") +
          sum(v1 == "FP" & v2 == "TN") +
          sum(v1 == "FN" & v2 == "TP") +
          sum(v1 == "FN" & v2 == "TN") +
          sum(v1 == "N0" & v2 == "TP") +
          sum(v1 == "N0" & v2 == "TN") +
          sum(v1 == "N1" & v2 == "TP") +
          sum(v1 == "N1" & v2 == "TN")

    M10 = sum(v1 == "TP" & v2 == "FP") +
          sum(v1 == "TP" & v2 == "FN") +
          sum(v1 == "TP" & v2 == "N0") +
          sum(v1 == "TP" & v2 == "N1") +
          sum(v1 == "TN" & v2 == "FP") +
          sum(v1 == "TN" & v2 == "FN") +
          sum(v1 == "TN" & v2 == "N0") +
          sum(v1 == "TN" & v2 == "N1")

    M00 = nrow(v1) - M11 - M01 - M10

    performance = matrix(c(M00, M10, M01, M11), nrow=2)

    pval = mcnemar.test(performance)$p.value

    return (pval)
}
