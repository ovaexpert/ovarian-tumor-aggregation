stat.accuracy = function(data){
    results = (data[,'TP']+data[,'TN']) /
        (data[,'TP']+data[,'TN']+data[,'FN']+data[,'FP'])
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.accuracy.all = function(data){
    results = (data[,'TP']+data[,'TN']) /
        (probe.size.positive(data) + probe.size.negative(data))
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.sensitivity = function(data){
    results = data[,'TP'] / (data[,'TP']+data[,'FN'])
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.sensitivity.all = function(data){
    results = data[,'TP'] / probe.size.positive(data)
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.specificity = function(data){
    results = data[,'TN'] / (data[,'TN']+data[,'FP'])
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.specificity.all = function(data){
    results = data[,'TN'] / probe.size.negative(data)
    results[is.nan(results)] = 0
    return(mean(results))
}

stat.decisiveness = function(data){
    results = (data[,'N0']+data[,'N1']) /
        (data[,'TP']+data[,'TN']+data[,'FN']+data[,'FP'] + data[,'N0']+data[,'N1'])
    results[is.nan(results)] = 0
    return(1 - mean(results))
}

stat.cost.matrix = function(data){
    results = 0 * data[, 'TP'] +
        0 * data[, 'TN'] +
        2.5 * data[, 'FP'] +
        5 * data[, 'FN'] +
        1 * data[, 'N0'] +
        2 * data[, 'N1']
    results[is.nan(results)] = 0
    return(mean(results))
}

#__________________________________________________________

probe.size.positive = function(data)
{
    data[, 'TP'] + data[, 'FN'] + data[, 'N1']
}

probe.size.negative = function(data)
{
    data[, 'FP'] + data[, 'TN'] + data[, 'N0']
}

#__________________________________________________________

STATS = c(stat.accuracy,
          stat.accuracy.all,
          stat.sensitivity,
          stat.sensitivity.all,
          stat.specificity,
          stat.specificity.all,
          stat.decisiveness,
          stat.cost.matrix)

STATS.NAME = c("Accuracy",
               "Accuracy (all)",
               "Sensitivity",
               "Sensitivity (all)",
               "Specificity",
               "Specificity (all)",
               "Decisiveness",
               "Cost matrix")
