getOptimizedAggregators = function(inputData, performanceMeasure)
{
    MEAN.OPT = apply(cbind(expand.grid(AGGR.WEIGHTS, AGGR.SELECTORS),
                           expand.grid(AGGR.WEIGHTS.NAME, AGGR.SELECTORS.NAME)),
                     1, function(row){
                         tmp = apply(cbind(expand.grid(AGGR.CUTOFFS.NUMERIC,AGGR.RMEANS),
                                           expand.grid(AGGR.CUTOFFS.NUMERIC.NAME,AGGR.RMEANS.NAME)),
                                     1, function(row2){
                                         list(AGG.GEN.MEAN.WEIGHTED(row[[1]],row[[2]],row2[[1]],r=row2[[2]]),
                                              paste('mean',row[[3]],row[[4]],row2[[3]],row2[[4]],sep='_'),
                                              'Mean', 'Numeric')
                                     })
                         df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))

                         best = ifelse(PERFORMANCE.MEASURE.DESC,
                                       arrange(df, desc(Value))[1,1],
                                       arrange(df, Value)[1,1])
                         return(best)
                     })


    MEAN.INTERVAL.OPT = apply(cbind(AGGR.WEIGHTS, AGGR.WEIGHTS.NAME),
                              1, function(row){
                                  tmp = apply(cbind(expand.grid(AGGR.CUTOFFS, AGGR.RMEANS),
                                                    expand.grid(AGGR.CUTOFFS.NAME, AGGR.RMEANS.NAME)),
                                              1, function(row2){
                                                  list(AGG.GEN.INTERVAL.MEAN(row[[1]],row2[[1]],r=row2[[2]]),
                                                       paste('iMean',row[[2]],row2[[3]],row2[[4]],sep='_'),
                                                       'Mean', 'Interval')
                                              })
                                  df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                                  best = ifelse(PERFORMANCE.MEASURE.DESC,
                                                arrange(df, desc(Value))[1,1],
                                                arrange(df, Value)[1,1])
                                  return(best)
                              })


    OWA.OPT = apply(cbind(expand.grid(AGGR.SELECTORS, AGGR.OWA.ORDERS),
                          expand.grid(AGGR.SELECTORS.NAME, AGGR.OWA.ORDERS.NAME)),
                    1, function(row){
                        tmp = apply(cbind(expand.grid(AGGR.OWA.WEIGHTS, AGGR.CUTOFFS.NUMERIC),
                                          expand.grid(AGGR.OWA.WEIGHTS.NAME, AGGR.CUTOFFS.NUMERIC.NAME)),
                                    1, function(row2){
                                        wg = GEN.WEIGHT.OWA(row2[[1]],row[[2]])
                                        wgn = paste('(',row2[[3]],'_',row[[4]],')',sep='')
                                        list(AGG.GEN.MEAN.WEIGHTED(wg,row[[1]],row2[[2]]),
                                             paste('mean',wgn,row[[3]],row2[[4]],sep='_'),
                                             'OWA', 'Numeric')
                                    })
                        df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                        best = ifelse(PERFORMANCE.MEASURE.DESC,
                                      arrange(df, desc(Value))[1,1],
                                      arrange(df, Value)[1,1])
                        return(best)
                    })


    OWA.INTERVAL.OPT = apply(cbind(AGGR.OWA.ORDERS, AGGR.OWA.ORDERS.NAME),
                             1, function(row){
                                 tmp = apply(cbind(expand.grid(AGGR.OWA.WEIGHTS, AGGR.CUTOFFS),
                                                   expand.grid(AGGR.OWA.WEIGHTS.NAME, AGGR.CUTOFFS.NAME)),
                                             1, function(row2){
                                                 wg = GEN.WEIGHT.OWA(row2[[1]],row[[1]])
                                                 wgn = paste('(',row2[[3]],'_',row[[2]],')',sep='')
                                                 list(AGG.GEN.INTERVAL.MEAN(wg,row2[[2]], wg),
                                                      paste('iMean',wgn,row2[[4]],sep='_'),
                                                      'OWA', 'Interval')
                                             })
                                 df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                                 best = ifelse(PERFORMANCE.MEASURE.DESC,
                                               arrange(df, desc(Value))[1,1],
                                               arrange(df, Value)[1,1])
                                 return(best)
                             })

    df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(OWA.INTERSECTION,"[[",2))
    OWA.INTERSECTION.OPT = ifelse(PERFORMANCE.MEASURE.DESC,
                                  arrange(df, desc(Value))[1,1],
                                  arrange(df, Value)[1,1])


    df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(OWA.SUM,"[[",2))
    OWA.SUM.OPT = ifelse(PERFORMANCE.MEASURE.DESC,
                         arrange(df, desc(Value))[1,1],
                         arrange(df, Value)[1,1])


    INTERGRAL.OPT = apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES, AGGR.SELECTORS),
                                expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME, AGGR.SELECTORS.NAME)),
                          1, function(row){
                              tmp = apply(cbind(AGGR.CUTOFFS.NUMERIC, AGGR.CUTOFFS.NUMERIC.NAME),
                                          1, function(row2){
                                              list(AGG.GEN.INTEGRAL(row[[2]],row[[3]],row2[[1]],row[[1]]),
                                                   paste(row[[4]],row[[5]],row[[6]],row2[[2]],sep='_'),
                                                   'Integral', 'Numeric')
                                          })
                              df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                              best = ifelse(PERFORMANCE.MEASURE.DESC,
                                            arrange(df, desc(Value))[1,1],
                                            arrange(df, Value)[1,1])
                              return(best)
                          })


    INTERGRAL.INTERVAL.OPT = apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES),
                                         expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME)),
                                   1, function(row){
                                       tmp = apply(cbind(AGGR.CUTOFFS,AGGR.CUTOFFS.NAME),
                                                   1, function(row2){
                                                       list(AGG.GEN.INTERVAL.INTEGRAL(row[[2]],row2[[1]],row[[1]]),
                                                            paste('i',row[[3]],row[[4]],row2[[2]],sep='_'),
                                                            'Integral', 'Interval')
                                                   })
                                       df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                                       best = ifelse(PERFORMANCE.MEASURE.DESC,
                                                     arrange(df, desc(Value))[1,1],
                                                     arrange(df, Value)[1,1])
                                       return(best)
                                   })


    T.OPERATION.OPT.1 = apply(cbind(expand.grid(AGGR.NORMS, AGGR.SELECTORS),
                               expand.grid(AGGR.NORMS.NAME, AGGR.SELECTORS.NAME)),
                         1, function(row){
                             tmp = apply(cbind(AGGR.CUTOFFS.NUMERIC, AGGR.CUTOFFS.NUMERIC.NAME),
                                         1, function(row2){
                                             list(AGG.GEN.T.OPERATION(row[[1]],row[[2]],row2[[1]]),
                                                  paste(row[[3]],row[[4]],row2[[2]],sep='_'),
                                                  't-operation', 'Numeric')
                                         })
                             df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                             best = ifelse(PERFORMANCE.MEASURE.DESC,
                                           arrange(df, desc(Value))[1,1],
                                           arrange(df, Value)[1,1])
                             return(best)
                         })

    T.OPERATION.OPT.2 = apply(cbind(expand.grid(AGGR.NORMS, AGGR.SELECTORS),
                               expand.grid(AGGR.NORMS.NAME, AGGR.SELECTORS.NAME)),
                         1, function(row){
                             tmp = apply(cbind(expand.grid(AGGR.CUTOFFS.NUMERIC,AGGR.SOFT.LAMBDAS),
                                               expand.grid(AGGR.CUTOFFS.NUMERIC.NAME, AGGR.SOFT.LAMBDAS.NAME)),
                                         1, function(row2){
                                             soft = GEN.SOFT(row[[1]],row2[[2]])
                                             softn = paste('(soft_',row[[3]],'_',row2[[4]],')',sep='')

                                             list(AGG.GEN.T.OPERATION(soft,row[[2]],row2[[1]]),
                                                  paste(softn,row[[4]],row2[[3]],sep='_'),
                                                  't-operation', 'Numeric')
                                         })
                             df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                             best = ifelse(PERFORMANCE.MEASURE.DESC,
                                           arrange(df, desc(Value))[1,1],
                                           arrange(df, Value)[1,1])
                             return(best)
                         })
    T.OPERATION.OPT = c(T.OPERATION.OPT.1, T.OPERATION.OPT.2)


    T.OPERATION.INTERVAL.OPT.1 = apply(cbind(AGGR.NORMS, AGGR.NORMS.NAME),
                                  1, function(row){
                                      tmp = apply(cbind(AGGR.CUTOFFS, AGGR.CUTOFFS.NAME),
                                                  1, function(row2){
                                                      list(AGG.GEN.INTERVAL.T.OPERATION(row[[1]],row2[[1]]),
                                                           paste('i',row[[2]],row2[[2]],sep='_'),
                                                           't-operation', 'Interval')
                                                  })
                                      df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                                      best = ifelse(PERFORMANCE.MEASURE.DESC,
                                                    arrange(df, desc(Value))[1,1],
                                                    arrange(df, Value)[1,1])
                                      return(best)
                                  })

    T.OPERATION.INTERVAL.OPT.2 = apply(cbind(AGGR.NORMS, AGGR.NORMS.NAME),
                                  1, function(row){
                                      tmp = apply(cbind(expand.grid(AGGR.CUTOFFS,AGGR.SOFT.LAMBDAS),
                                                        expand.grid(AGGR.CUTOFFS.NAME, AGGR.SOFT.LAMBDAS.NAME)),
                                                  1, function(row2){
                                                      soft = GEN.SOFT(row[[1]],row2[[2]])
                                                      softn = paste('(soft_',row[[2]],'_',row2[[4]],')',sep='')

                                                      list(AGG.GEN.INTERVAL.T.OPERATION(soft,row2[[1]]),
                                                           paste('i',softn,row2[[3]],sep='_'),
                                                           't-operation', 'Interval')
                                                  })
                                      df = subset(inputData, Measure==performanceMeasure & Method %in% sapply(tmp,"[[",2))
                                      best = ifelse(PERFORMANCE.MEASURE.DESC,
                                                    arrange(df, desc(Value))[1,1],
                                                    arrange(df, Value)[1,1])
                                      return(best)
                                  })

    T.OPERATION.INTERVAL.OPT = c(T.OPERATION.INTERVAL.OPT.1, T.OPERATION.INTERVAL.OPT.2)


    AGGREGATORS.OPT = c(MEAN.OPT,
                        MEAN.INTERVAL.OPT,
                        OWA.OPT,
                        OWA.INTERVAL.OPT,
                        OWA.INTERSECTION.OPT,
                        OWA.SUM.OPT,
                        INTERGRAL.OPT,
                        INTERGRAL.INTERVAL.OPT,
                        T.OPERATION.OPT,
                        T.OPERATION.INTERVAL.OPT)

    return(AGGREGATORS.OPT)
}