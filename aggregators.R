source('aggregators-helpers.R')


#########################
# Aggregation functions
#########################
# *m* passed to all aggregation functions as argument is matrix.

# str(m) gives following output:
# > num [1:2, 1:6] 0.676 0.781 0.639 0.833 0.688 ...
# The lower and upper bounds of i-th interval diagnosis can be obtained via:
# m[1,i] and m[2,i]
# Each aggragation function returns binary diangosis or *NA*.
#########################

AGG.GEN.T.OPERATION = function(t_oper, representant.selector, cutoff.numeric) {
    force(t_oper);force(cutoff.numeric);force(representant.selector)
    return(function(m){
        reps = representant.selector(m)
        result = t_oper(reps)
        return(cutoff.numeric(result))
    })
}
AGG.GEN.INTERVAL.T.OPERATION = function(t_oper, cutoff) {
    force(t_oper);force(cutoff)
    return(function(m){
        lower = t_oper(m[1, ])
        upper = t_oper(m[2, ])
        return(cutoff(c(lower, upper)))
    })
}

AGG.GEN.INTEGRAL = function(measure, representant.selector, cutoff.numeric,integral) {
    # Aggregates by computing integral of representants of intervals.
    # The numeric cutoff is then applied to resulting number to obtain binary
    # diagnosis. The measure is a function such as:
    # * measure(emptyset) = 0
    # * measure(universum) = 1
    # * measure(E1) < measure(E2) if E1 is a subset of E2
    force(measure);force(representant.selector);force(cutoff.numeric);force(integral)
    intg = integral(measure, representant.selector)
    return(function(m){
        result = intg(m)
        return(cutoff.numeric(result))
    })
}

AGG.GEN.INTERVAL.INTEGRAL = function(measure, cutoff, integral) {
    # Aggregates by computing interval-valued integral.
    # The interval cutoff is then applied to resulting interval to obtain binary
    # diagnosis. The measure is a function such as:
    # * measure(emptyset) = 0
    # * measure(universum) = 1
    # * measure(E1) < measure(E2) if E1 is a subset of E2
    force(measure);force(cutoff);force(integral)

    lower = integral(measure,SELECTOR.MIN)
    upper = integral(measure,SELECTOR.MAX)
    return(function(m){
        resultLower = lower(m)
        resultUpper = upper(m)
        return(cutoff(c(resultLower,resultUpper)))
    })
}

AGG.GEN.INTERVAL.MEAN = function(weightLower, cutoff, weightUpper = NULL, r=1){
    # Aggregates by computing of weighted mean of input intervals using interval
    # arithmetic. The interval cutoff is then applied to resulting interval to
    # obtain binary diagnosis. The *r* agrument defines the exponent in r-mean.
    force(weightLower);force(cutoff);force(weightUpper)
    return(function(m){
        wL = weightLower(m)
        sum.wL = sum(wL);
        if(is.null(weightUpper)){
            wU=wL
            sum.wU=sum.wL
        }else{
            wU = weightUpper(m)
            sum.wU = sum(wU);
        }

        if(sum.wL==0 || sum.wU==0){
            return(NA)
        } else {
            a=(sum((m[1,]^r) * wL) / sum.wL) ^ (1/r)
            b=(sum((m[2,]^r) * wU) / sum.wU) ^ (1/r)
            return(cutoff(c(a,b)))
        }
    })
}

AGG.GEN.MEAN.WEIGHTED = function(weight.gen, representant.selector, cutoff.numeric, r=1){
    # Aggregates by computing of weighted mean of representants of intervals.
    # Representatives are selected by selector. The numeric cutoff is then applied
    # to resulting number to obtain binary diagnosis. The *r* agrument defines
    # the exponent in r-mean.
    force(weight.gen);force(representant.selector);force(cutoff.numeric);force(r)
    return(function(m){
        representants = representant.selector(m) # length(representants) == length(m)
        weights = weight.gen(m) # length(weights) == length(m)

        sum.w = sum(weights)
        if(is.na(sum.w)){
            browser()
        }
        if(sum.w==0){
            return(NA)
        }else{
            if(r!=1){
                result = (sum((representants^r) * weights) / sum.w)^(1/r)
            }else{
                result = sum(representants * weights) / sum.w
            }
            return(cutoff.numeric(result))
        }
    })
}

AGG.GEN.INTERVAL.INTERSECTION = function(cutoff){
    # Aggregates by set intersection (common part of two sets).
    # Input intervals are treated as sets, aggregation computes intersection of them.
    # Interval cotoff is then applied to obtain binary diagnosis.
    force(cutoff)
    return(function(m){
        interval = c(max(m[1,]),min(m[2,]))
        if(interval[1] > interval[2]){
            return(NA)
        }else{
            return(cutoff(interval))
        }
    })
}

AGG.GEN.INTERVAL.SUM = function(cutoff){
    # Aggregates by sum of two intervals. Input intervals are treated as sets,
    # aggregation computes intersection of them. Interval cotoff is then applied
    # to obtain binary diagnosis.
    force(cutoff)
    return(function(m){
        interval = c(min(m[1,]),max(m[2,]))
        if(interval[1]>interval[2]){
            return(NA)
        }else{
            return(cutoff(interval))
        }
    })
}

#########################
# Lists of aggretation functions
#########################
MEAN = apply(cbind(expand.grid(AGGR.WEIGHTS, AGGR.SELECTORS, AGGR.CUTOFFS.NUMERIC,AGGR.RMEANS),
                   expand.grid(AGGR.WEIGHTS.NAME, AGGR.SELECTORS.NAME, AGGR.CUTOFFS.NUMERIC.NAME,AGGR.RMEANS.NAME)),
             1, function(row){
                 list(AGG.GEN.MEAN.WEIGHTED(row[[1]],row[[2]],row[[3]],r=row[[4]]),
                      paste('mean',row[[5]],row[[6]],row[[7]],row[[8]],sep='_'),
                      'Mean', 'Numeric')
             })

MEAN.INTERVAL = apply(cbind(expand.grid(AGGR.WEIGHTS, AGGR.CUTOFFS, AGGR.RMEANS),
                            expand.grid(AGGR.WEIGHTS.NAME, AGGR.CUTOFFS.NAME, AGGR.RMEANS.NAME)),
                      1, function(row){
                          list(AGG.GEN.INTERVAL.MEAN(row[[1]],row[[2]],r=row[[3]]),
                               paste('iMean',row[[4]],row[[5]],row[[6]],sep='_'),
                               'Mean', 'Interval')
                      })

OWA = apply(cbind(expand.grid(AGGR.WEIGHTS.OWA, AGGR.SELECTORS, AGGR.CUTOFFS.NUMERIC),
                  expand.grid(AGGR.WEIGHTS.OWA.NAME, AGGR.SELECTORS.NAME, AGGR.CUTOFFS.NUMERIC.NAME)),
            1, function(row){
                list(AGG.GEN.MEAN.WEIGHTED(row[[1]],row[[2]],row[[3]]),
                     paste('mean',row[[4]],row[[5]],row[[6]],sep='_'),
                 'OWA', 'Numeric')
            })
OWA.INTERVAL = apply(cbind(expand.grid(AGGR.WEIGHTS.OWA, AGGR.CUTOFFS),
                           expand.grid(AGGR.WEIGHTS.OWA.NAME, AGGR.CUTOFFS.NAME)),
                     1, function(row){
                         list(AGG.GEN.INTERVAL.MEAN(row[[1]],row[[2]], row[[1]]),
                              paste('iMean',row[[3]],row[[4]],sep='_'),
                              'OWA', 'Interval')
                     })
OWA.INTERSECTION = apply(cbind(AGGR.CUTOFFS,
                               AGGR.CUTOFFS.NAME),
                         1, function(row){
                             list(AGG.GEN.INTERVAL.INTERSECTION(row[[1]]),
                                  paste('inter',row[[2]],sep='_'),
                                  'OWA', 'Interval')
                         })
OWA.SUM = apply(cbind(AGGR.CUTOFFS,
                      AGGR.CUTOFFS.NAME),
                1, function(row){
                    list(AGG.GEN.INTERVAL.SUM(row[[1]]),
                         paste('sum',row[[2]],sep='_'),
                         'OWA', 'Interval')
                })
INTERGRAL = apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES, AGGR.SELECTORS, AGGR.CUTOFFS.NUMERIC),
                        expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME, AGGR.SELECTORS.NAME, AGGR.CUTOFFS.NUMERIC.NAME)),
                  1, function(row){
                      list(AGG.GEN.INTEGRAL(row[[2]],row[[3]],row[[4]],row[[1]]),
                           paste(row[[5]],row[[6]],row[[7]],row[[8]],sep='_'),
                           'Integral', 'Numeric')
                  })
INTERGRAL.INTERVAL = apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES, AGGR.CUTOFFS),
                                 expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME, AGGR.CUTOFFS.NAME)),
                           1, function(row){
                               list(AGG.GEN.INTERVAL.INTEGRAL(row[[2]],row[[3]],row[[1]]),
                                    paste('i',row[[4]],row[[5]],row[[6]],sep='_'),
                                    'Integral', 'Interval')
                           })
T.OPERATION = apply(cbind(expand.grid(AGGR.T.OPERATIONS, AGGR.SELECTORS, AGGR.CUTOFFS.NUMERIC),
                     expand.grid(AGGR.T.OPERATIONS.NAME, AGGR.SELECTORS.NAME, AGGR.CUTOFFS.NUMERIC.NAME)),
               1, function(row){
                   list(AGG.GEN.T.OPERATION(row[[1]],row[[2]],row[[3]]),
                        paste(row[[4]],row[[5]],row[[6]],sep='_'),
                        't-operation', 'Numeric')
               })
T.OPERATION.INTERVAL = apply(cbind(expand.grid(AGGR.T.OPERATIONS, AGGR.CUTOFFS),
                              expand.grid(AGGR.T.OPERATIONS.NAME, AGGR.CUTOFFS.NAME)),
                        1, function(row){
                            list(AGG.GEN.INTERVAL.T.OPERATION(row[[1]],row[[2]]),
                                 paste('i',row[[3]],row[[4]],sep='_'),
                                 't-operation', 'Interval')
                        })


# at least 2 aggrs must be defined
# name and class must not contain '-' nad '=' signs (must be valid data.frame column name)
AGGREGATORS.LIST = c(MEAN[1],
                     MEAN.INTERVAL[1],
                     OWA[1],
                     OWA.INTERVAL[1],
                     OWA.INTERSECTION[1],
                     OWA.SUM[1],
                     INTERGRAL[1],
                     INTERGRAL.INTERVAL[1],
                     T.OPERATION[1],
                     T.OPERATION.INTERVAL[1])

AGGREGATORS.LIST = sample(AGGREGATORS.LIST,length(AGGREGATORS.LIST))
AGGREGATORS = sapply(AGGREGATORS.LIST,'[[',1)
AGGREGATORS.NAME = sapply(AGGREGATORS.LIST,'[[',2)
AGGREGATORS.CLASS = sapply(AGGREGATORS.LIST,'[[',3)
AGGREGATORS.SUBCLASS = sapply(AGGREGATORS.LIST,'[[',4)

AGGREGATORS.BINDED.DESCRIPTION = data.frame(Method=AGGREGATORS.NAME,
                                            Class="Aggregation",
                                            Subclass=AGGREGATORS.CLASS,
                                            Subsubclass=AGGREGATORS.SUBCLASS)
