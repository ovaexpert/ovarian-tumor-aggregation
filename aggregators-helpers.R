#########################
# t-operations
#########################
TNORM.MIN = min
TCONORM.MIN = max
TNORM.PROD = prod
TCONORM.PROD = function(v){
    val = v[[1]]
    for(i in 2:length(v)){
        val = val + v[[i]] - val*v[[i]]
    }
    return(val)
}
TNORM.LUK = function(v){
    val = v[[1]]
    for(i in 2:length(v)){
        val = max(0,val + v[[i]] - 1)
    }
    return(val)
}
TCONORM.LUK = function(v){
    val = v[[1]]
    for(i in 2:length(v)){
        val = min(1,val + v[[i]])
    }
    return(val)
}
GEN.SOFT = function(copula, lambda){
    force(copula);force(lambda)
    return(function(v){
        return((1-lambda)*mean(v) + lambda*copula(v))
    })
}


#########################
# Fuzzy Integrals
#########################
CHOQUET.INTEGRAL = function(measure, representant.selector) {
    force(measure);force(representant.selector)
    return(function(m) {
        reps = representant.selector(m)
        ord = order(reps, decreasing =  TRUE)
        reps = reps[ord]

        n = length(reps)

        hCurr = c()
        hPrev = c()

        result = 0
        for(i in 1:length(reps)){
            ai = reps[[i]]
            hPrev=hCurr
            hCurr = c(hCurr, ord[[i]])

            result = result + (measure(hCurr,n) - measure(hPrev,n)) * ai
        }
        return(result)
    })
}

SUGENO.INTEGRAL = function(measure, representant.selector) {
    force(measure);force(representant.selector)
    return(function(m) {
        reps = representant.selector(m)
        ord = order(reps, decreasing =  TRUE)
        reps = reps[ord]

        n = length(reps)

        h = c()
        result = 0
        for(i in 1:length(reps)){
            ai = reps[[i]]
            h = c(h, ord[[i]])
            result = max(result, min(measure(h,n), ai) )
        }
        return(result)
    })
}

#########################
# Measures
#########################
# Measure is responsible for calculating the measure of classical crisp subset
# (most commonly it is cardinality based). Agrument *e* is the vector of of integers.
# Each integer represents the number of input decision. The argument *n* reflects
# the size of the universe of discourse. Returns Single number representing the
# measure of given fuzzy set.
#########################
MEASURE.CARDINALITY = function(e,n){
    return(length(e)/n)
}
W.AUC =c(0.9143,0.8839,0.8890,0.9241,0.9135, 0.8985)-0.85
W.AUC = W.AUC/sum(W.AUC)
MEASURE.ADDITIVE.AUC = function(e,n){
    return(sum(W.AUC[e]))
}




#########################
# Selectors
#########################
# Selector is responsible for choosing single number as representant of
# an interval. Agrument *m* has the same structure as this passed to aggretaion
# function. Returns vector of length *ncol(m)* containing selected representatnts.
# Selectors are used in e.g. weighted mean aggregatoin functions
#########################
SELECTOR.MIN = function(m){
    apply(m,2,function(v){
        return(v[[1]])
    })
}
SELECTOR.MAX = function(m){
    apply(m,2,function(v){
        return(v[[2]])
    })
}
SELECTOR.CENTER = function(m){
    apply(m,2,function(v){
        return((v[[1]]+v[[2]])/2)
    })
}


#########################
# OWA orders
#########################
# OWA order is responsible for computing order of input intervals.
# Agrument *m* has the same structure as this passed to aggretaion
# function. Returns integer vector of length *ncol(m)* containing numbers of columns
# from *m*.
#########################
GEN.ORDER.WEIGHT = function(weight.gen){
    # Basic OWA order. It orders columns with respect to weights returned by *weight.gen*.
    force(weight.gen)
    return(function(m){
        order(weight.gen(m),decreasing = TRUE)
    })
}


#########################
# Numeric cutoffs
#########################
# Numeric cutoff takes single number as its argument and returns binary diagnosis
# or *NA*.
#########################
CUTOFF.NUMERIC.CRISP = function(number){
    if(number>1 || number <0){
        stop("CUTOFF.NUMERIC.CRISP: Number ",number," agrument must be in [0,1].")
    }
    if(number>=0.5){
        return(1)
    }else{
        return(0)
    }
}
GEN.CUTOFF.NUMERIC.WITH.MARGIN = function(margin){
    force(margin)
    return(function(number){
        if(number>1 || number <0){
            stop("CUTOFF.NUMERIC.WITH.MARGIN: Number ",number," argument must be in [0,1].")
        }
        if(number>=0.5+margin){
            return(1)
        }else{
            if(number<=0.5-margin){
                return(0)
            }else{
                return(NA)
            }
        }
    })
}
CUTOFF.NUMERIC.MARGIN = GEN.CUTOFF.NUMERIC.WITH.MARGIN(0.025)


#########################
# Interval cutoffs
#########################
# Interval cutoff takes an interval as its argument and returns binary diagnosis
# or *NA*.
#########################
CUTOFF.CRISP = function(interval){
    if(interval[[1]]<0 || interval[[2]]>1 || round(interval[[1]],3)>round(interval[[2]],3)){
        stop("CUTOFF.CRISP: Interval [",interval[[1]],', ',interval[[2]],"] argument must be contained in [0,1].")
    }
    if(interval[[2]]<0.5){
        return(0)
    }else{
        if(interval[[1]]>=0.5){
            return(1);
        }else{
            return(NA)
        }
    }
}
GEN.CUTOFF.WITH.MARGIN = function(margin){
    force(margin)
    function(interval){
        if(interval[[1]]<0 || interval[[2]]>1 || round(interval[[1]],3)>round(interval[[2]],3)){
            stop("CUTOFF.WITH.MARGIN: Interval [",interval[[1]],', ',interval[[2]],"] argument must be contained in [0,1].")
        }
        if(interval[[1]]>=0.5-margin && interval[[2]]<0.5+margin){
            return(NA)
        }else{
            if(interval[[2]]<0.5+margin){
                return(0)
            }else{
                if(interval[[1]]>=0.5-margin){
                    return(1);
                }else{
                    return(NA);
                }
            }
        }
    }
}
CUTOFF.MARGIN = GEN.CUTOFF.WITH.MARGIN(0.025)
GEN.CUTOFF.FROM.NUMERIC = function(representant.selector, cutoff.numeric){
    force(representant.selector);force(cutoff.numeric)
    return(function(interval){
        rep = representant.selector(matrix(interval))
        return(cutoff.numeric(rep))
    })
}
CUTOFF.CENTER.MARGIN = GEN.CUTOFF.FROM.NUMERIC(SELECTOR.CENTER, CUTOFF.NUMERIC.MARGIN)
CUTOFF.MAX.MARGIN = GEN.CUTOFF.FROM.NUMERIC(SELECTOR.MAX, CUTOFF.NUMERIC.MARGIN)

INTERVAL.INTERSECTION = function (i1, i2) {
    a = max(i1[[1]], i2[[1]])
    b = min(i1[[2]], i2[[2]])
    if(a<=b){
        return(c(a, b))
    } else {
        return(NA)
    }
}

COMMON.PART = function(interval, margin) {
    resultNA = INTERVAL.INTERSECTION(interval, c(0.5-margin, 0.5+margin))
    resultNA = ifelse(is.logical(resultNA) && is.na(resultNA), 0, resultNA[[2]] - resultNA[[1]])

    resultBen = INTERVAL.INTERSECTION(interval, c(0, 0.5-margin))
    resultBen = ifelse(is.logical(resultBen) && is.na(resultBen), 0, resultBen[[2]] - resultBen[[1]])

    resultMal = INTERVAL.INTERSECTION(interval, c(0.5+margin, 1))
    resultMal = ifelse(is.logical(resultMal) && is.na(resultMal), 0, resultMal[[2]] - resultMal[[1]])
    return(c(resultBen, resultMal, resultNA))
}

GEN.CUTOFF.COMMON.PART = function(margin){
    force(margin)
    return(function(interval) {
        results = COMMON.PART(interval, margin)
        if(results[[3]] > max(results[[1]], results[[2]])){
            return(NA)
        }else{
            if(results[[1]] > results[[2]]){
                return(0)
            } else {
                return(1)
            }
        }
    })
}
GEN.CUTOFF.COMMON.PART.STRICT = function(margin){
    force(margin)
    return(function(interval) {
        results = COMMON.PART(interval, margin)
        if(results[[1]] >= results[[2]] + results[[3]]){
            return(0)
        }else{
            if(results[[2]] >= results[[1]] + results[[3]]){
                return(1)
            } else {
                return(NA)
            }
        }
    })
}

#########################
# Weights
#########################
# Selector is responsible for computing weight for each interval.
# Agrument *m* has the same structure as this passed to aggretaion function.
# Returns vector of length *ncol(m)* containing weights.
# Selectors are used in e.g. weighted mean aggregatoin functions
#########################
WEIGHT.1 = function(m){
    # constant weights
    return(rep(1,times=ncol(m)))
}
WEIGHT.WIDTH = function(m){
    # intervals weighted by their width
    return(apply(m,2,function(interval){
        return(1-(interval[[2]]-interval[[1]]))
    }))
}
WEIGHT.WIDTH.MEAN = function(m){
    # intervals weighted by their width
    return(apply(m,2,function(interval){
        return(mean(interval) * (1 - (interval[[2]] - interval[[1]])))
    }))
}
WEIGHT.ENTROPY.MEAN = function(m){
    # intervals weighted by the distance of their center from 0.5
    return(apply(m,2,function(interval){
        return(2*abs(0.5-(interval[[1]]+interval[[2]])/2))
    }))
}
WEIGHT.ENTROPY.ENDPOINT = function(m){
    # intervals weighted by minimum distance of their endpoints form 0.5
    return(apply(m,2,function(interval){
        if(interval[[1]]<0.5 && interval[[2]]>0.5){
            return(0)
        }else{
            if(interval[1]>=0.5){
                return((interval[1]-0.5)*2)
            }else{
                return((0.5-interval[2])*2)
            }
        }
    }))
}
# selectors and weights has the same interface and in this case it can be used interchangeably
WEIGHT.MEMBERSHIP.MIN = SELECTOR.MIN
WEIGHT.MEMBERSHIP.CENTER = SELECTOR.CENTER
WEIGHT.MEMBERSHIP.MAX = SELECTOR.MAX
GEN.WEIGHT.OWA = function(weights, order.func){
    # Implements basic OWA weighting strategy. *weights* argument should be
    # numeric vector. *order.func* takes *m* matrix and return vector of length
    # ncol(m) containing numbers of columns in *m*. The *weights* will be assigned
    # in the order corresponding to this returned from *order.func*.
    force(order.func);force(weights)
    if(sum(weights)==0) {
        stop("Sum of all weights cannot be 0.")
    }
    return(function(m) {
        ord = order.func(m)
        return(weights[ord])
    })
}

GEN.WEIGHT.NORMALISE = function(weight, best=F, worst=F,norm=ID){
    # allows to normalize weights obtained with other strategies
    force(weight);force(best);force(worst);force(norm)
    return(function(m){
        w = weight(m)
        if(best){
            w[w==max(w)]=0
            if(sum(w)==0){
                w[w==0]=1
            }
        }
        if(worst){
            w[w==min(w)]=0
            if(sum(w)==0){
                w[w==0]=1
            }
        }
        return(sapply(w,norm))
    })
}
#########################
# Weighting functions
#########################
# Weighting function is responsible for mapping values from [0,1] interval into
# [0,1] interval. Argument *x* in a single number. Returns single mapped number.
# Selectors are used to change weights.
#########################
ID = function(x){
    return(x)
}
LOGIT = function(x){
    return(1/(1+exp(-10*(x-0.5))))
}

#########################
# Initialisation of convenience objects
#########################
ORDER.MEMBERSHIP.CENTER = GEN.ORDER.WEIGHT(WEIGHT.MEMBERSHIP.CENTER)
ORDER.MEMBERSHIP.MAX = GEN.ORDER.WEIGHT(WEIGHT.MEMBERSHIP.MAX)
ORDER.MEMBERSHIP.MIN = GEN.ORDER.WEIGHT(WEIGHT.MEMBERSHIP.MAX)
ORDER.WIDTH = GEN.ORDER.WEIGHT(WEIGHT.WIDTH)
ORDER.ENTROPY.MEAN = GEN.ORDER.WEIGHT(WEIGHT.ENTROPY.MEAN)
ORDER.ENTROPY.ENDPOINT = GEN.ORDER.WEIGHT(WEIGHT.ENTROPY.ENDPOINT)

OWA.WEIGTHS.VECTOR.INC = c(0,0.25,0.5,0.5,0.75,1)
OWA.WEIGTHS.VECTOR.DEC = c(1,0.75,0.5,0.5,0.25,0)
OWA.WEIGTHS.VECTOR.HILL = c(0.1,0.5,1,1,0.5,0.1)
OWA.WEIGTHS.VECTOR.PIT = c(1,0.5,0.1,0.1,0.5,1)

OWA.WEIGTHS.VECTOR.INC = OWA.WEIGTHS.VECTOR.INC/sum(OWA.WEIGTHS.VECTOR.INC)
OWA.WEIGTHS.VECTOR.DEC = OWA.WEIGTHS.VECTOR.DEC/sum(OWA.WEIGTHS.VECTOR.DEC)
OWA.WEIGTHS.VECTOR.HILL = OWA.WEIGTHS.VECTOR.HILL/sum(OWA.WEIGTHS.VECTOR.HILL)
OWA.WEIGTHS.VECTOR.PIT = OWA.WEIGTHS.VECTOR.PIT/sum(OWA.WEIGTHS.VECTOR.PIT)

WEIGHT.OWA.WIDTH = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.WIDTH)
WEIGHT.OWA.ENTROPY.MEAN = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.ENTROPY.MEAN)
WEIGHT.OWA.ENTROPY.ENDPOINT = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.ENTROPY.ENDPOINT)
WEIGHT.OWA.MEMBERSHIP.CENTER.DEC = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.MEMBERSHIP.CENTER)
WEIGHT.OWA.MEMBERSHIP.CENTER.INC = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.INC, ORDER.MEMBERSHIP.CENTER)
WEIGHT.OWA.MEMBERSHIP.CENTER.HILL = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.HILL, ORDER.MEMBERSHIP.CENTER)
WEIGHT.OWA.MEMBERSHIP.CENTER.PIT = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.PIT, ORDER.MEMBERSHIP.CENTER)
WEIGHT.OWA.MEMBERSHIP.MIN.DEC = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.MEMBERSHIP.MIN)
WEIGHT.OWA.MEMBERSHIP.MAX.DEC = GEN.WEIGHT.OWA(OWA.WEIGTHS.VECTOR.DEC, ORDER.MEMBERSHIP.MAX)


#########################
# Lists of aggregation helpers
#########################
AGGR.SELECTORS = c(SELECTOR.MIN, SELECTOR.MAX, SELECTOR.CENTER)
AGGR.SELECTORS.NAME = c("min", "max", "cen")

AGGR.CUTOFFS.NUMERIC = c(CUTOFF.NUMERIC.CRISP,
                         GEN.CUTOFF.NUMERIC.WITH.MARGIN(0.025),
                         GEN.CUTOFF.NUMERIC.WITH.MARGIN(0.075),
                         GEN.CUTOFF.NUMERIC.WITH.MARGIN(0.15),
                         GEN.CUTOFF.NUMERIC.WITH.MARGIN(0.25))
AGGR.CUTOFFS.NUMERIC.NAME = c('0.0','0.025','0.075','0.15','0.25')

AGGR.CUTOFFS = apply(expand.grid(AGGR.SELECTORS, AGGR.CUTOFFS.NUMERIC), 1, function(row){
    GEN.CUTOFF.FROM.NUMERIC(row[[1]], row[[2]])
})
AGGR.CUTOFFS.NAME = apply(expand.grid(AGGR.SELECTORS.NAME, AGGR.CUTOFFS.NUMERIC.NAME), 1, function(row){
    paste('(',row[[1]], '_', row[[2]],')',sep='')
})
AGGR.CUTOFFS = c(AGGR.CUTOFFS,
                 CUTOFF.CRISP,
                 GEN.CUTOFF.WITH.MARGIN(0.025),
                 GEN.CUTOFF.WITH.MARGIN(0.075),
                 GEN.CUTOFF.WITH.MARGIN(0.15),
                 GEN.CUTOFF.WITH.MARGIN(0.25),
                 GEN.CUTOFF.WITH.MARGIN(-0.025),
                 GEN.CUTOFF.WITH.MARGIN(-0.075),
                 GEN.CUTOFF.WITH.MARGIN(-0.15),
                 GEN.CUTOFF.WITH.MARGIN(-0.25),
                 GEN.CUTOFF.COMMON.PART(0.025),
                 GEN.CUTOFF.COMMON.PART(0.075),
                 GEN.CUTOFF.COMMON.PART(0.15),
                 GEN.CUTOFF.COMMON.PART(0.25),
                 GEN.CUTOFF.COMMON.PART.STRICT(0.025),
                 GEN.CUTOFF.COMMON.PART.STRICT(0.075),
                 GEN.CUTOFF.COMMON.PART.STRICT(0.15),
                 GEN.CUTOFF.COMMON.PART.STRICT(0.25))
AGGR.CUTOFFS.NAME = c(AGGR.CUTOFFS.NAME, '0.0','0.025','0.075','0.15','0.25','m0.025','m0.075','m0.15','m0.25','(c_0.025)','(c_0.075)','(c_0.15)','(c_0.25)','(cs_0.025)','(cs_0.075)','(cs_0.15)','(cs_0.25)')

AGGR.WEIGHTS = c(WEIGHT.1,
                 WEIGHT.WIDTH,
                 WEIGHT.ENTROPY.MEAN,
                 WEIGHT.ENTROPY.ENDPOINT,
                 WEIGHT.MEMBERSHIP.MIN,
                 WEIGHT.MEMBERSHIP.CENTER,
                 WEIGHT.MEMBERSHIP.MAX,
                 WEIGHT.WIDTH.MEAN)
AGGR.WEIGHTS.NAME = c('1','wid','em','ep','min','cen','max','mean')

AGGR.RMEANS = c(0.5,1,2,3)
AGGR.RMEANS.NAME=c('0.5','1','2','3')

AGGR.INTEGRALS = c(SUGENO.INTEGRAL, CHOQUET.INTEGRAL)
AGGR.INTEGRALS.NAME = c("sug", "cho")

AGGR.MEASURES = c(MEASURE.ADDITIVE.AUC, MEASURE.CARDINALITY)
AGGR.MEASURES.NAME = c('auc','card')


AGGR.NORMS  = c(TNORM.MIN, TNORM.PROD, TNORM.LUK, TCONORM.MIN, TCONORM.PROD, TCONORM.LUK)
AGGR.NORMS.NAME = c('t.min','t.prod','t.luk','s.min','s.prod','s.luk')
AGGR.SOFT.LAMBDAS = c(0.25,0.5,0.75)
AGGR.SOFT.LAMBDAS.NAME = c('0.25','0.5','0.75')
AGGR.SOFT.NORM = apply(expand.grid(AGGR.NORMS, AGGR.SOFT.LAMBDAS), 1, function(row){
    GEN.SOFT(row[[1]],row[[2]])
})
AGGR.SOFT.NORM.NAME = apply(expand.grid(AGGR.NORMS.NAME, AGGR.SOFT.LAMBDAS.NAME), 1, function(row){
    paste('(soft_',row[[1]],'_',row[[2]],')',sep='')
})
AGGR.T.OPERATIONS = c(AGGR.NORMS, AGGR.SOFT.NORM)
AGGR.T.OPERATIONS.NAME = c(AGGR.NORMS.NAME, AGGR.SOFT.NORM.NAME)

AGGR.OWA.ORDERS = sapply(AGGR.WEIGHTS,GEN.ORDER.WEIGHT)
AGGR.OWA.ORDERS.NAME = paste('(owa_',AGGR.WEIGHTS.NAME,')',sep='')

AGGR.OWA.WEIGHTS = list(OWA.WEIGTHS.VECTOR.INC, OWA.WEIGTHS.VECTOR.DEC, OWA.WEIGTHS.VECTOR.HILL, OWA.WEIGTHS.VECTOR.PIT)
AGGR.OWA.WEIGHTS.NAME = c("inc",'dec','hill','pit')

AGGR.WEIGHTS.OWA = apply(expand.grid(AGGR.OWA.WEIGHTS, AGGR.OWA.ORDERS), 1, function(row){
    GEN.WEIGHT.OWA(row[[1]],row[[2]])
})
AGGR.WEIGHTS.OWA.NAME = apply(expand.grid(AGGR.OWA.WEIGHTS.NAME, AGGR.OWA.ORDERS.NAME), 1, function(row){
    paste('(',row[[1]],'_',row[[2]],')',sep='')
})