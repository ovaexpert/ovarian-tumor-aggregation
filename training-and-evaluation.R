# ---- init ----

rm(list=ls())

source("config.R")
source("stats.R")
source("methods.R")
source("aggregators.R")
source("aggregators-optimize.R")
source("mcn-test.R")
source("utils.R")

library(parallel)
library(reshape2)
library(dplyr)

# ---- read-datasets ----

printDebug("read datasets")

colClass = c("factor", "numeric", "integer", "integer",
             rep("numeric", times=2*length(METHODS)))

ds.training = read.csv(TRAINING.LOCATION, colClasses=colClass)
ds.test     = read.csv(TEST.LOCATION,     colClasses=colClass)

# ---- parallel-init ----

printDebug("paralell init")

if (THREADS > 1)
{
    CL = makeCluster(THREADS, outfile="")

    clusterExport(CL, c("SEED", "OBSUCRE.REPEAT"))
    clusterCall(CL, function(){ set.seed(SEED) })

    clusterExport(cl=CL, list('AGGREGATORS', 'AGGREGATORS.NAME', 'METHODS',
                              'METHODS.NAME', 'ds.training', 'ds.test',
                              'diagnosisToOutcome', 'CUTOFF.CRISP', 'W.AUC',
                              'COMMON.PART', 'INTERVAL.INTERSECTION'))

    usedLapply = function(...){ parLapply(CL, ...) }
} else {
    usedLapply = lapply
}

# ---- training-statistics-models-original ----

printDebug("training statistics models original")

training.stats.models.orig = melt(
        calculate.stats(
            aggregate.outcomes(
                orig.models.outcomes(ds.training)
            )
        ),
        id.vars=c("ObscureLevel"),
        variable.name="Method",
        value.name="Value") %>%
    rename(Measure=L1) %>%
    mutate(Class="Model", Subclass="Original", Subsubclass=NA)

training.stats.models.orig$Method = paste("orig.", training.stats.models.orig$Method)

# ---- training-statistics-models-uncertaintified ----

printDebug("training statistics models uncertaintified")

training.stats.models.uncer = melt(
        calculate.stats(
            aggregate.outcomes(
                unc.models.outcomes(ds.training)
            )
        ),
        id.vars=c("ObscureLevel"),
        variable.name="Method",
        value.name="Value") %>%
    rename(Measure=L1) %>%
    mutate(Class="Model", Subclass="Uncertaintified", Subsubclass=NA)

training.stats.models.uncer$Method = paste("unc.", training.stats.models.uncer$Method)

# ---- training-statistics-aggregators ----

printDebug("training statistics aggregators")

outcomes.aggrs = usedLapply(1:length(AGGREGATORS), function(i){

    aggr = AGGREGATORS[[i]]
    # selection of appropriate columns
    diags = apply(ds.training[, 5:(5+length(METHODS)*2-1)], 1, function(row) {
        # matrix in format required by aggregation method is created and passed into aggr
        return(aggr(matrix(row, nrow=2)))
    })
    converted = apply(cbind(diags, ds.training$MalignancyCharacter),
                      1, diagnosisToOutcome)
    return(converted)
})
outcomes.aggrs        = data.frame(ds.training[, 1:3], outcomes.aggrs)
names(outcomes.aggrs) = c(names(ds.training)[1:3], AGGREGATORS.NAME)

training.stats.aggrs = melt(calculate.stats(
                            aggregate.outcomes(outcomes.aggrs)
                        ),
                        id.vars = "ObscureLevel" ,
                        variable.name = "Method",
                        value.name = "Value") %>%
                   rename(Measure=L1)

training.stats.aggrs = suppressWarnings( # suppress different factor levels warning
                            left_join(training.stats.aggrs,
                                      AGGREGATORS.BINDED.DESCRIPTION,
                                      by="Method")
                       )

# ---- training-statistics-bind ----

printDebug("training statistics bind")

training.stats.all = bind_rows(training.stats.models.orig,
                               training.stats.models.uncer,
                               training.stats.aggrs)

# ---- training-statistics-performance-calculation ----

printDebug("training statistics performance calculation")

training.stats.all.perf = aggregate(training.stats.all$Value,
                                    list(Method=training.stats.all$Method,
                                         Measure=training.stats.all$Measure),
                                    mean) %>%
                          rename(Value=x)

training.stats.all.perf = left_join(training.stats.all.perf,
                                    distinct(select(training.stats.all,
                                              Method, Class, Subclass, Subsubclass)),
                                    by="Method")

# ---- select-optimized-aggregators ----

printDebug("select optimized aggregators")

optimizedAggregatorsNames = getOptimizedAggregators(training.stats.all.perf, PERFORMANCE.MEASURE)

training.stats.all.perf = subset(training.stats.all.perf,
                                 Method %in% c(optimizedAggregatorsNames,
                                             unique(as.character(training.stats.models.orig$Method)),
                                             unique(as.character(training.stats.models.uncer$Method))))

# ---- test-combine-obscuration-levels ----

printDebug("test combine obscuration levels")

ds.test$ObscureLevel = 0

# ---- test-statistics-models-original ----

printDebug("test statistics models original")

outcomes.orig.models = orig.models.outcomes(ds.test)

test.stats.orig.models = melt(
                            calculate.stats(
                                aggregate.outcomes(
                                    outcomes.orig.models
                                )
                            ),
                            id.vars="ObscureLevel",
                            variable.name="Method",
                         value.name="Value") %>%
                         rename(Measure=L1) %>%
                         mutate(Class="Model", Subclass="Original", Subsubclass=NA) %>%
                         select(-ObscureLevel)

test.stats.orig.models$Method = paste("orig.", test.stats.orig.models$Method)
colnames(outcomes.orig.models)[4:(4+length(METHODS)-1)] =
    paste("orig.", colnames(outcomes.orig.models))[4:(4+length(METHODS)-1)]

# ---- test-statistics-models-uncertaintified ----

printDebug("test statistics models uncertaintified")

outcomes.models.unc = unc.models.outcomes(ds.test)

test.stats.uncer.models = melt(
                            calculate.stats(
                                aggregate.outcomes(
                                    outcomes.models.unc
                                )
                            ),
                            id.vars="ObscureLevel",
                            variable.name="Method",
                            value.name="Value") %>%
                          rename(Measure=L1) %>%
                          mutate(Class="Model", Subclass="Uncertaintified", Subsubclass=NA) %>%
                          select(-ObscureLevel)

test.stats.uncer.models$Method = paste("unc.", test.stats.uncer.models$Method)
colnames(outcomes.models.unc)[4:(4+length(METHODS)-1)] =
    paste("unc.", colnames(outcomes.models.unc))[4:(4+length(METHODS)-1)]

# ---- test-statistics-aggregators ----

printDebug("test statistics aggregators")

aggregators.from.training = unique(subset(training.stats.all.perf, Class=="Aggregation")$Method)

if (THREADS > 1)
    clusterExport(CL, c("aggregators.from.training"))

outcomes.aggrs = usedLapply(1:length(aggregators.from.training), function(j){
    i = which(aggregators.from.training[j] == AGGREGATORS.NAME)
    aggr = AGGREGATORS[[i]]
    # selection of appropriate columns
    diags = apply(ds.test[, 5:(5+length(METHODS)*2-1)], 1, function(row) {
        # matrix in format required by aggregation method is created and passed into aggr
        return(aggr(matrix(row, nrow=2)))
    })
    converted = apply(cbind(diags, ds.test$MalignancyCharacter),
                      1, diagnosisToOutcome)
    return(converted)
})
outcomes.aggrs        = data.frame(ds.test[, 1:3], outcomes.aggrs)
names(outcomes.aggrs) = c(names(ds.test)[1:3], aggregators.from.training)

test.stats.aggrs = melt(calculate.stats(
                                aggregate.outcomes(outcomes.aggrs)
                            ),
                            id.vars = "ObscureLevel" ,
                            variable.name = "Method",
                            value.name = "Value") %>%
                       rename(Measure=L1) %>%
                       select(-ObscureLevel)

test.stats.aggrs = suppressWarnings( # suppress different factor levels warning
                        left_join(test.stats.aggrs,
                                  AGGREGATORS.BINDED.DESCRIPTION,
                                  by="Method")
                   )

# ---- test-statistics-bind ----

printDebug("test statistics bind")

test.stats.all = bind_rows(test.stats.orig.models,
                           test.stats.uncer.models,
                           test.stats.aggrs)

# ---- test-statistics-performance-bind-with-training ----

printDebug("test statistics performance bind with training")

binded.stats.all.perf = left_join(select(training.stats.all.perf, Method, Measure, Value),
                                  test.stats.all,
                                  by=c("Method", "Measure")) %>%
                        rename(Value.training=Value.x, Value.test=Value.y)

# ---- convert-statistics-performance-to-wide-format ----

printDebug("convert statistics performance to wide format")

training.stats.all.perf.wide = dcast(training.stats.all.perf,
                                     Method + Class + Subclass + Subsubclass ~ Measure,
                                     value.var="Value")

test.stats.all.wide          = dcast(test.stats.all,
                                     Method + Class + Subclass + Subsubclass ~ Measure,
                                     value.var="Value")

# ---- aggregators-selection-and-statistical-tests ----

printDebug("aggregators selection and statistical tests")

selected.aggrs = subset(test.stats.all.wide,
                        Class=="Aggregation" &
                            Decisiveness>=0.95 &
                            Decisiveness<1.0 &
                            Sensitivity>Specificity &
                            Sensitivity>=0.90 &
                            Specificity>0.8)

perf.selected.aggrs = subset(binded.stats.all.perf,
                             Measure==PERFORMANCE.MEASURE &
                                 Method %in% selected.aggrs$Method)


perf.all = rbind(perf.selected.aggrs,
                 subset(binded.stats.all.perf,
                        Measure==PERFORMANCE.MEASURE & Class=="Model" & Subclass=="Uncertaintified"))

outcomes.all = bind_cols(select(outcomes.models.unc, -(PatientId:ObscureRepeat)),
                         select(outcomes.aggrs,      -(PatientId:ObscureRepeat)))

pvals = sapply(subset(perf.all, Class=="Aggregation")$Method, function(a1) {
    sapply(perf.all$Method, function(a2) {
        mcn.test(outcomes.all[a1], outcomes.all[a2])
    })
})

pvals[upper.tri(pvals, diag=TRUE)] = NA

pvals = matrix(p.adjust(pvals, method = "BH",
                        n=sum(!is.na(pvals)|is.nan(pvals))),
               nrow=nrow(pvals),
               dimnames=list(rownames(pvals), colnames(pvals)))

# ---- parallel-shutdown ----

printDebug("parallel shutdown")

if (THREADS > 1)
    stopCluster(CL)

# ---- save-evaluation ----

printDebug("save evaluation")

save.image(EVALUATION.OUTPUT.LOCATION)
