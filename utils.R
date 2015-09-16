diagnosisToOutcome = function(p){
    # p is two element vector, p[[1]] contains diagnosis from model, p[[2]] containts
    # reference (real) diagnosis
    if(is.na(p[[1]]) && p[[2]]==1){
        return("N1")
    }else if(is.na(p[[1]]) && p[[2]]==0){
        return("N0")
    }else if(p[[1]]==1 && p[[2]]==1){
        return("TP")
    }else if(p[[1]]==1 && p[[2]]==0){
        return("FP")
    }else if(p[[1]]==0 && p[[2]]==1){
        return("FN")
    }else if(p[[1]]==0 && p[[2]]==0){
        return("TN")
    }
}

orig.models.outcomes = function(data){
    toReturn = usedLapply(1:length(METHODS),function(i){
        name = METHODS.NAME[[i]]
        column = (5+(i-1)*2)
        diags = apply(data[,column:(column+1)],1,
                      function(row){if(row[1]!=row[2]) NA else CUTOFF.CRISP(c(row[1],row[2]))})
        converted = apply(cbind(diags,data$MalignancyCharacter),1,diagnosisToOutcome)
        return(converted)
    })
    toReturn = data.frame(data[,1:3],toReturn)
    names(toReturn) = c(names(data)[1:3],METHODS.NAME)
    return(data.frame(toReturn))
}

unc.models.outcomes = function(data){
    toReturn = usedLapply(1:length(METHODS),function(i){
        name = METHODS.NAME[[i]]
        column = (5+(i-1)*2)
        diags = apply(data[,column:(column+1)],1,CUTOFF.CRISP)
        converted = apply(cbind(diags,data$MalignancyCharacter),1,diagnosisToOutcome)
        return(converted)
    })
    toReturn = data.frame(data[,1:3],toReturn)
    names(toReturn) = c(names(data)[1:3],METHODS.NAME)
    return(data.frame(toReturn))
}

count.outcomes = function(data){
    result = c(TP=sum(data=='TP',na.rm=TRUE),
               TN=sum(data=='TN',na.rm=TRUE),
               FN=sum(data=='FN',na.rm=TRUE),
               FP=sum(data=='FP',na.rm=TRUE),
               N0=sum(data=='N0',na.rm=TRUE),
               N1=sum(data=='N1',na.rm=TRUE)
    )
    return(result)
}
aggregate.outcomes = function(outcomes){
    aggregate(outcomes[,4:ncol(outcomes)],
                         by=list(ObscureLevel=outcomes$ObscureLevel,
                                 ObscureRepeat=outcomes$ObscureRepeat),
                         count.outcomes,
                         simplify = FALSE)
}

calculate.stats = function(aggregated){
    result = list()
    for(i in 1:length(STATS)){
        stat = STATS[[i]]; force(stat)
        name = STATS.NAME[[i]]
        stats = aggregate(aggregated[ , 3:ncol(aggregated)],
                          by = list(ObscureLevel=aggregated$ObscureLevel),
                          function(data){
                              outcomes = matrix(c(data, recursive=TRUE),
                                                ncol=6, byrow=TRUE,
                                                dimnames = list(NULL, c("TP","TN","FN","FP","N0","N1")))
                              return(stat(outcomes))
                          },
                          simplify = TRUE)
        result[[name]] = stats
    }
    return(result)
}

# auxiliary functions

printDebug = function(msg)
{
    if (DEBUG)
        cat(paste(format(Sys.time(), "[%X]"), msg, "\n"))
}
