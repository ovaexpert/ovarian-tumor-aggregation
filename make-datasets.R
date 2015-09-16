# ---- init ----

rm(list=ls())

source("config.R")
source("methods.R")
source("utils.R")

library(dplyr)
library(sampling)

# ---- db-read ----

db = read.csv(DATABASE.LOCATION, header=TRUE)
db = db %>%
     select(GivenId, MalignancyCharacter, one_of(COLS.ALL)) %>%
     rename(PatientId=GivenId)

# ---- db-preprocess ----

db[with(db, which(MalignancyCharacter == 2)), "MalignancyCharacter"] = 1

db[with(db, which(is.na(PapBloodFlow)    & Pap == 0)),    "PapBloodFlow"]    = 0
db[with(db, which(is.na(APapDimension)   & Pap == 0)),    "APapDimension"]   = 0
db[with(db, which(is.na(SeptumThickness) & Septum == 0)), "SeptumThickness"] = 0
db[with(db, which(grepl("^Sms", PatientId) & is.na(Ri))), "Ri"] = 1
db[with(db, which(grepl("^Sz",  PatientId) & is.na(Ri) & Color == 1)), "Ri"] = 1

# ---- db-divide ----

db.completeCases   = filter(db, complete.cases(db))
db.incompleteCases = filter(db, !complete.cases(db) &
                                complete.cases(db[, c("MalignancyCharacter", COLS.SURE)]))

inTrainingSet.benignSize    = round(nrow(filter(db.completeCases, MalignancyCharacter==0)) *
                                    TRAINING.SIZE/nrow(db.completeCases))
inTrainingSet.malignantSize = round(nrow(filter(db.completeCases, MalignancyCharacter==1)) *
                                    TRAINING.SIZE/nrow(db.completeCases))

db.completeCases = arrange(db.completeCases, MalignancyCharacter) # sort before strata

inTrainingSet = strata(db.completeCases, "MalignancyCharacter",
                       c(inTrainingSet.benignSize, inTrainingSet.malignantSize),
                       "srswor")$ID_unit

inTestSet = apply(select(db.incompleteCases, one_of(COLS.OBSC)), 1,
                  function(row) {sum(is.na(row))/length(row) <= OBSCURE.MAX})

db.training = db.completeCases[inTrainingSet, ]
db.test = bind_rows(db.completeCases[-inTrainingSet, ], # add a few remaining compl. cases
                    db.incompleteCases[inTestSet, ])

# ---- build-training-simulations-data ----

training.data = data.frame()

for (obsc.lvl in OBSCURE.PERCENTAGES)
{
    for (obsc.rep in 1:OBSUCRE.REPEAT)
    {
        printDebug(paste(obsc.lvl, obsc.rep))

        # sample the database to obtain balanced dataset
        db.training.sample = bind_rows(sample_n(filter(db.training, MalignancyCharacter==0),
                                                PROBE.SIZE.NEGATIVE),
                                       sample_n(filter(db.training, MalignancyCharacter==1),
                                                PROBE.SIZE.POSITIVE))

        if (obsc.lvl > 0)
        {
            naMat = matrix(0, nrow=nrow(db.training.sample), ncol=length(COLS.OBSC))

            # put NA values into the matrix to obtain required percentage
            # of missing values
            naMat[sample(1:length(naMat), floor(obsc.lvl*length(naMat)))] = NA

            # set NA value in random places in patient database
            # (NA added to any value results in NA)
            db.training.sample[, COLS.OBSC] = db.training.sample[, COLS.OBSC] + naMat
        }

        results = do.call(cbind,
                          sapply(seq_along(METHODS),
                                 function(m)
                                 {
                                   method = METHODS[[m]]
                                   name   = METHODS.NAME[[m]]
                                   cols   = METHODS.COL[[m]]

                                   t(apply(db.training.sample[cols], 1, method))
                                 },
                                 simplify=FALSE))

        training.data = bind_rows(training.data,
                                  with(db.training.sample,
                                       data.frame(PatientId,
                                                  ObscureLevel=obsc.lvl,
                                                  ObscureRepeat=obsc.rep,
                                                  MalignancyCharacter,
                                                  results)))
    }
}

# ---- build-test-data ----

results = do.call(cbind,
                  sapply(seq_along(METHODS),
                         function(m)
                         {
                            method = METHODS[[m]]
                            name   = METHODS.NAME[[m]]
                            cols   = METHODS.COL[[m]]

                            t(apply(db.test[cols], 1, method))
                         },
                         simplify=FALSE))

obscLevels = apply(select(db.test, one_of(COLS.OBSC)), 1,
                   function(row){round(sum(is.na(row))/length(row), 2)})

test.data = with(db.test, data.frame(PatientId,
                                     ObscureLevel=obscLevels,
                                     ObscureRepeat=1,
                                     MalignancyCharacter,
                                     results))

# ---- save-results ----

write.csv(training.data, TRAINING.LOCATION, row.names=FALSE)
write.csv(test.data,     TEST.LOCATION,     row.names=FALSE)
