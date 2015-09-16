SEED                = 1337
THREADS             = 32
DEBUG               = FALSE

TRAINING.SIZE       = 200
PROBE.SIZE.NEGATIVE = 75
PROBE.SIZE.POSITIVE = 75
PROBE.SIZE          = PROBE.SIZE.NEGATIVE + PROBE.SIZE.POSITIVE

OBSCURE.MAX         = 0.5
OBSCURE.PERCENTAGES = c(seq(0.0, OBSCURE.MAX, 0.05))
OBSUCRE.REPEAT      = 1000

PERFORMANCE.MEASURE      = "Cost matrix"
PERFORMANCE.MEASURE.DESC = FALSE

DATASETS.DIR           = 'datasets'
DATABASE.FILE          = 'db-2015-04-30.csv'
TRAINING.FILE          = 'training.csv'
TEST.FILE              = 'test.csv'
EVALUATION.OUTPUT.FILE = 'evaluation-output.RData'

DATABASE.LOCATION          = paste(DATASETS.DIR, DATABASE.FILE, sep='/')
TRAINING.LOCATION          = paste(DATASETS.DIR, TRAINING.FILE, sep='/')
TEST.LOCATION              = paste(DATASETS.DIR, TEST.FILE,     sep='/')
EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

GEN.DATA.URL = "http://min.wmi.amu.edu.pl/data/ovarian-tumor-aggregation"

set.seed(SEED)

if (file.exists("config.R.user"))
    source("config.R.user")