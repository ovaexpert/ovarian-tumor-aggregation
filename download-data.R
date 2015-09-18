source("config.R")

library(R.utils)
library(RCurl)

# download generated datasets

ds = data.frame(f=c(TRAINING.FILE,     TEST.FILE,     EVALUATION.OUTPUT.FILE),
                p=c(TRAINING.LOCATION, TEST.LOCATION, EVALUATION.OUTPUT.LOCATION))

if (!file.exists(DATASETS.DIR))
    dir.create(DATASETS.DIR)

for (i in 1:nrow(ds))
{
    f = as.character(ds[i, ]$f)
    p = as.character(ds[i, ]$p)

    if (!file.exists(p))
    {
        temp = tempfile()
        content = getBinaryURL(paste(GEN.DATA.URL, paste0(f, ".bz2"), sep="/"),
                               httpheader = c("User-Agent"="R"))
        writeBin(content, temp)
        bunzip2(temp, p)
        unlink(temp)
        print(paste("File downloaded:", p))
    } else {
        print(paste("File already exists:", p))
    }
}
