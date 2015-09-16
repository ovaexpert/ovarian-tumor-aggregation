source("config.R")

library(R.utils)

# download generated datasets

ds = data.frame(f=c(SIMULATIONS.FILE,     REALDATA.FILE,     EVALUATION.OUTPUT.FILE),
                p=c(SIMULATIONS.LOCATION, REALDATA.LOCATION, EVALUATION.OUTPUT.LOCATION))

if (!file.exists(DATASETS.DIR))
    dir.create(DATASETS.DIR)

for (i in 1:nrow(ds))
{
    f = as.character(ds[i, ]$f)
    p = as.character(ds[i, ]$p)

    if (!file.exists(p))
    {
        temp = tempfile()
        download.file(paste(GEN.DATA.URL, paste0(f, ".bz2"), sep="/"), temp)
        bunzip2(temp, p)
        unlink(temp)
    }
}
