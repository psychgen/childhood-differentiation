# 04.3_mi_sib_ipw.R

#install.packages("mice", repos = "file://tsd-evs/shared/R/cran")

## RUN ON HPC

#.libPaths( c( "/cluster/p/p471/cluster/lib" , .libPaths() ) )

library(mice)
library(tidyverse)

# load prepped data

load(file = './dat_for_MI.RData')

MI <- parlmice(dat_for_mi,n.core=60, n.imp.core=1, maxit = 15,  cl.type= "FORK")

save(MI, file="./MI_60imp_15its.RData")
