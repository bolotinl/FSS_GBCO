setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering")
if(file.exists(".here") == FALSE){here::set_here()}
source(here::here("R", "executables", "gapfill_normalize.R"))
# Warning message:
#   In regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :
#   collapsing to unique 'x' values
source(here::here("R", "executables", "stacking_timeseries.R"))
# Error in do.call(rbind, lapply(ts_list, FUN = transpose_func, var = var)) : 
#   variable names are limited to 10000 bytes 
source(here::here("R", "executables", "stacking_timeseries.R"))
# Error in do.call(rbind, lapply(ts_list, FUN = transpose_func, var = var)) : 
#   variable names are limited to 10000 bytes
install.packages("dtwclust")
library(dtwclust)
source(here::here("R", "executables", "cluster_validity_indices.R"))
install.packages("dendextend")
library()
source(here::here("R", "executables", "hierarchical_clustering.R"))
# Error in loadNamespace(name) : there is no package called ‘dendextend’ 