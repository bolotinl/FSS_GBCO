setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering")
if(file.exists(".here") == FALSE){here::set_here()}
library(here)

source(here::here("R", "executables", "gapfill_normalize.R"))
# Warning message:
#   In regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :
#   collapsing to unique 'x' values
# The above message went away completely once I made minor changes to syntax in gapfill_normalize.R

source(here::here("R", "executables", "stacking_timeseries.R"))

source(here::here("R", "executables", "calc_dtw_distance.R"))

source(here::here("R", "executables", "cluster_validity_indices.R"))

source(here::here("R", "executables", "hierarchical_clustering.R"))
