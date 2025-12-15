library(devtools)
library(tidyverse, warn.conflicts = F)
devtools::load_all()


d <- trips_read(2023)
