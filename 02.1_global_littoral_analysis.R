# analyze the global bathymetry for lakes over 1500 meters. 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, vroom)

high_lake_global <- vroom::vroom("./data/global_bathymetry/global_littoral_area.csv") %>%
  summarize()
