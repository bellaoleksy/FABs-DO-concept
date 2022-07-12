if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, stars, units, utils, parallel)

bathy_file <- list.files(path = "/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY")

extract_littoral <- function(bathy_file){

    stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY/",bathy_file,"")) %>%
      sf::st_as_sf(.) %>% 
      dplyr::rename(depth_m = paste0(bathy_file)) %>%
      dplyr::mutate(mean_depth_m = mean(depth_m, na.rm = T)) %>%
      dplyr::filter(depth_m <= 4) %>%
      dplyr::rename(mean_littoral_depth_m = depth_m) %>%
      dplyr::mutate(littoral_area_m2 = st_area(.)) %>% 
      sf::st_drop_geometry(.) %>% 
      units::drop_units(.)%>%
      dplyr::summarise_all(funs(mean), na.rm = T) %>%
      dplyr::mutate(hylak_id = sub("_.*", "", bathy_file)) %>%
      utils::write.table(., file = paste0("./data/global_bathymetry/global_littoral_area.csv"),
                  append = T,
                  row.names = F,
                  col.names = !file.exists("./data/global_bathymetry/global_littoral_area.csv"))
    
    return(unique(bathy_file))
  }
  
  
n.cores <- detectCores()-2

cl <- makeCluster(n.cores)

clusterExport(cl, list("%>%", "funs", "st_area"))

parLapply(cl, bathy_file, extract_littoral)

stopCluster(cl)





