#' ---
#' title: ms-network - iucn
#' date: 2024-08-22
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

# species list
fauna_list <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/00_species_list/fauna_species_list_frugivore.csv") %>% 
    dplyr::pull(species)
fauna_list

# import iucn data
iucn_all <- terra::vect("01_data/01_occurrences/01_raw/01_fauna/iucn/00_raw/All_frug_animals.shp")
iucn_all

iucn_list <- dir(path = "01_data/01_occurrences/01_raw/01_fauna/iucn/00_raw/polig_falt", 
                 pattern = ".shp", full.names = TRUE, recursive = TRUE)
iucn_list

# export
for(i in fauna_list[!fauna_list %in% c("Basileuterus culicivorus", "Troglodytes musculus")]){
    
    print(i)
    
    if(i %in% iucn_all$binomial){
        
        iucn_all_i <- terra::aggregate(iucn_all[iucn_all$binomial == i])

    } else{
        
        iucn_all_i <- terra::aggregate(terra::vect(grep(sub(" ", "_", i), iucn_list, value = TRUE)))

    }
    
    terra::writeVector(iucn_all_i, paste0("01_data/01_occurrences/01_raw/01_fauna/iucn/", i, ".shp"), overwrite = TRUE)
    
}

# end -------------------------------------------------------------------