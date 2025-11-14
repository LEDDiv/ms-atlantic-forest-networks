#' ---
#' title: ocorrencias - salve
#' date: 2024-01-30
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(future)
library(furrr)
library(tmap)

# options
options(timeout = 3e5)

# import data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

# species
fauna_list <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/00_species_list/fauna_species_list_frugivore.csv") %>% 
    dplyr::pull(species)
fauna_list

# integrated --------------------------------------------------------------

# list files
files_salve <- dir(path = "01_data/01_occurrences/01_raw/01_fauna/02_salve", 
                   pattern = ".csv", full.names = TRUE)
files_salve

# import
occ_salve <- NULL

for(i in files_salve){
    
    species_name <- sub(".csv", "", sub("_", " ", basename(i)))
    
    occ_salve_i <- readr::read_csv(i, col_types = cols()) %>% 
        dplyr::mutate(species = ifelse(Nome_cientifico == species_name, Nome_cientifico, species_name),
                      longitude = as.numeric(Longitude),
                      latitude = as.numeric(Latitude),
                      precision = Precisao_da_coordenada,
                      year = as.numeric(Ano),
                      source = "salve") %>% 
        dplyr::select(species, longitude, latitude, precision, year, source) %>% 
        tidyr::drop_na(longitude, latitude)
    occ_salve <- bind_rows(occ_salve, occ_salve_i)
}
occ_salve

# verify
occ_salve_filter <- occ_salve %>% 
    dplyr::filter(species %in% fauna_list)
occ_salve_filter

# vector
occ_salve_filter_v <- occ_salve_filter %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_salve_filter_v

# map
map <- tm_shape(li[li$iso_a2 == "BR", ]) +
    tm_polygons() +
    tm_shape(occ_salve_filter_v) +
    tm_dots()
map

# export
readr::write_csv(occ_salve_filter, "01_data/01_occurrences/01_raw/01_fauna/02_occ_raw_salve_fauna.csv")

# end ---------------------------------------------------------------------