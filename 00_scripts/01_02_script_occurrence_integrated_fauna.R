#' ---
#' title: ms-network
#' date: 2024-08-22
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(furrr)
library(future)
library(tmap)
library(viridis)

# import data -------------------------------------------------------------

## species list
fauna_list <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/00_species_list/fauna_species_list_frugivore.csv") %>% 
    dplyr::pull(species)
fauna_list

## specieslink and gbif ----
occ_specieslink_spocc_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/01_occ_raw_spocc_specieslink_fauna.csv") %>% 
  dplyr::select(frug_sp, longitude, latitude, date, datasource) %>% 
  dplyr::rename(species = frug_sp,
                year = date,
                source = datasource)
occ_specieslink_spocc_fauna

## salve ----
occ_salve_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/02_occ_raw_salve_fauna.csv") %>% 
  dplyr::select(species, longitude, latitude, year) %>% 
    dplyr::mutate(source = "salve")
occ_salve_fauna

## sibbr ----
occ_sibbr_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/03_occ_raw_sibbr_fauna.csv") %>% 
  dplyr::rename(species = name,
                year = date) %>% 
  dplyr::select(species, longitude, latitude, year) %>% 
    dplyr::mutate(source = "sibbr")
occ_sibbr_fauna

## data papers ----
occ_data_papers_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/04_occ_raw_data_paper_fauna.csv")
occ_data_papers_fauna

## portal biodiversidade ----
occ_portalbio_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/05_occ_raw_portalbio_fauna.csv") %>% 
  dplyr::select(species, longitude, latitude, year) %>% 
    dplyr::mutate(source = "portalbio")
occ_portalbio_fauna

## combine ----
occ_fauna <- dplyr::bind_rows(occ_specieslink_spocc_fauna, occ_data_papers_fauna,
                              occ_salve_fauna, occ_sibbr_fauna, occ_portalbio_fauna) %>% 
    dplyr::mutate(species = case_when(species == "Vireo chivi" ~ "Vireo olivaceus", .default = species)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::filter(species %in% fauna_list) %>%  
    dplyr::filter(source != "inat") %>%  
    dplyr::filter(longitude > -180 & longitude < 180) %>% 
    dplyr::filter(latitude > -90 & latitude < 90) %>% 
    dplyr::distinct(species, longitude, latitude, .keep_all = TRUE)
occ_fauna

## export ----
readr::write_csv(occ_fauna, "01_data/01_occurrences/01_raw/01_fauna/00_occ_raw_fauna.csv")

# end ---------------------------------------------------------------------
