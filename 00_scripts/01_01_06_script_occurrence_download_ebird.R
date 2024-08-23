#' ---
#' title: download occurrences - ebird
#' author: mauricio vancine
#' date: 2024-08-22
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)

# import data -------------------------------------------------------------

# species list
fauna_list <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/00_species_list/fauna_species_list_frugivore.csv") %>% 
    dplyr::pull(species)
fauna_list

# ebird
ebird_data <- arrow::open_delim_dataset("/media/mude/Seagate Expansion Drive/data/ebird_2024_06/0043987-240626123714530.csv", 
                                        parse_options = csv_parse_options(delimiter = "\t", newlines_in_values = TRUE),
                                        read_options = list(block_size = 100000000L))
ebird_data

# filter
ebird_data_filter <- ebird_data %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    dplyr::collect()
ebird_data_filter

# export
readr::write_csv(ebird_data_filter, "01_data/01_occurrences/01_raw/01_fauna/05_occ_raw_ebird.csv")

# end ---------------------------------------------------------------------
