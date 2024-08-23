#' ---
#' title: ocorrencias - sibbr
#' author: mauricio vancine
#' date: 2024-01-30
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)

# options
options(timeout = 3e5)

# import data -------------------------------------------------------------

# species list
fauna_list <- readr::read_csv2("01_data/01_occurrences/01_raw/01_fauna/00_species_list/fauna_species_list_frugivore.csv") %>% 
    dplyr::pull(species)
fauna_list

# download ----------------------------------------------------------------

# download
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/archive.do?r=ibge_anfibios_01&v=1.6", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_anfibios.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_repteis_01", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_repteis.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_mamiferos_01", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_mamiferos.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_aves_01", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_aves01.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_aves_02", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_aves02.zip",
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_aves_03", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_aves03.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_aves_04", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_aves04.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_aves_05", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_aves05.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_01", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_plantas_vasc01.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_02", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_plantas_vasc02.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_03", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_plantas_vasc03.zip", 
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_04", 
#               destfile = "01_data/00_occurrences/raw/03_sibbr/sibbr_plantas_vasc04.zip", 
#               mode = "wb")

# unzip
# directories <- sub("sibbr_", "", sub(".zip", "", dir(path = "01_data/00_occurrences/raw/03_sibbr/", pattern = ".zip$")))
# directories
# 
# for(i in directories){
#     
#     print(i)
#     unzip(zipfile = paste0("01_data/00_occurrences/raw/03_sibbr/sibbr_", i, ".zip"), 
#           exdir = paste0("01_data/00_occurrences/raw/03_sibbr/", i))
# }


# filter ------------------------------------------------------------------

# mamiferos
occ_data_sibbr_mamiferos <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/mamiferos/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_mamiferos

# aves
occ_data_sibbr_aves01 <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/aves01/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_aves01

occ_data_sibbr_aves02 <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/aves02/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_aves02

occ_data_sibbr_aves03 <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/aves03/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_aves03

occ_data_sibbr_aves04 <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/aves04/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_aves04

occ_data_sibbr_aves05 <- readr::read_delim("01_data/01_occurrences/01_raw/01_fauna/03_sibbr/aves05/occurrence.txt", delim = "\t") %>% 
    dplyr::mutate(species_searched = scientificName,
                  name = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  prov = "sibbr",
                  date = as.numeric(year),
                  key = as.character(catalogNumber)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species_searched, name, longitude, latitude, prov, date, key) %>% 
    dplyr::filter(species_searched %in% fauna_list)
occ_data_sibbr_aves05

# fauna
fauna_sibbr <- dplyr::bind_rows(occ_data_sibbr_aves01, occ_data_sibbr_aves02, 
                                occ_data_sibbr_aves03, occ_data_sibbr_aves04, 
                                occ_data_sibbr_aves05, occ_data_sibbr_mamiferos) %>% 
    dplyr::filter(longitude > -180, 
                  longitude < 180,
                  latitude > -90,
                  latitude < 90)
fauna_sibbr
# plot(fauna_sibbr$longitude, fauna_sibbr$latitude, pch = 20)

# export
readr::write_csv(fauna_sibbr, "01_data/01_occurrences/01_raw/01_fauna/03_occ_raw_sibbr_fauna.csv")

# end ---------------------------------------------------------------------