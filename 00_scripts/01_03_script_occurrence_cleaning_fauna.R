#' ---
#' title: plataforma sn - cleaning data
#' author: mauricio vancine
#' date: 2024-04-10
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(sf)
library(terra)
library(tmap)
library(CoordinateCleaner)
library(spThin)

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# import
occ_fauna <- readr::read_csv("01_data/01_occurrences/01_raw/01_fauna/00_occ_raw_fauna.csv") %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna

# information
nrow(occ_fauna)
length(unique(occ_fauna$species))

# date filter -------------------------------------------------------------

# temporal

# fauna
occ_fauna_filter_date <- occ_fauna %>% 
    dplyr::mutate(date_filter = ifelse(year >= 1985 & year <= 2024 | is.na(year), TRUE, FALSE))
occ_fauna_filter_date

# precision ---------------------------------------------------------------

# fauna
occ_fauna_filter_date_precision <- occ_fauna_filter_date %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE)) %>% 
    dplyr::mutate(precision_filter_date = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_fauna_filter_date_precision

# america filter ------------------------------------------------------

# vector
am <- sf::st_read("01_data/02_variables/01_raw/01_limits/america.shp") %>% 
    dplyr::mutate(fid = 1) %>% 
    dplyr::select(fid)
am
plot(am$geometry, col = "gray")

# fauna
occ_fauna_filter_date_precision_america <- occ_fauna_filter_date_precision %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    sf::st_join(am) %>% 
    dplyr::mutate(america_filter = ifelse(is.na(fid), FALSE, TRUE)) %>% 
    dplyr::select(-fid) %>% 
    sf::st_drop_geometry()
occ_fauna_filter_date_precision_america

# iucn filter ------------------------------------------------------------

# import
iucn_files <- dir("01_data/01_occurrences/01_raw/01_fauna/iucn", pattern = ".shp", full.names = TRUE)
iucn_files

# filter
occ_fauna_filter_date_precision_america_iucn <- NULL
for(i in unique(occ_fauna$species)){
    
    # iucn species filter
    if(i != "Troglodytes musculus"){
        
    iucn_i <- sf::st_read(grep(i, iucn_files, value = TRUE)) %>%
        dplyr::mutate(fid = 1) %>%
        dplyr::select(fid)

    # species filter
    occ_fauna_filter_date_precision_america_i <- occ_fauna_filter_date_precision_america %>%
        dplyr::filter(species == i)
    
    # spatial iucn filter
    occ_fauna_filter_date_precision_america_iucn_i <- occ_fauna_filter_date_precision_america_i %>%
        dplyr::mutate(lon = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_join(iucn_i, quiet = TRUE) %>%
        dplyr::mutate(iucn_filter = ifelse(is.na(fid), FALSE, TRUE)) %>%
        dplyr::select(-fid) %>%
        sf::st_drop_geometry()

    }else{
        
        # species filter
        occ_fauna_filter_date_precision_america_i <- occ_fauna_filter_date_precision_america %>%
            dplyr::filter(species == i)
        
        # spatial iucn filter
        occ_fauna_filter_date_precision_america_iucn_i <- occ_fauna_filter_date_precision_america_i %>%
            dplyr::mutate(iucn_filter = TRUE)
        
    }
    
    # bind
    occ_fauna_filter_date_precision_america_iucn <- dplyr::bind_rows(
        occ_fauna_filter_date_precision_america_iucn,
        occ_fauna_filter_date_precision_america_iucn_i)
    
}
occ_fauna_filter_date_precision_america_iucn

# spatial filter -----------------------------------------------------

# spatial distance filter ----
occ_fauna_filter_date_precision_america_iucn_spatial <- data.frame()
for(i in sort(unique(occ_fauna$species))){
  
  # information
  print(i)
  
  # filter
  occ_fauna_filter_date_precision_america_iucn_i <- occ_fauna_filter_date_precision_america_iucn %>% 
        dplyr::filter(species == i)
  
  # thin
  thinned_data <- spThin::thin(loc.data = occ_fauna_filter_date_precision_america_iucn_i, 
                               lat.col = "latitude", 
                               long.col = "longitude", 
                               spec.col = "species", 
                               thin.par = 3, 
                               reps = 1, 
                               locs.thinned.list.return = TRUE, 
                               write.files = FALSE, 
                               write.log.file = FALSE) %>% 
    .[[1]] %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(spatial_filter = TRUE) %>% 
    dplyr::rename(longitude = Longitude,
                  latitude = Latitude)
  thinned_data
  
  # join
  occ_fauna_filter_date_precision_america_iucn_spatial_i <- occ_fauna_filter_date_precision_america_iucn_i %>% 
      dplyr::left_join(thinned_data) %>% 
      tidyr::replace_na(replace = list(spatial_filter = FALSE))
  
  # bind
  occ_fauna_filter_date_precision_america_iucn_spatial <- dplyr::bind_rows(
      occ_fauna_filter_date_precision_america_iucn_spatial_i,
      occ_fauna_filter_date_precision_america_iucn_spatial)
      
}
occ_fauna_filter_date_precision_america_iucn_spatial

# bias filter -------------------------------------------------------------

# bias
occ_fauna_filter_date_precision_america_iucn_spatial_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_fauna_filter_date_precision_america_iucn_spatial, 
    species = "species",
    lon = "longitude", 
    lat = "latitude",
    tests = c("capitals", # radius around capitals
              "centroids", # radius around country and province centroids
              "duplicates", # records from one species with identical coordinates
              "equal", # equal coordinates
              "gbif", # radius around GBIF headquarters
              "institutions", # radius around biodiversity institutions
              # "outliers", # remove outliers
              "seas", # in the sea
              "urban", # within urban area
              "validity", # outside reference coordinate system
              "zeros" # plain zeros and lat = lon
    ),
    capitals_rad = 2000,
    centroids_rad = 2000,
    centroids_detail = "both",
    inst_rad = 100,
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(.cen = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .cen),
                  .summary = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .summary)) %>%     dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>% 
    dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_fauna_filter_date_precision_america_iucn_spatial_bias

# filter ------------------------------------------------------------------

# fauna
occ_fauna_cleaned_regional <- dplyr::bind_rows(occ_fauna_cleaned_regional_endemic, occ_fauna_cleaned_regional_not_endemic) %>% 
    dplyr::filter(precision_filter_date == TRUE,
                  america_filter == TRUE,
                  iucn_filter = TRUE, 
                  spatial_filter == TRUE,
                  bias_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna_cleaned_regional

# export ------------------------------------------------------------------

occ_fauna_cleaned_regional %>% 
    sf::st_drop_geometry() %>% 
    dplyr::relocate(group, .after = source) %>% 
    readr::write_csv("02_data/01_occurrences/03_cleaned/occ_cleaned_fauna_regional.csv")

# end ---------------------------------------------------------------------