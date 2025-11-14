#' ---
#' title: plataforma sn - cleaning data - flora
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

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# import
occ_flora_v2 <- dir("D:/plt_check/00_occ_sp_require_modeling", full.names = TRUE) %>% 
  vroom::vroom() %>% 
  dplyr::rename(species = scrubbed_species_binomial) %>% 
  dplyr::mutate(year = year(date_collected)) %>% 
  tibble::rowid_to_column(var = "id")
occ_flora_v2

# information
nrow(occ_flora_v2)
length(unique(occ_flora_v2$species))
species_list <- unique(occ_flora_v2$species)

# brazil
br <- geobr::read_country() %>% 
  sf::st_transform(4326)
br

tm_shape(br) +
  tm_polygons()

# date filter -------------------------------------------------------------

# temporal

# flora_v2
occ_flora_v2_filter_date <- occ_flora_v2 %>% 
  dplyr::mutate(date_filter = ifelse(year >= 1970 & year <= 2024 | is.na(year), TRUE, FALSE))
occ_flora_v2_filter_date

# precision ---------------------------------------------------------------

# flora_v2
occ_flora_v2_filter_date_precision <- occ_flora_v2_filter_date %>% 
  dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                            latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                          TRUE, FALSE)) %>% 
  dplyr::mutate(precision_filter_date = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_flora_v2_filter_date_precision

# america filter ------------------------------------------------------

# vector
am <- sf::st_read("01_data/01_variables/01_limits/america.shp") %>% 
  dplyr::mutate(FID = 1)
am
plot(am$geometry, col = "gray")

# flora_v2
occ_flora_v2_filter_date_precision_america <- occ_flora_v2_filter_date_precision %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  sf::st_join(am) %>% 
  dplyr::mutate(america_filter = ifelse(is.na(FID), FALSE, TRUE)) %>% 
  dplyr::select(-FID) %>% 
  sf::st_drop_geometry()
occ_flora_v2_filter_date_precision_america

# spatial filter -----------------------------------------------------

# spatial distance filter ----
datasets_thinned <- data.frame()
for(species in species_list){
  
  # information
  print(species)
  
  # filter
  species_data <- occ_flora_v2_filter_date_precision_america[occ_flora_v2_filter_date_precision_america$species == species, ]
  
  # thin
  thinned_data <- spThin::thin(
    loc.data = species_data, 
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
    dplyr::mutate(.thin = 1) %>% 
    dplyr::rename(longitude = Longitude,
                  latitude = Latitude)
  thinned_data
  
  # join
  species_data <- dplyr::left_join(tibble::as_tibble(species_data), thinned_data)
  
  # bind
  datasets_thinned <- rbind(datasets_thinned, species_data)
  
}

occ_flora_v2_filter_date_precision_america_thinned <- datasets_thinned
occ_flora_v2_filter_date_precision_america_thinned

# bias filter -------------------------------------------------------------

# bias
occ_flora_v2_filter_date_precision_america_thinned_bias <- CoordinateCleaner::clean_coordinates(
  x = occ_flora_v2_filter_date_precision_america_thinned, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "outliers", # remove outliers
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
occ_flora_v2_filter_date_precision_america_thinned_bias

# filter ------------------------------------------------------------------

# flora_v2
occ_flora_v2_cleaned <- dplyr::bind_rows(occ_flora_v2_filter_date_precision_america_thinned_bias) %>% 
  dplyr::filter(precision_filter_date == TRUE,
                america_filter == TRUE,
                .thin == TRUE,
                bias_filter == TRUE) %>% 
  dplyr::select(-id) %>% 
  tibble::rowid_to_column(var = "id")
occ_flora_v2_cleaned

plot(occ_flora_v2_cleaned$geometry, pch = 20)

# export ------------------------------------------------------------------

occ_flora_v2_cleaned %>% 
  sf::st_drop_geometry() %>% 
  readr::write_csv("01_data/00_occurrences/00_flora/occ_cleaned_flora_v2.csv")

# end ---------------------------------------------------------------------