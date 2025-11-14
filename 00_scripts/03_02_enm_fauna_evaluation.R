#' ---
#' title: sdm - evaluation
#' author: mauricio vancine
#' date: 2024-04-08
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(dismo)
library(ecospat)

# import ------------------------------------------------------------------

# species
sp_list <- dir(path = "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/02_results/01_fauna/", full.names = TRUE)[-1]
sp_list

birds <- read.table("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/00_occurrences/01_fauna/Bird_list.txt")
birds

mammals <- read.table("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/00_occurrences/01_fauna/Mammal_list.txt")
mammals

sp_list_birds <- sp_list[grepl(paste0(gsub("_", " ", birds$x), collapse = "|"), sp_list)]
sp_list_birds

sp_list_mammals <- sp_list[grepl(paste0(gsub("_", " ", mammals$x), collapse = "|"), sp_list)]
sp_list_mammals

# evaluation
data_ev_birds <- NULL
for(i in 1:length(sp_list_birds)){
  
  print(i)
  
  sp_path <- sp_list_birds[i]
  sp_name <- basename(sp_list_birds[i])
  
  # occ
  occ <- readr::read_csv(paste0(sp_path, "/01_occs_", sp_name, ".csv"), col_types = cols()) %>% 
    dplyr::select(longitude, latitude)
  
  occ_sf <- sf::st_as_sf(occ, coords = c("longitude", "latitude"), crs = 4326)
  occ_sf_buffer <- sf::st_as_sf(terra::buffer(terra::vect(occ_sf), 100e3))
  
  # bg
  bg <- readr::read_csv(paste0(sp_path, "/01_bg_", sp_name, ".csv"), col_types = cols()) %>% 
    dplyr::select(longitude, latitude) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  within_id <- st_within(bg, occ_sf_buffer, sparse = FALSE)[,1]
  bg_buffer <- bg[!within_id,]
  bg_buffer_sampled <- bg_buffer[sample(1:nrow(bg_buffer), nrow(occ_sf)*1, replace = FALSE), ]
  
  bg_buffer_sampled_coord <- as.data.frame(sf::st_coordinates(bg_buffer_sampled))
  
  # sdm
  sdm <- terra::rast(paste0(sp_path, "/03_05_pred_", sp_name, ".tif"))
  sdm

  # evaluation
  ev <- dismo::evaluate(p = terra::extract(sdm, occ, ID = FALSE)[, 1], 
                        a = terra::extract(sdm, bg_buffer_sampled_coord, ID = FALSE)[, 1])
  ev
  
  thr <- threshold(ev)
  thr
  
  thr_id <- NULL
  for(j in names(threshold(ev))) thr_id <- c(thr_id, which(ev@t == dismo::threshold(ev, j)))
  tss <- ev@TPR[thr_id] + ev@TNR[thr_id] - 1
  
  thr_tss <- rbind(thr, tss)
  thr_tss <- cbind(data.frame(metrics = c("thresholds", "tss")), thr_tss)
  thr_tss
  
  auc <- ev@auc
  auc
  
  boyce <- ecospat::ecospat.boyce(fit = as.vector(na.omit(terra::values(sdm))), 
                                  obs = na.omit(terra::extract(sdm, occ)[, 2]),
                                  PEplot = FALSE)
  boyce
  
  data_ev_birds_i <- tibble::tibble(
    tax_group = "birds",
    species = sp_name,
    auc_roc = auc,
    tss_max_sens_spec = thr_tss[2, ]$spec_sens,
    boyce = boyce$cor)
  
  data_ev_birds <- rbind(data_ev_birds, data_ev_birds_i)
  
}
data_ev_birds


# mammlas
data_ev_mammals <- NULL
for(i in 1:length(sp_list_mammals)){
  
  print(i)
  
  sp_path <- sp_list_mammals[i]
  sp_name <- basename(sp_list_mammals[i])
  
  # occ
  occ <- readr::read_csv(paste0(sp_path, "/01_occs_", sp_name, ".csv"), col_types = cols()) %>% 
    dplyr::select(longitude, latitude)
  
  occ_sf <- sf::st_as_sf(occ, coords = c("longitude", "latitude"), crs = 4326)
  occ_sf_buffer <- sf::st_as_sf(terra::buffer(terra::vect(occ_sf), 100e3))
  
  # bg
  bg <- readr::read_csv(paste0(sp_path, "/01_bg_", sp_name, ".csv"), col_types = cols()) %>% 
    dplyr::select(longitude, latitude) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  within_id <- st_within(bg, occ_sf_buffer, sparse = FALSE)[,1]
  bg_buffer <- bg[!within_id,]
  bg_buffer_sampled <- bg_buffer[sample(1:nrow(bg_buffer), nrow(occ_sf)*1, replace = FALSE), ]
  
  bg_buffer_sampled_coord <- as.data.frame(sf::st_coordinates(bg_buffer_sampled))
  
  # sdm
  sdm <- terra::rast(paste0(sp_path, "/03_05_pred_", sp_name, ".tif"))
  sdm
  
  # evaluation
  ev <- dismo::evaluate(p = terra::extract(sdm, occ, ID = FALSE)[, 1], 
                        a = terra::extract(sdm, bg_buffer_sampled_coord, ID = FALSE)[, 1])
  ev
  
  thr <- threshold(ev)
  thr
  
  thr_id <- NULL
  for(j in names(threshold(ev))) thr_id <- c(thr_id, which(ev@t == dismo::threshold(ev, j)))
  tss <- ev@TPR[thr_id] + ev@TNR[thr_id] - 1
  
  thr_tss <- rbind(thr, tss)
  thr_tss <- cbind(data.frame(metrics = c("thresholds", "tss")), thr_tss)
  thr_tss
  
  auc <- ev@auc
  auc
  
  boyce <- ecospat::ecospat.boyce(fit = as.vector(na.omit(terra::values(sdm))), 
                                  obs = na.omit(terra::extract(sdm, occ)[, 2]),
                                  PEplot = FALSE)
  boyce
  
  data_ev_mammals_i <- tibble::tibble(
    tax_group = "mammals",
    species = sp_name,
    auc_roc = auc,
    tss_max_sens_spec = thr_tss[2, ]$spec_sens,
    boyce = boyce$cor)
  data_ev_mammals_i
  
  data_ev_mammals <- rbind(data_ev_mammals, data_ev_mammals_i)
  
}
data_ev_mammals

# summarise
data_ev_birds_sum <- data_ev_birds %>% 
  dplyr::group_by(tax_group) %>% 
  dplyr::summarise(
    auc_roc_median = median(auc_roc),
    auc_roc_min = min(auc_roc),
    auc_roc_max = max(auc_roc),
    tss_median = median(tss),
    tss_min = min(tss),
    tss_max = max(tss),
    boyce_median = median(boyce, na.rm = TRUE),
    boyce_min = min(boyce, na.rm = TRUE),
    boyce_max = max(boyce, na.rm = TRUE)) %>% 
  tidyr::pivot_longer(-tax_group, names_to = "metric", values_to = "values")
data_ev_birds_sum

data_ev_mammals_sum <- data_ev_mammals %>% 
  dplyr::group_by(tax_group) %>% 
  dplyr::summarise(
    auc_roc_median = median(auc_roc),
    auc_roc_min = min(auc_roc),
    auc_roc_max = max(auc_roc),
    tss_median = median(tss),
    tss_min = min(tss),
    tss_max = max(tss),
    boyce_median = median(boyce, na.rm = TRUE),
    boyce_min = min(boyce, na.rm = TRUE),
    boyce_max = max(boyce, na.rm = TRUE)) %>% 
  tidyr::pivot_longer(-tax_group, names_to = "metric", values_to = "values")
data_ev_mammals_sum

# export
readr::write_csv(data_ev_birds, "02_results/03_evaluation/data_ev_birds.csv")
readr::write_csv(data_ev_birds_sum, "02_results/03_evaluation/data_ev_birds_sum.csv")

readr::write_csv(data_ev_mammals, "02_results/03_evaluation/data_ev_mammals.csv")
readr::write_csv(data_ev_mammals_sum, "02_results/03_evaluation/data_ev_mammals_sum.csv")

# end --------------------------------------------------------------------
