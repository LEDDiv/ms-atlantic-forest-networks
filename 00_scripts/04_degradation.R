#' ---
#' title: degradation
#' author: mauricio vancine
#' date: 2024-11-07
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(rgrass)
library(tmap)

# options
sf::sf_use_s2(FALSE)
tmap_options(check.and.fix = TRUE)

# import data -------------------------------------------------------------

## grass ----
rgrass::initGRASS(gisBase = "C:/Program Files/GRASS GIS 8.3",
                  gisDbase = "01_data/01_variables/04_degradation/grassdb",
                  location = "newProject",
                  mapset = "PERMANENT",
                  override = TRUE)

## percentage ----
habitat_files <- dir(path = "01_data/01_variables/04_degradation/raw/", pattern = "habitat", full.names = TRUE)
habitat_files

for(i in 1:length(habitat_files)){
  print(basename(habitat_files[i]))
  rgrass::execGRASS(cmd = "r.in.gdal", flags = "overwrite", input = habitat_files[i], output = paste0("habitat0", i))
}

rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), raster = paste0("habitat0", 1:7, collapse = ","))

rgrass::execGRASS(cmd = "r.patch", flags = "overwrite", input = paste0("habitat0", 1:7, collapse = ","), output = "habitat")
rgrass::execGRASS(cmd = "r.out.gdal", flags = "overwrite", input = "habitat", output = "01_data/01_variables/04_degradation/habitat.tif", createopt = "COMPRESS=DEFLATE")

## distance ----
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "habitat_null = if(habitat == 1, 1, null())")
rgrass::execGRASS(cmd = "r.grow.distance", flags = "overwrite", input = "habitat_null", distance = "distance", metric = "geodesic")
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "distance = int(distance)")
rgrass::execGRASS(cmd = "r.out.gdal", flags = "overwrite", input = "distance", output = "01_data/01_variables/04_degradation/distance.tif", createopt = "COMPRESS=DEFLATE,BIGTIFF=YES")

## edge ----
edge_files <- dir(path = "01_data/01_variables/04_degradation/raw/", pattern = "edge", full.names = TRUE)
edge_files

for(i in 1:length(edge_files)){
  print(basename(edge_files[i]))
  rgrass::execGRASS(cmd = "r.in.gdal", flags = "overwrite", input = edge_files[i], output = paste0("edge0", i))
}

rgrass::execGRASS(cmd = "r.patch", flags = "overwrite", input = paste0("edge0", 1:7, collapse = ","), output = "edge")
rgrass::execGRASS(cmd = "r.out.gdal", flags = "overwrite", input = "edge", output = "01_data/01_variables/04_degradation/edge.tif", createopt = "COMPRESS=DEFLATE")

## fire ----
fire_files <- dir(path = "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/raw", pattern = "fire", full.names = TRUE)
fire_files

for(i in 1:length(fire_files)){
  print(basename(fire_files[i]))
  rgrass::execGRASS(cmd = "r.in.gdal", flag = "overwrite", input = fire_files[i], output = paste0("fire0", i))
}

rgrass::execGRASS(cmd = "r.patch", flags = "overwrite", input = paste0("fire0", 1:18, collapse = ","), output = "fire")
# rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "fire_pct = round(fire/100)")
rgrass::execGRASS(cmd = "r.out.gdal", flags = "overwrite", input = "fire", output = "01_data/01_variables/04_degradation/fire.tif", createopt = "COMPRESS=DEFLATE")

## secondary ----
secondary_files <- dir(path = "01_data/01_variables/04_degradation/raw/", pattern = "secondary", full.names = TRUE)
secondary_files

for(i in 1:length(secondary_files)){
  print(basename(secondary_files[i]))
  rgrass::execGRASS(cmd = "r.in.gdal", flags = "overwrite", input = secondary_files[i], output = paste0("secondary0", i))
}

rgrass::execGRASS(cmd = "r.patch", flags = "overwrite", input = paste0("secondary0", 1:18, collapse = ","), output = "secondary")
# rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "secondary_pct = round(secondary/100)")
rgrass::execGRASS(cmd = "r.out.gdal", flags = "overwrite", input = "secondary", output = "01_data/01_variables/04_degradation/secondary.tif", createopt = "COMPRESS=DEFLATE")

# analysis ----------------------------------------------------------------

## import data ----

## limit ----
br <- rnaturalearth::ne_countries(scale = 10, country = "Brazil") %>% 
  terra::vect()
br

af <- terra::vect("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/limites/MA_ecorr.shp") %>% 
  terra::crop(br)
plot(af, col = "gray") ##Atlantic Forest Ecoregions 2017

plot(af, col = "gray")
plot(br, border = "red", add = TRUE)

## grid ----
grid_r <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/veg.tif") %>% 
  terra::mask(af)
grid_r
plot(grid_r)

grid <- terra::as.polygons(grid_r, aggregate = FALSE)
grid$id <- 1:nrow(grid)
grid <- grid[, -1]
grid
plot(grid)

## import rasters ----
habitat <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/habitat.tif") %>% 
  terra::mask(af)
habitat
plot(habitat)

# distance <- terra::rast("01_data/01_variables/04_degradation/distance.tif") %>% 
#   terra::crop(brazil, mask = TRUE)
# distance
# plot(distance)
# writeRaster(distance, "01_data/01_variables/04_degradation/distance_brazil.tif")

distance <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/distance.tif") %>% 
  terra::mask(af)
distance
plot(distance)

# edge <- terra::rast("01_data/01_variables/04_degradation/edge.tif") %>% 
#   terra::crop(brazil, mask = TRUE)
# edge
# plot(edge)
# writeRaster(edge, "01_data/01_variables/04_degradation/edge_brazil.tif")
# writeRaster(edge, "01_data/01_variables/04_degradation/edge_brazil_binary.tif")

edge <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/edge.tif") %>% 
  terra::mask(af)
edge
plot(edge)

# fire <- terra::rast("01_data/01_variables/04_degradation/fire.tif") %>% 
#   terra::crop(brazil, mask = TRUE)
# fire
# plot(fire)
# writeRaster(fire, "01_data/01_variables/04_degradation/fire_brazil.tif")

fire <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/fire.tif") %>% 
  terra::mask(af)
fire
plot(fire)

# secondary <- terra::rast("01_data/01_variables/04_degradation/secondary.tif") %>% 
#   terra::crop(brazil, mask = TRUE)
# secondary
# plot(secondary)
# writeRaster(secondary, "01_data/01_variables/04_degradation/secondary_brazil.tif")

secondary <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation/secondary.tif") %>% 
  terra::mask(af)
secondary
plot(secondary)

## analysis ----
degradation <- tibble::tibble()
for(i in grid$id){

  print(i)
  grid_i <- grid[grid$id == i, ]
  
  habitat_i <- terra::crop(habitat, grid_i, mask = TRUE)
  habitat_i_na <- terra::ifel(habitat_i == 1, 1, NA)

  pct <- habitat_i %>% 
    terra::freq() %>% 
    dplyr::mutate(per = count/sum(count))
  
  if(nrow(pct) == 1){
    if(pct$value == 0){
      pct <- 0
    }else{
      pct <- pct$per  
    }
  }else{
    pct <- pct %>% 
      filter(value == 1) %>% 
      pull(per)
  }
    
  dis <- terra::mask(terra::crop(distance, grid_i, mask = TRUE), habitat_i_na, inverse = TRUE) %>% 
    terra::global(., mean, na.rm = TRUE) %>% 
    unlist() %>% 
    as.vector()
  
  edg <- terra::ifel((terra::crop(edge, grid_i, mask = TRUE) * habitat_i_na) > 0, 1, 0) %>% 
    terra::freq() %>% 
    dplyr::mutate(per = count/sum(count))
  
  if(nrow(edg) == 1){
    if(edg$value == 0 | is.na(edg$value)){
      edg <- 0
    }else{
      edg <- edg$per  
    }
  }else{
    edg <- edg %>% 
      filter(value == 1) %>% 
      pull(per)
  }
  
  fir <- (terra::crop(fire, grid_i, mask = TRUE) * habitat_i_na) %>% 
    terra::global(., mean, na.rm = TRUE) %>% 
    unlist() %>% 
    as.vector()
  fir <- ifelse(is.na(fir), 0, fir)
  
  secondary_i <- terra::crop(secondary, grid_i, mask = TRUE)
  secondary_i <- secondary_i * habitat_i_na
  secondary_i <- terra::ifel(secondary_i > 0, secondary_i, NA)
  
  # sec_age <- secondary_i %>% 
  #   terra::global(., mean, na.rm = TRUE) %>% 
  #   unlist() %>% 
  #   as.vector()
  
  sec_pct <- (terra::ifel(is.na(secondary_i), 0, 1) * habitat_i_na) %>% 
    terra::freq() %>% 
    dplyr::mutate(per = count/sum(count))
  
  if(nrow(sec_pct) == 1){
    if(sec_pct$value == 0 | is.na(sec_pct$value)){
      sec_pct <- 0
    }else{
      sec_pct <- sec_pct$per  
    }
  }else{
    sec_pct <- sec_pct %>% 
      filter(value == 1) %>% 
      pull(per)
  }
  
  da_i <- tibble::tibble(
    id = grid_i$id,
    pct = pct,
    dis = dis,
    edg = edg,
    fir = fir,
    # sec_age = sec_age,
    sec_pct = sec_pct)
  
  degradation <- dplyr::bind_rows(degradation, da_i)
  
}
degradation

nrow(degradation)
length(grid$id)

# degradation index
degradation_index <- degradation %>%
  dplyr::mutate(dis = log10(ifelse(is.na(dis), 0, dis)+1)) %>% 
  dplyr::mutate(fir = ifelse(is.na(fir), 0, fir)) %>% 
  dplyr::mutate(pct = ifelse(is.na(pct), 0, pct)) %>% 
  dplyr::mutate(edg = ifelse(is.na(edg), 0, edg)) %>% 
  dplyr::mutate(dis_pad = as.numeric(vegan::decostand(dis, "range")), .after = dis) %>% 
  dplyr::mutate(fir_pad = as.numeric(vegan::decostand(fir, "range")), .after = fir) %>% 
  dplyr::mutate(loss = 1 - pct, .after = pct) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(degradation_index_pct = round(sum(loss, dis_pad, edg, fir_pad, sec_pct)/5, 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(degradation_index_pct_pad = as.numeric(vegan::decostand(degradation_index_pct, "range")), 
                .after = degradation_index_pct)
degradation_index

hist(degradation_index$loss)
hist(degradation_index$dis)
hist(degradation_index$dis_pad)
hist(degradation_index$edg)
hist(degradation_index$fir_pad)
hist(degradation_index$sec_pct)
hist(degradation_index$degradation_index_pct)
hist(degradation_index$degradation_index_pct_pad)

## raster
grid_deg_index <- grid %>%
  sf::st_as_sf() %>% 
  dplyr::left_join(degradation_index) %>% 
  terra::vect()
grid_deg_index

grid_habitat_loss_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "loss")
grid_distance_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "dis_pad")
grid_edge90m_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "edg")
grid_fire_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "fir_pad")
grid_secondary_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "sec_pct")
grid_degradation_index_raster <- terra::rasterize(x = grid_deg_index, y = grid_r, field = "degradation_index_pct_pad")

plot(grid_habitat_loss_raster, col = viridis::turbo(100))
plot(grid_distance_raster, col = viridis::turbo(100))
plot(grid_edge90m_raster, col = viridis::turbo(100))
plot(grid_fire_raster, col = viridis::turbo(100))
plot(grid_secondary_raster, col = viridis::turbo(100))

plot(grid_degradation_index_raster, col = viridis::turbo(100))
plot(grid_degradation_index_raster < 0.5)
plot(grid_degradation_index_raster < 0.25)

# export
terra::writeRaster(grid_habitat_loss_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/habitat_loss.tif", overwrite = TRUE)
terra::writeRaster(grid_distance_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/distance_raster.tif", overwrite = TRUE)
terra::writeRaster(grid_edge90m_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/edge90m_raster.tif", overwrite = TRUE)
terra::writeRaster(grid_fire_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/fire_raster.tif", overwrite = TRUE)
terra::writeRaster(grid_secondary_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/secondary_raster.tif", overwrite = TRUE)
terra::writeRaster(grid_degradation_index_raster, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/01_data/01_variables/04_degradation02_results/degradation/degradation_index_raster.tif", overwrite = TRUE)

# end ---------------------------------------------------------------------

grid_degradation_index_raster_af <- terra::rast("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/02_results/degradation/degradation_index_raster.tif") %>% 
  terra::mask(af)
grid_degradation_index_raster_af

terra::writeRaster(grid_degradation_index_raster_af, "D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/02_results/degradation/degradation_index_raster_af.tif", overwrite = TRUE)
