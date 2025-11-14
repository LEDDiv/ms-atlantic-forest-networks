#Morini et al. 
#Creating networks for each AF cell
#======================================
#------- loading packages-------------
library(tidyverse)
library(vroom)

#edge list - metanetwork
#edge_list <- read.csv("./data/final_edge_list_july25.csv", sep=",")
edge_list <- read.csv("./data/final_edge_list_Nov25.csv", sep=",")

edge_list <- edge_list %>%
  mutate(frug_sp = gsub("_", " ", frug_sp))
edge_list <- edge_list %>%
  mutate(plant_sp = gsub("_", " ", plant_sp))

#defining dirs
maps_plants_dir <- "./maps/flora_maps"
maps_animals_dir <- "./maps/fauna_maps"
output_dir <- "./maps/maps_multiplied"

for (i in 1:nrow(edge_list)) {
  plant_sp <- edge_list$plant_sp[i]
  frug_sp <- edge_list$frug_sp[i]
  
  plant_dir <- file.path(maps_plants_dir, plant_sp, paste0("03_05_pred_", plant_sp, "_af_msdm.tif"))
  animal_dir <- file.path(maps_animals_dir, frug_sp, paste0("03_05_pred_", frug_sp, "_af_msdm.tif"))
  
  plant_sp_dir <- gsub(" ", "_", plant_sp)
  frug_sp_dir <- gsub(" ", "_", frug_sp)
  output_name <- paste0(plant_sp_dir, "_", frug_sp_dir, ".tif")
  
  output_filepath <- file.path(output_dir, output_name)
  
  if(!file.exists(output_filepath)){
    print(i)
    if (file.exists(plant_dir) & file.exists(animal_dir)) {
      a.map <- terra::rast(animal_dir)
      
      p.map <- terra::rast(plant_dir)
      
      p.map <- terra::resample(p.map, a.map) #checking
      
      ap.map <- a.map * p.map #co-suitability map
      
      ap.map <- terra::aggregate(ap.map, fact = 5) #reducing resolution (5x5)
      
      ap.map <- round(ap.map,3)
      
      terra::writeRaster(ap.map, output_filepath, overwrite = FALSE)
      
    } else {
      warning(paste("there is no map for", plant_sp, "or", frug_sp))
    }
  }
}

######## creating lists of interactions for each pixel ######

base.map <- terra::rast("./maps/basemap.tiff")
#spatial cells within AF limits
filled_cells <- which(!is.na(terra::values(base.map)))

#af <- terra::vect("./maps/AF_limit/MA_ecorr_br.shp")

#subsetting maps only for valid interactions
ap.map.names <- list.files("./maps/maps_multiplied",pattern = ".tif")
map.filename <- paste0(edge_list$plant_sp,'_',edge_list$frug_sp,".tif")
map.filename <- gsub(" ","_", map.filename)

ap.map.names <- ap.map.names[match(map.filename,ap.map.names)]
ap.map.names.path <- paste0("./maps/maps_multiplied/",na.omit(ap.map.names))

#stack with all combined maps
map.stack <- terra::rast(ap.map.names.path)
map.stack <- map.stack*base.map

#Creating different batches for compiling the dataframe
batch <- seq(1,dim(map.stack)[3], by = 1000)
batch <- c(batch,dim(map.stack)[3])

ltemp <- list()
for (k in 1:(length(batch)-1)){
  map.stack.temp <- map.stack[[batch[k]:(batch[k+1])]]
  map.stack.temp <- map.stack.temp*base.map
  temp <- as.data.frame(map.stack.temp)
  ltemp[[k]] <- temp
}

#Combining maps in a data.frame
df.maps <- do.call(cbind,ltemp)
colnames(df.maps) <- NULL

#Making sure that all interactions and maps are in the same order
edge_list_temp <- edge_list
edge_list_temp <- edge_list_temp[-which(is.na(ap.map.names)),]

# ------------------Generating local networks------------------

#local edge lists is the product of interaction probabilities and local co-suitability

for (z in 1:length(filled_cells)){
  print(z)

  cell <- filled_cells[z]

  vals <- as.numeric(df.maps[z,])

  local_edge <- edge_list 
  
  local_edge$int_freq <- round(local_edge$int_freq*vals,3) #rounding to reduce file size
  
  #removing interactions that are too unlikely
  local_edge <- local_edge[local_edge$int_freq > 0,]
  
  #exporting
  #write.csv(local_edge, paste0('.data/E_lists_final/edgelist_',cell,'csv'), row.names = FALSE)
  
}

