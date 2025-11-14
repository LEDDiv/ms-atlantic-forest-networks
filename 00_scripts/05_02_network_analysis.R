#Morini et al. 
#Network analysis
#===========================================

#------------ Computing local network structural metrics-----------
#load("D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/R.Datas/full_edgelists2_18_05.RData")
setwd("D:/Rubia e Mauricio/AF_networks")
#----------------Function to generate a matrix--------------

# packages
library(bipartite)
library(igraph)
library(vegan)
library(reshape2)
library(ggridges)

matrix_from_edgelist <- function(elist){
  r.names <- unique(as.data.frame(elist)[,1])
  c.names <- unique(as.data.frame(elist)[,2])
  m <- length(r.names)
  n <- length(c.names)
  A <- matrix(0,m,n)
  rownames(A) <- r.names
  colnames(A) <- c.names
  
  for(i in 1:nrow(elist)){
    anim <- as.character(elist[i,1])
    plant <- as.character(elist[i,2])
    A[anim,plant] <- as.numeric(elist[i,3])
  }
  return(A)
}


#------------------------------------------------------------------------------
#Initial settings for analysis
bin <- FALSE #TRUE for binary networks
filter <- FALSE #TRUE when using the filter for recent records


#-------------------------------------------------------------------------------------------
#Loading map with AF limits

base.map <- terra::rast("./maps/basemap.tiff")
terra::plot(base.map)

#spatial cells within AF limits
filled_cells <- which(!is.na(terra::values(base.map)))


#----------------Computing metrics for local edge_lists-----------------

Sp.A <-  rep(NA,length(filled_cells))
Sp.P <-  rep(NA,length(filled_cells))
L.vec <- rep(NA, length(filled_cells))
Connect <- rep(NA, length(filled_cells))
k.A <- list()
k.P <- list()
kmean.A <- rep(NA, length(filled_cells))
kmean.P <-  rep(NA, length(filled_cells))
H2 <-  rep(NA,length(filled_cells))
Nest <-  rep(NA, length(filled_cells))
Nest_w <-  rep(NA,length(filled_cells))
MOD <- rep(NA, length(filled_cells))
FC.A <-  rep(NA, length(filled_cells))
FC.P <-  rep(NA, length(filled_cells))

M <- list()

if(filter == TRUE){
  cell_sp_birds <- vroom::vroom("./data/04_cell_municipality_species_presence/birds/00_grid_buffer50km_mun_birds_cell.csv")
  
  cell_sp_mammals <- vroom::vroom("./data/04_cell_municipality_species_presence/mammals/00_grid_buffer50km_mun_mammals_cell.csv")
}


for (z in 1:length(filled_cells)) { 
  
  path <- file.path("./data/E_lists_final", paste0("edgelist_", filled_cells[z], ".csv"))
  
  cell_id <- filled_cells[z]
  
  local_edge <- read.csv(path)
  local_edge <- local_edge[,-1]
  
  #removing interactions that are too unlikely
  #local_edge <- local_edge[local_edge$int_freq > 0.1,]
  
  if(bin == TRUE){
    local_edge$int_freq[local_edge$int_freq >= 0.1] <- 1
    local_edge <- local_edge[-which(local_edge$int_freq < 0.1),]
  }
  
  
  if(filter == TRUE){
    
    sp.frug.names <- (local_edge$frug_sp)
    
    cell_sp_birds_sub <- cell_sp_birds[which(cell_sp_birds$id == cell_id),]
    cell_sp_mammals_sub <- cell_sp_mammals[which(cell_sp_mammals$id == cell_id),]
    
    aux <- which(!is.na(match(sp.frug.names,c(cell_sp_birds_sub$species,cell_sp_mammals_sub$species)))) 
    local_edge <- local_edge[aux,]
    
    #removing interactions that are very unlikely
    local_edge <- local_edge[local_edge$int_freq > 0.1,]
    
  }
  
  
  if(nrow(local_edge)>0){
    
    Sp.A[z] <- length(unique(local_edge$frug_sp)) #number of Animals
    Sp.P[z] <- length(unique(local_edge$plant_sp)) #number of Plants
    
    L.vec[z] <- sum(local_edge$int_freq, na.rm = T)
    
    Connect[z] <- L.vec[z]/(Sp.A[z]*Sp.P[z])
    
    aux.a <- tapply(local_edge$int_freq, local_edge$frug_sp, sum,na.rm = T) #animal degree
    k.A[[z]] <- aux.a
    kmean.A[z] <- sum(aux.a)/sum(aux.a>0)
    
    aux.p <- tapply(local_edge$int_freq, local_edge$plant_sp, sum,na.rm = T) #plant degree
    k.P[[z]] <- aux.p
    kmean.P[z] <- sum(aux.p)/sum(aux.p>0)
    
    #modularity
    G <- graph_from_edgelist(as.matrix(local_edge[1:2]), directed = FALSE)
    E(G)$weight <- as.numeric(unlist(local_edge[,3]))
    M[[z]] <- cluster_louvain(G,weights = E(G)$weight)
    MOD[z] <- round(as.numeric(M[[z]]$modularity[2]),3)
    
    #nestedness
    A <- matrix_from_edgelist(local_edge[,1:3])
    if(nrow(A)>2 && ncol(A)>2){
      NODF_value <- nested(A, method = 'NODF2')
      Nest[z] <- NODF_value
      
      
      NODF_valuew <- nested(A, method = 'weighted NODF')
      Nest_w[z] <- NODF_valuew
      #Specialization H2
      H2[z] <- networklevel(A, index="H2", H2_integer=FALSE)
      
      #functional complementarity
      FC.A[z] <- grouplevel(A, index="functional complementarity")[1]
      FC.P[z] <- grouplevel(A, index="functional complementarity")[2]
      
    }
  }
  
  print(z)
}


#------ Maps for each metric----------------
# blank raster
blank.map <- base.map
blank.map[] <- NA

metrics <- c('Sp.A', 'Sp.P', 'L.vec', 'Connect', 'kmean.A', 'kmean.P', 'H2', 'Nest','Nest_w', 'MOD', 'FC.A', 'FC.P')


for (metric in metrics){
  metric_values <- get(metric)
  metric_map <- blank.map
  metric_map[filled_cells] <- metric_values[1:length(filled_cells)]
  terra::plot(metric_map, main = metric)
}

#----------------#metric distributions--------------------

#plotting distributions
df.std <- decostand(df[2:12], method = "standardize")
df.std <- melt(df.std)
plot_metrics <- ggplot(df.std, aes(y = variable, x = value))+
  geom_density_ridges(fill = "#3CA55C") +
  theme_ridges() +
  xlim(-5,5)


#================================================================================
#Data frame with the values for all network metrics for each cell
df <- data.frame(cell = filled_cells, Sp.A, Sp.P, L.vec, Connect, kmean.A, kmean.P, H2, Nest, MOD, FC.A, FC.P)


#removing NAs
error <- which(is.na(rowSums(df[,2:12]))) 
df <- df[-error,]



save(Sp.A, Sp.P, L.vec, Connect, kmean.A, kmean.P, H2, Nest,Nest_w, MOD, FC.A, FC.P, df,
     file = './results/netmetrics.RData')


