#================================================
#Computing Topological Richness and Evenness 
#------------------------------------------------

T_div <- function(df = NULL){
  
  require(raster)
  res <- 200
  TR <- c()
  TE <- c()
 
  #data frame with pca values 
  r.rast <- data.frame(df,1)
  colnames(r.rast) <- c('x', 'y', 'z')
  
  #defining extent
  e <- extent(r.rast[,1:2])
  
  #creating raster
  r <- raster(e, ncol=res, nrow=res)
  x <- rasterize(r.rast[, 1:2], r, r.rast[,3], fun=sum)
  
  x.even <- rasterize(r.rast[, 1:2], r, r.rast[,3], fun=mean)
  
  #topological richness
  TR[1] <- sum(values(x.even), na.rm = TRUE)/prod(dim(x.even))
  
  #even point density - total even 
  prob.even <- na.omit(values(x.even)/sum(values(x.even), na.rm = T))
  
  #real point density
  prob.real <- na.omit(values(x)/sum(values(x), na.rm = T))
  
  #topological evenness
  KL.dist <- sum(prob.real*log(prob.real/prob.even))
  
  TE[1] <- 1-KL.dist
  
  return(list(TR = TR,TE = TE))
  
}

