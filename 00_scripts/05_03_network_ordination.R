#Morini et al. 
#Ordination analysis and kernel properties
#======================================
#------- loading packages-------------

require(bipartite)
require(igraph)
require(ggplot2)
require(vegan)
require(ggfortify)
require(dichromat)
require(moments)
require(rayshader)
require(ggridges)
library(svglite)
library(hrbrthemes)
require(TPD)
require(patchwork)
library(terra)

#-------------Standardization of color limits -------------------------------

cor_high <- function(total_elements, n_elements, n = 30, low="#3CA55C", high="#333333"){
  palette <- colorRampPalette(c(low, high))(n)
  indices <- cut(seq_len(total_elements), breaks = n, labels = FALSE, include.lowest = TRUE)
  indices_n <- indices[seq_len(n_elements)]
  indice_max <- max(indices_n)
  color_results <- palette[indice_max]
  return(color_results)
}

# --------loading data-------------
#load('./results/netmetrics.RData')

#load('D:/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/R.Datas/netmetrics_July25.RData')


#-------------------------- Loading maps Degradation------------------------------------
map.degradation <- terra::rast('./maps/degradation_index_raster_af.tif')

plot(map.degradation)

hist(values(map.degradation))

filled_degrad <- which(!is.na(terra::values(map.degradation)))

map.degradation_values <- values(map.degradation)[filled_degrad]

#descriptive statistics
summary(map.degradation_values)

#---------------- Protected areas ----------------

prot.map <- terra::rast('./maps/pa_af_touches.tif')

temp_raster <- base.map
temp_raster[!is.na(temp_raster)] <- 1

prot.map <- temp_raster*prot.map

filled.prot <- which(values(prot.map) == 1)
filled.unprot <- which(values(prot.map) == 0)

dens_unprot <- density(values(map.degradation)[filled.unprot], na.rm = TRUE)
dens_unprot$y <- dens_unprot$y * length(na.omit(values(map.degradation)[filled.unprot]))

dens_prot <- density(values(map.degradation)[filled.prot], na.rm = TRUE)
dens_prot$y <- dens_prot$y * length(na.omit(values(map.degradation)[filled.prot]))

plot(dens_unprot,main = "", xlab = "Degradation score", col = "#ecad7d", lwd = 2, ylim = c(0, max(dens_unprot$y, dens_prot$y)))
polygon(dens_unprot, col = adjustcolor("#ecad7d", alpha.f = 0.7), border = NA)

lines(dens_prot, col = "#6bbacb", lwd = 2)
polygon(dens_prot, col = adjustcolor("#6bbacb", alpha.f = 0.85), border = NA)

#--------------Ordination  -----------------------

#Color definition
map.colors <- terra::rast('.maps/biv_rast2.tif')

map.colors <- terra::resample(map.colors,map.degradation)
df.colors <- as.data.frame(map.colors, xy = TRUE, cells = FALSE, na.rm = FALSE) 
names(df.colors) <- c('x','y','R','G','B','alpha')
df.colors <- df.colors[df$cell,]
df.colors$hex <- rgb(df.colors$R,df.colors$G,df.colors$B, maxColorValue = 255)

#PCA
pca_res <- prcomp(df[,2:12], scale. = TRUE)
pca_plot <- autoplot(pca_res, loadings = TRUE, loadings.colour = 'black',
         loadings.label.colour = 'black',
         loadins.width = 6,
         loadings.label.vjust = -1,
         loadings.label = TRUE, loadings.label.size = 7, 
         colour = df.colors$hex)+
  ylim(-0.03, 0.02)+
  theme_light() + 
  theme(axis.text = element_blank(),
      axis.ticks =  element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
     axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20))
  #theme(axis.text.x = element_text(size = 16),
   #     axis.text.y = element_text(size = 16),
    #    axis.title.y = element_text(size = 20),
     #   axis.title.x = element_text(size = 20),
      #  axis.line = element_line(linewidth = 2))

#ggsave(filename = "./figs/PCA_fig1.png", plot = pca_plot, wi = 30, he = 20, dpi = 300, un = "cm")

#ggsave(filename = "./figs/PCA_fig1.svg", plot = pca_plot, wi = 40, he = 20, un = "cm")

df.pca <- data.frame(pc1 = pca_res$x[,1], pc2 = pca_res$x[,2])
df.pca.std <- decostand(df.pca, method = 'range')

#--------------------- Kernel------------------------------------- 

#subsetting to remove deforested sites
aux <- match(filled.degrad,df$cell)

aux <- aux[-which(is.na(aux))]

df.pca.frag <- df.pca.std[aux,]


# >>>>>>>>>>>>>>>

figlist <- list()
n <- 100
br <- seq(0.005,0.99, by = 0.05)

br <- c(br,0.99)
br <- c(0.005,0.01,0.05,seq(0.1,1, 0.1))

figlist[[1]] <- ggplot(df.pca.std, aes(x = pc1, y = pc2)) +
  geom_density_2d_filled(aes(alpha = (..level..)),
                         contour_var = "ndensity", h=c(0.1,0.1),
                         breaks = br)+ 
  geom_density_2d(contour_var = "ndensity",h=c(0.1,0.1),
                  breaks = c(0.005,0.1,0.25,0.5,0.75,0.95), colour = "black") +
  scale_fill_manual(values = colorRampPalette(
    c("#3CA55C", cor_high(total_elements = nrow(df.pca.std), 
                          n_elements = nrow(df.pca.std))))(length(br)))+
  labs(x = "PC1", y = "PC2") +
  xlim(-.05, 1.05) + ylim(.3, 1.02) +
  scale_x_continuous(
    limits = c(-.05, 1.05),
    breaks = seq(0, 1, 0.2)) +
  labs(fill = "Percentage") +
  theme_classic(base_size = 20) +
  theme(legend.position = 'none') +
  guides(alpha = "none")


figlist[[2]] <- ggplot(df.pca.frag, aes(x = pc1, y = pc2)) +
  geom_density_2d_filled(aes(alpha = (..level..)),
                         contour_var = "ndensity",h=c(0.1,0.1),
                         breaks = br)+ 
  geom_density_2d(contour_var = "ndensity",h=c(0.1,0.1),
                  breaks = c(0.005,0.1,0.25,0.5,0.75,0.95), colour = "black")+
  scale_fill_manual(values = colorRampPalette(
    c("#3CA55C", cor_high(total_elements = nrow(df.pca.std), 
                          n_elements = nrow(df.pca.frag))))(length(br)))+
  xlim(-.05, 1.05) + ylim(.3, 1.02) +
  labs(x = "PC1", y = "PC2") +
  scale_x_continuous(
    limits = c(-.05, 1.05),
    breaks = seq(0, 1, 0.2)) +
  labs(fill = "Percentage") +
  theme_classic(base_size = 20) +
  theme(legend.position = 'none') +
  guides(alpha = "none")

fig_std_frag <- figlist[[1]] + figlist[[2]] + plot_layout(2,1)

#--------------Simulating the progressive loss of degraded forests -----------
df.degrad <- df[aux,]

map.degradation_values <- values(map.degradation)[df.degrad$cell]

#---------------------------------------------------------------------

th.seq <- seq(1,0.1,-0.1)
figlist <- list()
figlist_point <- list()

for(k in 1:length(th.seq)){
  
  th <- th.seq[k]
  aux2 <- which(map.degradation_values < th)
    
  figlist[[k]] <- ggplot(df.pca.frag[aux2,], aes(x = pc1, y = pc2)) +
    stat_density_2d(geom = "polygon",
                    aes(alpha = (..level..), fill = ..level..),
                    contour_var = "ndensity", h=c(0.1,0.1),
                    breaks = br)+
    geom_density_2d(contour_var = "ndensity", h=c(0.1,0.1),
                    breaks = c(0.005,0.1,0.25,0.5,0.75,0.95), colour = "black")+
    scale_fill_gradient(low="#3CA55C",
                        high=cor_high(
                          total_elements = nrow(df.pca.std),
                          n_elements = nrow(df.pca.frag[aux2,]))) +
    labs(title = paste0('Min. degradation score = ', ifelse(th.seq[k] == 1, paste0("1.0"), th.seq[k])),
         x ="PC1", y = "PC2")+
    xlim(-.05, 1.05) + ylim(.28, 1.02) +
    scale_x_continuous(
      limits = c(-.05, 1.05),
      breaks = seq(0, 1, .2)
    ) +
    theme_classic() +
    theme(legend.position="none",
          text = element_text(size = 12),
          plot.title = element_text(size=10))
}

fig_degrad <- figlist[[1]]+figlist[[4]]+figlist[[6]]+figlist[[9]]+figlist[[10]]+ 
  plot_layout(ncol = 3, byrow = TRUE)

#--------------------- Kernels protected areas ----------
pixel_prot <- filled.prot 
pixel_unprot <- filled.unprot

aux_prot <- which(df$cell%in%pixel_prot)
aux_unprot <- which(df$cell%in%pixel_unprot)

df.pca.protec.std <- df.pca.std[aux_prot,]
df.pca.unprotec.std <- df.pca.std[aux_unprot,]


k_protec <- ggplot(df.pca.protec.std, aes(x = pc1, y = pc2)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = (..level..), fill = ..level..),
                  contour_var = "ndensity", h=c(0.1,0.1),
                  breaks = br)+
  geom_density_2d(contour_var = "ndensity", h=c(0.1,0.1),
                  breaks = c(0.005,0.1,0.25,0.5,0.75,0.95), colour = "black")+
  xlim(-.05, 1.05) + ylim(.3, 1.02) +
  labs(x = "PC1", y = "PC2") +
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, 0.2)) +
  scale_fill_gradient(low="#3CA55C", 
                      high=cor_high(total_elements = nrow(df.pca.std), 
                                    n_elements = nrow(df.pca.frag))) +
  theme_classic()+ theme(legend.position="none",text = element_text(size = 18))



k_unprotec <- ggplot(df.pca.unprotec.std, aes(x = pc1, y = pc2)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = (..level..), fill = ..level..),
                  contour_var = "ndensity", h=c(0.1,0.1),
                  breaks = br)+
  geom_density_2d(contour_var = "ndensity", h=c(0.1,0.1),
                  breaks = c(0.005,0.1,0.25,0.5,0.75,0.95), colour = "black")+
  scale_fill_gradient(low="#3CA55C", high=cor_high(total_elements = nrow(df.pca.std),
                                                   n_elements = nrow(df.pca.frag),
                                                   n = n)) +
  xlim(-.05, 1.05) + ylim(.3, 1.02) +
  labs(x = "PC1", y = "PC2") +
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, 0.2)) +
  theme_classic()+ 
  theme(legend.position="none",text = element_text(size = 18))

fig_pca_protec <- k_unprotec + k_protec


#-------------Computing topological richness and evenness from kernels---------

source('./codes/T_div_function.r')

#total
T_div(df.pca.std)

#historical fragmentation
T_div(df.pca.frag)

#protected areas
T_div(df.pca.protec.std)

#degradation 
th.seq <- seq(1,0.1,-0.1)

TR.rand.list <- list()
TE.rand.list <- list()

TR.degrad <- c()
TE.degrad <- c()

for(k in 1:length(th.seq)){
  print(k)
  th <- th.seq[k]
  aux2 <- which(map.degradation_values < th)
  df.degrad.th <- df.pca.frag[aux2,] 
  
  TR.degrad[k] <- T_div(df.degrad.th)[1]
  TE.degrad[k] <- T_div(df.degrad.th)[2]
  
  #random cell removal
  TR.degrad.rand <- c()
  TE.degrad.rand <- c()
  
  for(j in 1:10){
    rand.aux2 <- sample(1:nrow(df.pca.frag), length(aux2))
    df.degrad.rand <- df.pca.frag[rand.aux2,]
    TR.degrad.rand[j] <- T_div(df.degrad.rand)[[1]]
    TE.degrad.rand[j] <- T_div(df.degrad.rand)[[2]]
  }
  
  TR.rand.list[[k]] <- TR.degrad.rand
  TE.rand.list[[k]] <- TE.degrad.rand
  
}

df_RIC <- data.frame(th.seq = th.seq, 
                     RIC = unlist(TR.degrad), 
                     R_rand_mean = unlist(lapply(TR.rand.list, mean)),
                     R_rand_min = unlist(lapply(TR.rand.list, min)),
                     R_rand_max = unlist(lapply(TR.rand.list, max)),
                     EVE = unlist(TE.degrad),
                     E_rand_mean = unlist(lapply(TE.rand.list, mean)),
                     E_rand_min = unlist(lapply(TE.rand.list, min)),
                     E_rand_max = unlist(lapply(TE.rand.list, max)))


df_RIC$th.seq<-factor(df_RIC$th.seq, levels=c(df_RIC$th.seq))

plot_RIC <- df_RIC %>% 
  dplyr::mutate(th.seq = ifelse(as.character(th.seq) == "1", "1.0", as.character(th.seq))) %>%
  dplyr::mutate(th.seq = forcats::as_factor(th.seq)) %>%
  ggplot(aes(x=th.seq, y=RIC)) + 
  geom_point(color="black", fill="#305200", shape=22, alpha=0.5, size=6, stroke = 1) +
  geom_errorbar(aes(ymin=R_rand_min, ymax=R_rand_max), width=.1, color = "gray30")+
  geom_point(aes(y = R_rand_mean), color="gray30", fill="gray30", shape=21, size=2, stroke = 1)+
  theme_bw(base_size = 20)+
  labs(title = "", x = "Degradation score", y = "Topological richness")
plot_RIC

plot_EVE <- df_RIC %>% 
  dplyr::mutate(th.seq = ifelse(as.character(th.seq) == "1", "1.0", as.character(th.seq))) %>%
  dplyr::mutate(th.seq = forcats::as_factor(th.seq)) %>% 
  ggplot(aes(x=th.seq, y=EVE)) + 
  geom_point(color="black", fill="#305200", shape=22, alpha=0.5, size=6, stroke = 1) +
  theme_bw(base_size = 20)+
  geom_errorbar(aes(ymin=E_rand_min, ymax=E_rand_max), width=.1, color = "gray30")+
  geom_point(aes(y = E_rand_mean), color="gray30", fill="gray30", shape=21, size=2, stroke = 1)+
  labs(title = "", x = "Degradation score", y = "Topological evenness")
plot_EVE



