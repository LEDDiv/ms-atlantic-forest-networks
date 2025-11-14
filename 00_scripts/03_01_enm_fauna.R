#' ---
#' title: sdm enmeval fauna
#' date: 2023-04-21
#' ---

# prepare r ---------------------------------------------------------------

# options
options(scipen = 2, java.parameters = "-Xmx32g") ##options é uma função do R usada para ajustar opções globais de comportamento do R. Aqui ta rolando algo para não limitar a memoria 
gc()  ### limpa a memória de objetos não utilizados.

# packages
library(tidyverse) #manipulação e visualização de dados
library(ggsci) #Paleta de cores para gráficos ggplot2. 
library(sf) #Suporte para dados espaciais simples (SIMPLE FEATURES)
library(terra) #Manipulação e analise de dados raster e vetoriais.
library(dismo) #Modelagem de distribuição de espécies
library(ecospat) #Ferramentas para analise de dados espaciais ecológicos
library(rJava) #Integração entre R e Java
library(ENMeval) #Avaliação de modelos de nicho ecológico
library(tmap) #Ferramentas para criar mapas temáticos
library(rnaturalearth)

# options
sf_use_s2(FALSE)

# maxent
## Baixando e extraindo o software Maxent.
#download.file(url = "https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
#              destfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"), mode = "wb")
#unzip(zipfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"),
#      exdir = system.file("java", package = "dismo"), junkpaths = TRUE)
dir(system.file("java", package = "dismo")) ##listando os arquivos do diretório para verificar se a extração foi bem sucedida. QUESTION: UMA VEZ FEITO ISSO NÃO É NECESSÁRIO FAZER DE NOVO, NÉ?

# import data -------------------------------------------------------------

# o limite da Mata Atlantica do Ecorr. esta aqui "G:/coisas mestrado_espacial/MA_ecorr.shp"

# occ
all_occ <- read_csv("01_data/00_occurrences/01_fauna/animal_occ_iucn.csv") %>% 
  dplyr::rename(id = 1,
                species = sp,
                longitude = lon,
                latitude = lat) %>%
  dplyr::group_by(species) %>% 
  dplyr::mutate(n = n()) %>%  
  dplyr::filter(n >= 10) %>% 
  dplyr::arrange(species)
all_occ

length(unique(all_occ$species))

da_int <- count(readr::read_csv2("../../Master/BIOMAS/Andre/matrizes_andre_dados1cap/planilhas biomas_cap1/Mata_atlantica_int.csv"), frugivorous_species, sort = TRUE)

da_occ_int <- dplyr::left_join(da_occ, da_int, by = join_by(species == frugivorous_species))
da <- da_occ_int %>% filter(n.x >= 10)
da

# selecionando só uma inicialmente
# occ <- all_occ %>%
#   dplyr::filter(species == "Abuta selloana") 
# occ
# names(occ)
# head(occ)

# vector 
## convertendo os dados de ocorrencia para um formato espacial 
occ_sf <- all_occ %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) ##define o sistema de referencial espacial como WGS 84
occ_sf

# limit ### vou usar o limite da america 
li <- sf::st_read("01_data/01_variables/01_limits/america.shp")
li

# brazil
br <- rnaturalearth::countries110[countries110$ADM0_A3 == "BRA",]
br

# filter
### filtrando os dados espaciais e criando um mapa para visualizar as ocorrencias
occ_sf_sa <- occ_sf[li, ] 
occ_sf_sa

# crop_map <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units',
#                        returnclass = 'sf') %>% 
#  filter(region_wb == "Latin America & Caribbean")

occ_sa <- sf::st_drop_geometry(occ_sf_sa) 
occ_sa

tm_shape(li, bbox = occ_sf_sa)+
  tm_polygons() +
  tm_shape(occ_sf_sa) +
  tm_bubbles()

# covarariate 
#covar <- geodata::worldclim_global(var = "bio", res = 10, version="2.1", path = "~/Downloads/")
#names(covar) <- c("bio01", paste0("bio0", 2:9), paste0("bio", 10:19))
#names(covar)


#arquivos <- list.files(path = "D:/coisas mestrado_espacial/Mauricio/camadas", full.names = T)
#lista_covariaveis <- list()

# Loop para ler cada arquivo e armazenar na lista
#for (arquivo in arquivos) {
#  nome_variavel <- tools::file_path_sans_ext(basename(arquivo))  
#  lista_covariaveis[[nome_variavel]] <- raster::raster(arquivo)  
#}

#covar <- stack(lista_covariaveis)
#covar
files <- dir(path = "01_data/01_variables/02_adjusted/", pattern = "tif", full.names = TRUE)[c(1:19, 21:22)]
covar <- terra::rast(files)
names(covar) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9), "slope_md", "slope_sd")
names(covar)
#covar_li <- terra::crop(covar, li, mask = TRUE)

# plot(covar[["slope_sd"]], col = viridis::turbo(n = 100))

# tm_shape(covar[["slope_sd"]]) +
#   tm_raster() +
#   tm_layout(legend.show = FALSE) ###roda isso aqui no pc do lab.

# corte para o limite da mata atlantica - isso é para predizer os pontos. 
af <- terra::vect("01_data/01_variables/01_limits/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp")
af
plot(af)

covar_af <- terra::crop(covar, af)
covar_af <- terra::mask(covar_af, af)

tm_shape(covar_af[[1]]) +
  tm_raster(col = "bio01",
            col.scale = tm_scale_continuous(values = "-spectral"),
            col.legend = tm_legend(position = tm_pos_in("right", "bottom"),
                                   reverse = TRUE)) +
  tm_shape(af) +
  tm_borders() 

# sdm ---------------------------------------------------------------------

# buffer radius
buffer_radius <- 300e3
buffer_radius

# cores
ncores <- parallel::detectCores() - 2
ncores

# tune args ## dois hiper parametros do MaxEnt
tune_args <- list(fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), rm = seq(.5, 4, .5))
tune_args


# enmeval
sp_enm_fitted <- dir(path = "02_results/01_fauna")[-1]
sp_enm_fitted

sp_enm <- unique(occ_sa$species)
sp_enm_without_fit <- sp_enm[!sp_enm %in% sp_enm_fitted]
sp_enm_without_fit

### isso é para a avaliação do modelo
for(i in sp_enm_without_fit){}
  
  ## information 
  print(i)
  
  ## species ----
  occ_i <- occ_sa %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(3:4)
  
  occ_v <- terra::vect(occ_i, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") ##converte dados em role espacial
  
  ## convhull and buffer ----
  occ_v_buffer <- terra::buffer(x = occ_v, width = buffer_radius) ##um buffer em torno das ocorrencias
  
  ## covariates crop ----
  covar_buffer <- terra::crop(covar, occ_v_buffer) ##corta as variaveis ambientais para o tamanho do buffer
  covar_buffer <- terra::mask(covar_buffer, occ_v_buffer) ##corta as variaveis ambientais para o tamanho do buffer
  
  ## covariates selection ----
  covar_buffer_vif <- usdm::vifstep(covar_buffer, th = 3) ###VIF 
  covar_buffer_sel <- usdm::exclude(covar_buffer, covar_buffer_vif)
  
  covar_af_sel <- usdm::exclude(covar_af, covar_buffer_vif)
  
  ## registers
  n_occ <- nrow(na.omit(terra::extract(covar_buffer_sel, occ_i, ID = FALSE)))
  
  if(n_occ < 10){
    
    
  }else{
    
    ## sdm ----
    
    ### data ----
    bg <- terra::spatSample(x = covar_buffer_sel[[1]], size = 1e4, "random", as.df = TRUE, xy = TRUE, values = FALSE, na.rm = TRUE) ##amostrando pontos aleatórios em volta das ocorrencias e as covariaveis ambientais dela. esses são os pontos de fundo, o background do modelo 
    colnames(bg) <- colnames(occ_i) ##renomeando colunas
    
    occs_z <- cbind(occ_i, terra::extract(covar_buffer_sel, occ_i, ID = FALSE)) ##combina as ocorrencias da espécie com os valores das variaveis ambientais do local de ocorrencia
    bg_z <- cbind(bg, terra::extract(covar_buffer_sel, bg))##mesmo esquema, mas combinas os pontos de não ocorrencia com as variaveis ambientais dele
    
    ### fit ---- ##  
    ## o pacote ENMevaluate é o brabo. Ele é o que de fato constroi os modelos com uma variaedade de ajustes que a gente mesmo especifica. 
    eval_fit <- ENMeval::ENMevaluate(occs = occs_z, 
                                     bg = bg_z,
                                     tune.args = tune_args, 
                                     algorithm = "maxent.jar", 
                                     partitions = "block", # leia aqui o site dos blocos. Esse método "block" é bom em casos onde há autocorrelação espacial dos dados, ai ela mantem a integridade estrutural durante a validação. Isso evita uma sub ou superestimação do modelo devido a autocorrelação espacial não considerada. 
                                     parallel = TRUE, 
                                     numCores = ncores)
    
    
    ### results ----  
    eval_resul <- eval_fit@results ##variando hiperparametros e o valor de auc pra cada um 
    eval_resul_partitions <- eval_fit@results.partitions
    
    ### selection ----
    eval_resul_aic <-  dplyr::filter(eval_resul, delta.AICc == 0)[1, ] ##combinação de hiperparametros com menor auc é o que procuramos
    eval_fit_aic <- eval.models(eval_fit)[[eval_resul_aic$tune.args]] ##esse é o modelo ajustado
    
    #eval_resul %>% 
    #  filter(delta.AICc <= 2) %>%
    #  filter(or.10p.avg == min(or.10p.avg)) %>% 
    #  filter(auc.val.avg == max(auc.val.avg))
    
    plot_delta_aicc_tune_args <- ggplot(eval_resul, aes(x = rm, y = delta.AICc, color = fc, group = fc)) +
      geom_line() +
      geom_point(color = "black", shape = 1) +
      geom_point(data = eval_resul_aic, aes(x = rm, y = delta.AICc), color = "red", size = 5) +
      geom_hline(yintercept = 2, lty = 2) +
      scale_color_lancet() +
      labs(x = "Regularization multiplier", y = "Delta AICc", color = "Feature classes") +
      theme_bw(base_size = 15)
    
    plot_ormtp_tune_args <- ggplot(eval_resul, aes(x = rm, y = or.mtp.avg, color = fc, group = fc)) +
      geom_line() +
      geom_point(color = "black", shape = 1) +
      geom_point(data = eval_resul_aic, aes(x = rm, y = or.mtp.avg), color = "red", size = 5) +
      scale_color_lancet() +
      labs(x = "Regularization multiplier", y = "Average ORMTP ('Minimum Training Presence' omission rate)", color = "Feature classes") +
      theme_bw(base_size = 15)
    
    plot_auc_tune_args <- ggplot(eval_resul, aes(x = rm, y = auc.val.avg, color = fc, group = fc)) +
      geom_line() +
      geom_point(color = "black", shape = 1) +
      geom_point(data = eval_resul_aic, aes(x = rm, y = auc.val.avg), color = "red", size = 5) +
      geom_hline(yintercept = .75, lty = 2) +
      scale_color_lancet() +
      ylim(min(eval_resul$auc.val.avg) - .15, max(eval_resul$auc.val.avg) + .1) +
      labs(x = "Regularization multiplier", y = "Average validation Area Under Curve (AUC)", color = "Feature classes") +
      theme_bw(base_size = 15)
    
    plot_cbi_tune_args <- ggplot(eval_resul, aes(x = rm, y = cbi.val.avg, color = fc, group = fc)) +
      geom_line() +
      geom_point(color = "black", shape = 1) +
      geom_point(data = eval_resul_aic, aes(x = rm, y = cbi.val.avg), color = "red", size = 5) +
      scale_color_lancet() +
      labs(x = "Regularization multiplier", y = "Average validation Continuous Boyce Index (CBI)", color = "Feature classes") +
      theme_bw(base_size = 15)
    
    ### null models ----
    #mod_null <- ENMeval::ENMnulls(e = eval_fit, no.iter = 100, parallel = FALSE, numCores = 10,
    #                              mod.settings = list(fc = as.character(eval_resul_aic$fc),
    #                                                  rm = as.numeric(as.character(eval_resul_aic$rm))))
    #mod_null_results <- null.emp.results(mod_null)
    #plot_mod_null_hist <- evalplot.nulls(mod_null, stats = c("or.mtp", "auc.val"), plot.type = "histogram")
    
    ### covariate importance ----
    eval_resul_aic_varimp <- dplyr::arrange(eval_fit@variable.importance[[eval_resul_aic$tune.args]], -percent.contribution)
    
    plot_covar_imp <- ggplot(eval_resul_aic_varimp,
                             aes(x = percent.contribution, 
                                 y = reorder(variable, percent.contribution))) +
      geom_bar(stat = "identity", fill = "gray") +
      geom_text(aes(label = round(percent.contribution, 1)), size = 7) +
      scale_x_continuous(labels = scales::percent_format(scale = 1)) +
      labs(x = "Percent contribution", y = "") +
      theme_bw(base_size = 20) 
    
    ### covariate response ----
    eval_resul_aic_response <- NULL
    for(j in covar_buffer_vif@results$Variables){
      
      eval_resul_aic_response_i <- tibble::as_tibble(dismo::response(eval.models(eval_fit)[[eval_resul_aic$tune.args]], var = j)) %>% 
        dplyr::rename(value = 1, predict = 2) %>% 
        dplyr::mutate(covar = j)
      eval_resul_aic_response <- rbind(eval_resul_aic_response, eval_resul_aic_response_i) 
    }
    eval_resul_aic_response
    
    plot_covar_res <- ggplot(eval_resul_aic_response, aes(x = value, y = predict)) +
      geom_line(color = "red", lwd = 1) +
      facet_wrap(~covar, scales = "free") +
      labs(x = "Values", y = "Cloglog") +
      theme_bw(base_size = 20)
    
    ### prediction ----
    eval_fit_aic_predict <- enm.maxent.jar@predict(
      mod = eval_fit@models[[eval_resul_aic$tune.args]], 
      envs = covar_buffer_sel, 
      other.settings = list(pred.type = "cloglog"))
    
    eval_fit_aic_predict_af <- enm.maxent.jar@predict(
      mod = eval_fit@models[[eval_resul_aic$tune.args]], 
      envs = covar_af_sel, 
      other.settings = list(pred.type = "cloglog"))
    
    ## threshold ----
    ev <- dismo::evaluate(p = terra::extract(eval_fit_aic_predict, eval_fit@occs[, 1:2], ID = FALSE)[, 1], 
                          a = terra::extract(eval_fit_aic_predict, eval_fit@bg[, 1:2], ID = FALSE)[, 1])
    thr <- threshold(ev)
    
    eval_fit_aic_predict_af_thr_equal_sens_spec <- eval_fit_aic_predict_af >= thr$equal_sens_spec
    eval_fit_aic_predict_af_thr_sensitivity <- eval_fit_aic_predict_af >= thr$sensitivity
    eval_fit_aic_predict_af_thr_spec_sens <- eval_fit_aic_predict_af >= thr$spec_sens
    eval_fit_aic_predict_af_thr_no_omission <- eval_fit_aic_predict_af >= thr$no_omission
    
    ### tss ---
    thr_id <- NULL
    for(j in names(threshold(ev))) thr_id <- c(thr_id, which(ev@t == dismo::threshold(ev, j)))
    tss <- ev@TPR[thr_id] + ev@TNR[thr_id] - 1
    
    thr_tss <- rbind(thr, tss)
    thr_tss <- cbind(data.frame(metrics = c("thresholds", "tss")), thr_tss)
    
    ## export ----
    dir.create("02_results", sho)
    path_sp <- paste0("02_results/", i)
    
    dir.create(path = path_sp)
    
    readr::write_csv(covar_buffer_vif@results, paste0(path_sp, "/02_vif_", i, ".csv"))
    
    readr::write_csv(occs_z, paste0(path_sp, "/01_occs_", i, ".csv"))
    readr::write_csv(bg_z, paste0(path_sp, "/01_bg_", i, ".csv"))
    
    readr::write_csv(eval_resul, paste0(path_sp, "/03_01_eval_results_", i, ".csv"))
    readr::write_csv(eval_resul_partitions, paste0(path_sp, "/03_01_eval_resul_partitions_", i, ".csv"))
    readr::write_csv(eval_resul_aic, paste0(path_sp, "/03_01_eval_resul_aic_", i, ".csv"))
    
    ggsave(paste0(path_sp, "/03_01_delta_aicc_tune_args_", i, ".png"), plot_delta_aicc_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)
    ggsave(paste0(path_sp, "/03_01_ormtp_tune_args_", i, ".png"), plot_ormtp_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)
    ggsave(paste0(path_sp, "/03_01_auc_tune_args_", i, ".png"), plot_auc_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)
    
    # readr::write_csv(mod_null_results, paste0(path_sp, "/03_02_mod_null_results_", i, ".csv"))
    # ggsave(paste0(path_sp, "/03_02_mod_null_hist_", i, ".png"), plot_mod_null_hist, wi = 30, he = 20, un = "cm", dpi = 300)
    
    ggsave(paste0(path_sp, "/03_03_covarimp_", i, ".png"), plot_covar_imp, wi = 30, he = 20, un = "cm", dpi = 300)
    ggsave(paste0(path_sp, "/03_03_covarrep_", i, ".png"), plot_covar_res, wi = 30, he = 20, un = "cm", dpi = 300)
    
    readr::write_csv(thr_tss, paste0(path_sp, "/03_04_thresholds_tss_", i, ".csv"))
    
    terra::writeRaster(eval_fit_aic_predict, paste0(path_sp, "/03_05_pred_", i, ".tif"), overwrite = TRUE)
    terra::writeRaster(eval_fit_aic_predict_af, paste0(path_sp, "/03_05_pred_", i, "_af.tif"), overwrite = TRUE)
    
    terra::writeRaster(eval_fit_aic_predict_af_thr_equal_sens_spec, paste0(path_sp, "/03_05_pred_", i, "_af_thr_equal_sens_spec.tif"), overwrite = TRUE)
    terra::writeRaster(eval_fit_aic_predict_af_thr_sensitivity, paste0(path_sp, "/03_05_pred_", i, "_af_thr_sensitivity.tif"), overwrite = TRUE)
    terra::writeRaster(eval_fit_aic_predict_af_thr_spec_sens, paste0(path_sp, "/03_05_pred_", i, "_af_thr_spec_sens.tif"), overwrite = TRUE)
    terra::writeRaster(eval_fit_aic_predict_af_thr_no_omission, paste0(path_sp, "/03_05_pred_", i, "_af_thr_no_omission.tif"), overwrite = TRUE)
    
    # msdm
    occ_v_convhull_buffer_msdm <- terra::buffer(x = terra::convHull(occ_v), width = 30e3)
    
    eval_fit_aic_predict_af_msdm <- terra::crop(eval_fit_aic_predict_af, occ_v_convhull_buffer_msdm)
    eval_fit_aic_predict_af_msdm <- terra::mask(eval_fit_aic_predict_af_msdm, occ_v_convhull_buffer_msdm)
    
    eval_fit_aic_predict_af_msdm <- terra::resample(eval_fit_aic_predict_af_msdm, eval_fit_aic_predict_af, threads = TRUE)
    eval_fit_aic_predict_af_msdm <- terra::ifel(is.na(eval_fit_aic_predict_af_msdm), 0, eval_fit_aic_predict_af_msdm)
    plot(eval_fit_aic_predict_af_msdm)
    
    eval_fit_aic_predict_af_1 <- terra::ifel(!is.na(eval_fit_aic_predict_af), 1, NA)
    
    eval_fit_aic_predict_af_msdm <- eval_fit_aic_predict_af_msdm * eval_fit_aic_predict_af_1
    plot(eval_fit_aic_predict_af_msdm)
    
    terra::writeRaster(eval_fit_aic_predict_af_msdm, paste0(path_sp, "/03_05_pred_", i, "_af_msdm.tif"), overwrite = TRUE)
    
    ## map
    dir.create("04_results/01_maps")
    map_enm <- tm_shape(eval_fit_aic_predict) +
      tm_raster(col = "maxent",
                col.scale = tm_scale_continuous(values = "-spectral"),
                col.legend = tm_legend(position = tm_pos_out("right", "center"),
                                       reverse = TRUE,
                                       title = "Suitability")) +
      tm_shape(br) +
      tm_borders(col = "gray") +
      tm_shape(af) +
      tm_borders() +
      tm_shape(occ_v) +
      tm_bubbles(size = .4, fill = "white") +
      tm_grid(lines = FALSE) +
      tm_title(i)
    tmap::tmap_save(map_enm, paste0("04_results/01_maps/map_enm_", i, ".png"), 
                    wi = 20, he = 18, un = "cm", dpi = 300)
    
    
    map_enm_af <- tm_shape(eval_fit_aic_predict_af) +
      tm_raster(col = "maxent",
                col.scale = tm_scale_continuous(values = "-spectral"),
                col.legend =  tm_legend(position = tm_pos_in("right", "bottom"),
                                        reverse = TRUE,
                                        title = "Suitability")) +
      tm_shape(occ_v) +
      tm_bubbles(size = .4, fill = "white") +
      tm_grid(lines = FALSE) +
      tm_title(i)
    tmap::tmap_save(map_enm_af, paste0("04_results/01_maps/map_enm_", i, "_af.png"), 
                    wi = 20, he = 18, un = "cm", dpi = 300)
    
    map_enm_af_msdm <- tm_shape(eval_fit_aic_predict_af_msdm) +
      tm_raster(col = "maxent",
                col.scale = tm_scale_continuous(values = "-spectral"),
                col.legend =  tm_legend(position = tm_pos_in("right", "bottom"),
                                        reverse = TRUE,
                                        title = "Suitability")) +
      tm_shape(occ_v) +
      tm_bubbles(size = .4, fill = "white") +
      tm_grid(lines = FALSE) +
      tm_title(i)
    tmap::tmap_save(map_enm_af_msdm, paste0("04_results/01_maps/map_enm_", i, "_af_msdm.png"), 
                    wi = 20, he = 18, un = "cm", dpi = 300)
    
  }
  
}
