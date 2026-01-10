# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(magrittr)

library(usdm)

library(sdm)

# Dados ----

## Shapefile da Caatinga ----

### Importando ----

caatinga <- sf::st_read("shapefile_caatinga.shp")

### Visualizando ----

caatinga

ggplot() +
  geom_sf(data = caatinga)

## Registros d ocorrência filtrados ----

### Importando ----

registros <- readr::read_csv("registros_filtrados.csv")

### Visualizando ----

registros

registros |> dplyr::glimpse()

### Transformando em vetor espacial ----

registros_vect <- registros |> 
  dplyr::mutate(sp = sp |> stringr::str_replace(" ", "_")) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |> 
  terra::vect()

registros_vect

## Variáveis bioclimáticas para o presente ----

### Importando ----

bio_presente <- terra::rast("bioclim_presente_res_2.5_arcmin.tif")

names(bio_presente) <- c(paste0("Bio0", 1:9),
                         paste0("Bio", 10:19))

### Visualizando ----

bio_presente

ggplot() +
  tidyterra::geom_spatraster(data = bio_presente) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Variáveis bioclimáticas para o futuro ----

### Importando ----

importar_variaveis_futuras <- function(variavel, tempo){
  
  variavel_futura <- terra::rast(variavel)
  
  names(variavel_futura) <- c(paste0("Bio0", 1:9),
                              paste0("Bio", 10:19))
  
  assign(paste0("bio_futuro_", tempo),
         variavel_futura,
         envir = globalenv())
    
}

variavel <- list.files(pattern = "bioclim_futuro")

variavel

tempo <- c("2021-2040",
           "2041-2060",
           "2061-2080",
           "2081-2100")

tempo

purrr::map2(variavel, tempo, importar_variaveis_futuras)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2021-2040`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2041-2060`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2061-2080`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2081-2100`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Recortando e excluindo as variáveis dos rasters ----

## Recortando ----

recortar_variaveis <- function(variavel, nome){
  
  variavel_crop <- variavel |> 
    terra::crop(caatinga) |> 
    terra::mask(caatinga)
  
  assign(nome,
         variavel_crop,
         envir = globalenv())
  
}

variavel <- ls(pattern = "bio_") |> 
  mget(envir = globalenv())

variavel

nome <- ls(pattern = "bio_")

nome

purrr::map2(variavel, nome, recortar_variaveis)

ggplot() +
  tidyterra::geom_spatraster(data = bio_presente[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2021-2040`[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2041-2060`[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2061-2080`[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2081-2100`[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Excluindo ----

excluindo_variaveis <- function(variavel, nome){
  
  variavel_exc <- variavel[[-c(1:3, 5, 6, 8:10, 12:15, 17)]]
  
  assign(nome,
         variavel_exc,
         envir = globalenv())
  
}

variavel <- ls(pattern = "bio_") |> 
  mget(envir = globalenv())

variavel

nome <- ls(pattern = "bio_")

nome

purrr::map2(variavel, nome, excluindo_variaveis)

ggplot() +
  tidyterra::geom_spatraster(data = bio_presente) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2021-2040`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2041-2060`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2061-2080`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bio_futuro_2081-2100`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Criando os modelos ----

## Objeto sdmdata ----

sdmdata <- sdm::sdmData(sp ~ .,
                         train = registros_vect,
                         predictors = bio_presente,
                         bg = list(method = "gRandom", n = 1000))

sdmdata

## Lista de algorítimos ----

sdm::getmethodNames()

## Modelos sdm ----

modelo_sdm <- sdm::sdm(Cenostigma_pyramidale ~ .,
                       data = sdmdata,
                       methods = c("gam",
                                   "glm",
                                   "maxent",
                                   "maxlike"),
                       replication = "sub",
                       test.percent = 30,
                       n = 5)

modelo_sdm

## Exportando e importando o modelo ----

modelo_sdm |> sdm::write.sdm("modelo_sdm.sdm")

modelo_sdm <- sdm::read.sdm("modelo_sdm.sdm")

# Predição ----

## Presente ----

### Criando ----

predicao_presente <- terra::predict(modelo_sdm,
                                    bio_presente,
                                    overwrite = TRUE)

### Ensemble do modelo ----

ensemble_presente <- sdm::ensemble(modelo_sdm,
                                   newdata = predicao_presente,
                                   setting = list(method = "weighted",
                                                  stat = "AUC"))

### Visualizando ----

ensemble_presente

ggplot() +
  tidyterra::geom_spatraster(data = ensemble_presente) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1))

### Exportando ----

ensemble_presente |> terra::writeRaster("ensemble_presente.tif",
                                        overwrite = TRUE)

## Futuro ----

### Criando ----

### Ensemble do modelo ----

### Visualizando ----

### Exportando ----

# Área de presença ----

## Presente ----

### Criando ----

### Visualizando ----

### Exportando ----

## Futuro ----

### Criando ----

### Visualizando ----

### Exportando ----