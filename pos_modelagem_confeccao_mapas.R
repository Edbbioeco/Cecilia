# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(magrittr)

library(ggspatial)

library(ggview)

library(patchwork)

# Dados ----

## Shapefile da Caatinga ----

### Importando ----

caatinga <- sf::st_read("shapefile_caatinga.shp")

### Visualizando ----

caatinga

ggplot() +
  geom_sf(data = caatinga)

## Brasil ----

### Importando ----

br <- sf::st_read("shapefile_brasil.shp")

### Visualizando ----

br

ggplot() +
  geom_sf(data = br) +
  geom_sf(data = caatinga)

## Raster de probabilidade de ocorrência para o presente ---- 

### Importando ----

prob_presente <- terra::rast("ensemble_presente.tif")

### Visualizando ----

prob_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = prob_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

## Raster de probabilidade de ocorrência para o futuro ---- 

### Importando ----

prob_futuro <- terra::rast("ensemble_futuro.tif")

### Visualizando ----

prob_futuro

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = prob_futuro) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Raster de área de nicho de ocorrência para o presente ---- 

### Importando ----

nicho_presente <- terra::rast("area_nicho_presente.tif")

### Visualizando ----

nicho_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = nicho_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA) 

### Tratando os rasters ----

nicho_presente %<>%
  tidyterra::mutate(ensemble_weighted = dplyr::case_when(ensemble_weighted == 0 ~ "Não área de nicho", 
                                                         ensemble_weighted == 1 ~ "Área de nicho") |> 
                      forcats::fct_relevel(c("Não área de nicho",
                                             "Área de nicho"))) %<>%
  tidyterra::drop_na()

nicho_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = nicho_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_d(na.translate = FALSE) 

## Raster de área de nicho de ocorrência para o futuro ----  

### Importando ----

nicho_futuro <- terra::rast("area_nicho_futuro.tif")

### Visualizando ----

nicho_futuro

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = nicho_futuro) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

### Tratando os rasters ----

nicho_futuro %<>%
  tidyterra::mutate(dplyr::across(dplyr::everything(),
                                  ~dplyr::case_when(.x == 0 ~ "Não área de nicho",
                                                    .x == 1 ~ "Área de nicho") |> 
                                    forcats::fct_relevel(c("Não área de nicho",
                                                           "Área de nicho")))) %<>%
  tidyterra::drop_na()

nicho_futuro

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = nicho_futuro) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_d(na.translate = FALSE) +
  facet_wrap(~lyr)

# Mapa de área de probabilidade de ocorrência para o presente ----

# Mapa de área de probabilidade de ocorrência para o futuro -----

# Mapa de área de nicho para o presente ----

# Mapa de área de nicho para o futuro -----