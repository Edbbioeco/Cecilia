# Pacotes ----

library(geobr)

library(tidyverse)

library(geodata)

library(tidyterra)

library(magrittr)

library(terra)

# Dados ----

## Shapefile da Caatinga ----

### Importando ----

br <- geobr::read_country()

### Visualizando ----

br

ggplot() +
  geom_sf(data = br)

## Raster para o presente ----

### Importando ----

bioclim_presente <- geodata::worldclim_country(country = "BRA",
                                               var = "bio", 
                                               path = getwd(),
                                               res = 0.5)

### Visualizando ----

bioclim_presente

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_presente) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

### Recortando ----

bioclim_presente %<>%
  terra::crop(br) %<>%
  terra::mask(br)

bioclim_presente

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_presente) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Cenários futuros ----

### Importando ----

baixar_cenarios_futuros <- function(cenario){
  raster_futuro <- geodata::cmip6_world()
}

### Visualizando ---- 

# Exportando ----

## Cenário presente ----

bioclim_presente |> terra::writeRaster("bioclim_presente_res_0.5_arcmin.tif")

zip(files = "bioclim_presente_res_0.5_arcmin.tif",
    zipfile = "bioclim_presente_res_0.5_arcmin.zip")

## Cenário futuro ----

zip(files = ls(pattern = "bioclim_futuro"),
    zipfile = "bioclim_futuro_res_0.5_arcmin.zip")