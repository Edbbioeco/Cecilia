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
  
  raster_futuro <- geodata::cmip6_world(model = "CanESM5", 
                                        ssp = cenario, 
                                        time = "2041-2060", 
                                        var = "bioc", 
                                        res = 0.5, 
                                        path = "dados_worldclim")
  
  raster_futuro %<>%
    terra::crop(br) %<>%
    terra::mask(br)
  
  assign(paste0("bioclim_futuro_", cenario),
         raster_futuro,
         envir = globalenv())
  
}

cenario <- c("245", "370", "585")

purrr::map(cenario, baixar_cenarios_futuros)

### Visualizando ---- 

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_futuro_245) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_futuro_370) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_futuro_585) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Exportando ----

## Cenário presente ----

bioclim_presente |> terra::writeRaster("bioclim_presente_res_0.5_arcmin.tif")

zip(files = "bioclim_presente_res_0.5_arcmin.tif",
    zipfile = "bioclim_presente_res_0.5_arcmin.zip")

## Cenário futuro ----

zip(files = ls(pattern = "bioclim_futuro"),
    zipfile = "bioclim_futuro_res_0.5_arcmin.zip")