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

### Pasta para manter os raster ----

dir.create("./var_presente")

### Importando ----

bioclim_presente <- geodata::worldclim_country(country = "BRA",
                                               var = "bio",
                                               path = "./var_presente/",
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
  tidyterra::geom_spatraster(data = bioclim_presente[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Cenários futuros ----

### Importando ----

baixar_cenarios_futuros <- function(tempo){

  raster_futuro <- geodata::cmip6_world(model = "ACCESS-CM2",
                                        ssp = "585",
                                        time = tempo,
                                        var = "bioc",
                                        res = 2.5,
                                        path = "dados_worldclim")

  raster_futuro %<>%
    terra::crop(br) %<>%
    terra::mask(br)

  assign(paste0("bioclim_futuro_", tempo),
         raster_futuro,
         envir = globalenv())

}

tempo <- c("2021-2040",
           "2041-2060",
           "2061-2080",
           "2081-2100")

tempo

purrr::map(tempo, baixar_cenarios_futuros)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = `bioclim_futuro_2021-2040`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bioclim_futuro_2041-2060`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bioclim_futuro_2061-2080`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = `bioclim_futuro_2081-2100`) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Exportando ----

## Cenário presente ----

bioclim_presente |> terra::writeRaster("bioclim_presente_res_2.5_arcmin.tif")

## Cenário futuro ----

exportar_cenarios_futuros <- function(rasters, tempo){

  rasters |> terra::writeRaster(paste0("bioclim_futuro_res_2.5_arcmin_",
                                      tempo,
                                      ".tif"))

}

rasters <- ls(pattern = "bioclim_futuro_") |>
  mget(envir = globalenv())

rasters

purrr::map2(rasters,
            tempo,
            exportar_cenarios_futuros)
