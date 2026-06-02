# Pacotes ----

library(sf)

library(tidyverse)

library(geodata)

library(tidyterra)

library(magrittr)

library(terra)

# Dados ----

## Shapefile da Caatinga ----

### Importando ----

caa <- sf::st_read("shapefile_caatinga.shp")

### Visualizando ----

caa

ggplot() +
  geom_sf(data = caa)

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
  tidyterra::geom_spatraster(data = bioclim_presente[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

### Recortando ----

bioclim_presente %<>%
  terra::crop(caa |>
                sf::st_concave_hull(ratio = 0.15)) %<>%
  terra::mask(caa |>
                sf::st_concave_hull(ratio = 0.15))

bioclim_presente

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_presente[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Cenários futuros ----

### Criar pastas dos cenários e tempos----

cenario <- c("126",
             "245",
             "370",
             "585")

cenario

tempo <- c("2021-2040",
           "2041-2060",
           "2061-2080",
           "2081-2100")

tempo

purrr::map(cenario, \(cenario){

  purrr::map(tempo, \(tempo){

    dir.create(paste0("./cenario_", cenario, "/", tempo),
             recursive = TRUE)

    })

  })

### Importando ----

bioclim_futuro <- purrr::map2(cenario |> rep(each = 4),
                              tempo |> rep(times = 4),
                              \(cenario, tempo){

    geodata::cmip6_world(country = "BRA",
                         model = "ACCESS-CM2",
                         ssp = cenario,
                         time = tempo,
                         var = "bioc",
                         res = 0.5,
                         path = paste0("cenario_", cenario, "/", tempo)) |>
      terra::crop(caa |>
                    sf::st_concave_hull(ratio = 0.15)) |>
      terra::mask(caa |>
                    sf::st_concave_hull(ratio = 0.15))

  },
  .progress = TRUE) |>
  setNames(paste0("cenario_", cenario, "-", tempo))

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
