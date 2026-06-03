# Pacotes ----

library(sf)

library(tidyverse)

library(geodata)

library(tidyterra)

library(magrittr)

library(terra)

library(pastclim)

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

dir.create("./var_futuro")

### Importando ----

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

bioclim_futuro <- purrr::map2(cenario |> rep(each = 4),
                              tempo |> rep(times = 4),
                              \(cenario, tempo){

    geodata::cmip6_world(country = "BRA",
                         model = "ACCESS-CM2",
                         ssp = cenario,
                         time = tempo,
                         var = "bioc",
                         res = 0.5,
                         path = "./var_futuro") |>
      terra::crop(caa |>
                    sf::st_concave_hull(ratio = 0.15)) |>
      terra::mask(caa |>
                    sf::st_concave_hull(ratio = 0.15))

  },
  .progress = TRUE) |>
  setNames(paste0("cenario_",
                  cenario |> rep(each = 4), "-",
                  tempo |> rep(times = 4)))

bioclim_futuro

### Visualizando ----

purrr::imap(bioclim_futuro, ~ggplot() +
              tidyterra::geom_spatraster(data = .x[[1]]) +
              labs(title = .y),
            .progress = TRUE)

## Cenário passado ----

### Criar pasta ----

dir.create("./var_passado")

### Baixar ----

passado <- rpaleoclim::paleoclim(period = "lgm",
                                 resolution = "30s",
                                 region = caa |>
                                   sf::st_concave_hull(ratio = 0.15) |>
                                   terra::ext(),
                                 skip_cache = TRUE) |>
  terra::crop(caa |>
                sf::st_concave_hull(ratio = 0.15)) |>
  terra::mask(caa |>
                sf::st_concave_hull(ratio = 0.15))

### Visualizar ----

passado

ggplot() +
  tidyterra::geom_spatraster(data = passado[[1]])

# Exportando ----

## Cenário presente ----

bioclim_presente |> terra::writeRaster("./var_presente/presente.tif")

## Cenário futuro ----

purrr::map2(bioclim_futuro,
            bioclim_futuro |>
              names() |>
              stringr::str_remove("cenario_") |>
              stringr::str_c(".tif"),
           \(raster, arquivo){

             terra::writeRaster(raster,
                                filename = paste0("./var_futuro/",
                                                  arquivo),
                                overwrite = TRUE)

             },
           .progress = TRUE)
