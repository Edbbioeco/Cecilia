# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggview)

# Dados ----

## Shapefile da caatinga ----

### Importar ----

caa <- sf::st_read("shapefile_caatinga.shp")

### Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black")

## Rasters ----

### Importar ----

rasters <- purrr::map(c("area_nicho_passado.tif",
                        "area_nicho_presente.tif",
                        list.files(pattern = "^area_nicho_futuro_pa")),
                      terra::rast,
                      .progress = TRUE) |>
  setNames(c("area_nicho_passado.tif",
             "area_nicho_presente.tif",
             list.files(pattern = "^area_nicho_futuro_pa")) |>
             stringr::str_remove(".tif"))

### Visualizar ----

purrr::map(rasters, ~ggplot() +
             tidyterra::geom_spatraster(data = .x) +
             scale_fill_viridis_c(na.value = "transparent"),
           .progress = TRUE)

# Área de nicho ----

## Recortar rasters ----

rasters <- purrr::map(rasters, purrr::in_parallel(~.x |>
                                                    terra::crop(caa) |>
                                                    terra::mask(caa)),
                      .progress = TRUE)

purrr::map(rasters, ~ggplot() +
             tidyterra::geom_spatraster(data = .x) +
             scale_fill_viridis_c(na.value = "transparent"),
           .progress = TRUE)

## Calcular área de nicho ----

areas_nicho <- purrr::map_dbl(rasters,
                              purrr::in_parallel(~.x |>
                                                   tidyterra::filter(
                                                     ensemble_weighted == 1) |>
                                                   terra::as.polygons() |>
                                                   sf::st_as_sf(crs = 4674) |>
                                                   sf::st_area() / 1e6),
                              .progress = TRUE)

areas_nicho
