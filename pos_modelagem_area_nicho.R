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
