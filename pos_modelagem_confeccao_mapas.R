# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(magrittr)

library(ggview)

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

## Raster do Presente ----

### Ensemble ----

#### Importando ----

ensemble_presente <- terra::rast("ensemble_presente.tif")

#### Visualizando ----

ensemble_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = ensemblepresente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

### Área de nicho ----

#### Importar ----

area_nicho_presente <- terra::rast("area_nicho_presente.tif")

#### Visualizar ----

area_nicho_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

## Raster do futuro ----

### Ensemble ----

#### Importando ----

ensemble_futuro <- purrr::map(list.files(pattern = "^ensemble_futuro_"),
                          terra::rast,
                          .progress = TRUE) |>
  setNames(list.files(pattern = "^ensemble_futuro_") |>
             stringr::str_remove(".tif$"))

#### Visualizando ----

ensemble_futuro

purrr::map(ensemble_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_c(na.value = NA),
           .progress = TRUE)

### Área de nicho ----

#### Importar ----

area_nicho_futuro <- purrr::map(list.files(pattern = "^area_nicho_futuro_"),
                                terra::rast,
                                .progress = TRUE) |>
  setNames(list.files(pattern = "^area_nicho_futuro_") |>
             stringr::str_remove(".tif$"))

#### Visualizar ----

area_nicho_futuro

purrr::map(area_nicho_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_c(na.value = NA),
           .progress = TRUE)

## Raster do passado ---

### Ensemble -----

#### Importando ----

ensemble_passado <- terra::rast("ensemble_passado.tif")

#### Visualizando ----

ensemble_passado

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = ensemble_passado) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

### Área de nicho ----

#### Importar ----

area_nicho_passado <- terra::rast("area_nicho_passado.tif")

#### Visualizar ----

area_nicho_passado

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_passado) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

# Tratar rasters ----

## Recortar os raster para a Caatinga ----

### Ensembles ----

ensemble_passado <- ensemble_passado |>
  terra::crop(caatinga) |>
  terra::mask(caatinga)

ensemble_passado

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = ensemble_passado) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

ensemble_presente <- ensemble_presente |>
  terra::crop(caatinga) |>
  terra::mask(caatinga)

ensemble_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = ensemble_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

ensemble_futuro <- purrr::map(ensemble_futuro, ~.x |>
                                terra::crop(caatinga) |>
                                terra::mask(caatinga),
           .progress = TRUE) |>
  setNames(ensemble_futuro |> names())

ensemble_futuro

purrr::map(ensemble_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_c(na.value = NA),
           .progress = TRUE)

# Mapa de área de probabilidade de ocorrência para o presente ----

ggplot() +
  geom_sf(data = br, linewidth = 1, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = prob_presente) +
  geom_sf(data = caatinga, fill = NA, linewidth = 1, aes(color = "Caatinga")) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20)) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 1, fill = NA, color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_presente.png",
       height = 10, width = 12)

# Mapa de área de probabilidade de ocorrência para o futuro -----

ggplot() +
  geom_sf(data = br, linewidth = 0.5, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = prob_futuro) +
  geom_sf(data = caatinga, fill = NA, linewidth = 0.5, aes(color = "Caatinga")) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20)) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.5, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(size = 20, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_futuro.png",
       height = 10, width = 12)

# Mapa de área de nicho para o presente ----

ggplot() +
  geom_sf(data = br, linewidth = 1, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = nicho_presente) +
  geom_sf(data = caatinga, fill = NA, linewidth = 1, aes(color = "Caatinga")) +
  scale_fill_viridis_d(na.value = NA,
                       name = NULL,
                       guide = guide_legend(title.position = "top",
                                            title.hjust = 0.5),
                       na.translate = FALSE) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  guides(color = guide_legend(order = 1),
         fill  = guide_legend(order = 2)) +
  geom_sf(data = br, linewidth = 1, fill = NA, color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_area_nicho_ocorrencia_presente.png",
       height = 10, width = 12)

# Mapa de área de nicho para o futuro -----

ggplot() +
  geom_sf(data = br, linewidth = 1, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = nicho_futuro) +
  geom_sf(data = caatinga, fill = NA, linewidth = 1, aes(color = "Caatinga")) +
  scale_fill_viridis_d(na.value = NA,
                       name = NULL,
                       guide = guide_legend(title.position = "top",
                                            title.hjust = 0.5),
                       na.translate = FALSE) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  guides(color = guide_legend(order = 1),
         fill  = guide_legend(order = 2)) +
  geom_sf(data = br, linewidth = 1, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(size = 20, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_area_nicho_ocorrencia_futuro.png",
       height = 10, width = 12)
