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

### Área de nicho ----

area_nicho_passado <- area_nicho_passado |>
  terra::crop(caatinga) |>
  terra::mask(caatinga)

area_nicho_passado

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_passado) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

area_nicho_presente <- area_nicho_presente |>
  terra::crop(caatinga) |>
  terra::mask(caatinga)

area_nicho_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_c(na.value = NA)

area_nicho_futuro <- purrr::map(area_nicho_futuro, ~.x |>
                                  terra::crop(caatinga) |>
                                  terra::mask(caatinga),
                                .progress = TRUE) |>
  setNames(ensemble_futuro |> names())

area_nicho_futuro

purrr::map(area_nicho_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_c(na.value = NA),
           .progress = TRUE)

## Alterar valores dos rasters de área de nicho ----

area_nicho_passado <- area_nicho_passado |>
  dplyr::mutate(ensemble_weighted = dplyr::case_when(
    ensemble_weighted == 0 ~ "Não área de nicho",
    ensemble_weighted == 1 ~ "Área de nicho"))


area_nicho_passado

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_passado) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_d(na.translate = FALSE, direction = -1)

area_nicho_presente <- area_nicho_presente |>
  dplyr::mutate(ensemble_weighted = dplyr::case_when(
    ensemble_weighted == 0 ~ "Não área de nicho",
    ensemble_weighted == 1 ~ "Área de nicho"))


area_nicho_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = area_nicho_presente) +
  geom_sf(data = caatinga, fill = NA) +
  scale_fill_viridis_d(na.translate = FALSE, direction = -1)

area_nicho_futuro <- purrr::map(area_nicho_futuro,
                                ~.x |>
                                  dplyr::mutate(
                                    ensemble_weighted = dplyr::case_when(
                                      ensemble_weighted == 0 ~ "Não área de nicho",
                                      ensemble_weighted == 1 ~ "Área de nicho")),
                                .progress = TRUE)

area_nicho_futuro

purrr::map(area_nicho_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_d(na.translate = FALSE, direction = -1))

# Mapas de ensemble ----

## Passado ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_passado) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_passado.png",
       height = 10, width = 12)

## Presente ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_presente) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_presente.png",
       height = 10, width = 12)

## Futuro ----

### Cenário 126 ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_futuro[
    ensemble_futuro |>
      names() |>
      stringr::str_detect("126")] |>
      terra::rast() |>
      setNames(ensemble_futuro |>
                 names() %>%
                 .[ensemble_futuro |>
                     names() |>
                     stringr::str_detect("126")] |>
                 stringr::str_remove("ensemble_futuro_126-"))) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_futuro_cenario126.png",
       height = 10, width = 12)

### Cenário 126 ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_futuro[
    ensemble_futuro |>
      names() |>
      stringr::str_detect("245")] |>
      terra::rast() |>
      setNames(ensemble_futuro |>
                 names() %>%
                 .[ensemble_futuro |>
                     names() |>
                     stringr::str_detect("245")] |>
                 stringr::str_remove("ensemble_futuro_245-"))) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_futuro_cenario245.png",
       height = 10, width = 12)

### Cenário 370 ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_futuro[
    ensemble_futuro |>
      names() |>
      stringr::str_detect("370")] |>
      terra::rast() |>
      setNames(ensemble_futuro |>
                 names() %>%
                 .[ensemble_futuro |>
                     names() |>
                     stringr::str_detect("370")] |>
                 stringr::str_remove("ensemble_futuro_370-"))) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_futuro_cenario370.png",
       height = 10, width = 12)

### Cenário 585 ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = ensemble_futuro[
    ensemble_futuro |>
      names() |>
      stringr::str_detect("585")] |>
      terra::rast() |>
      setNames(ensemble_futuro |>
                 names() %>%
                 .[ensemble_futuro |>
                     names() |>
                     stringr::str_detect("585")] |>
                 stringr::str_remove("ensemble_futuro_585-"))) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       name = "Probabilidade ocorrência",
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 20,
                                               frame.colour = "black",
                                               ticks.colour = "black")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  facet_wrap(~lyr) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_probabilidade_ocorrencia_futuro_cenario585.png",
       height = 10, width = 12)

# Mapas de área de nicho ----

## Passado ----

ggplot() +
  geom_sf(data = br, aes(color = "Brasil")) +
  tidyterra::geom_spatraster(data = area_nicho_passado) +
  geom_sf(data = caatinga, aes(color = "Caatinga"),
          fill = NA, linewidth = 0.75) +
  scale_fill_viridis_d(na.translate = FALSE,
                       direction = -1,
                       name = "Nicho",
                       guide = guide_legend(title.position = "top",
                                            title.hjust = 0.5),
                       breaks = c("Não área de nicho", "Área de nicho")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Caatinga" = "orangered"),
                     name = NULL) +
  geom_sf(data = br, linewidth = 0.75, fill = NA, color = "black") +
  coord_sf(xlim = c(-45.07807, -35.06698),
           ylim = c(-16.71256, -2.748381)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_area_nicho_passado.png",
       height = 10, width = 12)
