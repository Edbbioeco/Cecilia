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

prob_presente <- terra::rast("ensemble_presente.tif")

#### Visualizando ----

prob_presente

ggplot() +
  geom_sf(data = br) +
  tidyterra::geom_spatraster(data = prob_presente) +
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

prob_futuro <- purrr::map(list.files(pattern = "^ensemble_futuro_"),
                          terra::rast,
                          .progress = TRUE) |>
  setNames(list.files(pattern = "^ensemble_futuro_") |>
             stringr::str_remove(".tif$"))

#### Visualizando ----

prob_futuro

purrr::map(prob_futuro, ~ggplot() +
             geom_sf(data = br) +
             tidyterra::geom_spatraster(data = .x) +
             geom_sf(data = caatinga, fill = NA) +
             scale_fill_viridis_c(na.value = NA),
           .progress = TRUE)

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
