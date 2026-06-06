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

## Data frame dos valores ----

area_df <- tibble::tibble(Cenário = c("Passado",
                                      "Presente",
                                      rep(c("126", "245", "370", "585"),
                                          each = 4)),
                          Tempo = c(NA,
                                    NA,
                                    rep(c("2021-2040",
                                          "2041-2060",
                                          "2061-2080",
                                          "2081-2100"),
                                        times = 4)),
                          `Área (km²)` = areas_nicho)

area_df

## Gráfico ----

area_df |>
  tidyr::drop_na() |>
  ggplot(aes(Tempo, `Área (km²)`, color = Cenário, fill = Cenário, group = Cenário)) +
  geom_line(linewidth = 2) +
  geom_point(shape = 21, color = "black", size = 5, stroke = 1) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
         fill  = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_hline(yintercept = areas_nicho[[1]],
             color = "royalblue4",
             linewidth = 2,
             linetype = "dashed") +
  geom_label(aes("2021-2040", areas_nicho[[1]], label = "Passado"),
             color = "black",
             fill = "royalblue",
             size = 7.5) +
  geom_hline(yintercept = areas_nicho[[2]],
             color = "tomato4",
             linewidth = 2,
             linetype = "dashed") +
  geom_label(aes("2021-2040", areas_nicho[[2]], label = "Presente"),
             color = "black",
             fill = "tomato",
             size = 7.5) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "areas_nichos_comparacao.png",
       height = 10, width = 12)
