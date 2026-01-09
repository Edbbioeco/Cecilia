#  Pacote ----

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(reshape2)

library(ggview)

# Dados ----

## Registros ----

### Importando ----

registros <- readr::read_csv("registros_filtrados.csv")

### Visualizando ----

registros

registros |> dplyr::glimpse()

### Transformando em shapefile ----

registros_sf <- registros |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

registros_sf

ggplot() +
  geom_sf(data = registros_sf)

## Variaveis ambientais do presente ----

### Importando ----

bioclim <- terra::rast("bioclim_presente_res_2.5_arcmin.tif")

### Visualizando ----

bioclim

ggplot() +
  tidyterra::geom_spatraster(data = bioclim) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

### Renomeando ----

names(bioclim) <- c(paste0("bio0", 1:9),
                    paste0("bio", 10:19))

bioclim

# Multicolinearidade ----

## Extraindo os valores ----

valores <- bioclim |> 
  terra::extract(registros_sf)

valores

## Correlação de Spearman ----

### Criando a matriz ----

multicol <- valores |> 
  dplyr::select(-1) |> 
  cor(method = "spearman") |> 
  as.matrix()

multicol

### Tratando a matriz ----

multicol[multicol |> upper.tri()] <- NA

multicol

### Gráfico de calor ----

multicol |>
  reshape2::melt() |>
  tidyr::drop_na() |>
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não"),
                value = value |> round(2)) |>
  dplyr::filter(igual == "Não") |>
  dplyr::select(Var1, Var2, `Índice de Correlação` = value) |>
  ggplot(aes(Var1, Var2, fill = `Índice de Correlação`, label = `Índice de Correlação`)) +
  geom_tile(color = "black") +
  geom_text() +
  coord_equal() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       guide = guide_colorbar(title.hjust = 0.5,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              barheight = 20)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 15, color = "black", angle = 90, hjust = 1),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

## Variáveis para excluir ----

# Bio01, Bio02, Bio03, Bio05, Bio06, Bio08, Bio09, Bio10, Bio12, Bio13, Bio14, Bio15, Bio17