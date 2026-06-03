# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(sdm)

# Dados ----

## Shapefile da Caatinga ----

### Importando ----

caatinga <- sf::st_read("shapefile_caatinga.shp")

### Visualizando ----

caatinga

ggplot() +
  geom_sf(data = caatinga)

## Registros d ocorrência filtrados ----

### Importando ----

registros <- readr::read_csv("registros_filtrados.csv")

### Visualizando ----

registros

registros |> dplyr::glimpse()

### Transformando em vetor espacial ----

registros_vect <- registros |>
  dplyr::mutate(sp = sp |> stringr::str_replace(" ", "_")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  terra::vect()

registros_vect

## Variáveis bioclimáticas para o presente ----

### Importando ----

bio_presente <- terra::rast("./var_presente/presente.tif")

names(bio_presente) <- c(paste0("Bio0", 1:9),
                         paste0("Bio", 10:19))

### Visualizando ----

bio_presente

ggplot() +
  tidyterra::geom_spatraster(data = bio_presente[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Variáveis bioclimáticas para o futuro ----

### Importando ----

variavel <- list.files(path = "./var_futuro/",
                       full.names = TRUE)

variavel

bio_futuro <- purrr::map(variavel, \(variavel){

  variavel_futura <- terra::rast(variavel)

  names(variavel_futura) <- c(paste0("Bio0", 1:9),
                              paste0("Bio", 10:19))

  variavel_futura

  }) |>
  setNames(list.files(path = "./var_futuro/") |>
             stringr::str_remove(".tif"))

bio_futuro

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = bio_futuro[[1]][[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

## Variáveis bioclimáticas para o passado ----

### Importar ----

bio_passado <- terra::rast("./var_passado/passado.tif")

names(bio_passado) <- c(paste0("Bio0", 1:9),
                         paste0("Bio", 10:19))

### Visualizar ----

bio_passado

ggplot() +
  tidyterra::geom_spatraster(data = bio_passado[[1]]) +
  scale_fill_viridis_c(na.value = NA)

# Excluir variáveis ----

## Presente ----

bio_presente <- bio_presente[[-c(1:3, 5, 6, 8:10, 12:15, 17)]]

bio_presente

## Futuro ----

bio_futuro <- purrr::map(bio_futuro, ~.x[[-c(1:3, 5, 6, 8:10, 12:15, 17)]])

bio_futuro

## Passado ----

bio_passado <- bio_passado[[-c(1:3, 5, 6, 8:10, 12:15, 17)]]

bio_passado

# Criando os modelos ----

## Objeto sdmdata ----

sdmdata <- sdm::sdmData(sp ~ .,
                         train = registros_vect,
                         predictors = bio_presente,
                         bg = list(method = "gRandom", n = 1000))

sdmdata

## Lista de algorítimos ----

sdm::getmethodNames()

## Modelos sdm ----

### Criar multiplos modelos ----

modelos_sdm <- purrr::map(1:10, \(id){

  sdm::sdm(Cenostigma_pyramidale ~ .,
           data = sdmdata,
           methods = c("gam",
                       "glm",
                       "maxent",
                       "maxlike"),
           replication = "sub",
           test.percent = 30,
           n = 5)

  },
  .progress = TRUE) |>
  setNames(paste0("modelo_", 1:10))

modelos_sdm

### Escolher o modelo ----

modelo_escolhido <- modelos_sdm[[purrr::map2(modelos_sdm,
                                             paste0("modelo_", 1:10),
                                             ~sdm::getEvaluation(x = .x) |>
                                               dplyr::mutate(modelo = .y)) |>
                                   dplyr::bind_rows() |>
                                   dplyr::summarise(AUC = AUC |> mean(),
                                                    TSS = TSS |> mean(),
                                                    .by = modelo) |>
                                   dplyr::arrange(AUC |> dplyr::desc(), TSS |>
                                                    dplyr::desc()) |>
                                   dplyr::slice_head(n = 1) |>
                                   dplyr::pull(modelo)]]

modelo_escolhido

## Exportando e importando o modelo ----

modelo_escolhido |> sdm::write.sdm("modelo_sdm.sdm",
                                   overwrite = TRUE)

modelo_sdm <- sdm::read.sdm("modelo_sdm.sdm")

# Predição e ensemble ----

## Presente ----

### Criando ----

predicao_presente <- terra::predict(modelo_sdm,
                                    bio_presente,
                                    overwrite = TRUE)

### Ensemble do modelo ----

ensemble_presente <- sdm::ensemble(modelo_sdm,
                                   newdata = predicao_presente,
                                   setting = list(method = "weighted",
                                                  stat = "AUC"))

### Visualizando ----

ensemble_presente

ggplot() +
  tidyterra::geom_spatraster(data = ensemble_presente) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1))

### Exportando ----

ensemble_presente |> terra::writeRaster("ensemble_presente.tif",
                                        overwrite = TRUE)

## Futuro ----

### Criando a predição e o ensemble ----

ensemble_futuro <- purrr::map(bio_futuro,
                              \(variavel, nome){

  terra::predict(modelo_sdm,
                 variavel,
                 overwrite = TRUE)

  },
  .progress = TRUE) |>
  purrr::map(~sdm::ensemble(modelo_sdm,
                            newdata = .x,
                            setting = list(method = "weighted",
                                           stat = "AUC")),
             .progress = TRUE)

### Visualizando ----

ensemble_futuro

purrr::map(ensemble_futuro, ~ggplot() +
             tidyterra::geom_spatraster(data = .x[[1]]) +
             scale_fill_viridis_c(na.value = NA,
                                  limits = c(0, 1)))

### Exportando ----

purrr::map2(ensemble_futuro,
            ensemble_futuro |> names(),
            ~terra::writeRaster(.x,
                                paste0("ensemble_futuro_",
                                       .y,
                                       ".tif")))

## Passado ----

### Criando ----

predicao_passado <- terra::predict(modelo_sdm,
                                   bio_passado,
                                   overwrite = TRUE)

# Área de nicho ----

## Presente ----

### Criando ----

area_nicho_presente <- sdm::pa(ensemble_presente,
                               modelo_sdm)

### Visualizando ----

area_nicho_presente

ggplot() +
  tidyterra::geom_spatraster(data = area_nicho_presente) +
  scale_fill_viridis_c(na.value = NA,
                       breaks = c(0, 1),
                       limits = c(0, 1))

### Exportando ----

area_nicho_presente |> terra::writeRaster("area_nicho_presente.tif",
                                          overwrite = TRUE)

## Futuro ----

### Criando ----

areas_nichos_futuros <- function(ensembles, nome){

  nicho_futuro <- sdm::pa(ensembles,
                          modelo_sdm)

  assign(paste0("nicho_area_futuro_", nome),
         nicho_futuro,
         envir = globalenv())

}

ensembles <- ls(pattern = "ensemble_futuro_") |>
  mget(envir = globalenv())

nome <- c("2021-2040",
          "2041-2060",
          "2061-2080",
          "2081-2100")

purrr::map2(ensembles, nome, areas_nichos_futuros)

### Visualizando ----

area_nicho_futuro <- ls(pattern = "nicho_area_futuro") |>
  mget(envir = globalenv()) |>
  terra::rast()

area_nicho_futuro

names(area_nicho_futuro) <- c("2021-2040",
                              "2041-2060",
                              "2061-2080",
                              "2081-2100")

ggplot() +
  tidyterra::geom_spatraster(data = area_nicho_futuro) +
  scale_fill_viridis_c(na.value = NA,
                       breaks = c(0, 1),
                       limits = c(0, 1)) +
  facet_wrap(~lyr)

### Exportando ----

area_nicho_futuro |> terra::writeRaster("area_nicho_futuro.tif",
                                        overwrite = TRUE)
