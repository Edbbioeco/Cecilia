# Pacotes ----

library(sf)

library(tidyverse)

library(magrittr)

library(spThin)

# Dados ----

## Caatinga ----

caatinga <- sf::st_read("shapefile_caatinga.shp")

### Importando ----

caatinga

ggplot() +
  geom_sf(data = caatinga)

### Visualizando ----

## Registros ----

### Importando ----

registros <- readr::read_csv("Cenostigma_pyramidale.csv")

### Visualizando ----

registros

registros |> dplyr::glimpse()

ggplot() +
  geom_sf(data = caatinga) +
  geom_point(data = registros, aes(Longitude, Latitude))

### Excluindo registros repetidos ----

registros %<>%
  dplyr::distinct(Longitude, Latitude, .keep_all = TRUE)

registros

# Excluindo os pontos fora da Caatinga ----

## Transformando os registros em um shapefile ----

registros_sf <- registros |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

registros_sf

ggplot() +
  geom_sf(data = caatinga) +
  geom_sf(data = registros_sf)

## Recortando para a caatinga -----

registros_sf %<>%
  sf::st_intersection(caatinga)

registros_sf

ggplot() +
  geom_sf(data = caatinga) +
  geom_sf(data = registros_sf)

# Excluindo pontos muito próximos ----

## Excluindo ----

registros_sf |>
  sf::st_coordinates() |> 
  as.data.frame() |> 
  dplyr::mutate(sp = "Cenostigma pyramidale") |> 
  dplyr::rename("Longitude" = X,
                "Latitude" = Y) |> 
  spThin::thin(long.col = "Longitude",
               lat.col = "Latitude",
               spec.col = "name", 
               thin.par = 4.65 * 2,
               reps = 5,
               out.dir = getwd())

## Importando os novos registros ----

registros_thin <- readr::read_csv("thinned_data_thin1.csv")
  
registros_thin

# Exportando ----

# devido a não exclusão de nenhum registro, não foi necessário o uso dos registros gerados pelo pacote spThin

registros_sf |>
  sf::st_coordinates() |> 
  as.data.frame() |> 
  dplyr::mutate(sp = "Cenostigma pyramidale") |> 
  dplyr::rename("Longitude" = X,
                "Latitude" = Y) |> 
  readr::write_csv("registros_filtrados.csv")
