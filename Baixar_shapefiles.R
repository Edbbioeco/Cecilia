# Pacotes -----

library(geobr)

library(tidyverse)

library(sf)

# Dados -----

## Caatinga ----

### Importando ----

caatinga <- geobr::read_biomes() |> 
  dplyr::filter(name_biome == "Caatinga")

### Visualizando ----

caatinga

ggplot() +
  geom_sf(data = caatinga)

## Brasil ----

### Importando ----

brasil <- geobr::read_state()

### Visualizando ----

brasil

ggplot() +
  geom_sf(data = brasil)

# Exportando -----

## Caatinga ----

caatinga |> sf::st_write("shapefile_caatinga.shp")

## Brasil ----

brasil |> sf::st_write("shapefile_brasil.shp")
