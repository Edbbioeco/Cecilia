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
