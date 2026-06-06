# Pacotes ----

library(bib2df)

library(tidyverse)

# Checar padrão ----

bib2df::bib2df("C:/Users/LENOVO/OneDrive/Documentos/bib/export.bib") |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::filter(TITLE |> stringr::str_detect("terra"))
