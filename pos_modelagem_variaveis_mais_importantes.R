# Pacotes ----

library(sdm)

library(tidyverse)

library(ggview)

# Dados ----

## Importando ----

modelo_sdm <- sdm::read.sdm("modelo_sdm.sdm")

## Visualizando ----

modelo_sdm

# Métricas do modelo ----

## Tabela da esttísticas ----

### Criar a tabela ----

modelo_sdm |>
  getEvaluation(stat = c("AUC", "TSS")) |>
  dplyr::mutate(Modelo = rep(c("gam",
                               "glm",
                               "maxent",
                               "maxlike"),
                             each = 5),
                .before = modelID,
                AUC = AUC |> round(2),
                TSS = TSS |> round(2)) |>
  dplyr::group_by(Modelo) |>
  dplyr::mutate(modelID = 1:dplyr::n()) |>
  dplyr::mutate(modelID = paste0(Modelo, "-", modelID)) |>
  dplyr::rename("Id" = 2)

# Curva ROC ----

modelo_sdm |> sdm::roc()

# Importância das variáveis ----

modelo_sdm |> sdm::getVarImp() |> plot() + theme_bw()

# Curva de resposta ----

modelo_sdm |> sdm::getResponseCurve() |> plot() + theme_bw()
