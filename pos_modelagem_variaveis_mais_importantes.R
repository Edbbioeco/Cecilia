# Pacotes ----

library(sdm)

library(tidyverse)

# Dados ----

## Importando ----

modelo_sdm <- sdm::read.sdm("modelo_sdm.sdm")

## Visualizando ----

modelo_sdm

# Métricas do modelo ----

modelo_sdm |> getEvaluation(stat = c("AUC", "TSS"))

# Curva ROC ----

modelo_sdm |> sdm::roc()

# Importância das variáveis ----

modelo_sdm |> sdm::getVarImp() |> plot() + theme_bw()

# Curva de resposta ----

modelo_sdm |> sdm::getResponseCurve() |> plot() + theme_bw()
