# Pacotes ----

library(sdm)

library(tidyverse)

library(flextable)

library(ggview)

# Dados ----

## Importando ----

modelo_sdm <- sdm::read.sdm("modelo_sdm.sdm")

## Visualizando ----

modelo_sdm

# Métricas do modelo ----

## Tabela da esttísticas ----

### Criar a tabela ----

tabela_auc_tss_flex <- modelo_sdm |>
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
  dplyr::rename("Id" = 2) |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1, j = 2) |>
  flextable::fontsize(size = 12, part = "all")

tabela_auc_tss_flex

### Exportar tabela ----

tabela_auc_tss_flex |>
  flextable::save_as_docx(path = "tabela_auc_tsss.docx")

# Curva ROC ----

## Criando multiplas curvas ROC ----

dados_roc <- purrr::map(c("gam", "glm", "maxent", "maxlike"), \(modelo){

  purrr::map(1:5, \(id){

    modelo_sdm |>
      sdm::getRoc(method = modelo, p = id) |>
      as.data.frame() |>
      dplyr::mutate(algoritimo = modelo,
                    id = id |> as.character()) |>
      dplyr::arrange(sensitivity) |>
      dplyr::mutate(uni = 1:dplyr::n())

    })

  },
  .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::rename("1-Especificidade" = 1)

dados_roc

# Importância das variáveis ----

modelo_sdm |> sdm::getVarImp() |> plot() + theme_bw()

# Curva de resposta ----

modelo_sdm |> sdm::getResponseCurve() |> plot() + theme_bw()
