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

## Criar modelo nulo ----

modelo_nulo_roc <- data.frame(`1-Especificidade` = seq(0, 1,
                                                       length.out = dados_roc$un |>
                                                         max()),
                              sensibilidade = seq(0, 1,
                                                  length.out = dados_roc$un |>
                                                    max())) |>
  dplyr::rename("1-Especificidade" = 1)

modelo_nulo_roc

### Gráfico ----

ggplot(data = dados_roc,
       aes(`1-Especificidade`, sensitivity, color = id)) +
  geom_line() +
  scale_color_manual(values = rep("black", 5)) +
  geom_line(data = modelo_nulo_roc, aes(x = `1-Especificidade`,
                                        y = sensibilidade),
            color = "blue", linetype = "dashed", linewidth = 1,
            inherit.aes = FALSE) +
  facet_wrap(~algoritimo) +
  labs(y = "Sensividade") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.position = "one",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(color = "gray", linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "curva_roc.png",
       height = 10, width = 12)

# Importância das variáveis ----

## Extrair dados ----

var_imp <- modelo_sdm |>
  sdm::getVarImp() %>%
  .@varImportanceMean |>
  purrr::imap(~.x |>
                dplyr::rename(`Importância relativa` = 2) |>
                dplyr::mutate(tipo = .y)) |>
  dplyr::bind_rows()

var_imp

## Gráfico ----

var_imp |>
  ggplot(aes(`Importância relativa`, variables)) +
  geom_errorbar(aes(xmin = lower,
                    xmax = upper,
                    y = variables),
                width = 0.25, linewidth = 1,
                color = "blue") +
  geom_point(color = "black", stroke = 1, size = 5) +
  labs(y = "Variável") +
  facet_wrap(~tipo, scales = "free_x") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.position = "one",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(color = "gray", linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "importancia_variaveis.png",
       height = 10, width = 12)

# Curva de resposta ----

## Extrair valores ----

var_resposta <- modelo_sdm |>
  sdm::getResponseCurve() |>
  (\(rc) rc@response)() |>
  purrr::imap(~.x |>
                dplyr::rename("Gradiente" = 1) |>
                dplyr::mutate(Variavel = .y,
                              .before = Variavel),
              .progress = TRUE) |>
  dplyr::bind_rows() |>
  tidyr::pivot_longer(cols = 2:21,
                      names_to = "variavel",
                      values_to = "Resposta do modelo")

var_resposta

## Gráfico ----

var_resposta |>
  ggplot(aes(Gradiente, `Resposta do modelo`, )) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Variavel, scales = "free") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.position = "one",
        strip.text = element_text(size = 20, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(color = "gray", linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)
