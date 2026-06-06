# Pacote ----

library(gert)

# Status ----

gert::git_status() |>
  as.data.frame()

# Adiconar arquivo ----

gert::git_add(files = "git_commands.R")

# Commitando ----

gert::git_commit("Script para comandos de git")

# Pusshando ----

gert::git_push(remote = "origin")

# Pullando ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft(ref = "HEAD~1")
