# Pacote ----

library(gert)

# Adiconar arquivo ----

gert::git_add(list.files(pattern = "git_commands"))

# Commitando ----

gert::git_commit("Script para comandos de git")

# Pusshando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()