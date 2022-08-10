
# Calcular as métricas - DADOS ESTÁTICOS ----------------------------------

library(tidyverse)
library(magrittr)

# Carregar funções a serem utilizadas
source("code/calc_cop_area.R")

# Obter dados do Momento 1 ------------------------------------------------

# Caminho dos arquivos do momento 2
path_m1 <- "../dados/Dados da plataforma/Dados da plataforma M1/"

# Lista de arquivos
M1 <- list.files(path_m1, recursive = T)
M1 <- M1[!grepl(".docx", M1)]

# Nomes dos arquivos
fn <- sapply(strsplit(M1, "\\/"), "[[", 3)

# Criar metadado com as informações dos indivíduos para o M1
meta_m1 <- data.frame(
  momento = 1, 
  grupo   = sapply(strsplit(M1, "\\/"), "[[", 1),
  ind     = sapply(strsplit(M1, "\\/"), "[[", 2),
  id      = paste0("Ind", as.integer(as.factor(sapply(strsplit(M1, "\\/"), "[[", 2)))),
  fn      = fn,
  path    = M1,
  trat    = tolower(sapply(strsplit(fn, "_"), "[[", 3))
)

# Calcular as métricas para cada paciente do momento 1
map_df(1:nrow(meta_m1), ~ {
  
  df_f <- filter_cop(
    file_name = paste0(path_m1, meta_m1$path[.x]),
    probs     = c(0.1, 0.9)
  )
  
  df_a <- calculate_area_cop(df_f)
  
  res <- data.frame(t(df_a), id = meta_m1$id[.x], trat = meta_m1$trat[.x])
  res
  
}) -> m1

# Obter dados do Momento 2 ------------------------------------------------

# Caminho dos arquivos do momento 2
path_m2 <- "dados/Dados da plataforma/Dados da plataforma M2/"

# Lista de arquivos
M2 <- list.files(path_m2, recursive = T)
M2 <- M2[!grepl(".docx", M2)]

# Nomes dos arquivos
fn <- sapply(strsplit(M2, "\\/"), "[[", 3)

# Criar metadado com as informações dos indivíduos para o M2
meta_m2 <- data.frame(
  momento = 2, 
  grupo   = sapply(strsplit(M2, "\\/"), "[[", 1),
  ind     = sapply(strsplit(M2, "\\/"), "[[", 2),
  id      = paste0("Ind", as.integer(as.factor(sapply(strsplit(M2, "\\/"), "[[", 2)))),
  fn      = fn,
  path    = M2,
  trat    = tolower(sapply(strsplit(fn, "_"), "[[", 3))
)

# Remover os dados de Primiano Francisco, checar se estão corretos:
meta_m2 %<>% 
  filter(!ind == "Primiano Francisco")

# Calcular as métricas para cada paciente do momento 2
map_df(1:nrow(meta_m2), ~ {
  
  #print(paste(meta_m2$path[.x]))
  df_f <- filter_cop(
    file_name = paste0(path_m2, meta_m2$path[.x]),
    probs     = c(0.1, 0.9)
  )
  
  df_a <- calculate_area_cop(df_f)
  
  res <- data.frame(t(df_a), id = meta_m2$id[.x], trat = meta_m2$trat[.x])
  res
  
}) -> m2

# Salvar resultados
save(m1, meta_m1, m2, meta_m2, file = "output/biomecanica_processado.RData")
