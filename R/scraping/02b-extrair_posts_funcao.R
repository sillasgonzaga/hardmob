source("setup.R")

df_topicos <- readRDS("data/02-df_paginas_topicos.Rds")


# rodar loop e salvar resultados
urls <- unique(df_topicos$pgs_topico)

for (i in 3205:length(urls)){
  print(i)
  
  url_loop <- urls[i]
  df_loop <- extrair_dados_posts(url_loop)
  
  ind_loop <- str_pad(i, width = 4, pad =  "0")
  arquivo <- paste0("data/02b/", ind_loop, ".Rds")
  write_rds(df_loop, arquivo)
}
