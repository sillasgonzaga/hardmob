source("setup.R")

df_topicos <- read_rds("data/01-df_topicos.Rds")

# extrair conteudo apenas dos 30 topicos mais comentados
tpcs <- df_topicos$link_topico[4:100]

lst_res <- map(tpcs, gerar_url_paginas)
df_res <- bind_rows(lst_res)

# salvar resultados
write_rds(df_res, "data/02-df_paginas_topicos.Rds")


