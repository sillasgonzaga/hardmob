source("setup.R")

df_topicos <- read_rds("data/01-df_topicos.Rds")

# gerar todas url para todas as paginas do topico
gerar_url_paginas <- function(link_topico){
  ex <- link_topico
  
  qtd_paginas <- ex %>% 
    read_html() %>% 
    html_nodes(".first_last a , #yui-gen13") %>% 
    html_attr("href") %>% 
    .[1]
  
  qtd_paginas <- str_extract_all(qtd_paginas, "[0-9]*.html") %>%
    str_replace_all("\\..*", "") %>% 
    as.numeric()
  
  # para anexar a qtd de paginas no link, remover html para depois colocar de volta
  x <- str_replace(ex, "\\b.html\\b", "")
  ind <- 1:qtd_paginas
  pgs_topico <- glue(x, "-{ind}.html")
  
  link_topico <- rep(link_topico, length(pgs_topico))
  tibble(link_topico, pgs_topico)
}

# extrair conteudo apenas dos 30 topicos mais comentados
tpcs <- df_topicos$link_topico[4:30]

lst_res <- map(tpcs, gerar_url_paginas)
df_res <- bind_rows(lst_res)

# salvar resultados
write_rds(df_res, "data/02-df_paginas_topicos.Rds")


# # transformar codigo abaixo em função
# ex <- df_topicos$link_topico[10]
# # gerar links com todas as paginas do topico
# qtd_paginas <- ex %>% 
#   read_html() %>% 
#   html_nodes(".first_last a , #yui-gen13") %>% 
#   html_attr("href") %>% 
#   .[1]
# 
# qtd_paginas <- str_extract_all(qtd_paginas, "[0-9]*.html") %>%
#   str_replace_all("\\..*", "") %>% 
#   as.numeric()
# 
# # para anexar a qtd de paginas no link, remover html para depois colocar de volta
# x <- str_replace(ex, "\\b.html\\b", "")
# ind <- 1:qtd_paginas
# pgs_topico <- glue(x, "-{ind}.html")
# 
