source("setup.R")

url_boteco <- "http://www.hardmob.com.br/boteco-hardmob/"
css_ult_pagina <- ".first_last a"

url_boteco %<>% read_html()
bot_ult_pagina <- url_boteco %>% html_nodes(css_ult_pagina) %>% html_attr("href") %>% unique()

# gerar vetor de páginas do boteco
ult_pagina_ind <- as.numeric(str_extract(bot_ult_pagina, "[0-9]{2}"))
vetor_pg <- 1:ult_pagina_ind
vetor_pg <- glue("http://www.hardmob.com.br/boteco-hardmob/index{vetor_pg}.html")

# extrair dados sobre cada topico da pagina:
# url_topico | titulo_topico 
extrair_topicos_de_pagina <- function(pg_boteco){
  # input do tipo: "http://www.hardmob.com.br/boteco-hardmob/index2.html"
  pg_boteco <- pg_boteco %>% read_html()
  list_topico <- pg_boteco %>% 
    html_nodes(".title")
  
  vetor_link_topico <- list_topico %>% html_attr("href")
  vetor_nome_topico <- list_topico %>% html_text()
  
  vetor_qtd_posts <- pg_boteco %>% html_nodes(".alt .understate") %>% html_text()
  vetor_qtd_posts <- as.numeric(str_replace_all(vetor_qtd_posts, "[.]", ""))
  
  tibble(link_topico = vetor_link_topico,
         titulo_topico = vetor_nome_topico,
         qtd_posts = vetor_qtd_posts)
}

# x <- extrair_topicos_de_pagina(vetor_pg[1])

extrair_topicos_de_pagina_pos <- possibly(extrair_topicos_de_pagina, otherwise = NA)


system.time({
  df_topicos <- vetor_pg %>% map(extrair_topicos_de_pagina_pos)
})

df_topicos_bind <- df_topicos %>%
  bind_rows(df_topicos) %>%
  unique() %>%
  arrange(desc(qtd_posts)) %>% 
  filter(!is.na(qtd_posts))

df_topicos_bind %>% 
  filter(qtd_posts < 10000) %>% 
  ggplot(aes(x = qtd_posts)) + 
   stat_ecdf() + 
    geom_hline(yintercept = 0.9)

# distribuiçao de resultados:
df_topicos_bind %>% 
  select(link_topico, qtd_posts) %>% 
  mutate(cums = cumsum(qtd_posts)/sum(qtd_posts),
         ind = row_number()) %>% 
  filter(cums <= 0.80)
  
# salvar dataframe
df_topicos_bind %>% write_rds("data/01-df_topicos.Rds")

