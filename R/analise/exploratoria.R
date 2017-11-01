source("setup.R")

df <- read_rds("data/analise-dados-limpos-posts.Rds")
df_usuarios <- read_rds("data/analise-dados-limpos-usuarios.Rds")

theme_538 <- function(){
  theme_fivethirtyeight() +
    theme(axis.title = element_text())
}

## Panorama geral sobre a Hardmob: quantos dados foram coletados
n_topicos <- 96
n_paginas <- length(unique(df$link_topico))
n_posts <- nrow(df)
n_usuarios <- length(unique(df$usuario_nome))
glue("Foram coletados dados de {x1} tópicos diferentes, totalizando {x2} páginas, {x3} posts e {x4} usuários",
     x1 = n_topicos, x2 = n_paginas, x3 = n_posts, x4 = n_usuarios)

# grafico de registros de usuario por mes
df_usuarios %>% 
  filter(!is.na(usuario_info_registro)) %>% 
  mutate(data = floor_date(usuario_info_registro, "quarter")) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = n)) +
    geom_line() + geom_smooth() +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
    labs(x = NULL, y= NULL,
         title = "Novos membros ativos na Hardmob por trimestre") + 
    theme_fivethirtyeight(12)


# histograma de verdinhas
ggplot(df_usuarios, aes(x = usuario_info_verdinhas)) +
  geom_histogram()

# distribuicao acumulada de verdinhas
ggplot(df_usuarios, aes(x = usuario_info_verdinhas)) +
  stat_ecdf(pad = FALSE) +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  geom_hline(yintercept = 0.95, alpha = 0.5, linetype = "dotted", color = "red") +
  theme_538() +
  labs(y = "Proporção de usuários", x = "Verdinhas recebidas",
    title = "Distribuição acumulada de verdinhas na Hardmob")

# o 5% mais esverdeado contem quantos % de verdinhas?
top5 <- df_usuarios %>% arrange(desc(usuario_info_verdinhas)) %>% head(0.05 * nrow(df_usuarios))
round(100 * sum(top5$usuario_info_verdinhas)/sum(df_usuarios$usuario_info_verdinhas), 1)

# grafico de mensagens por dia
df %>% 
  count(ano = year(post_datetime), data = as.Date(post_datetime)) %>% 
  ggplot(aes(y = n, x = as.factor(ano))) + 
    geom_jitter() + 
    scale_y_continuous(breaks = seq(0, 500, 50))

df

df %>% 
  mutate(data = as.Date(post_datetime) %>% floor_date("month")) %>% 
  filter(!is.na(data)) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = n)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# sazonalidade
df %>% 
  group_by(as.Date(post_datetime), mes = weekdays(post_datetime)) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  #mutate(mes = factor(mes, 1:12)) %>% 
  #filter(n < 100) %>% 
  ggplot(aes(x = mes, y = n)) + 
    geom_boxplot()


# interacoes mais comuns
network <- df %>% 
  filter(!str_detect(usuario_nome, "protected") & !str_detect(post_membro_citado, "protected")) %>% 
  filter(!is.na(usuario_nome),  !is.na(post_membro_citado)) %>% 
  count(usuario_nome, post_membro_citado, sort = TRUE) %>% 
  filter(row_number() <= 100)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

network %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(arrow = a) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel  = TRUE)


# hora media de post
df %>% 
  mutate(horario = hour(post_datetime),
         data = floor_date(post_datetime, "month")) %>% 
  group_by(data) %>% 
  summarise(m = mean(horario)) %>% 
  ggplot(aes(x = data, y= m)) +
  geom_line()


# analise de sentimento
library(lexiconPT)
library(tidytext)

df_tok <- df %>% 
  unnest_tokens(palavra, post_corpo_original, token = "ngrams", n = 1, drop  = FALSE)

data("sentiLex_lem_PT02")
mais_falam <- df %>% 
  count(usuario_nome, sort = TRUE) %>% 
  filter(row_number() <= 50) %>% 
  pull(usuario_nome)

top_verdinha <- df_usuarios %>% 
  arrange(desc(usuario_info_verdinhas)) %>% 
  filter(row_number() <= 100)

df_tok %>% 
  filter(usuario_nome %in% mais_falam) %>% 
  inner_join(sentiLex_lem_PT02, by = c("palavra" = "term")) %>% 
  group_by(usuario_nome) %>% 
  summarise(sent = mean(polarity)) %>% 
  ungroup() %>% 
  mutate(usuario_nome = reorder(usuario_nome, sent)) %>% 
  ggplot(aes(x = usuario_nome, y = sent)) +
  geom_col() + 
  coord_flip()

# investigar Raphok
df_tok %>% 
  filter(usuario_nome == "Raphok") %>% 
  inner_join(sentiLex_lem_PT02, by = c("palavra" = "term")) %>% 
  group_by(usuario_nome, post_corpo_original) %>% 
  summarise(sent = sum(polarity)) %>% 
  arrange((sent)) %>%
  slice(1) %>% 
  pull(post_corpo_original)

# quantidade total de palavras vs quantidade distinta de palavras
df_tok %>% 
  group_by(usuario_nome) %>% 
  summarise(palavras_tt = n(),
            palavras_dist = n_distinct(palavra)) %>%
  mutate(taxa = palavras_dist/palavras_tt) %>% 
  top_n(50, palavras_tt) %>% 
  ggplot(aes(x = palavras_tt, y = palavras_dist, color = taxa)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")


df %>% 
  #filter(usuario_nome %in% mais_falam) %>% 
  mutate(tamanho = nchar(post_corpo_original)) %>% 
  group_by(usuario_nome) %>% 
  summarise(m = median(tamanho)) %>%
  ggplot(aes(x = m)) +
  geom_histogram()

usuarios_tf_idf <- c("Bobleo", "Isaac")

df_tok %>% 
  filter(usuario_nome %in% usuarios_tf_idf) %>% 
  count(usuario_nome, palavra) %>% 
  bind_tf_idf(palavra, usuario_nome, n) %>% 
  group_by(usuario_nome) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(row_number() <= 10) %>% 
  ggplot(aes(x = palavra, y = tf_idf)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ usuario_nome, scales =  "free")

