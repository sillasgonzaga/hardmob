source("setup.R")

azul <- "#01a2d9"
vermelho <- "#90353B"

df <- read_rds("data/analise-dados-limpos-posts.Rds")
df_usuarios <- read_rds("data/analise-dados-limpos-usuarios.Rds")

theme_538 <- function(){
  theme_fivethirtyeight() +
    theme(axis.title = element_text())
}

#### parte 01: a hardmob ####

## Panorama geral sobre a Hardmob: quantos dados foram coletados
n_topicos <- 96
n_paginas <- length(unique(df$link_topico))
n_posts <- nrow(df)
n_usuarios <- length(unique(df$usuario_nome))
glue("Foram coletados dados de {x1} tópicos diferentes, totalizando {x2} páginas, {x3} posts e {x4} usuários",
     x1 = n_topicos, x2 = n_paginas, x3 = n_posts, x4 = n_usuarios)

# posts por mes
df %>% 
  mutate(data = as.Date(post_datetime) %>% floor_date("month")) %>% 
  filter(!is.na(data)) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = n)) +
    geom_line() + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    theme_538() + 
    labs(x = NULL, y = NULL,
         title = "Evolução da quantidade de posts por mês")
ggsave("hm01.png", width = 9)

# grafico de registros de usuario por mes
df_usuarios %>% 
  filter(!is.na(usuario_info_registro)) %>% 
  mutate(data = floor_date(usuario_info_registro, "month")) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = n)) +
    geom_line() + geom_smooth(se = FALSE) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
    labs(x = NULL, y= NULL,
         title = "Novos membros ativos na Hardmob por trimestre") + 
    theme_fivethirtyeight(12)
ggsave("hm02.png", width = 9)

# mosaico dia da semana x hora
df %>% 
  #filter(year(post_datetime) > 2015) %>% 
  mutate(dia_semana = wday(post_datetime, label = TRUE),
         hora = factor(hour(post_datetime), levels = (0:23))) %>% 
  count(dia_semana, hora) %>% 
  ggplot(aes(x = hora, y = dia_semana, fill = n)) + 
    geom_tile() +
    scale_fill_viridis() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, title = "Distribuição de posts por dia da semana e horário",
         #subtitle = "A Hardmob bomba mais justamente no horário comercial! Vão trabalhar, vagabundos!",
         fill = "Quantidade de posts")
ggsave("hm03.png", width = 9)

#### parte 02: usuários ####

# histograma de verdinhas
df_usuarios
ggplot(df_usuarios, aes(x = usuario_info_verdinhas)) +
  geom_histogram(binwidth = 1000) +
  scale_x_continuous(breaks = seq(0, 60000, 5000)) +
  theme_538() +
  labs(x = NULL, y = NULL, 
       title = "Distribuição de verdinhas por membro")
ggsave("hm04.png", width = 9)
mean(df_usuarios$usuario_info_verdinhas < 2000)

# o 5% mais esverdeado contem quantos % de verdinhas?
top5 <- df_usuarios %>% arrange(desc(usuario_info_verdinhas)) %>% head(0.05 * nrow(df_usuarios))
round(100 * sum(top5$usuario_info_verdinhas)/sum(df_usuarios$usuario_info_verdinhas), 1)

# top 20 esverdeados
top_usuarios <- top_n(df_usuarios, 20, usuario_info_verdinhas)
ggplot(top_usuarios, aes(x = reorder(usuario_nome, usuario_info_verdinhas), y = usuario_info_verdinhas)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = usuario_info_verdinhas), hjust = 1.1, color = "white") +
  theme_538() +
  labs(x = NULL, y = "Verdinhas recebidas")
ggsave("hm05.png", width = 9)

# grafico sobre relação entre qtd de verdinhas e de mensagens
df_usuarios %>% 
  arrange(desc(usuario_info_verdinhas)) %>% 
  head(100) %>% 
  mutate(usuario_nome = reorder(usuario_nome, usuario_info_verdinhas)) %>% 
  ggplot(aes(y = usuario_info_verdinhas, x = usuario_info_mensagens)) + 
    geom_point() + 
    geom_text_repel(data = top_usuarios, aes(label = usuario_nome)) +
    #geom_smooth(se = FALSE, method = "lm") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_538() +
    labs(y = "Verdinhas", x = "Posts",
         title = "Relação entre quantidade de verdinhas\n e de posts",
         subtitle = "Membros acima da reta tracejada possuem mais de uma verdinha por post")
ggsave("hm06.png", width = 9)

# melhores taxas de verdinha
df_usuarios %>% 
  filter(usuario_info_mensagens > 0) %>% 
  top_n(100, usuario_info_verdinhas) %>% 
  mutate(taxa = usuario_info_verdinhas/usuario_info_mensagens) %>% 
  arrange(desc(taxa)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(usuario_nome, taxa), y = taxa)) +
    geom_col(fill = azul) + 
    coord_flip() +
    theme_538() +
    labs(x = NULL, y = "Verdinhas / Post",
         title = "Membros da hardmob com maiores taxa de verdinhas por post") +
    theme_538()
ggsave("hm07.png", width = 9)

# piores taxas de verdinhas
df_usuarios %>% 
  filter(usuario_info_mensagens > 0) %>% 
  top_n(100, usuario_info_verdinhas) %>% 
  mutate(taxa = usuario_info_verdinhas/usuario_info_mensagens) %>% 
  arrange(desc(taxa)) %>% 
  tail(20) %>% 
  ggplot(aes(x = reorder(usuario_nome, -taxa), y = taxa)) +
    geom_col(fill = vermelho) + 
    coord_flip() +
  theme_538() +
  labs(x = NULL, y = "Verdinhas / Post",
       title = "Membros da hardmob com piores taxa de verdinhas por post") +
  theme_538()

# membro que mais aparece na primeira pagina dos topicos
df %>% 
  mutate(primeira_pagina = post_posicao <= 25) %>% 
  filter(!is.na(usuario_nome)) %>% 
  group_by(usuario_nome) %>% 
  summarise(qtd_tt = n(),
            qtd_primeira_pag = mean(primeira_pagina)) %>% 
  arrange(desc(qtd_primeira_pag)) %>% 
  ungroup() %>% 
  filter(qtd_tt > 100)


ggsave("hm08.png", width = 9)
# # distribuicao acumulada de verdinhas
# ggplot(df_usuarios, aes(x = usuario_info_verdinhas)) +
#   stat_ecdf(pad = FALSE) +
#   scale_y_continuous(breaks = seq(0, 1, 0.05)) +
#   geom_hline(yintercept = 0.95, alpha = 0.5, linetype = "dotted", color = "red") +
#   theme_538() +
#   labs(y = "Proporção de usuários", x = "Verdinhas recebidas",
#        title = "Distribuição acumulada de verdinhas na Hardmob")


#### parte 03: sna ####
# substituir email protected por Predatar
# a <- bracketX(df$post_membro_citado)
# a[is.na(a)] <- ""
# ind <- which(a == "email protected")
# 
# 
# a <- bracketXtract(df$post_membro_citado, bracket = "square")
# a <- a %>% map_chr(1, .default = "")
# a <- unname(a)
# em <- a[974]
# a[str_detect(a, em)]
# 

# ind <- str_which(df$post_membro_citado, re)
# a2 <- unique(a[ind])
# gsub("")
# 
# grepl("[email protected]", df$post_membro_citado)
# ind <- str_detect(df$post_membro_citado, "email protected")
# ind[is.na(ind)] <- FALSE



# interacoes mais comuns
network <- df %>%
  mutate(topico_id = extrair_topico_id(link_topico)) %>% 
  filter(!is.na(usuario_nome),  !is.na(post_membro_citado)) %>%
  #distinct(topico_id, usuario_nome, post_membro_citado) %>% 
  count(usuario_nome, post_membro_citado, sort = TRUE) %>% 
  set_names(c('citou', "citado", "n"))

# id linhas com [email protected]
ind <- str_which(network$citou, "email")
network$citou[ind] <- "pReD@ToR"

ind <- str_which(network$citado, "email")
network$citado[ind] <- "pReD@ToR"

network %<>% filter(row_number() <= 100)


a <- grid::arrow(type = "closed", length = unit(4, "mm"))

network %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(arrow = a, alpha = 0.25) + 
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), check_overlap = TRUE, repel  = TRUE) +
  theme_void()
ggsave("hm09.png", width = 9, height = 9)



#### Parte 04: Análise de sentimento ####

df_tok <- df %>% 
  unnest_tokens(palavra, post_corpo_original, token = "ngrams", n = 1, drop  = FALSE)

data("sentiLex_lem_PT02")
data("oplexicon_v3.0")
mais_falam <- df %>% 
  count(usuario_nome, sort = TRUE) %>% 
  filter(row_number() <= 50) %>% 
  pull(usuario_nome)

top_verdinha <- df_usuarios %>% 
  arrange(desc(usuario_info_verdinhas)) %>% 
  filter(row_number() <= 100)

df_tok %>% 
  mutate(palavra = rm_accent(palavra)) %>% 
  filter(usuario_nome %in% top_verdinha$usuario_nome) %>% 
  inner_join(oplexicon_v3.0, by = c("palavra" = "term")) %>% 
  group_by(usuario_nome) %>% 
  summarise(sent = mean(polarity)) %>% 
  ungroup() %>% 
  # coluna de cor
  mutate(cor = ifelse(sent >= 0, azul, vermelho)) %>% 
  mutate(usuario_nome = reorder(usuario_nome, sent)) %>% 
  ggplot(aes(x = usuario_nome, y = sent)) +
    geom_col(aes(fill = cor)) + 
    coord_flip() +
    scale_fill_identity() +
    labs(x = NULL, y = "Sentimento médio")
ggsave("hm10.png", height = 14, width = 7)

# investigar Raphok
df_tok %>% 
  filter(usuario_nome == "GRM177") %>% 
  inner_join(oplexicon_v3.0, by = c("palavra" = "term")) %>% 
  group_by(usuario_nome, post_corpo_original) %>% 
  summarise(sent = sum(polarity)) %>% 
  arrange((sent)) %>%
  slice(1) %>% 
  pull(post_corpo_original) %>% 
  cat()

# quantidade total de palavras vs quantidade distinta de palavras
# df_tok %>% 
#   group_by(usuario_nome) %>% 
#   summarise(palavras_tt = n(),
#             palavras_dist = n_distinct(palavra)) %>%
#   mutate(taxa = palavras_dist/palavras_tt) %>% 
#   top_n(50, palavras_tt) %>% 
#   ggplot(aes(x = palavras_tt, y = palavras_dist, color = taxa)) +
#   geom_point() +
#   geom_smooth(se = FALSE, method = "lm")
# 

# td idf
usuarios_tf_idf <- c("edgrd", "GRM177")
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
  facet_wrap(~ usuario_nome, scales =  "free") +
  theme_538() +
  labs(x = NULL, y = NULL,
       title = "Comparação léxica entre membros de \nsentimentos opostos")
ggsave("hm11.png", width = 9, height = 9)

# quais membros falam mais menciona a dilma
palavroes <- c("porra", "caralho", "buceta", "merda", "bosta", "puta", "viado", "foder", "fuder", "foda")

sum(df_tok$palavra == 'lula')

# trocar palavra abaixo por puta
df_tok %>% 
  filter(palavra == 'mangina') %>% 
  count(usuario_nome, sort = TRUE) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(usuario_nome, n), y = n)) + 
    geom_col() +
    coord_flip() +
    theme_538() +
    labs(x = NULL, y = "Quantidade",
         title = "Top 10 membros em uso da\n palavra 'mangina'")
ggsave("hm12.png", width = 9, height = 9)


### requests de usuarios
x <- df_tok %>% 
  filter(usuario_nome == "JSquid") %>% 
  pull(palavra) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, c(stopwords("pt"))) #%>% tm_map(stemDocument, language = "portuguese")
  
wordcloud(x, max.words = 100)


##### v2: maior tempo sem postar
df %>% 
  filter(usuario_nome %in% top_verdinha$usuario_nome) %>% 
  arrange(usuario_nome, post_datetime) %>% 
  mutate(post_datetime = as.POSIXct(post_datetime)) %>% 
  mutate(diff = difftime(post_datetime, lag(post_datetime), units = "hours"),
         diff = as.numeric(diff)) %>% 
  select(usuario_nome, post_datetime,  diff) %>% 
  group_by(usuario_nome) %>% 
  summarise(qtd = n(), max_diff = max(diff, na.rm = TRUE)) %>% 
  View()
  



