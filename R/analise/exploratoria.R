source("setup.R")


df <- read_rds("data/03-posts.Rds")
glimpse(df)
# limpar dados
df$usuario_info_verdinhas %<>% as.numeric()
df$post_datetime %<>% remove_spec_html_char()
df$post_corpo_original %<>% remove_spec_html_char()

df$post_datetime %<>% str_replace_all("[\n]", "")  

# corrigir datetime
dt <- df$post_datetime

# corrigir ontem e hoje por data certa
dt %<>% str_replace_all("Ontem", "29-10-2017")
dt %<>% str_replace_all("Hoje", "30-10-2017")
# converter para datetime
dt %<>% dmy_hm()
df$post_datetime <- dt
# Consertar Sodoma
df$usuario_nome %<>% str_replace_all("&amp;", "&")
df$post_membro_citado %<>% str_replace_all("&amp;", "&")

# extrair dataframe de usuarios
df_usuarios <- df %>% select(starts_with("usuario"))
# remover duplicatas
df_usuarios %<>%
  group_by_all() %>%
  summarise(n = n())

df_usuarios %<>%
  group_by(usuario_nome) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

df_usuarios$usuario_info_verdinhas %<>% as.numeric()

# grafico de registros de usuario por mes
df_usuarios %>% 
  filter(!is.na(usuario_info_registro)) %>% 
  mutate(data = floor_date(usuario_info_registro, "month")) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = nn)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%y")



# histograma de verdinhas
ggplot(df_usuarios, aes(x = usuario_info_verdinhas)) +
  geom_histogram()


# grafico de mensagens por dia
df %>% 
  mutate(data = as.Date(post_datetime) %>% floor_date("month")) %>% 
  filter(!is.na(data)) %>% 
  count(data) %>% 
  ggplot(aes(x = data, y = n)) +
  geom_line()

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

