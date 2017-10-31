source("setup.R")

arquivos <- dir("data/02b/", full.names = TRUE)

df <- arquivos %>% map(read_rds) %>% bind_rows()


# vetor com urls a serem expurgadas
url_expurgar <- c("http://www.hardmob.com.br/boteco-hardmob/288711-oficialmob-desktopmob-poste-ai-8.html")
df %<>% filter(!link_topico %in% url_expurgar)

# salvar dados

write_rds(df, "data/03-posts.Rds")
# excluir arquivos temporarios
file.remove(arquivos)


df %>% 
  select(starts_with("usuario")) %>% 
  group_by_all() %>% 
  summarise(n = n()) %>% 
  View()
