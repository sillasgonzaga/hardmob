source("setup.R")

df <- read_rds("data/03-posts.Rds")
glimpse(df)
# limpar dados
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

# extrair dataframe de usuarios
df_usuarios <- df %>% select(starts_with("usuario")) %>% unique()

