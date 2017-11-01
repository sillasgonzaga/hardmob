source("setup.R")
# mudar locale
loc <- Sys.getlocale(category = "LC_ALL")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

df <- read_rds("data/03-posts.Rds")
glimpse(df)
# corrigir classe das variaveis do dataframe
df$post_posicao %<>% as.integer()
df$usuario_info_verdinhas %<>% as.numeric()
df$usuario_info_mencionado %<>% as.numeric()
df$usuario_info_mensagens %<>% as.numeric()
df$usuario_info_registro %<>% as.yearmon() %>% as.Date()
df$post_corpo_original %<>% remove_spec_html_char()
# corrigir datetime
df$post_datetime %<>% remove_spec_html_char()
df$post_datetime %<>% str_replace_all("[\n]", "")  

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
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  select(-n)
# pegar maximo para remover duplis
df_usuarios %<>%
  filter(!is.na(usuario_info_verdinhas)) %>% 
  group_by(usuario_nome) %>% 
  summarise_all(max, na.rm = TRUE)

# salvar dados limpos
write_rds(df_usuarios, "data/analise-dados-limpos-usuarios.Rds")
write_rds(df, "data/analise-dados-limpos-posts.Rds")

