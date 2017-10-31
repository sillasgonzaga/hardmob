source("setup.R")

arquivos <- dir("data/02b/", full.names = TRUE)

df <- arquivos %>% map(read_rds) %>% bind_rows()

# salvar dados

write_rds(df, "data/03-posts.Rds")
# excluir arquivos temporarios
file.remove(arquivos)
