library(rvest)
library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)
library(glue)
library(zoo)

remove_spec_html_char <- function(x){
  str_replace_all(x, "[\r]|[\t]", "")
}

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



extrair_dados_posts <- function(link_topico_hm){
  
  # link_topico_hm: url para uma pagina de um topico da hm
  ex <- link_topico_hm %>% read_html()
  
  # de cada pagina do topico, montar dataframe com:
  # post_id | post_posicao | usuario_nome | #usuario_url# | usuario_registro | usuario_mensagens | usuario_verdinhas | post_datetime | 
  # post_corpo | post_verdinhas | post_citacao
  
  
  # post_id
  
  # post_posicao
  post_posicao <- ex %>% html_nodes(".postcounter") %>% html_text()
  post_posicao <- str_replace_all(post_posicao, "#| ", "") %>% str_to_lower()
  # detectar existencia de publicdade
  ind_publi <- post_posicao != "publicidade"
  remover_publi <- function(x) x[ind_publi]
  post_posicao <- post_posicao[ind_publi]
  
  # usuario_nome
  usuario_nome <- ex %>% html_nodes("a strong") %>% html_text()
  
  # usuario_url
  # impossivel ate o momento
  
  # usuario_registro
  #(usuario_url <- ex %>% html_nodes(".userinfo") %>% html_nodes("*"))
  xpath_userinfo = '//*[@name="postbittab"]/div/dl/dd'
  usuario_info <- ex %>% html_nodes(xpath = '//*[@class="userinfo"]') %>% html_text() %>% remover_publi()
  usuario_info <- usuario_info %>% str_replace_all("[\n]|[\r]|[\t]", "") %>% str_trim
  # usuario_info_registro
  
  # pensar depois em como fazer scraping quando tem post da moderação no meio
  # ex: http://www.hardmob.com.br/boteco-hardmob/505056-isso-nao-piramide-mmn-274.html
  # ex %>% html_nodes(xpath = '//*[@class="usertitle"]') %>% html_text() 
  
  usuario_info_registro <- usuario_info %>%
    str_extract("(Jan|Fev|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{4}") #%>%
  #  as.yearmon() %>%
  #  as.Date()
  
  usuario_info %<>% str_replace_all("\\.+", "")
  regex_msg <- "Mensagens\\s\\d+"
  regex_verdinhas <- "(Verdinhas\\d+)"
  regex_mencionado <- "(Mencionado\\s\\d+)"
  regex_citado <- "(Citado\\s\\d+)"
  usuario_info_mensagens <- usuario_info %>% str_extract(regex_msg) %>% str_extract("\\d+")
  usuario_info_verdinhas <- usuario_info %>% str_extract(regex_verdinhas) %>% str_extract("\\d+")
  usuario_info_mencionado <- usuario_info %>% str_extract(regex_mencionado) %>% str_extract("\\d+")
  usuario_info_citado <- usuario_info %>% str_extract(regex_citado) %>% str_extract("\\d+")
  
  # extrair post datetime
  post_datetime <- ex %>%
    html_nodes(xpath = '//*[@class="postdate old"]') %>%
    html_text() %>%    remove_spec_html_char() #%>%     dmy_hm(tz = "Brazil/East")
  post_datetime <- post_datetime[ind_publi]
  
  # post_corpo
  post_corpo <- ex %>% html_nodes(xpath = '//*[@class="content"]') %>% html_text()
  # remover publicidade se houver
  post_corpo <- post_corpo[!str_detect(post_corpo, "googletag.cmd.push")]
  # separar citaçao de post original
  post_corpo %<>% str_split("\r\n\t\t\t\r\n\t\t\r\n\t\r\n\r\n")
  # da lista de vetores do corpo, extrair apenas o string sem a palavra citando
  post_citacao <- post_corpo %>%
    map(str_subset, "Citando") %>% 
    map_if(~ length(.) == 0, NA_character_) %>%
    map_chr(1, .default = NA)
  
  
  # extrair o termo entre "Citando" e "\r"
  extrair_membro_citado <- function(x) str_match(x, "Citando (.*?)[\r]")[,2]
  if(all(is.na(post_citacao))){
    post_membro_citado <- post_citacao
  } else{
    post_membro_citado <- extrair_membro_citado(post_citacao)
  }
  
  # extrair o post original sem citação
  post_corpo_original <- post_corpo %>% map_chr(tail, 1) %>% remove_spec_html_char()
  
  
  ### post_verdinhas: extrair quantidade de verdinhas do post
  post_verdinhas <- ex %>%
    html_nodes(xpath = '//*[@class="vbseo_liked"]') %>%
    html_text()
  
  # se o character possui a palavra mais: numero depois do mais + 3
  # se possui virgula e nao possui mais: 3
  # se eh vazio, 0
  # para o resto, use 1
  extrair_verdinha_mais <- function(x) as.numeric(str_match(x, "e mais (.*?)\\.")[,2 ]) + 3
  post_verdinhas <- case_when(
    str_detect(post_verdinhas, "e mais") ~ extrair_verdinha_mais(post_verdinhas),
    str_detect(post_verdinhas, ",") & !str_detect(post_verdinhas, "e mais") ~ 3,
    post_verdinhas == "" ~ 0,
    TRUE ~ 1
  )
  link_topico = rep(link_topico_hm, length(post_posicao))
  # montar dataframe final
  tibble(link_topico,
         post_posicao, usuario_nome, usuario_info_registro, usuario_info_mencionado,
         usuario_info_mensagens, usuario_info_verdinhas, post_datetime,
         post_corpo_original, post_verdinhas, post_membro_citado
  ) 
}
