# Preparação da sessão ----------------------------------------------------

# Pacotes
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(rebus))

# Opção de paralelismo do pacote furrr
plan(multiprocess)

# Carregue as funções coletoras
source("scraping_functions.R")

# Função auxiliar
fill_empty_listcol <- as_mapper(~ if(is.null(.x) || nrow(.x) == 0){
  
  return(NA)
  
} else {
  
  return(.x)
  
})

# Extração dos dados ------------------------------------------------------

# Identifique os subgêneros e suas URLs
main <- extract_genres()

# Tome os artistas de cada subgênero e suas URLs
main <- main %>% 
  mutate(artist_data = future_map(.x = url_genre,
                                  .f = extract_artists,
                                  .progress = TRUE)) %>% 
  unnest(cols = c(artist_data))

# Tome os lançamentos de cada artista
main <- main %>% 
  mutate(album_data = future_map(.x = url_artist,
                                 .f = extract_albums,
                                 .progress = TRUE)) %>% 
  mutate(album_data = map(.x = album_data,
                          .f = fill_empty_listcol)) %>% 
  unnest(cols = c(album_data)) %>% 
  remove_empty(which = "cols")

# # Tome a distribuição da notas de cada lançamento - o processo é demorado
# main <- main %>%
#   mutate(ratings = future_map(.x = url_album,
#                               .f = extract_ratings,
#                               .progress = TRUE)) %>% 
#   mutate(ratings = map(.x = ratings,
#                        .f = fill_empty_listcol)) %>% 
#   unnest(cols = c(ratings)) %>% 
#   remove_empty(which = "cols")

# Salve os dados
save(main,
     file = paste0("ProgArchives_",
                   today() %>% 
                     strftime(format = "%Y%m%d"),
                   ".RData"))
