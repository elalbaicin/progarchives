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

# Extração dos dados ------------------------------------------------------

# Identifique os subgêneros e suas URLs
main <- extract_genres()

# Tome os artistas de cada subgênero e suas URLs
main <- main %>% 
  mutate(artist_data = map(.x = url_genre,
                           .f = extract_artists)) %>% 
  unnest(cols = c(artist_data))

# Tome os lançamentos de cada artista
main <- main %>% 
  mutate(album_data = future_map(.x = url_artist,
                                 .f = insistently(extract_albums,
                                                  rate = rate_backoff()),
                                 .progress = TRUE)) %>% 
  unnest(cols = c(album_data),
         keep_empty = TRUE)

# # Tome a distribuição da notas de cada lançamento - o processo é demorado
# main <- main %>%
#   mutate(ratings = future_map(.x = url_album,
#                               .f = insistently(extract_ratings),
#                               .progress = TRUE)) %>%
#   unnest(cols = c(ratings),
#          keep_empty = TRUE)

# Salve os dados
save(main,
     file = paste0("ProgArchives_",
                   today() %>% 
                     strftime(format = "%Y%m%d"),
                   ".RData"))
