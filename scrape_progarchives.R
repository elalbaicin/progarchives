# Prepara��o da sess�o ----------------------------------------------------

# Pacotes
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(rebus))

# Op��o de paralelismo do pacote furrr
plan(multiprocess)

# Carregue as fun��es coletoras
source("scraping_functions.R")

# Extra��o dos dados ------------------------------------------------------

# Identifique os subg�neros e suas URLs
main <- extract_genres()

# Tome os artistas de cada subg�nero e suas URLs
main <- main %>% 
  mutate(artist_data = map(.x = url_genre,
                           .f = extract_artists)) %>% 
  unnest(cols = c(artist_data))

# Tome os lan�amentos de cada artista
main <- main %>% 
  mutate(album_data = future_map(.x = url_artist,
                                 .f = insistently(extract_albums,
                                                  rate = rate_backoff()),
                                 .progress = TRUE)) %>% 
  unnest(cols = c(album_data),
         keep_empty = TRUE)

# # Tome a distribui��o da notas de cada lan�amento - o processo � demorado
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
