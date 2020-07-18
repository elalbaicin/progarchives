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
genres <- extract_genres()

# Tome os artistas de cada subgênero e suas URLs
artists <- genres$url_genre %>% 
  future_map(.f = extract_artists,
             .progress = TRUE) %>% 
  map_df(.f = ~ .x)

# Tome os lançamentos de cada artista
albums <- artists$url_artist %>% 
  future_map(.f = extract_albums,
             .progress = TRUE) %>% 
  map_df(.f = ~ .x)

# Tome a distribuição das notas por lançamento
album_info <- albums$url_album %>% 
  future_map(.f = extract_album_info,
             .progress = TRUE) %>% 
  map_df(.f = ~ .x)

# Dataframe principal
main <- albums %>% 
  left_join(artists,
            by = "url_artist")

# Salve os dados
save(main,
     file = paste0("ProgArchives_",
                   today() %>% 
                     strftime(format = "%Y%m%d"),
                   ".RData"))
