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
genres <- extract_genres()

# Tome os artistas de cada subg�nero e suas URLs
artists <- genres$url_genre %>% 
  future_map(.f = extract_artists,
             .progress = TRUE) %>% 
  map_df(.f = ~ .x)

# Tome os lan�amentos de cada artista
albums <- artists$url_artist %>% 
  future_map(.f = extract_albums,
             .progress = TRUE) %>% 
  map_df(.f = ~ .x)

# Tome a distribui��o das notas por lan�amento
ratings <- albums$url_album %>% 
  future_map(.f = extract_ratings,
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