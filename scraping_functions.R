# Extra��o de subg�neros --------------------------------------------------

extract_genres <- function(){
  
  # Carregue a p�gina principal do ProgArchives
  main_page <- read_html("http://www.progarchives.com/")
  
  # Localize os nodes com as p�ginas de cada g�nero
  nodes_genres <- main_page %>% 
    html_nodes(xpath = '//*[@id="navGenre"]/ul/li[*]/a')
  
  # Extraia os nomes e as URLs correspondentes a cada g�nero
  genres <- tibble(date = today(),
                   genre = html_text(x = nodes_genres), 
                   url_genre = html_attr(x = nodes_genres,
                                         "href") %>% 
                     paste0("http://www.progarchives.com", .))
  
}

# Extra��o de artistas ----------------------------------------------------

extract_artists <- function(url_genre){
  
  # Carregue a p�gina de g�nero
  genre_page <- read_html(url_genre,
                          encoding = "ISO-8859-1")
  
  # Identifique o subg�nero
  genre <- genres[genres$url_genre == url_genre, "genre"] %>% 
    unlist() %>% 
    unique()
  
  # Extraia o n� com a tabela de artistas
  node_artists <- genre_page %>% 
    html_node(xpath = '//*[@id="main"]/div[2]/table[3]')
  
  # Caso o n� solicitado n�o exista, procure o substituto
  if(length(node_artists) == 0){
    
    node_artists <- genre_page %>% 
      html_node(xpath = '//*[@id="main"]/div[2]/table[2]')
    
  }
  
  # Extraia as URLs de cada artista
  urls_artists <- node_artists %>% 
    html_nodes(css = "a") %>% 
    html_attr("href") %>% 
    paste0("http://www.progarchives.com/", .)
  
  # Forme a tabela completa de artistas
  artists <- node_artists %>% 
    html_table() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    transmute(date = today(),
              genre = genre,
              artist = bands_artists,
              country,
              url_artist = urls_artists)
  
  # Entregue o resultado
  return(artists)
  
}

# Extra��o de �lbuns ------------------------------------------------------

extract_albums <- function(url_artist){
  
  # Carregue a p�gina do artista
  artist_page <- read_html(url_artist,
                           encoding = "ISO-8859-1")
  
  # Extraia os n�s com as tabelas de �lbuns
  node_albums <- artist_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div/div[*]/table[*]')
  
  # Se n�o houver discos registrados, tente um XPath diferente.
  if(length(node_albums) == 0){
    
    node_albums <- artist_page %>% 
      html_nodes(xpath = '//*[@id="main"]/div[*]/table[*]')
    
  }
  
  # Se ainda n�o houver discos registrados, interrompa a fun��o.
  if(length(node_albums) == 0){
    
    stop(paste0("Artista sem discos registrados. Vide ", url_artist))
    
  }
  
  # Extraia as notas m�dias, o n�mero de avalia��es e os anos
  suppressWarnings(data_albums <- node_albums %>% 
                     html_nodes(css = "span") %>% 
                     html_text() %>% 
                     as.numeric() %>% 
                     .[which(x = !is.na(.))] %>% 
                     matrix(nrow = length(.) / 3,
                            ncol = 3,
                            byrow = TRUE) %>% 
                     as_tibble(.name_repair = "minimal") %>% 
                     set_names(nm = c("avg_rating",
                                      "n_ratings",
                                      "year")))
  
  # Nomes, anos e URLs dos �lbuns
  meta_albums <- tibble(album = node_albums %>% 
                          html_nodes(css = "strong") %>% 
                          html_text(),
                        url_album = node_albums %>% 
                          html_nodes(css = "a:nth-child(1)") %>% 
                          html_attr("href") %>% 
                          paste0("http://www.progarchives.com/", .))
  
  # Re�na os resultados em um �nico dataframe
  albums_df <- bind_cols(meta_albums,
                         data_albums) %>% 
    mutate(url_artist = url_artist) %>% 
    select(url_artist,
           url_album,
           album,
           year,
           everything())
  
  # Entregue o resultado
  return(albums_df)
  
}

# Extra��o dos tipos de �lbum ---------------------------------------------

extract_album_info <- function(url_album){
  
  # Carregue a p�gina e localize a tabela com informa��es do lan�amento
  album_table <- url_album %>%
    read_html(encoding = "ISO-8859-1") %>% 
    html_node(xpath = '//*[@id="main"]/div[1]/div[2]/table')
  
  # Extraia o tipo de lan�amento da tabela e remova detalhes redundantes
  type <- album_table %>% 
    html_node(css = "td:nth-child(2) > strong") %>% 
    html_text() %>% 
    str_remove(pattern = ", released in " %R% repeated(DGT, lo = 4, hi = 4))
  
  # Crie um dataframe com a URL do lan�amento e seu tipo
  info <- tibble(url_album = url_album,
                 type = type)
  
  # Entregue o resultado
  return(info)
  
}