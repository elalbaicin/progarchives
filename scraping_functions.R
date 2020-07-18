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
  nodes_albums <- pmap(.l = list(xpath = paste0('//*[@id="main"]/div/div[*]/table[', 1:5, ']')),
                       .f = html_node,
                       x = artist_page)
  
  # Conte quantos elementos h� em cada n�
  td_count <- nodes_albums %>% 
    map(.f = html_nodes,
        xpath = "td") %>% 
    map_int(length)
  
  # Se n�o houver discos registrados, tente um XPath diferente
  if(all(td_count == 0)){
    
    nodes_albums <- pmap(.l = list(xpath = paste0('//*[@id="main"]/div[*]/table[', 1:5, ']')),
                         .f = html_node,
                         x = artist_page)
    
    td_count <- nodes_albums %>% 
      map(.f = html_nodes,
          xpath = "td") %>% 
      map_int(length)
    
  }
  
  # Se ainda n�o houver discos registrados, interrompa a fun��o
  if(all(td_count == 0)){
    
    # Avise que o artista n�o tem discos registrados
    warning(paste0("Artista sem discos registrados. Vide ", url_artist))
    
    # Entregue NULL
    return(NULL)
    
  }
  
  # Extraia o tipo, as notas m�dias, o n�mero de avalia��es e os anos
  data_albums <- nodes_albums %>% 
    map2_df(.y = list("Studio Album",
                      "Live",
                      "DVD/Video",
                      "Boxset/Compilation",
                      "Singles/EPs/Fan Club/Promo"),
            .f = ~ html_nodes(.x,
                              css = "span") %>% 
              html_text() %>% 
              as.numeric() %>% 
              na.omit() %>% 
              matrix(nrow = length(.) / 3,
                     ncol = 3,
                     byrow = TRUE) %>% 
              as_tibble(.name_repair = "minimal") %>% 
              set_names(nm = c("avg_rating",
                               "n_ratings",
                               "year")) %>% 
              mutate(type = .y))
  
  # Nomes e URLs dos �lbuns
  meta_albums <- tibble(album = nodes_albums %>% 
                          map(.f = ~ html_nodes(.x,
                                                css = "strong") %>% 
                                html_text) %>% 
                          unlist(),
                        url_album = nodes_albums %>% 
                          map(.f = ~ html_nodes(.x,
                                                css = "a:nth-child(1)") %>% 
                                html_attr("href")) %>% 
                          unlist() %>% 
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

# EM CONSTRU��O - Extra��o de informa��es do �lbum ------------------------

extract_ratings <- function(url_album){
  
  # Carregue a p�gina de avalia��es/notas
  review_page <- url_album %>%
    str_replace(pattern = "album",
                replacement = "album-reviews") %>% 
    read_html(encoding = "ISO-8859-1")
  
  # Tabela de avalia��es
  review_table <- review_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]')
  
  # Avalia��es
  reviews <- tibble(collaborator = review_table %>% 
                      html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/div[*]/div[1]') %>% 
                      html_text() %>% 
                      str_remove_all(pattern = or(START %R% repeated("\n", lo = 1, hi = 5),
                                                  repeated("\n", lo = 1, hi = 5) %R% END)) %>% 
                      str_split(pattern = "\n") %>% 
                      map_lgl(.f = ~ !is.na(.x[2])),
                    stars = review_table %>% 
                      html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/div[*]/div[2]/img') %>% 
                      html_attr("alt") %>% 
                      str_remove(pattern = " stars") %>% 
                      as.numeric()) %>% 
    mutate(weight = ifelse(collaborator,
                           yes = 20,
                           no = 10))
  
  # Notas
  ratings <- tibble(stars = review_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/ul/li[*]/img') %>% 
    html_attr("alt") %>% 
    str_remove(pattern = " stars") %>% 
    as.numeric()) %>% 
    mutate(weight = 1)
  
  # Crie um dataframe com a URL do lan�amento e seu tipo
  info <- bind_rows(reviews,
                    ratings) %>% 
    mutate(url_album = url_album) %>% 
    select(url_album,
           everything()) %>% 
    count(url_album,
          collaborator,
          weight,
          stars,
          name = "count")
  
  # Entregue o resultado
  return(info)
  
}