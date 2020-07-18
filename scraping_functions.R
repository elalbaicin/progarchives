# Extração de subgêneros --------------------------------------------------

extract_genres <- function(){
  
  # Carregue a página principal do ProgArchives
  main_page <- read_html("http://www.progarchives.com/")
  
  # Localize os nodes com as páginas de cada gênero
  nodes_genres <- main_page %>% 
    html_nodes(xpath = '//*[@id="navGenre"]/ul/li[*]/a')
  
  # Extraia os nomes e as URLs correspondentes a cada gênero
  genres <- tibble(date = today(),
                   genre = html_text(x = nodes_genres), 
                   url_genre = html_attr(x = nodes_genres,
                                         "href") %>% 
                     paste0("http://www.progarchives.com", .))
  
}

# Extração de artistas ----------------------------------------------------

extract_artists <- function(url_genre){
  
  # Carregue a página de gênero
  genre_page <- read_html(url_genre,
                          encoding = "ISO-8859-1")
  
  # Identifique o subgênero
  genre <- genres[genres$url_genre == url_genre, "genre"] %>% 
    unlist() %>% 
    unique()
  
  # Extraia o nó com a tabela de artistas
  node_artists <- genre_page %>% 
    html_node(xpath = '//*[@id="main"]/div[2]/table[3]')
  
  # Caso o nó solicitado não exista, procure o substituto
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

# Extração de álbuns ------------------------------------------------------

extract_albums <- function(url_artist){
  
  # Carregue a página do artista
  artist_page <- read_html(url_artist,
                           encoding = "ISO-8859-1")
  
  # Extraia os nós com as tabelas de álbuns
  nodes_albums <- pmap(.l = list(xpath = paste0('//*[@id="main"]/div/div[*]/table[', 1:5, ']')),
                       .f = html_node,
                       x = artist_page)
  
  # Conte quantos elementos há em cada nó
  td_count <- nodes_albums %>% 
    map(.f = html_nodes,
        xpath = "td") %>% 
    map_int(length)
  
  # Se não houver discos registrados, tente um XPath diferente
  if(all(td_count == 0)){
    
    nodes_albums <- pmap(.l = list(xpath = paste0('//*[@id="main"]/div[*]/table[', 1:5, ']')),
                         .f = html_node,
                         x = artist_page)
    
    td_count <- nodes_albums %>% 
      map(.f = html_nodes,
          xpath = "td") %>% 
      map_int(length)
    
  }
  
  # Se ainda não houver discos registrados, interrompa a função
  if(all(td_count == 0)){
    
    # Avise que o artista não tem discos registrados
    warning(paste0("Artista sem discos registrados. Vide ", url_artist))
    
    # Entregue NULL
    return(NULL)
    
  }
  
  # Extraia o tipo, as notas médias, o número de avaliações e os anos
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
  
  # Nomes e URLs dos álbuns
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
  
  # Reúna os resultados em um único dataframe
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

# EM CONSTRUÇÃO - Extração de informações do álbum ------------------------

extract_album_info <- function(url_album){
  
  # Carregue a página de avaliações/notas
  review_page <- url_album %>%
    str_replace(pattern = "album",
                replacement = "album-reviews") %>% 
    read_html(encoding = "ISO-8859-1")
  
  # Tabela de avaliações
  review_table <- review_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]')
  
  # Avaliações
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
  
  # Crie um dataframe com a URL do lançamento e seu tipo
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
