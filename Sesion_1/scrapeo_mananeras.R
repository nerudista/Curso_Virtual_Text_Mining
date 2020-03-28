pacman::p_load(tidyverse, rvest, stringr)


# Usar Selector Gadget: encontramos que los títulos de las entradas,
# responden al selector CSS .entry-title a

links_totales <- NULL #Objeto vacío
links <- NULL #Objeto vacío


for(i in 1:5){
  
  cat(i, "...")
  
  url <- paste0("https://lopezobrador.org.mx/transcripciones/page/", i)
  
  pg <- read_html(url) ### Lee todo el código fuente
  
  links <- pg %>% 
    html_nodes(".entry-title a") %>% # Ubicación del selector
    html_attr("href") %>%  # Enlaces a todos los discursos de AMLO
    as_tibble()
  
  links_totales <- bind_rows(links_totales, links)
}

#write.csv(links_totales, "links_totales.csv")


##### Obtener el texto de cada discurso

base <-  NULL
bind <- NULL


for (x in 1:nrow(links_totales)) {
  
  cat(x, "...") #Contador
  
  url <- links_totales$value[x]
  
  pg <- read_html(url) 
  
  titulo <- pg %>% 
    html_node(".entry-title") %>% 
    html_text()
  
  if(length(titulo) == 0) {
    titulo <- NA 
  }
  
  texto <- pg %>% 
    html_nodes(".p1") %>% ## Selector para el párrafo
    html_text() %>% 
    na.omit()
  
  if(length(texto) == 0) {
    texto <- NA 
  }
  
  bind <- tibble(titulo, texto)
  base <- bind_rows(base, bind)
}

base <- base %>% 
  na.omit()

write.csv(base, "mananeras.csv")


