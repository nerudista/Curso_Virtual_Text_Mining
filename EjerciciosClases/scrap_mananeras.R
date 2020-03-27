# Scrapping
pacman::p_load(tidyverse,rvest,stringr)

# Usar selector gadget
# el selector CSS es .entry-title a

links_totales <- NULL #Objeto vacio
links <-  NULL  #Objeto vacio

# hacer loop para scrappear

for (i in 1:5){
  
  cat (i,"...")
  #pega sin espacions ni separado
  url <- paste0("https://lopezobrador.org.mx/transcripciones/page/",i)
  
  # leo la página
  pg <- read_html(url)
  
  links <- pg %>% 
           html_nodes(".entry-title a") %>%  # ubicacion del selector
           html_attr("href") %>%                  # enlaces finales
           as_tibble()
  # rbind 
  links_totales <- rbind(links_totales,links) # tambien puede ser bind_rows()
} 

#write_csv(links_totales,"links_totales.csv")

## Obtener el texto de cada discurso
nrow(links_totales)
base <- NULL
bind <- NULL

for (x in 1:nrow(links_totales)){
  
  cat(x,"...") # contador
  url <- links_totales$value[x]
  
  pg <- read_html(url)
  
  titulo <- pg %>% 
    html_node(".entry-title") %>% 
    html_text()
  
  if (length(titulo)==0){
    titulo <- NA
  }
  
  
  texto <- pg %>% 
    html_nodes(".p1") %>%   #nodes para que tome todos  y no sólo uno
    html_text()
  
  if (length(texto)==0){
    titulo <- NA
  }
  
  
  bind <- tibble(titulo,texto)
  base <- bind_rows(base,bind)
  
}

write.csv(base,"mananeras.csv")
