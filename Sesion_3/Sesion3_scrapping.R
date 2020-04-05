# Scrapping
pacman::p_load(tidyverse,rvest,stringr)

links_totales <- NULL #Objeto vacío
links <- NULL #Objeto vacío


for(i in 1:20){
  
  cat(i, "...")
  Sys.sleep(1)
  
  url <- paste0("https://www.gob.mx/presidencia/es/archivo/articulos?idiom=es&order=DESC&page=", i)
  
  pg <- read_html(url) ### Lee todo el código fuente
  
  titulo <- pg %>% 
    html_nodes("h2") %>% # Ubicación del selector
    html_text() %>%  # Título
    enframe() %>% 
    separate(value, c("titulo", "borrar"), sep="continuar leyendo") %>% 
    select(name, titulo)
  
  links <- pg %>% 
    html_nodes("a") %>% # Ubicación del selector
    html_attr("href") %>%  # Enlaces a las conferencias
    enframe() %>% 
    filter(!grepl("DESC", value)) %>% 
    rename(link=value) %>% 
    mutate(link = gsub('\\"', "", link),
           link = gsub('\\\"', "", link))
  
  infopg <- full_join(titulo, links)
  
  links_totales <- bind_rows(links_totales, infopg)
}


covid <- links_totales %>% 
  filter(grepl("COVID|(C|c)ovid",titulo)) %>% 
  mutate(link=str_remove(link,"\\\\")) %>% 
  mutate(link=paste0("https://www.gob.mx",link)) 
  
covid[1,3]

## Obtener el texto de cada discurso
nrow(covid)
base <- NULL
bind <- NULL

for (x in 1:nrow(covid)){
  
  tryCatch(
    expr = {
      
      cat(x,"...") # contador
      url <- covid$link[x]
      
      pg <- read_html(url)
      
      fecha <- pg %>% 
        html_node("dd~ dd") %>% 
        html_text()
      
      titulo <- pg %>% 
        html_node(".bottom-buffer") %>% 
        html_text()
      
      #if (length(titulo)==0){
      #  titulo <- NA
      #}
      
      
      texto <- pg %>% 
        html_nodes(".article-body p") %>%   #nodes para que tome todos  y no sólo uno
        html_text()
      
      #if (length(texto)==0){
      #  titulo <- NA
      #}
      bind <- tibble(fecha,titulo,texto)
      base <- bind_rows(base,bind)
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('All done, quitting.')
    }
  )
}


base <- base %>% 
  na.omit()

write.csv(base,"base_covit.csv", fileEncoding = "UTF-8")

