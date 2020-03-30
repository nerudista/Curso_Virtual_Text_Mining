#scrapping inmuebles 24

#cargar bilbiotecas

pacman:: p_load(tidyverse,rvest,stringr)

# el selector CSS es .posting-title

#Creo objetos vacios para los links
links_totales <- NULL
links <-  NULL

# hacer loop para scrappear



print ("###########  INICIO DEL LOOP ###############")
for (i in 1:7){
  tryCatch(
   expr = {
     pagina <- "https://www.inmuebles24.com/departamentos-en-renta-en-azcapotzalco-de-8000-a-14000-pesos.html"
     cat (i,"...")
     
     if (i > 1){
       #pega sin espacions ni separado
       pagina <- str_remove(pagina,".html")
       pagina <- paste0(pagina,"-pagina-",i,".html")
     }
     # leo la página
     links <- read_html(pagina) %>% 
         html_nodes(".posting-title .go-to-posting") %>% 
         html_attr("href") %>% 
         as_tibble()
    
     links$value <- paste0("https://www.inmuebles24.com",links$value)
     #print(links)
     
     # rbind 
     links_totales <- rbind(links_totales,links) # tambien puede ser bind_rows()
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
    
  ) #trycatch
  
} #for

#Ahora toca leer cada página

print (links_totales)

## Obtener el texto de cada discurso
nrow(links_totales)
base <- NULL
bind <- NULL

for (x in 1:nrow(links_totales)){
  tryCatch(
    expr ={
      
      cat(x,"...") # contador
      url <- links_totales$value[x]
      
      pg <- read_html(url)
      
      descripcion <- pg %>% 
        html_node("#verDatosDescripcion") %>% 
        html_text()
      
      #print (descripcion)
      
      precio <- pg %>% 
        html_node("#sidebar-price-container span") %>% 
        html_text()
      
      print (precio)
      
      bind <- tibble(x,url,precio,descripcion)
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
      message('Link leído')
    }
    
  )
}

# sacar fecha
fecha <-  Sys.Date()

write.csv(base,paste0("inmuebles24-",fecha,".csv"))
