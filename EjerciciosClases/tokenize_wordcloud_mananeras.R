pacman::p_load(tidyverse,stringr,tidytext,janitor,tm,wordcloud,SnowballC)
# base read.csv
# tidyverse read_csv

base <- read_csv("mananeras.csv", 
                 locale = locale(encoding = "LATIN1")) 


# Filtros

## BASE R
conservador.base <- base %>% 
  filter(grepl("conservador|Concervador",texto))

## DPLYR
sorteo <- base %>% 
  filter(str_detect(texto," rifa"))

sorteo <- base %>% 
  filter(grepl("\\brifa",texto))


#LIMPIEZA

## ASCII

base.limpia <- base %>% 
  mutate(texto = iconv(texto, "LATIN1", "ASCII//TRANSLIT")) %>% # QUITA ACENTOS Y CARAC. ESPECIALES
  mutate(texto = tolower(texto)) %>% 
  mutate(texto = str_remove_all(texto,"\\bla\\b|\\blas\\b|\\bel\\b|\\bdel\\b|\\bque\\b")) %>%   #dplyr
  #mutate(gsub("la","",texto)) #Base R
  mutate(texto = str_remove_all(texto,"\\by\\b|\\bde\\b|\\bsea\\b|\\bo\\b|\\blos\\b|\\ba\\b")) %>% 
  mutate(texto = str_remove_all(texto,"[[:punct:]]") ) %>% 
  mutate(texto = str_squish(texto))    #quita dobles espacios, "exprime"


base.limpia <- base %>% 
  mutate(texto = stringi::stri_trans_general(texto, "Latin-ASCII")) %>%  # Quitar acentos y ñ, etc.
  mutate(texto = tolower(texto)) %>% 
  mutate(texto = str_remove_all(texto, "\\bla\\b|\\blas\\b|\\bel\\b|\\bdel\\b")) %>% 
  # mutate(texto = gsub("la", "", texto)) #Base R
  mutate(texto = str_remove_all(texto, "\\bde\\b|\\by\\b|\\bsea\\b|\\bo\\b|\\ba\\b|\\blos\\b")) %>% 
  mutate(texto = str_remove_all(texto, "[[:punct:]]")) %>% 
  mutate(texto = str_squish(texto))



## BUSQUEDA PALABRAS

base.limpia.cuenta <- base.limpia %>% 
  mutate(conservador = str_count(texto,"conservador")) %>% 
  mutate(sorteo = str_count(texto,"\\sorteo")) %>% 
  mutate(rifa = str_count(texto,"\\brifa")) %>% 
  mutate(corrupcion = str_count(texto,"corrup")) %>% 
  mutate(pueblo = str_count(texto,"\\bpueblo")) %>% 
  mutate(fifi = str_count(texto,"\\bfifi")) %>% 
  janitor::adorn_totals()



## TIDYTEXT
## TOKENIZAZION
tokens  <- base.limpia %>% 
  unnest_tokens(palabras,texto)

cuenta.palabras <- tokens %>% 
  count(palabras) %>% 
  arrange(-n) %>%
  filter(!palabras %in% c("de","a","que","la","y","el","en","se","no","los","para","con",
                         "es","eso","pero","como","las","los","por","porque","al","del",
                         "un","lo","ya","va"))

cuenta.palabras


#### LIMPIEZA AUTOMATICA DE PALABRAS VACIAS
stopwords(kind="es")

cuenta.palabras.auto <- tokens %>% 
  #mutate(palabras = wordStem(palabras,language ="spanish")) %>% 
  count(palabras) %>% 
  arrange(-n) %>% 
  filter(!palabras %in% stopwords(kind="es"))

cuenta.palabras.auto
  

## VISUALIZACION

png("nube.png",width = 600, height=600)

cuenta.palabras.auto %>% 
  with(wordcloud(palabras,n,max.words = 300,
                 random.order = FALSE,
                 colors = rev(brewer.pal(5,"Spectral"))))

dev.off()

#jmtoralcruz@gmail.com
