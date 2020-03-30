# Sesion 2
pacman::p_load(tidyverse,tidytext,tm,pdftools,janeaustenr, gutenbergr, scales, igraph, ggraph)

#leo el pdf y convierdo los /r y /n 
cronopios_famas <- pdf_text("./cronopios_famas.pdf") %>% 
                    read_lines() %>% 
                   enframe() %>% # nuevo as_tibble()
                  mutate(value = str_squish(value)) %>%  #quita espacios
                  filter(nchar(value)> 0)  #uso nchar para contar los caracteres

detectives <- pdf_text("./detectives.pdf") %>% 
  read_lines() %>% 
  enframe() %>% # nuevo as_tibble()
  mutate(value = str_squish(value)) %>%  #quita espacios
  filter(nchar(value)> 0)  #uso nchar para contar los caracteres

################# Frecuencias

stop_words <-  stopwords(kind="es")

#ley de zipf
#las palabras que menos importan son las que más frecuentes son

tokens.cronopios <- cronopios_famas %>% 
                  unnest_tokens(word,value) %>%  #tokenizar (combinacion de carac. única)
                  filter(!str_detect(word,"[[:digit:]]")) %>% #expresion regular en PERL
                  rename(linea = name) %>% 
                  filter(!word %in% stop_words) %>% 
                  mutate(libro = "Historias de Cronopios y Famas")

tokens.detectives <- detectives %>% 
  unnest_tokens(word,value) %>%  #tokenizar (combinacion de carac. única)
  filter(!str_detect(word,"[[:digit:]]")) %>% #expresion regular en PERL
  rename(linea = name) %>% 
  filter(!word %in% stop_words) %>% 
  mutate(libro = "Detectives Salvajes")            

# Crear frecuencias
freq.cronopios <- tokens.cronopios %>% 
  count(word,libro) %>% 
  arrange(-n)


freq.detectives <- tokens.detectives %>% 
  count(word,libro) %>% 
  arrange(-n)


#este es un indicador de diversidad lingüistica
# diversidad lingüistica = numero palabras unicas / numero total palabras 
nrow(freq.cronopios)/nrow(tokens.cronopios) #0.48
nrow(freq.detectives)/nrow(tokens.detectives) #0.16

frecuencia.total <- bind_rows(freq.cronopios, freq.detectives) %>% 
  group_by(libro) %>% 
  mutate(suma = sum(n)) %>% 
  mutate(proporcion = n/suma)  %>% #n() es voy a contar lo de la línea anterior
  select(word,libro,proporcion) %>% 
  pivot_wider(names_from = libro, values_from = proporcion)

#creamos grafiquita
ggplot(data=frecuencia.total,
       aes(x=`Historias de Cronopios y Famas`,
           y=`Detectives Salvajes`))+
       geom_text(aes(label= word, 
                     color=ifelse(`Historias de Cronopios y Famas`> 0.003, 'red', 'black') ), 
                 check_overlap = T,
                 vjust = 1.5)+
       scale_x_log10(labels=percent_format())+
       scale_y_log10(labels=percent_format())+
       geom_abline(color="red", lty=2)+
       theme_classic()

#creamos grafiquita
ggplot(data=frecuencia.total,
       aes(x=`Historias de Cronopios y Famas`,
           y=`Detectives Salvajes`,
           color=(`Historias de Cronopios y Famas`-`Detectives Salvajes`)>0))+
  geom_jitter(alpha= 0.1, size=0.8, width=0.3, height=0.3, color="grey10")+
  geom_text(aes(label= word), 
            check_overlap = T,
            vjust = 1.5)+
  scale_x_log10(labels=percent_format())+
  scale_y_log10(labels=percent_format())+
  geom_abline(color="red", lty=2)+
  scale_color_manual(values = c("red","blue"))+
  guides(color=F)

################## TF-IDF
# Term frequency - inerse document frequency


quijote <- gutenberg_download(2000) 

quijote.limpio <- quijote %>% 
  mutate(text = iconv(text,  to="latin1")) %>% 
  filter(nchar(text)> 0) %>%  #uso nchar para contar los caracteres
  mutate(libro="Don Quijote") %>% 
  unnest_tokens(word,text) %>%  #tokenizar (combinacion de carac. única)
  filter(!str_detect(word,"[[:digit:]]")) %>% #expresion regular en PERL
  filter(!word %in% stop_words) %>% 
  count(word,libro) %>% 
  arrange(-n)


comparar <- bind_rows(freq.cronopios, freq.detectives, quijote.limpio)
  
tfidf <- comparar %>% 
  bind_tf_idf(word,libro,n) #este orden es muy importante. Debe respetarse

# tf - frecuencia de terminos -- division, comun al interior de nuestro libro
# idf frecuencia inversa de documento , comun esa palabra en todos los documentos
# tf idf - ponderacion de las dos últimas. sancho es palabra muy presente en quijote pero no los demás
#         mientas más alto, más único del odocumento
# tf_iddf = tf * idf
# solo es grande si el tf es grande (aparece mucho en un texto) 
# y el idf es grande (aparece en menos textos). El idf es un logaritmo. aquí (2/3) 
# está en 2 de 3 documentos


tfidf %>% 
  arrange(-tf_idf) %>% 
  group_by(libro) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder( word,tf_idf),
             y=tf_idf,
             fill = libro))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~libro, scales = "free")+
  coord_flip()+
  labs(title="Palabras más representativas de tres clásicos",
              subtitle = "Medidos por el tf-idf",
       y = "tf-idf",
       x = "Palabras")


######## N-GRAMAS

#Bigrama: 2 Gram

bigramas_cronopios <- cronopios_famas %>% 
  unnest_tokens(bigrama,value,token="ngrams", n=2) %>% 
  na.omit() %>% 
  separate(bigrama, c("palabra1","palabra2"), sep=" ") %>% 
  filter(!palabra1 %in% stop_words) %>% 
  filter(!palabra2 %in% stop_words) %>% 
  count(palabra1,palabra2,sort =T) %>% 
  unite(bigrama,palabra1,palabra2, sep=" ") %>% 
  mutate (libro = "Cronopios y Famas")


bigramas_detectives <- detectives %>% 
  unnest_tokens(bigrama,value,token="ngrams", n=2) %>% 
  na.omit() %>% 
  separate(bigrama, c("palabra1","palabra2"), sep=" ") %>% 
  filter(!palabra1 %in% stop_words) %>% 
  filter(!palabra2 %in% stop_words) %>% 
  count(palabra1,palabra2,sort =T) %>% 
  unite(bigrama,palabra1,palabra2, sep=" ") %>% 
  mutate (libro = "Detectives Salvajes")

bigramas.total <- bind_rows(bigramas_cronopios,
                            bigramas_detectives) %>% 
  bind_tf_idf(bigrama,libro,n)

bigramas.total %>% 
  arrange(-tf_idf) %>% 
  group_by(libro) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder( bigrama,tf_idf),
             y=tf_idf,
             fill = libro))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~libro, scales = "free")+
  coord_flip()+
  labs(title="Palabras más representativas de tres clásicos",
       subtitle = "Medidos por el tf-idf",
       y = "tf-idf",
       x = "Palabras")


grafo <-  bigramas.total %>%
  filter(n>20) %>% 
  filter(libro=="Detectives Salvajes") %>% 
  separate(bigrama, c("palabra1","palabra2"), sep=" ") %>% 
  graph_from_data_frame()

a <- grid::arrow(type = 'closed', length = unit(.09, "inches"))

ggraph(grafo, layout="fr")+
  geom_edge_link(aes(edge_alpha= n),
                 arrow = a)+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust=1, hjust=1)+
  theme_void()


ggraph(grafo, layout="sphere")+
  geom_edge_link(aes(edge_alpha= n),
                 arrow = a)+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust=1, hjust=1)+
  theme_void()
