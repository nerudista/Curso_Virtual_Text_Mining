# Sesion 2
pacman::p_load(tidyverse,tidytext,schrute,tm,ggthemes,pals)

mydata <- schrute::theoffice

mydata

################# Frecuencias

stop_words <-  stopwords(kind="en")
other_stop <- c("oh","ok","okay","uh","yeah","hey","well")

#ley de zipf
#las palabras que menos importan son las que más frecuentes son

tokens.office <- mydata %>% 
  select(season,episode,text,imdb_rating) %>% 
  unnest_tokens(word,text) %>%  #tokenizar (combinacion de carac. única)
  filter(!word %in% stop_words) %>% 
  filter(!word %in% other_stop) %>% 
  filter(!str_detect(word,"[[:digit:]]")) #expresion regular en PERL


# Crear frecuencias
freq.office <- tokens.office %>% 
  count(word,season) %>% 
  arrange(-n)


top.office <- freq.office %>% 
  group_by(season,word)%>% 
  summarise(max = max(n)) %>% 
  top_n(5) %>% 
  arrange(season,-max)
   
  



  
top.office %>% 
  arrange(-max) %>% 
  group_by(season) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder( word,max),
             y=max,
             fill = word))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~season, scales = "free")+
  coord_flip()+
  labs(title="Palabras más representativas en The Office",
       subtitle = "Top 5 por temporada",
       y = "Repeticiones",
       x = "Palabras")+
  scale_fill_manual(values=as.vector(stepped3(20)))
  #theme_solarized_2()
  #theme_gdocs()
  theme_wsj()
  
