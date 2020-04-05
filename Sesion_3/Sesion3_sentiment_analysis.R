pacman::p_load(tidyverse,rvest,stringr,syuzhet,textdata,tidytext,
               lubridate, #manejo de fechas
               GGally,    #para graficar correlaciones
               reshape2,   #para el acast
               rtweet
)


#rm(list = ls())

covid <- read_csv("base_covit.csv") %>% 
  select(-X1) %>% 
  rowid_to_column("id")

#Ya hay diccionarios que se usann para analizar sentimientos
#Hay pocos ejemplos en español y no implementados en R
#Aquí entra al rescate syuzhet

#En inglés hay 3 tipps de diccionarios:
# - afinn
# - bing
# - nrc

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")


get_nrc_sentiment(covid$texto[1], language="spanish")

sentimientos <- get_nrc_sentiment(covid$texto[1:7430] , language = "spanish")

covid.sentimientos <- covid %>% 
  left_join(
    sentimientos %>% 
      rowid_to_column("id")
  ) %>% 
  mutate(fecha = gsub("de ","",fecha))%>% 
  mutate(fecha = as.Date(fecha,format="%d %B %Y"))

myfecha <- "03 abril 2020"

glimpse(covid.sentimientos)



covid.sentimientos.long <-  covid.sentimientos %>% 
  group_by(fecha) %>% 
  summarise(anger= sum(anger),
            anticipation = sum(anticipation),
            disgust = sum(disgust),
            fear = sum(fear),
            joy= sum(joy),
            sadness = sum(sadness),
            surprise = sum(surprise),
            trust = sum(trust),
            negative = sum(negative),
            positive = sum(positive)) %>% 
  pivot_longer(-fecha, names_to = "sentimiento", values_to = "count")


ggplot(data=covid.sentimientos.long,
       aes(x=fecha,
           y=count,
           fill=sentimiento)) +
  geom_col()+
  theme_light()

#### dia positivo o negativo
covid.sentimientos %>%
  group_by(fecha) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative)) %>% 
  ggplot(aes(x=fecha,
             y = positive - negative,
             fill = (positive -negative )> 0)) +
  geom_col()


covid.sentimientos %>%
  select(-id) %>% 
  group_by(fecha) %>% 
  summarise_if(is.numeric,sum) %>% 
  pivot_longer(-fecha, names_to="sentimiento",values_to = "puntaje") %>% 
  ggplot(aes(x=fecha,
             y=puntaje,
             fill=sentimiento))+
  geom_col()+
  facet_wrap(~sentimiento)+
  guides(fill=F)


#otra grafica  
covid.sentimientos %>%
  select(-id) %>%
  summarise_if(is.numeric,sum) %>% 
  pivot_longer(everything(), names_to="sentimiento",values_to = "puntaje") %>% 
  ggplot(aes(x=sentimiento,
             y=puntaje,
             fill=sentimiento))+
  geom_col()

#modelito
covid.sentimientos %>%
  select(anger:positive) %>% 
  GGally::ggpairs()


#cargo diccionario en español
afinn.esp <- read_csv("https://raw.githubusercontent.com/jmtoral/Curso_Virtual_Text_Mining/master/lexico_afinn.en.es.csv",
                      locale=locale(encoding = "LATIN1"))

#minar tuits
tmls <- get_timeline(c("JLozanoA","fernandeznorona","LuisitoComunica"),
                     n=3200,
                     include_rts = FALSE)

pal <- tmls %>% 
  rowid_to_column("id") %>% 
  unnest_tokens(palabras,text) %>% 
  select(id,screen_name, created_at, palabras) %>% 
  filter(!palabras %in% tm::stopwords(kind="es")) %>%
  filter(!palabras %in% c("https","t.co")) %>%
  count(screen_name,palabras)

#diversidad lingüistica
pal %>% 
  group_by(screen_name) %>% 
  summarise(total.diferentes=n(),
            total.palabras=sum(n))

pal.sentimientos <- pal %>% 
  inner_join(afinn.esp ,
             by=c("palabras"="Palabra")) %>% 
  distinct(palabras, .keep_all = T) 


pal.sentimientos %>% 
  group_by(screen_name) %>% 
  summarise(neto = sum(Puntuacion))

#wordcloud
pal.sentimientos %>% 
  mutate(sentimiento = case_when(
    Puntuacion > 0 ~ "Positivo",
    Puntuacion < 0 ~ "Negativo"
  )) %>% 
  arrange(-n) %>% 
  acast(palabras ~ sentimiento, fill=0,value.var = "n") %>% 
  wordcloud::comparison.cloud(colors = c("red","blue"))


png("comparacion.png", width = 600, height = 600, res=100)

pal.sentimientos %>% 
  filter(screen_name != "LuisitoComunica") %>% 
  arrange(-n) %>% 
  acast(palabras ~ screen_name, fill=0,value.var = "n") %>% 
  wordcloud::comparison.cloud(colors = c("red","blue"),
                              max.words = 200)

dev.off()
