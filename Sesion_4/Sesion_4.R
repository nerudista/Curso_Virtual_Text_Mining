#devtools::install_github("quanteda/quanteda.corpora")

library(pacman)

p_load(quanteda, readtext, topicmodels, quanteda.corpora, tidyverse, tidytext,tm,ggthemes)

# base con articulos sobre violencia contra periodistas
bd <-  read_csv("Sesion_4/bd_salazar_1.csv", locale = locale(encoding= "UTF-8"))

#glimpse((bd))

bd.limpia <- bd %>% 
  mutate(texto.limpio = str_replace_all(body,"<.*?>"," "),
         texto.limpio = str_replace_all(texto.limpio,"[\r\n\t]"," "),
         #texto.limpio = gsub("[^[:alnum:][:blank:]?&/\\-]"," ",texto.limpio)
         
         #Esta funcion quita caracteres Unicode de Control
         texto.limpio = str_replace_all(texto.limpio,"[:cntrl:]"," "),
         texto.limpio = str_replace_all(texto.limpio,"\""," "),
         texto.limpio = str_remove_all(texto.limpio,"writePostTexto\\(\\)"),
         texto.limpio = str_remove_all(texto.limpio, "—"),
         texto.limpio = str_remove_all(texto.limpio, "“"),
         texto.limpio = str_remove_all(texto.limpio, "“"),
         texto.limpio = str_trim(texto.limpio,"both")
         ) %>% 
  mutate(texto.limpio = case_when(
    grepl("googletag", texto.limpio) ~ str_extract(texto.limpio, "El Diario.*?googletag.cmd.push"),
    TRUE ~ texto.limpio
  )) %>%
  mutate(texto.limpio = str_remove_all(texto.limpio, "googletag.cmd.push")) %>% 
  select(id,fecha,periodico,format,abstract,texto.limpio,texto) %>% 
  filter(id != 430556099) %>% 
  rowid_to_column()


#bd.limpia$texto.limpio[grepl("vale madre",bd.limpia$texto.limpio)]

#borra el objeto bd 
rm(bd)


#bd.limpia$texto.limpio[1:3]

#### DOCUMENT TERM MATRIX

# Es un tipo de formato que nos deja usar los texto ocmo si fueran corpuses.

#LDA
# TRATA DE ENCONTRAR AQUELLAS PALABRAS QUE DEFINEN UNA CATEGORIA
# cada observación es un documento
# 

# Crear un corpus
# lee el vector base.lpimpia$texto.limpio y lo convierte en un corpus
corpus.bd <- Corpus(VectorSource(bd.limpia$texto.limpio))
# Como hay mucahs palabras vacias:
corpus.bd <- tm_map(corpus.bd, removeWords, stopwords("es"))
corpus.bd <- tm_map(corpus.bd, removePunctuation)
corpus.bd <- tm_map(corpus.bd, removeNumbers)


#Crae matriz de terminos de documentso. Es como tokenizar per te lo deja como 
# termDocumentMatrix
dtm <- DocumentTermMatrix(corpus.bd)

# > tdm
# <<TermDocumentMatrix (terms: 44884, documents: 708)>>
#   Non-/sparse entries: 227467/31550405
# Sparsity           : 99%
# Maximal term length: 202
# Weighting          : term frequency (tf)


inspect(dtm)

#Esto convierte el TermDocumentMatrix en un DataFrame

dtm.df <- tidy(dtm)

rowTotals <- apply(dtm , 1, sum)
dtm <- dtm[rowTotals>0,]

bd.lda <- LDA(dtm,k=5,control=list(seed=1234))



bd.topics <- tidy(bd.lda, matrix="beta") #Prob por topico por palabra

#para que bd.docs no me de exponentes en en los resultados.
options(scipen=99)

bd.docs <- tidy(bd.lda, matrix="gamma") %>%  #Prob por topico por documento
  pivot_wider(names_from = topic, values_from = gamma) 
  

top_terminos <- bd.topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup %>% 
  arrange(topic, -beta)

# las noticias tienen que tener todos estos terminos
terms(bd.lda, 10)

topics(bd.lda)

bd.limpia$texto.limpio %>% 
  filter(grepl("jueves",texto.limpio),
         grepl("agosto",texto.limpio)) -> x

top_terminos %>% 
  mutate(term = reorder_within(term,beta,topic)) %>% 
  ggplot(aes(term,
             beta,
             fill=factor(topic)))+
  geom_col()+
  facet_wrap(~ factor(topic), scales = "free")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_reordered()+ # necesita el mutate de arriba. Quita el __1, __2 que éste pone.
  theme_clean()+
  theme(legend.position="none")


bd.topics %>% 
  mutate(topic=paste0("topic_",topic)) %>% 
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic_1 > .003 | topic_2 > .003) %>% 
  mutate (razon_log = log2(topic_2/topic_1)) %>% 
  ggplot(aes(x = reorder(term,razon_log),
             y = razon_log))+
  geom_col()+
  coord_flip()+
  theme_clean()
  

miroslava <- bd.limpia$rowid[grepl("Miroslava",bd.limpia$texto.limpio)]

tidy(bd.lda,matrix="gamma") %>% 
  group_by(topic) %>% 
  filter(document %in% sample(miroslava,15)) %>% 
  ggplot(aes(x = factor(topic),
             y= gamma))+
  geom_col()+
  facet_wrap(~ document)

###############
## QUANTIDA   #
###############


quant.corp <- quanteda::corpus(bd.limpia$texto.limpio, 
                     docvars = tibble(id = bd.limpia$id))
print(quant.corp)
summary(quant.corp)

dfmat<- quanteda::dfm(quant.corp, remove_punct = TRUE, remove = stopwords('es')) %>% 
  quanteda::dfm_trim(min_termfreq = 0.95, 
                     termfreq_type = "quantile", 
                     max_docfreq = 0.1, #If max_docfreq = 0.1, features that occur in more than 10% of the documents are removed.
                     docfreq_type = "prop")

dfmat <- dfmat[quanteda::ntoken(dfmat) > 0,]

nfeat(dfmat)

#obtain the names of documents and features by docnames()
head(docnames(dfmat), 20)

#obtain the names of documents and features by docnames()
head(featnames(dfmat), 20)


#The most frequent features can be found using topfeatures().
topfeatures(dfmat, 10)

quanteda_options(threads = 4)

dtm <- convert(dfmat, to = "topicmodels")
lda <- LDA(dtm, k =8)

terms(lda, 10)



