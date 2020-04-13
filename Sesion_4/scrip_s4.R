#devtools::install_github("quanteda/quanteda.corpora")


library(pacman)

p_load(quanteda, readtext, topicmodels, quanteda.corpora, tidyverse, tm, tidytext)

bd <- read_csv("bd_salazar_1.csv", locale = locale(encoding = "UTF-8"))


glimpse(bd)

unique(bd$format)

bd.limpia <-  bd %>% 
  mutate(texto.limpio = str_replace_all(body, "<.*?>", " "),
         texto.limpio = str_replace_all(texto.limpio , "[ \t\r\n\v\f]", " "), #PERL
         texto.limpio = str_replace_all(texto.limpio, "[:cntrl:]", " "),
         texto.limpio = str_replace_all(texto.limpio, "\"", " "),
         texto.limpio = str_remove_all(texto.limpio, "writePostTexto\\(\\)"),
         texto.limpio = str_remove_all(texto.limpio, "—"),
         texto.limpio = str_remove_all(texto.limpio, "“"),
         texto.limpio = str_squish(texto.limpio),
         texto.limpio = str_trim(texto.limpio, "both")) %>% 
  select(fecha, periodico, id, format, abstract, texto.limpio) %>% 
  filter(id != 430556099) %>% 
  rowid_to_column()



write.csv(bd.limpia, "base_limpia_noticias.csv", fileEncoding = "UTF-8")

rm(bd)

### Ejemplo de RegEx
x <-  "masculino  de 54 años acusado de una violación al artículo 9 fracción ii inciso a del codigoio penal federal"

stringr::str_detect(x, "artículo.*9.*ii.*a")

grepl("artículo.*9.*ii.*a", x)


################# LDA (Latent Dirichlet Association)



data("AssociatedPress")

ap <- tidy(AssociatedPress)

corp_news <- download('data_corpus_guardian')

gdn <- tidy(corp_news)

#### comer, dormir, jugar, maullar y ladrar

#### Categoria A: gato
### comer 10%, dormir 30%, jugar 20%, maullar 40%, ladrar 0%

#### Categoría B: perro
### comer 20%, dormir 10%, jugar 40%, maullar 0%, ladrar 30%

bd.limpia$texto.limpio[88]

corpus.bd <-Corpus(VectorSource(bd.limpia$texto.limpio[-88]))
corpus.bd <- tm_map(corpus.bd, removeWords, stopwords("es"))
corpus.bd <- tm_map(corpus.bd, removePunctuation)
corpus.bd <- tm_map(corpus.bd, removeNumbers)



dtm <- DocumentTermMatrix(corpus.bd)
inspect(dtm)

dtm.df <- tidy(dtm)

bd.lda <- LDA(dtm, k=5, control=list(seed=1234))

bd.topics <- tidy(bd.lda, matrix="beta")

top_terminos <- bd.topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup %>% 
  arrange(topic, -beta)


terms(bd.lda, 10)

topics(bd.lda)

bd.limpia$texto.limpio[c(410)]






