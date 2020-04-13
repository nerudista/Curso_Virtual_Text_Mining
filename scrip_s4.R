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
  mutate(texto.limpio = case_when(
    grepl("googletag", texto.limpio) ~ str_extract(texto.limpio, "El Diario.*?googletag.cmd.push"),
    TRUE ~ texto.limpio
  )) %>%
  mutate(texto.limpio = str_remove_all(texto.limpio, "googletag.cmd.push")) %>% 
  select(fecha, periodico, id, format, abstract, texto.limpio) %>% 
  filter(id != 430556099) %>% 
  rowid_to_column()


bd.limpia %>% 
  filter(grepl("jueves", texto.limpio),
         grepl("agosto", texto.limpio)) ->x

x$texto.limpio[5]

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

corpus.bd <-Corpus(VectorSource(bd.limpia$texto.limpio))
corpus.bd <- tm_map(corpus.bd, removeWords, stopwords("es"))
corpus.bd <- tm_map(corpus.bd, removePunctuation)
corpus.bd <- tm_map(corpus.bd, removeNumbers)

dtm <- DocumentTermMatrix(corpus.bd)
inspect(dtm)

dtm.df <- tidy(dtm)

rowTotals <- apply(dtm , 1, sum)
dtm <- dtm[rowTotals>0,]


bd.lda <- LDA(dtm, k=5, control=list(seed=1234))

terms(bd.lda, 10)

topics(bd.lda)

options(scipen = 99)

bd.topics <- tidy(bd.lda, matrix="beta") #### Probabilidad por tópico por palabra

bd.docs <- tidy(bd.lda, matrix="gamma") %>%  #### Probabilidad por tópico por documento
  pivot_wider(names_from = topic, values_from = gamma)

bd.limpia$rowid[grepl("Miroslava", bd.limpia$texto.limpio)]

bd.limpia$texto.limpio[2]

top_terminos <- bd.topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup %>% 
  arrange(topic, -beta)

top_terminos %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col() +
  facet_wrap(~ factor(topic), scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_reordered()




bd.topics %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic_1 > .003 | topic_2 > .003) %>% 
  mutate(razon_log = log2(topic_2/topic_1)) %>% 
  ggplot(aes(x = reorder(term, razon_log), y= razon_log)) +
  geom_col() +
  coord_flip()


miroslava <- bd.limpia$rowid[grepl("Miroslava", bd.limpia$texto.limpio)]
veracruz <- bd.limpia$rowid[grepl("Veracruz", bd.limpia$texto.limpio)]

tidy(bd.lda, matrix="gamma") %>% 
  group_by(topic) %>% 
  filter(document %in% sample(veracruz, 20)) %>% 
  ggplot(aes(x = factor(topic), gamma)) +
  geom_col() +
  facet_wrap(~document)







###################################

quant.corp <- corpus(bd.limpia$texto.limpio, 
                     docvars = tibble(id = bd.limpia$id))
print(quant.corp)
summary(quant.corp)

dfmat<- dfm(quant.corp, remove_punct = TRUE, remove = stopwords('es')) %>% 
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
           max_docfreq = 0.1, docfreq_type = "prop")

dfmat <- dfmat[ntoken(dfmat) > 0,]

quanteda_options(threads = 4)

dtm <- convert(dfmat, to = "topicmodels")
lda <- LDA(dtm, k =10)

terms(lda, 10)



bd.docs <- tidy(lda, matrix="gamma") %>%  #### Probabilidad por tópico por documento
  pivot_wider(names_from = topic, values_from = gamma)
