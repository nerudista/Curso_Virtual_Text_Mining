#Other exercises in Quanteda

require(quanteda)
require(quanteda.corpora)
require(ggplot2)

corp_tweets <- download(url = 'https://www.dropbox.com/s/846skn1i5elbnd2/data_corpus_sampletweets.rds?dl=1')

toks_tweets <- tokens(corp_tweets, remove_punct = TRUE) 

dfmat_tweets <- dfm(toks_tweets, select = "#*")

tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = "lang")

head(tstat_freq, 20)

dfmat_tweets %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()



set.seed(132)
quanteda::textplot_wordcloud(dfmat_tweets, max_words = 50)



# create document-level variable indicating whether Tweet was in English or other language
corp_tweets$dummy_english <- factor(ifelse(corp_tweets$lang == "English", "English", "Not English"))

# create a grouped dfm and compare groups
dfmat_corp_language <- dfm(corp_tweets, select = "#*", groups = "dummy_english")

# create worcloud
set.seed(132)
textplot_wordcloud(dfmat_corp_language, comparison = TRUE, max_words = 100)

#LEXICAL DIVERSITY

toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords('en'))
tstat_lexdiv <- textstat_lexdiv(dfmat_inaug)
tail(tstat_lexdiv, 5)

plot(tstat_lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = dfmat_inaug$President)

# DOCUMENT/FEATURE SIMILARITY
# textstat_dist()

toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords('en'))
tstat_dist <- as.dist(textstat_dist(dfmat_inaug))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)
