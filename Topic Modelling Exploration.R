# Topic Modeling Exploration
# https://tutorials.quanteda.io/machine-learning/topicmodel/
# Data: https://github.com/quanteda/quanteda_landing/blob/master/content/performance_data/data_corpus_guardian.RDS 

require(quanteda)
require(quanteda.corpora)
require(seededlda)
require(lubridate)

corp_news <- readRDS("~/Documents/Umbrella Review/data_corpus_guardian.RDS")

corp_news_2016 <- corpus_subset(corp_news, year(date) == 2016)
ndoc(corp_news_2016)

toks_news <- tokens(corp_news_2016, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) # removes punctuation, numbers, and symbols
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst")) # removes stopwords, and time related words

# Keep the top 20% of the most frequent features that appear in less than 10% of all document to focus on common but distinguishing features.
dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

# Topic Modelling using LDA
tmod_lda <- textmodel_lda(dfmat_news, k = 10)
terms(tmod_lda, 10)

head(topics(tmod_lda), 20)

# assign topic as a new document-level variable
dfmat_news$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(dfmat_news$topic)