# Topic Modelling on the Abstracts/title/keywords

#### Setting up the script ####
# Loading required packages
library(bibliometrix) 
library(igraph)
library(stringr)
library(dplyr)
library(xtable) # converts tables to latex tables
library(kableExtra)
require(quanteda)
require(quanteda.textplots)
require(seededlda)
require(lubridate)

set.seed(123)

# Convert cleaned data to data frame
M <- convert2df(file="savedrecs.txt", dbsource="wos",format="plaintext")

# Extract the columms that I really need, which are AB (Abstract), DE(), and TI
akt.df  = cbind.data.frame(M$AB, M$DE, M$TI)
colnames(akt.df) = c("AB","DE","TI")

#### Abstract Co-occurrence Network ####
names = rownames(M)
abs = cbind.data.frame(names,M$AB)
colnames(abs) = c("docnames","texts")

abs.corp = corpus(abs,text_field="texts")
ndoc(abs.corp) #29

set.seed(100)
toks <- abs.corp %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) %>%
  tokens_remove(pattern = c(stopwords(source="smart"), "purpose", "design", "methodology", "approach", "finding*", "research", "implication*", "conclusion*","review","resilien*", padding = FALSE)) %>% 
  tokens_tolower()
fcmat <- fcm(toks_abs, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 50))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = 0.5)

graph = as.igraph(fcmat)
plot(graph,)
cluster_louvain(graph)

#### Sequential LDA with only the Abstract ####

names = rownames(M)
abs = cbind.data.frame(names,M$AB)
colnames(abs) = c("docnames","texts")

abs.corp = corpus(abs,text_field="texts")
ndoc(abs.corp)

abs.corp.sent = corpus_reshape(abs.corp, to="sentences")

toks_abs <- tokens(abs.corp.sent, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) # removes punctuation, numbers, and symbols
toks_abs <- tokens_remove(toks_abs, pattern = c(stopwords(source="smart"), "purpose", "design", "methodology", "approach", "finding*", "research", "implication*", "conclusion*","review","resilien*")) # removes stopwords, and usual research related words

dfm_abs = dfm(toks_abs) %>% dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",ax_docfreq = 0.1, docfreq_type = "prop")

for (x in 1:30) {
  tmod_lda <- textmodel_lda(dfm_abs, k = x, gamma=0.5)
  divunr = divergence(tmod_lda,regularize = F)
  divr = divergence(tmod_lda, regularize = T)
  print(c("# of Topics",x, divunr, divr))
}

lda_seq <- textmodel_lda(dfm_abs, k = 10, gamma = 0.5)
terms(lda_seq,10)

#### NOT USING THIS Topic Modelling with all three (AKT) ####
akt = as.data.frame(paste(akt.df$AB, akt.df$DE, akt.df$TI))
colnames(akt) = "texts"

akt.corp = corpus(akt,text_field="texts")
ndoc(akt.corp)

toks_akt <- tokens(akt.corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) # removes punctuation, numbers, and symbols
toks_akt <- tokens_remove(toks_akt, pattern = c(stopwords("en"), "purpose", "design", "methodology", "approach", "finding-*", "research", "implication-*", "conclusion-*","review","resilien-*")) # removes stopwords, and usual research related words
dfm_akt = dfm(toks_akt)

# Finding the optimal number of topics. Seems like k=9 or k=7 is the best one... Especially if you do it a few times. 
divvalues = c()
for (x in 1:30) {
  tmod_lda <- textmodel_lda(dfm_akt, k = x)
  div = divergence(tmod_lda)
  print(c("# of Topics",x, div))
  divvalues = c(divvalues,div)
}

tmod_lda <- textmodel_lda(dfm_akt, k = 7)
terms(tmod_lda, 10)
head(topics(tmod_lda), 20)

# assign topic as a new document-level variable
dfm_akt$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(dfm_akt$topic)

## Sequential LDA (Sentence Level) ##
akt.corp.sent = corpus_reshape(akt.corp, to="sentences")

toks_akt <- tokens(akt.corp.sent, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) # removes punctuation, numbers, and symbols
toks_akt <- tokens_remove(toks_akt, pattern = c(stopwords("en"), "purpose", "design", "methodology", "approach", "finding*", "research", "implication*", "conclusion*","review","resilien*")) # removes stopwords, and usual research related words

dfm_akt = dfm(toks_akt) %>% dfm_trim(min_termfreq = 0.5, termfreq_type = "quantile",
                                     max_docfreq = 0.1, docfreq_type = "prop")

lda_seq <- textmodel_lda(dfm_akt, k = 10, gamma = 0.5)
terms(lda_seq,10)


