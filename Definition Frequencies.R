# Definitions with Quanteda

# Setting Up ####

# viridis color palette coordination
# Community: magma
# Social: plasma
# Disaster: cividis
# Infrastructure: mako
# Urban: viridis
# Organization: rocket
# All: turbo

# Remove unnecessary objects and plots in the environment
rm(list = ls(all.names = TRUE))
# dev.off()
# graphics.off()

# Load required packages
library(tidyverse)
library(googlesheets4)

googlesheets4::gs4_auth(email = "tonyhung.th@gmail.com")

# Load data through Google Sheets
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")

df = gather(df_wide,type,definitions,c(`Def 0`:`Def 35`)) # reshape the data into a long format
df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(Doc_id = ID, texts = definitions, Type = type) %>% relocate(texts)

library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

normal_words = c("resilien*","transport*","define*","refer*","water")

# All Definitions ####

# Load packages
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(quanteda.corpora)
library(ggplot2)

set.seed(123)

corp = corpus(df,text_field = "texts") # create corpus

## 1-gram ####
def_tok <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% # makes everything lower case
  tokens_remove(pattern = stopwords("en", source = "smart")) %>% # removes stopwords
  tokens_remove(normal_words) %>% # removes common words that are specific to this topic
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% # lemmatize words
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex") # an additional line to make sure that the tokens are really regular words.

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("All Definitions, Top 10, 1-gram")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "All Resilience Definition Word Cloud, 1-gram")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::turbo(15)))

## 2-gram ####

def_tok <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_ngrams(n=2) %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("All Definitions, Top 10, 2-gram")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "All Resilience Definition Word Cloud, 2-gram")
textplot_wordcloud(dfmat, max_words = 100, color = viridis::turbo(15))

## 1+2-gram ####

def_tok <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_ngrams(n=1:2) %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_wordstem() 

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("All Definitions, Top 10, 1+2-grams")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "All Resilience Definition Word Cloud, 1+2-grams")
textplot_wordcloud(dfmat, max_words = 100, color = viridis::turbo(15))

# From all these wordclouds, the ones that really makes sense is the 1-gram. 

# Infrastructure Definitions ####
infra = corpus_subset(corp, Classification == "Infrastructure")

## 1-gram ###
def_tok <- tokens(infra, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Infrastructure Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Infrastructure Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = viridis::mako(15))

# Community Definitions ####
commun = corpus_subset(corp, Classification == "Community")

## 1-gram ###

def_tok <- tokens(commun, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove("community") %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Community Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Community Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::magma(15)))

# Urban Definitions ####
urban = corpus_subset(corp, Classification == "Urban")

## 1-gram ###

def_tok <- tokens(urban, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove("urban") %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Urban Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Urban Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::viridis(15)))

# Disaster Definitions ####
disas = corpus_subset(corp, Classification == "Disaster")

## 1-gram ###

def_tok <- tokens(disas, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove("disaster*") %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Disaster Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Disaster Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::cividis(15)))

# Organization Definitions ####
organ = corpus_subset(corp, Classification == "Organization")

## 1-gram ###

def_tok <- tokens(organ, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove(c("organization*","organisation*")) %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Organization Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Organizational Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::rocket(15)))

# Social Definitions ####
soci = corpus_subset(corp, Classification == "Social")

## 1-gram ###

def_tok <- tokens(soci, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>%
  tokens_remove(normal_words) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove("social") %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")

dfmat <- dfm(def_tok)

dfmat %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Words", y = "Frequency") +
  theme_minimal() + 
  ggtitle("Social Resilience Definitions, Top 10 Word Occurences")

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Social Resilience Definition Word Cloud")
textplot_wordcloud(dfmat, max_words = 100, color = rev(viridis::plasma(15)), main="Title")

# Similarity Comparison using H Clust ####
library(tidyverse)

df = df_wide %>% unite("definitions", c(`Def 0`:`Def 35`),na.rm=T) %>% 
  rename(Doc_id = ID, texts = definitions) %>% relocate(texts) # This is to collapse all of the definitions into one column.

corp_cat = corpus(df,text_field = "texts") # create corpus

toks <- tokens(corp_cat, remove_punct=T)
dfmat <- dfm(toks, remove = stopwords("en", source = "smart"), tolower=T)
docnames(dfmat) = docvars(dfmat)$Abbreviation
tstat_dist <- as.dist(textstat_dist(dfmat))
clust <- hclust(tstat_dist)
# plot(clust, xlab = "Distance", ylab = NULL)

library(ggdendro)
ddata_x <- dendro_data(clust)
labs <- label(ddata_x)
labs = labs %>% left_join(df,by = join_by(label == Abbreviation)) %>% select(-c("texts", "Doc_id","Article Name"))
ggplot(segment(ddata_x),horiz=T) +
  geom_segment(aes(y=x, x=y, yend=xend, xend=yend)) +
  geom_text(data=label(ddata_x), aes(label=label, y=x, x=y, colour=labs$Classification)) +
  ggtitle("Definition Similarity")

# I know this is an ugly picture, but it's easier to see which ones are which category. It really is a mix of results. It's interesting to see the definitions for urban resilience is rather close to community resilience and the other urban definition is close infrastructure resilience. Organizational resilience is the most removed. Not surprising, especially if one looks at the kind of definition that was used. 

ggdendrogram(clust, rotate=T) + 
  ggtitle("Definition Similarity")
# Will this plot instead.