# Quanteda Exploration
# Based on the Quanteda tutorial: https://tutorials.quanteda.io/

# Install packages
# install.packages("quanteda")
# install.packages("quanteda.textmodels")
# install.packages("quanteda.textstats")
# install.packages("quanteda.textplots")
# install.packages("readtext")
# install.packages("devtools")
# devtools::install_github("quanteda/quanteda.corpora") #This is to install sample data from their github repo
# install.packages("spacyr")
# install.packages("newsmap")
# install.packages("seededlda")

require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
require(quanteda.corpora)
require(newsmap)
require(seededlda)

# quanteda_options("threads" = 3) # Setting the number of threads for parallel computing. This Mac can use a maximum of 4 threads. By default, it is only set to one

# Chapter 2: Import Data ####

path_data <- system.file("extdata/", package = "readtext")
dat_inaug <- read.csv(paste0(path_data, "/csv/inaugCorpus.csv")) #loads in pre-formatted files that come in a "spreadsheet format" 
dat_dail <- readtext(paste0(path_data, "/tsv/dailsample.tsv"), text_field = "speech") # Loading in files that contains text and any associated document-level variables

# Loading in multiple text files
path_data <- system.file("extdata/", package = "readtext")
dat_udhr <- readtext(paste0(path_data, "/txt/UDHR/*"))
# Generate document-level variables based on the file names
dat_eu <- readtext(paste0(path_data, "/txt/EU_manifestos/*.txt"),
                   docvarsfrom = "filenames", 
                   docvarnames = c("unit", "context", "year", "language", "party"),
                   dvsep = "_", # This specifies the value separator
                   encoding = "ISO-8859-1") # This specifies the character encodings
str(dat_eu)

# Read in PDF files
dat_udhr <- readtext(paste0(path_data, "/pdf/UDHR/*.pdf"), 
                     docvarsfrom = "filenames", 
                     docvarnames = c("document", "language"),
                     sep = "_")

# Read in Microsoft Word files
dat_word <- readtext(paste0(path_data, "/word/*.docx"))

## Solving different character encodings ####

path_temp <- tempdir()
unzip(system.file("extdata", "data_files_encodedtexts.zip", package = "readtext"), exdir = path_temp)
filename <- list.files(path_temp, "^(Indian|UDHR_).*\\.txt$")
head(filename)
filename <- gsub(".txt$", "", filename) #extract encoding info
encoding <- sapply(strsplit(filename, "_"), "[", 3) 
head(encoding)
setdiff(encoding, iconvlist()) # check which encoding is supported by R

# Pass the encoding to readtext() to convert various character encodings into UTF-8
path_data <- system.file("extdata/", package = "readtext")
dat_txt <- readtext(paste0(path_data, "/data_files_encodedtexts.zip"), 
                    encoding = encoding,
                    docvarsfrom = "filenames", 
                    docvarnames = c("document", "language", "input_encoding"))
print(dat_txt, n = 50)

# Chapter 3: Basic Operations ####

## Construct a corpus ####

# Through a character vector
corp_immig <- corpus(data_char_ukimmig2010, 
                     docvars = data.frame(party = names(data_char_ukimmig2010)))
print(corp_immig)
summary(corp_immig)

# Through a data frame
path_data <- system.file("extdata/", package = "readtext")
dat_inaug <- read.csv(paste0(path_data, "/csv/inaugCorpus.csv"))
names(dat_inaug)
corp_inaug <- corpus(dat_inaug, text_field = "texts") # construct a corpus from the "texts" column 
print(corp_inaug)
summary(corp_inaug, 5)
# Edit the docnames for a corpus to change them from text1 to a more meaningful identifier
docid <- paste(dat_inaug$Year, 
               dat_inaug$FirstName, 
               dat_inaug$President, sep = " ")
docnames(corp_inaug) <- docid
print(corp_inaug)

# Through Vcorpus from the tm package
corp_tm <- tm::VCorpus(tm::VectorSource(data_char_ukimmig2010))
corp_quanteda <- corpus(corp_tm)

## Document-level Variables ####

corp <- data_corpus_inaugural
head(docvars(corp))

# Extracting document-level variables
docvars(corp, field = "Year")
corp$Year

# Assigning document-level variables
docvars(corp, field = "Century") <- floor(docvars(corp, field = "Year") / 100) + 1 # creating a new variable called century
head(docvars(corp))
corp$Century <- floor(corp$Year / 100) + 1 # Another way to do so

## Subset Corpus ####

corp <- data_corpus_inaugural
ndoc(corp)
head(docvars(corp))
corp_recent <- corpus_subset(corp, Year >= 1990) #subset according to year
ndoc(corp_recent) #checks for number of documents
corp_dem <- corpus_subset(corp, President %in% c("Obama", "Clinton", "Carter")) #subset according to special selection
ndoc(corp_dem)

## Change units of texts ####
corp <- corpus(data_char_ukimmig2010)
print(corp)
ndoc(corp)
corp_sent <- corpus_reshape(corp, to = "sentences") # change documents to sentences
print(corp_sent)
ndoc(corp_sent)

corp_doc <- corpus_reshape(corp_sent, to = "documents") # change from sentences to documents
print(corp_doc)
ndoc(corp_doc)

## Extract tags from texts ####

# Create document sections
corp_tagged <- corpus(c("##INTRO This is the introduction.
                         ##DOC1 This is the first document.  Second sentence in Doc 1.
                         ##DOC3 Third document starts here.  End of third document.",
                        "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
corp_sect <- corpus_segment(corp_tagged, pattern = "##*")
cbind(docvars(corp_sect), text = as.character(corp_sect))

# Create speaker identifiers
corp_speeches <- corpus("Mr. Smith: Text.
                        Mrs. Jones: More text.
                        Mr. Smith: I'm speaking, again.")
corp_speakers <- corpus_segment(corp_speeches, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:", valuetype = "regex")
cbind(docvars(corp_speakers), text = as.character(corp_speakers))

## Construct a tokens object ####

corp_immig <- corpus(data_char_ukimmig2010)
toks_immig <- tokens(corp_immig)

toks_nopunct <- tokens(data_char_ukimmig2010, remove_punct = TRUE) # removes punctuations
print(toks_nopunct)

# Keyword in contexts 
# kwic() enables you to see how keywords are used in the actual contexts in a concordance view
toks <- tokens(data_char_ukimmig2010)
kw_immig <- kwic(toks, pattern =  "immig*")
head(kw_immig, 10)
kw_immig2 <- kwic(toks, pattern = c("immig*", "migra*")) # Can also take multiple different keywords
head(kw_immig2, 10)

kw_immig3 <- kwic(toks, pattern = c("immig*", "migra*"), window = 3) # limits the number of words displayed around the keyword
head(kw_immig3, 10)

kw_asylum <- kwic(toks, pattern = phrase("asylum seeker*")) # multi-word expressions
head(kw_asylum)
View(kw_asylum) # look at it in a data frame 

## Selecting Tokens ####

toks <- tokens(data_char_ukimmig2010)
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove") #remove stopwords
print(toks_nostop)

toks_nostop2 <- tokens_remove(toks, pattern = stopwords("en")) #another way to do the same thing
print(toks_nostop2)

# Removal of tokens changes the lengths of documents, but they remain the same if you set padding = TRUE. This option is useful especially when you perform positional analysis.
toks_nostop_pad <- tokens_remove(toks, pattern = stopwords("en"), padding = TRUE)
print(toks_nostop_pad)

# Removing all other words besides the ones you are interested in
toks_immig <- tokens_select(toks, pattern = c("immig*", "migra*"), padding = TRUE)
print(toks_immig)

# Keeping words around the keywords you've selected
toks_immig_window <- tokens_select(toks, pattern = c("immig*", "migra*"), padding = TRUE, window = 5)
print(toks_immig_window)

## Compound Tokens ####

toks <- tokens(data_char_ukimmig2010)
kw_multiword <- kwic(toks, pattern = phrase(c("asylum seeker*", "british citizen*")))
head(kw_multiword, 10)

toks_comp <- tokens_compound(toks, pattern = phrase(c("asylum seeker*", "british citizen*")))
kw_comp <- kwic(toks_comp, pattern = c("asylum_seeker*", "british_citizen*"))
head(kw_comp, 10)

## Look up dictionary ####
# Doesn't work. Basically, you use a dictionary (set of predefined words) to identify tokens.
toks <- tokens(data_char_ukimmig2010)
dict_newsmap <- dictionary(file = "../../dictionary/newsmap.yml")

## Generate N-grams ####
toks <- tokens(data_char_ukimmig2010, remove_punct = TRUE)
toks_ngram <- tokens_ngrams(toks, n = 2:4) # generate 2 to 4 n-grams
head(toks_ngram[[1]], 30)
tail(toks_ngram[[1]], 30)

toks_skip <- tokens_ngrams(toks, n = 2, skip = 1:2) # can also skip tokens in between
head(toks_skip[[1]], 30)

# generate selective ngrams
toks_neg_bigram <- tokens_compound(toks, pattern = phrase("not *"))
toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase("not_*"))
head(toks_neg_bigram_select[[1]], 30)

## Document-feature matrix ####

## Constract a DFM
toks_inaug <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmat_inaug <- dfm(toks_inaug)
print(dfmat_inaug)

ndoc(dfmat_inaug) # get the number of documents
nfeat(dfmat_inaug) # get the number of features

head(docnames(dfmat_inaug), 20) # list the names of the documents
head(featnames(dfmat_inaug), 20) # list the names of the features

head(rowSums(dfmat_inaug), 10) # calculate the marginal row sums. In this case, the number of tokens of each document
head(colSums(dfmat_inaug), 10) # calculate the marginal column sums. In this case, the number of tokens of each feature.

topfeatures(dfmat_inaug, 10) # shows the top ten most frequent features

dfmat_inaug_prop <- dfm_weight(dfmat_inaug, scheme  = "prop") # convert frequency counts to proportions. 
print(dfmat_inaug_prop)

dfmat_inaug_tfidf <- dfm_tfidf(dfmat_inaug) # convert frequency counts to weights, depending on the uniqueness of the features.
print(dfmat_inaug_tfidf)

## Select features ####

toks_inaug <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmat_inaug <- dfm(toks_inaug)
print(dfmat_inaug)

dfmat_inaug_nostop <- dfm_select(dfmat_inaug, pattern = stopwords("en"), selection = "remove") #selects features
print(dfmat_inaug_nostop)

dfmat_inaug_nostop <- dfm_remove(dfmat_inaug, pattern = stopwords("en")) # another way to do the same thing
print(dfmat_inaug_nostop)

dfmat_inaug_long <- dfm_keep(dfmat_inaug, min_nchar = 5) # selecting features based on the length of the features. This one only keep features consisting of at least five characters.
print(dfmat_inaug_long)
topfeatures(dfmat_inaug_long, 10) 

dfmat_inaug_freq <- dfm_trim(dfmat_inaug, min_termfreq = 10) # selects features that occur less thant 10 times in the corpus.
print(dfmat_inaug_freq)

dfmat_inaug_docfreq <- dfm_trim(dfmat_inaug, max_docfreq = 0.1, docfreq_type = "prop")  # features that occur in more than 10% of the documents are removed
print(dfmat_inaug_docfreq)

## Group documents ####
toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug)
print(dfmat_inaug)

head(colSums(dfmat_inaug), 10)

dfmat_party <- dfm_group(dfmat_inaug, groups = Party) # merges documents based on a vector given to the groups arguem
print(dfmat_party)
head(colSums(dfmat_party), 10)
docvars(dfmat_party)

## Feature co-occurence matrix ####

corp_news <- download("data_corpus_guardian")

toks_news <- tokens(corp_news, remove_punct = TRUE) # removes punctuations
dfmat_news <- dfm(toks_news) # construct the document feature matrix
dfmat_news <- dfm_remove(dfmat_news, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst")) # remove stopwords and any patterns that usually describe the publication time and date of articles
dfmat_news <- dfm_trim(dfmat_news, min_termfreq = 100) # keep only terms that occur at least 100 times in the DFM

nfeat(dfmat_news)

topfeatures(dfmat_news) # shows number of features

fcmat_news <- fcm(dfmat_news) # construct the feature co-occurence matrix
dim(fcmat_news)

topfeatures(fcmat_news)

feat <- names(topfeatures(fcmat_news, 50)) # select the top 50 features
fcmat_news_select <- fcm_select(fcmat_news, pattern = feat, selection = "keep")
dim(fcmat_news_select)

# Using FCM to visualize semantic network analysis
size <- log(colSums(dfm_select(dfmat_news, feat, selection = "keep"))) 
set.seed(144)
textplot_network(fcmat_news_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

# Removing Stopwords ####
# reshape corpus to the level of paragraphs
corp_eng <- corpus_reshape(data_corpus_udhr["eng"], to = "paragraphs")

# tokenize corpus and apply pre-processing
toks_eng <- tokens(corp_eng, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("en", source = "stopwords-iso")) %>% 
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex")
print(toks_eng[2], max_ndoc = 1, max_ntoken = -1)

# Statistical Analysis ####

## Simple Frequency Analysis ####

corp_tweets <- download(url = "https://www.dropbox.com/s/846skn1i5elbnd2/data_corpus_sampletweets.rds?dl=1")

# Analyze the most frequent hashtags 

# Keeping hashtags
toks_tweets <- tokens(corp_tweets, remove_punct = TRUE) %>% 
  tokens_keep(pattern = "#*")
dfmat_tweets <- dfm(toks_tweets)
tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = lang) #t-statistic, just token stat
head(tstat_freq, 20)

# Plot frequencies using ggplot
library(ggplot2)
dfmat_tweets %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# Create word cloud
set.seed(132)
textplot_wordcloud(dfmat_tweets, max_words = 100)

# Create two word clouds to compare

# create document-level variable indicating whether tweet was in English or other language
corp_tweets$dummy_english <- factor(ifelse(corp_tweets$lang == "English", "English", "Not English"))

# tokenize texts
toks_tweets <- tokens(corp_tweets)

# create a grouped dfm and compare groups
dfmat_corp_language <- dfm(toks_tweets) %>% 
  dfm_keep(pattern = "#*") %>% 
  dfm_group(groups = dummy_english)

# create wordcloud
set.seed(132) # set seed for reproducibility
textplot_wordcloud(dfmat_corp_language, comparison = TRUE, max_words = 100) # Doesn't work that well. Might need some adjustments.

## Lexical diversity ####
# Useful for analyzing spearkers' or writers' linguistic skills, or the compelxirty of ideas expressed in documents

toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords("en"))

tstat_lexdiv <- textstat_lexdiv(dfmat_inaug)
tail(tstat_lexdiv, 5)

plot(tstat_lexdiv$TTR, type = "l", xaxt = "n", xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = dfmat_inaug$President)

## Document / Feature Similarity ####

toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords("en"))

tstat_dist <- as.dist(textstat_dist(dfmat_inaug))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)

## Relative frequency analysis (keyness)
# Keyness compares frequencies of words between target and reference documents. 
library(lubridate)

corp_news <- download("data_corpus_guardian")

toks_news <- tokens(corp_news, remove_punct = TRUE) 
dfmat_news <- dfm(toks_news)

tstat_key <- textstat_keyness(dfmat_news, 
                              target = year(dfmat_news$date) >= 2016)
textplot_keyness(tstat_key)

## Collocation Analysis
# Collocation analysis allows us to identify contiguous collocations of words. One of the most common types of multi-word expressions are proper names, which can be identified simply based on capitalization in English texts.

corp_news <- download("data_corpus_guardian")

toks_news <- tokens(corp_news, remove_punct = TRUE)
tstat_col_caps <- tokens_select(toks_news, pattern = "^[A-Z]", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(tstat_col_caps, 20)

# Can also discover collocations longer than two words.
tstat_col2 <- tokens_select(toks_news, pattern = "^[A-Z]", 
                            valuetype = "regex", 
                            case_insensitive = FALSE, 
                            padding = TRUE) %>% 
  textstat_collocations(min_count = 100, size = 3)
head(tstat_col2, 20)

# Advanced Operations ####

## Compute similarity between authors ####
# Doesn't work, because no json file found...

## Compound multi-word expressions ####
# Identify sequences of capitalized words and compound them as proper names, which are important linguistic features of newspaper articles.

corp_news <- download("data_corpus_guardian") # load in data

toks_news <- tokens(corp_news, remove_punct = TRUE, remove_symbols = TRUE, padding = TRUE) %>% 
  tokens_remove(stopwords("en"), padding = TRUE) #removes punctuations, symbols, and stopwords

toks_news_cap <- tokens_select(toks_news, # extracts tokens that are proper nouns
                               pattern = "^[A-Z]",
                               valuetype = "regex",
                               case_insensitive = FALSE, 
                               padding = TRUE)

tstat_col_cap <- textstat_collocations(toks_news_cap, min_count = 10, tolower = FALSE)
head(tstat_col_cap, 20)

# Compound strongly associated multi-word expression, where the z-score is more than 3
toks_comp <- tokens_compound(toks_news, pattern = tstat_col_cap[tstat_col_cap$z > 3,], 
                             case_insensitive = FALSE)
kw_comp <- kwic(toks_comp, pattern = c("London_*", "British_*"))
head(kw_comp, 10)

## Apply dictionary to specific contexts ####
# Detect occurrences of words in specific contexts by selectively applying a dictionary. In this example, a sentiment dictionary is applied to segments of news articles that mentions the (British) government

corp_news <- download("data_corpus_guardian")

# tokenize corpus
toks_news <- tokens(corp_news, remove_punct = TRUE)

# get relevant keywords and phrases
gov <- c("government", "cabinet", "prime minister")

# only keep tokens specified above and their context of ±10 tokens
# note: use phrase() to correctly score multi-word expressions
toks_gov <- tokens_keep(toks_news, pattern = phrase(gov), window = 10) # select tokens surrounding keywords related to the government

lengths(data_dictionary_LSD2015) # apply the Lexicoder Sentiment Dictionary to the selected contexts using tokens_lookup()

# select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

toks_gov_lsd <- tokens_lookup(toks_gov, dictionary = data_dictionary_LSD2015_pos_neg)

# create a document document-feature matrix and group it by day
dfmat_gov_lsd <- dfm(toks_gov_lsd) %>% 
  dfm_group(groups = date)

# Plot it
matplot(dfmat_gov_lsd$date, dfmat_gov_lsd, type = "l", lty = 1, col = 1:2,
        ylab = "Frequency", xlab = "")
grid()
legend("topleft", col = 1:2, legend = colnames(dfmat_gov_lsd), lty = 1, bg = "white")

# Compute daily sentiment scores by taking the difference betweeen the frequency of positive and negative words
plot(dfmat_gov_lsd$date, dfmat_gov_lsd[,"positive"] - dfmat_gov_lsd[,"negative"], 
     type = "l", ylab = "Sentiment", xlab = "")
grid()
abline(h = 0, lty = 2)

# Apply kernel smoothing to show the trend more clearly
dat_smooth <- ksmooth(x = dfmat_gov_lsd$date, 
                      y = dfmat_gov_lsd[,"positive"] - dfmat_gov_lsd[,"negative"],
                      kernel = "normal", bandwidth = 30)
plot(dat_smooth$x, dat_smooth$y, type = "l", ylab = "Sentiment", xlab = "")
grid()
abline(h = 0, lty = 2)

## Identify related words of keywords ####
# Identify related words of keyowrds based on their distance in the documents. 
# This example uses a created list of words related the EU by comparing frequency of words inside and outside of their contexts.

corp_news <- download("data_corpus_guardian")
toks_news <- tokens(corp_news, remove_punct = TRUE)

eu <- c("EU", "europ*", "european union") # create a dictionary
toks_inside <- tokens_keep(toks_news, pattern = eu, window = 10) # select two tokens objects for words inside of the 10-word windows of the keywords
toks_inside <- tokens_remove(toks_inside, pattern = eu) # remove the keywords
toks_outside <- tokens_remove(toks_news, pattern = eu, window = 10) # select two tokens objects for words outside the 10-word windows of the keywords (eu)

# Compute the association between words with the keywords using textstat_keyness()
dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)))
head(tstat_key_inside, 50)

# Scaling and Classification ####
# How to derive latent positions from text data and how to classify documents

## Naive Bayes Classifier ####
# Naive Bayes is a supervised model that usually classify documents into two or more categories. We train the classifier using class labels attached to documents, and predict the most likely classes of new unlabeled documents.
# Basically ML. We have a test and a train split.

corp_movies <- data_corpus_moviereviews
summary(corp_movies, 5) 
# The variable "Sentiment" indicates whether a movie review was classified as positive or negative. In this example, 1500 reviews are used as the training set and build a Naive Bayes classifier based on this subset. In the second step, we will predict the sentiment for the remaining reviews (our test set).
# Since the first 1000 reviews are negative and the remaining reviews are classified as positive, we need to draw a random sample of the documents. 

# generate 1500 numbers without replacement
set.seed(300)
id_train <- sample(1:2000, 1500, replace = FALSE)
head(id_train, 10)

# create docvar with ID
corp_movies$id_numeric <- 1:ndoc(corp_movies)

# tokenize texts
toks_movies <- tokens(corp_movies, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem()
dfmt_movie <- dfm(toks_movies)

# get training set
dfmat_training <- dfm_subset(dfmt_movie, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmt_movie, !id_numeric %in% id_train)

# train the naive Bayes classifier
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$sentiment)
summary(tmod_nb)

# Naive Bayes can only take features into consideration that occur both in the training set and the test set, but we can make the features identical using dfm_match()
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$sentiment
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

library(caret)
confusionMatrix(tab_class, mode = "everything", positive = "pos")

## Regularized Regression Classifier ####
# Regularized regression is a classification technique where the category of interest is regressed on text features using a penalized form of regression where parameter estimates are biased towards zero.
# Here we use the Least Absolute Shrinkage and Selection Operator (LASSO). However, the main alternative to LASSO, ridge regression, is conceptually very similar.
# In the LASSO estimator, the degree of penalization is determined by the regularization parameter lambda. 
# We can use the cross-validation function available in the glmnet package to select the optimal value for lambda. We train the classifier using class labels attached to documents, and predict the most likely class(es) of new unlabelled documents. Although regularized regression is not prt of the quanteda.textmodels package, the functions for regularized regression from the glmnet package can be easily worked into a quanteda workflow. 

library(glmnet)

corp_movies <- data_corpus_moviereviews
summary(corp_movies, 5)

# generate 1500 numbers without replacement
set.seed(300)
id_train <- sample(1:2000, 1500, replace = FALSE)
head(id_train, 10)

# create docvar with ID
corp_movies$id_numeric <- 1:ndoc(corp_movies)

# tokenize texts
toks_movies <- tokens(corp_movies, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem()
dfmt_movie <- dfm(toks_movies)

# get training set
dfmat_training <- dfm_subset(dfmt_movie, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmt_movie, !id_numeric %in% id_train)

# uses cv.glmnet() to select the value of lambda that yields the smallest classification error. 
lasso <- cv.glmnet(x = dfmat_training,
                   y = as.integer(dfmat_training$sentiment == "pos"),
                   alpha = 1,
                   nfold = 5,
                   family = "binomial")

index_best <- which(lasso$lambda == lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[, index_best]

head(sort(beta, decreasing = TRUE), 20)

# predict.glmnet can only take features into consideration that occur both in the training set and the test set, but we can make the features identical using dfm_match()
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

# Obtain predicted probabilities for each review in the test set
pred <- predict(lasso, dfmat_matched, type = "response", s = lasso$lambda.min)
head(pred)

# Inspect how well the classification worked.
actual_class <- as.integer(dfmat_matched$sentiment == "pos")
predicted_class <- as.integer(predict(lasso, dfmat_matched, type = "class"))
tab_class <- table(actual_class, predicted_class)
tab_class

confusionMatrix(tab_class, mode = "everything")

## Wordscores #### 
# Wordscores is a scaling model for estimating the position (mostly of political actors) for dimensions that are specified a priori.
# Training a Wordscores model requires reference scores for texts whose policy positions on well-defined a priori dimensions are "known". Afterwards, Wordscores estimates the positions for the remaining "virgin" texts. 
# In this example, we will use manifestos of the 2013 and 2017 German federal elections. For the 2013 elections we will assign the average expert evaluations from the 2014 Chapel Hill Expert Survey for the five major parties in order to predict the party positions for the 2017 manifestos. 

corp_ger <- download(url = "https://www.dropbox.com/s/uysdoep4unfz3zp/data_corpus_germanifestos.rds?dl=1")
summary(corp_ger)

# tokenize texts
toks_ger <- tokens(corp_ger, remove_punct = TRUE)

# create a document-feature matrix
dfmat_ger <- dfm(toks_ger) %>% 
  dfm_remove(pattern = stopwords("de"))

# apply Wordscores algorithm to document-feature matrix
tmod_ws <- textmodel_wordscores(dfmat_ger, y = corp_ger$ref_score, smooth = 1)
summary(tmod_ws)

# predict the Wordscores for the unknown virgin texts
pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = dfmat_ger)
textplot_scale1d(pred_ws)

## Wordfish ####
# Wordfish is a Poisson scaling model of one-dimensional document positions. Wordfish also allows for scaling documents, but in comparison to Wordscores, reference scores/texts are not required. Wordfish is an unsupervised one-dimensional text scaling method, meaning that it estimates the positions of documents solely based on observed word frequencies

toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)
tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(6, 5))
summary(tmod_wf)
textplot_scale1d(tmod_wf)
textplot_scale1d(tmod_wf, groups = dfmat_irish$party)

textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("government", "global", "children", 
                                 "bank", "economy", "the", "citizenship",
                                 "productivity", "deficit"))

## Correspondence Analysis ####
# Correspondence analysis is a technique to scale documents on multiple dimensions. Correspondence analysis is similar to principal component analysis but works for categorical variables (contingency table).

toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish) %>% 
  dfm_remove(pattern = stopwords("en"))

tmod_ca <- textmodel_ca(dfmat_irish)
textplot_scale1d(tmod_ca)

dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)

plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))

## Topic Models ####
# Topic models are unsupervised document classification techniques. By modeling distributions of topics over words and words over documents, topic models identify the most discriminatory groups of documents automatically. 

library(lubridate)

corp_news <- download("data_corpus_guardian")

corp_news_2016 <- corpus_subset(corp_news, year(date) == 2016)
ndoc(corp_news_2016)

toks_news <- tokens(corp_news_2016, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

### LDA ####
# k = 10 specifies the number of topics to be discovered. This is an important parameter and you should try a variety of values and validate the outputs of your topic models thoroughly.

tmod_lda <- textmodel_lda(dfmat_news, k = 10)
terms(tmod_lda, 10)
head(topics(tmod_lda), 20)
# assign topic as a new document-level variable
dfmat_news$topic <- topics(tmod_lda)
# cross-table of the topic frequency
table(dfmat_news$topic)

### Seeded LDA ####
# In seeded LDA, you can pre-define topics in LDA using a dictionary of "seed" words

# load dictionary containing seed words
dict_topic <- dictionary(file = "../dictionary/topics.yml") # doesn't work
print(dict_topic)

## Newsmap ####
# Newsmap is a semi-supervised model for geographical document classification. This semi-supervised model learns from "seed words" in dictionaries

require(newsmap)
require(maps)
require(ggplot2)

corp_news <- download(url = "https://www.dropbox.com/s/r8zhsu8zvjzhnml/data_corpus_yahoonews.rds?dl=1")
ndoc(corp_news)
range(corp_news$date)

# Proper nouns are the most useful features of documents for geographical classification. However, not all capitalized words are proper nouns, so we define custom stopwords.
month <- c("January", "February", "March", "April", "May", "June",
           "July", "August", "September", "October", "November", "December")
day <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
agency <- c("AP", "AFP", "Reuters")

toks_news <- tokens(corp_news, remove_punct = TRUE) %>% 
  tokens_remove(pattern = c(stopwords("en"), month, day, agency), 
                valuetype = "fixed", padding = TRUE)

toks_label <- tokens_lookup(toks_news, dictionary = data_dictionary_newsmap_en, # Uses the English seed geographical dictionaries
                            levels = 3) # level 3 is countries
dfmat_label <- dfm(toks_label, tolower = FALSE)

dfmat_feat <- dfm(toks_news, tolower = FALSE)
dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z0-9]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label)

coef(tmod_nm, n = 15)[c("US", "GB", "FR", "BR", "JP")]

# As we can see here, it's a bit shabby. Names of people, organizations and places are often multi-word expressions. To distinguish between “New York” and “York”, for example, it is useful to compound tokens using tokens_compound() as explained in Advanced Operations.

pred_nm <- predict(tmod_nm)
head(pred_nm, 20)

count <- sort(table(factor(pred_nm, levels = colnames(dfmat_label))), decreasing = TRUE)
head(count, 20)

dat_country <- as.data.frame(count, stringsAsFactors = FALSE)
colnames(dat_country) <- c("id", "frequency")

world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region) # convert country name to ISO code

ggplot(dat_country, aes(map_id = id)) +
  geom_map(aes(fill = frequency), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_continuous(name = "Frequency") +
  theme_void() +
  coord_fixed()

## Latent Semantic Scaling ####
# Latent Semantic Scaling is a flexible and cost-efficient semi-supervised document scaling technique. The technique relies on word 

require(LSX)

corp_news <- download("data_corpus_guardian")

# We must segment news articles into sentences in the corpus to accurately estimate semantic proximity between words. 
# tokenize text corpus and remove various features
corp_sent <- corpus_reshape(corp_news, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en", source = "marimo")) %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com"))  

# create a document feature matrix from the tokens object
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

topfeatures(dfmat_sent, 20)

seed <- as.seedwords(data_dictionary_sentiment) # use generic sentiment seed words to perform sentiment analysis
print(seed)

# With the seed words, LSS computes the polarity of words frequent in the context of economy. We can identify context words by char_context(pattern="econom*") before fitting the model.
# identify context words 
eco <- char_context(toks_sent, pattern = "econom*", p = 0.05)
# run LSS model
tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words
tail(coef(tmod_lss), 20) # most negative words

textplot_terms(tmod_lss, data_dictionary_LSD2015["negative"])

# We must reconstruct original articles from their sentences using dfm_group() before predicting polarity of documents
dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss, newdata = dfmat_doc)

# We can smooth polarity scores of documents to visualize the trend using smooth_lss(). locfit smoothing is very fast even when there are many documents.
dat_smooth <- smooth_lss(dat, engine = "locfit")
head(dat_smooth)

# In the plot below, the circles are polarity scores of documents and the curve is their local means with 95% CI
plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))
