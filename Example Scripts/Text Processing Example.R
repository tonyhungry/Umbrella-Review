# Source: https://github.com/srosh2000/book-review-analysis-example/blob/main/text_cleaning.R

# Load packages

# install.packages("tm")
library(tm)
library(googledrive)
library(data.table)
library(dplyr)
library(tidyr) # data manipulation
library(tidytext)
library(tokenizers)
library(wordcloud)
library(textstem)

# Load data
# drive_deauth()
# data_id<- "https://drive.google.com/file/d/1ZKmKQIXs8I4p74qaSvCxmI_AKagKo3Mm/view?usp=share_link"
# drive_download(as_id(data_id), overwrite = TRUE)
kindle_reviews<- fread('kindle_reviews.csv')
kindle_reviews<- kindle_reviews %>%
  relocate(reviewText, .after = asin)
# rename colnames to match the requirements of dataframe source to create corpus
kindle_reviews<- kindle_reviews %>%
  rename(doc_id = asin,
         text = reviewText)
# take sample data to reduce computation time
set.seed(123)
kindle_reviews<-sample_n(kindle_reviews,1000)

# create corpus
review_corpus<- VCorpus(DataframeSource(kindle_reviews))
## Step 1: Eliminating extra whitespace
review_corpus<- tm_map(review_corpus, stripWhitespace)
## Step 2: Transform to lowercase
review_corpus<- tm_map(review_corpus, content_transformer(tolower))
## remove punctuation
review_corpus<- tm_map(review_corpus, removePunctuation)
## remove numbers
review_corpus<- tm_map(review_corpus, removeNumbers)

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
review_corpus <- tm_map(review_corpus, f, 'http\\S+\\s*')
review_corpus <- tm_map(review_corpus, f, '#\\S+')
review_corpus <- tm_map(review_corpus, f, '[[:cntrl:]]')
review_corpus <- tm_map(review_corpus, f, "^[[:space:]]*")
review_corpus <- tm_map(review_corpus, f, "[[:space:]]*$")
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
review_corpus <- tm_map(review_corpus, g, ' +')

## create custom function to remove other misc characters
# text_preprocessing<- function(x,pattern) 
# {gsub(pattern,'http\\S+\\s*','',x) # remove URLs
#   gsub(pattern,'#\\S+','',x) # remove hashtags
#   gsub(pattern,'[[:cntrl:]]','',x) # remove controls and special characters
#   gsub(pattern,"^[[:space:]]*","",x) # remove leading whitespaces
#   gsub(pattern,"[[:space:]]*$","",x) # remove trailing whitespaces
#   gsub(pattern,' +', ' ', x) # remove extra whitespaces
# }
# review_corpus<-tm_map(review_corpus,content_transformer(text_preprocessing)) 

# remove stopwords
review_corpus<- tm_map(review_corpus, removeWords, stopwords("SMART"))

# library(stopwords)
# review_corpus<- tm_map(review_corpus, removeWords, stopwords::stopwords("en"))


# you can also create custom stopwords based on your context
#mystopwords<- c(stopwords("english"),"book","people")
#review_corpus<- tm_map(review_corpus, removeWords, mystopwords
#review_corpus<- tm_map(review_corpus, stripWhitespace)

library(textstem)
review_corpus<- tm_map(review_corpus, PlainTextDocument)
review_corpus<- tm_map(review_corpus, content_transformer(lemmatize_words))

# create term document matrix
tdm<- TermDocumentMatrix(review_corpus, control = list(wordlengths = c(1,Inf)))
# inspect frequent words
freq_terms<- findFreqTerms(tdm, lowfreq=50)
#View(freq_terms)
term_freq<- rowSums(as.matrix(tdm))
term_freq<- subset(term_freq, term_freq>=20)
df<- data.frame(term = names(term_freq), freq = term_freq)

View(df)
# plot word frequency
library(ggplot2)
df_plot<- df %>%
  top_n(25)
# Plot word frequency
ggplot(df_plot, aes(x = reorder(term, +freq), y = freq, fill = freq)) + geom_bar(stat = "identity")+ scale_colour_gradientn(colors = terrain.colors(10))+ xlab("Terms")+ ylab("Count")+coord_flip()

# create word cloud
# install.packages("wordcloud2")
library(wordcloud2)

m<- as.matrix(tdm)
# calculate the frequency of words as sort it by frequency
word_freq<- sort(rowSums(m), decreasing = T)
wordcloud2(df, color = "random-dark", backgroundColor = "white")
