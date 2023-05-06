# Text Analysis Basics

# Source: https://sicss.io/2020/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html
# Video: https://www.youtube.com/watch?v=TAlO-5NJB7I 

#### GREP ####
duke_web_scrape<- "Duke Experts: A Trusted Source for Policy Makers\n\n\t\t\t\t\t\t\t" 
grepl("Experts", duke_web_scrape)

gsub("\t", "", duke_web_scrape) # deletes \t
gsub("\t|\n", "", duke_web_scrape) # deletes \n

some_text<-c("Friends","don'tt","let","friends","make","wordclouds")
some_text[grep("^[F]", some_text)] #Finds all words in a string that starts with F

# Helpful cheatsheet: https://github.com/rstudio/cheatsheets/blob/main/regex.pdf 

text_chunk<-c("[This Professor is not so Great]")
gsub("[","", text_chunk) # This doesn't work, because it cannot recognize [. You would need to use backslashes.
gsub('\\[|\\]',"", text_chunk) # This works

#### Corpus Creation ####
load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
head(trumptweets$text)

library(tm)
trump_corpus <- Corpus(VectorSource(as.vector(trumptweets$text))) 
trump_corpus

#### Tidy-text ####

library(tidytext)
library(dplyr)
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
head(tidy_trump_tweets)

tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

#### Text Pre-processing with tidy ####
data("stop_words") # removing stopwords
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words)

tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

# Note: tidytext removes punctuation and lowers word case automatically.

tidy_trump_tweets<-tidy_trump_tweets[-grep("\\b\\d+\\b", tidy_trump_tweets$word),] # removes numbers
tidy_trump_tweets$word <- gsub("\\s+","",tidy_trump_tweets$word) # removes whitespaces

library(SnowballC) #stemming
tidy_trump_tweets<-tidy_trump_tweets %>% # This function doesn't work :(
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_trump_DTM<-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)


#### Text Pre-processing with tm ####

trump_corpus <- tm_map(trump_corpus, removeWords, stopwords("english")) #removes stopwords

trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation)) #removes punctuation
trump_corpus <- tm_map(trump_corpus, content_transformer(removeNumbers)) #removes numbers

trump_corpus <- tm_map(trump_corpus,  content_transformer(tolower)) #makes all letter into lowercase 
trump_corpus <- tm_map(trump_corpus, content_transformer(stripWhitespace))#strip all possible whitespace

trump_corpus  <- tm_map(trump_corpus, content_transformer(stemDocument), language = "english") #stemming

# Worth exploring: qdap and quanteda package
# www.tidytextmining.com
# NLP textbook: Jurafsky and MArtin's textbook

#### Document-Term Matrix ####

trump_DTM <- DocumentTermMatrix(trump_corpus, control = list(wordLengths = c(2, Inf)))
inspect(trump_DTM[1:5,3:8])
