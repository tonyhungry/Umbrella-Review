# Text Processing of Resilience Definitions

#### Setting Up ####

# Remove unnecessary objects in the environment
rm(list = ls())
rm(list = ls(all.names = TRUE))

# Load required packages
library(tm)
library(googledrive)
library(data.table)
library(dplyr)
library(tidyr) # data manipulation
library(tidytext)
library(tokenizers)
library(wordcloud)
library(textstem)
library(ggplot2)

# Load data through Google Sheets
library(googlesheets4)
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")

df = gather(df_wide,type,definitions,c(`Def 0`:`Def 23`)) # reshape the data into a long format
df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(doc_id = ID, text = definitions)


#### All papers together ####

## Text Pre-processing 

# create corpus
def_corpus <- VCorpus(DataframeSource(df))
## Step 1: Eliminating extra whitespace
def_corpus<- tm_map(def_corpus, stripWhitespace)
## Step 2: Transform to lowercase
def_corpus<- tm_map(def_corpus, content_transformer(tolower))
## remove punctuation
def_corpus<- tm_map(def_corpus, removePunctuation)
## remove numbers
def_corpus<- tm_map(def_corpus, removeNumbers)

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# remove stopwords
def_corpus<- tm_map(def_corpus, removeWords, stopwords("SMART"))
def_corpus = tm_map(def_corpus, removeWords, "resilience")

# Running it all again just to be sure
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

library(textstem)
def_corpus<- tm_map(def_corpus, PlainTextDocument)
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_strings))

# create term document matrix
tdm <- TermDocumentMatrix(def_corpus, control = list(wordlengths = c(1,Inf)))
# inspect frequent words
freq_terms <- findFreqTerms(tdm, lowfreq=50)
#View(freq_terms)
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq>=5)
dfm <- data.frame(term = names(term_freq), freq = term_freq)

# plot word frequency
df_plot <- dfm %>% top_n(20)
# Plot word frequency
ggplot(df_plot, aes(x = reorder(term, +freq), y = freq, fill = freq)) + geom_bar(stat = "identity")+ scale_colour_gradientn(colors = terrain.colors(10))+ xlab("Terms")+ ylab("Count")+coord_flip()

# Word Cloud 
library(wordcloud2)
m <- as.matrix(tdm)
# calculate the frequency of words as sort it by frequency
word_freq<- sort(rowSums(m), decreasing = T)

library(wesanderson)
mix_palette = rep(wes_palette("FantasticFox1",n=5),round(nrow(dfm)/5)+nrow(dfm)%%5)

wordcloud2(dfm, color = mix_palette, backgroundColor = "white")

#### Just Infrastructure Papers ####
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")
df_meta = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Metadata")

df_meta$`Article Name` == df_wide$`Article Name` # if the rows and columns do line up from both datasets. 
df_wide = cbind(df_wide,df_meta$Cassification) 
df_wide = df_wide %>%  
  rename(classification = `df_meta$Cassification`) %>% # rename the column
  filter(classification == "Infrastructure") %>% # filter for only infrastructure papers
  select(-classification)
df = gather(df_wide,type,definitions,c(`Def 0`:`Def 35`)) # reshape the data into a long format
df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(doc_id = ID, text = definitions)

## Text Pre-processing 

# create corpus
def_corpus <- VCorpus(DataframeSource(df))
## Step 1: Eliminating extra whitespace
def_corpus<- tm_map(def_corpus, stripWhitespace)
## Step 2: Transform to lowercase
def_corpus<- tm_map(def_corpus, content_transformer(tolower))
## remove punctuation
def_corpus<- tm_map(def_corpus, removePunctuation)
## remove numbers
def_corpus<- tm_map(def_corpus, removeNumbers)


f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# remove stopwords
def_corpus<- tm_map(def_corpus, removeWords, stopwords("SMART"))
def_corpus = tm_map(def_corpus, removeWords, "resilience")

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

library(textstem)
def_corpus<- tm_map(def_corpus, PlainTextDocument)
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_strings))

# create term document matrix
tdm <- TermDocumentMatrix(def_corpus, control = list(wordlengths = c(1,Inf)))
# inspect frequent words
freq_terms <- findFreqTerms(tdm, lowfreq=5)
# View(freq_terms)
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq>=5)
dfm <- data.frame(term = names(term_freq), freq = term_freq)

# plot word frequency
df_plot <- dfm %>% top_n(25)
# Plot word frequency
ggplot(df_plot, aes(x = reorder(term, +freq), y = freq, fill = freq)) + geom_bar(stat = "identity")+ scale_colour_gradientn(colors = terrain.colors(10))+ xlab("Terms")+ ylab("Count")+coord_flip()+scale_fill_gradient(name="Frequency")

# Word Cloud 
library(wesanderson)
infra_palette = rep(wes_palette("Cavalcanti1",n=5),round(nrow(dfm)/5)+nrow(dfm)%%5)

m <- as.matrix(tdm)
# calculate the frequency of words as sort it by frequency
word_freq<- sort(rowSums(m), decreasing = T)
infra_cloud = wordcloud2(dfm, color = infra_palette, backgroundColor = "white")

library(htmlwidgets) 
saveWidget(infra_cloud,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)

library(htmlwidgets) 
#install.packages("webshot")
#webshot::install_phantomjs()
library(wordcloud2)
hw <- wordcloud2(demoFreq,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)

#### Organization Paper ####
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")
df_meta = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Metadata")

df_meta$`Article Name` == df_wide$`Article Name` # if the rows and columns do line up from both datasets. 
df_wide = cbind(df_wide,df_meta$Cassification) 
df_wide = df_wide %>%  
  rename(classification = `df_meta$Cassification`) %>% # rename the column
  filter(classification == "Organization") %>% # filter for only infrastructure papers
  select(-classification) # delete 
df = gather(df_wide,type,definitions,c(`Def 0`:`Def 35`)) # reshape the data into a long format
df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(doc_id = ID, text = definitions)

## Text Pre-processing 

# create corpus
def_corpus <- VCorpus(DataframeSource(df))
## Step 1: Eliminating extra whitespace
def_corpus<- tm_map(def_corpus, stripWhitespace)
## Step 2: Transform to lowercase
def_corpus<- tm_map(def_corpus, content_transformer(tolower))
## remove punctuation
def_corpus<- tm_map(def_corpus, removePunctuation)
## remove numbers
def_corpus<- tm_map(def_corpus, removeNumbers)


f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# remove stopwords or common words
def_corpus<- tm_map(def_corpus, removeWords, stopwords("SMART"))
def_corpus = tm_map(def_corpus, removeWords, "resilience")
def_corpus = tm_map(def_corpus, removeWords, "organization")

library(textstem)
def_corpus<- tm_map(def_corpus, PlainTextDocument)
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_strings))

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

def_corpus = tm_map(def_corpus, removeWords, "organization")
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_strings))

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# create term document matrix
tdm <- TermDocumentMatrix(def_corpus, control = list(wordlengths = c(1,Inf)))
# inspect frequent words
freq_terms <- findFreqTerms(tdm)
# View(freq_terms)
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq>=2)
dfm <- data.frame(term = names(term_freq), freq = term_freq)

# plot word frequency
df_plot <- dfm %>% top_n(25)
# Plot word frequency
ggplot(df_plot, aes(x = reorder(term, +freq), y = freq, fill = freq)) + geom_bar(stat = "identity")+ scale_colour_gradientn(colors = terrain.colors(10))+ xlab("Terms")+ ylab("Count")+coord_flip()+scale_fill_gradient(name="Frequency")

# Word Cloud
m <- as.matrix(tdm)
# calculate the frequency of words as sort it by frequency
word_freq<- sort(rowSums(m), decreasing = T)

organ_palette = rep(wes_palette("Darjeeling1",n=5),round(nrow(dfm)/5)+nrow(dfm)%%5)
organ_cloud = wordcloud2(dfm, color = organ_palette, backgroundColor = "white")

saveWidget(organ_cloud,"2.html",selfcontained = F)
webshot::webshot("2.html","2.png",vwidth = 1992, vheight = 1744, delay =10)

library(htmlwidgets) 
#install.packages("webshot")
#webshot::install_phantomjs()
library(wordcloud2)
hw <- wordcloud2(demoFreq,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)

#### Community Papers ####
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")
df_meta = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Metadata")

df_meta$`Article Name` == df_wide$`Article Name` # if the rows and columns do line up from both datasets. 
df_wide = cbind(df_wide,df_meta$Cassification) 
df_wide = df_wide %>%  
  rename(classification = `df_meta$Cassification`) %>% # rename the column
  filter(classification == "Community") %>% # filter for only infrastructure papers
  select(-classification) # delete 
df = gather(df_wide,type,definitions,c(`Def 0`:`Def 35`)) # reshape the data into a long format
df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(doc_id = ID, text = definitions)

## Text Pre-processing 

# create corpus
def_corpus <- VCorpus(DataframeSource(df))
## Step 1: Eliminating extra whitespace
def_corpus<- tm_map(def_corpus, stripWhitespace)
## Step 2: Transform to lowercase
def_corpus<- tm_map(def_corpus, content_transformer(tolower))
## remove punctuation
def_corpus<- tm_map(def_corpus, removePunctuation)
## remove numbers
def_corpus<- tm_map(def_corpus, removeNumbers)


f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# remove stopwords or common words
def_corpus<- tm_map(def_corpus, removeWords, stopwords("SMART"))
def_corpus = tm_map(def_corpus, removeWords, "resilience")
def_corpus = tm_map(def_corpus, removeWords, "urban")
def_corpus = tm_map(def_corpus, removeWords, "community")

library(textstem)
def_corpus<- tm_map(def_corpus, PlainTextDocument)
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_strings))
def_corpus<- tm_map(def_corpus, content_transformer(lemmatize_words))

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
def_corpus <- tm_map(def_corpus, f, 'http\\S+\\s*') # remove URLs
def_corpus <- tm_map(def_corpus, f, '#\\S+') # remove hashtags
def_corpus <- tm_map(def_corpus, f, '[[:cntrl:]]') # remove controls and special characters
def_corpus <- tm_map(def_corpus, f, "^[[:space:]]*") # remove leading whitespaces
def_corpus <- tm_map(def_corpus, f, "[[:space:]]*$") # remove trailing whitespaces 
g <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 
def_corpus <- tm_map(def_corpus, g, ' +') #remove extra whitespaces 

# create term document matrix
tdm <- TermDocumentMatrix(def_corpus, control = list(wordlengths = c(1,Inf)))
# inspect frequent words
freq_terms <- findFreqTerms(tdm)
# View(freq_terms)
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq>=2)
dfm <- data.frame(term = names(term_freq), freq = term_freq)

# plot word frequency
df_plot <- dfm %>% top_n(25)
# Plot word frequency
ggplot(df_plot, aes(x = reorder(term, +freq), y = freq, fill = freq)) + geom_bar(stat = "identity")+ scale_colour_gradientn(colors = terrain.colors(10))+ xlab("Terms")+ ylab("Count")+coord_flip()+scale_fill_gradient(name="Frequency")

# Word Cloud
m <- as.matrix(tdm)
# calculate the frequency of words as sort it by frequency
word_freq<- sort(rowSums(m), decreasing = T)

organ_palette = rep(wes_palette("Darjeeling1",n=5),round(nrow(dfm)/5)+nrow(dfm)%%5)
organ_cloud = wordcloud2(dfm, color = organ_palette, backgroundColor = "white")

saveWidget(organ_cloud,"2.html",selfcontained = F)
webshot::webshot("2.html","2.png",vwidth = 1992, vheight = 1744, delay =10)

library(htmlwidgets) 
#install.packages("webshot")
#webshot::install_phantomjs()
library(wordcloud2)
hw <- wordcloud2(demoFreq,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)
