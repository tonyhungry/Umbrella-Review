# Source: https://cengel.github.io/R-text-analysis/ 

library(sotu)
library(tidyverse)
library(tidytext)
library(readtext)

#### Reading Text into R ####

# Let's take a look at the state of the union metadata
str(sotu_meta)
# sotu_dir writes the text files to disk in a temporary dir, 
# but you could also specify a location.
file_paths <- sotu_dir()
head(file_paths)

# let's read in the files with readtext
sotu_texts <- readtext(file_paths)
glimpse(sotu_texts)

# Merge with metadata
sotu_whole <- 
  sotu_meta %>%  
  arrange(president) %>% # sort metadata
  bind_cols(sotu_texts) %>% # combine with texts
  as_tibble() # convert to tibble for better screen viewing

glimpse(sotu_whole)

#### String Operations ####

# Counting occurrences
# How man times does the word “citizen” appear in each of the speeches?
sotu_whole %>% 
  mutate(n_citizen = str_count(text, "citizen")) 

sotu_whole %>% 
  mutate(n_citizen = str_count(text, "citizen"),
         n_cCitizen = str_count(text, "[C|c]itizen")) 

# Detecting Patterns
# What are the names of the documents where the words "citizen" and "Citizen" do not occur?
sotu_whole %>% 
  filter(!str_detect(text, "[C|c]itizen")) %>% 
  select(doc_id) 

# Extracting words
# Extracting the first five words of each speech by Woodrow Wilson
sotu_whole %>% 
  filter(president == "Woodrow Wilson") %>%  # sample a few speeches as demo
  pull(text) %>% # we pull out the text vector only
  word(end = 5) 

# Replacing and removing characters
# We use the str_replace_all function to replace all the ocurrences of the \n pattern with a white space " ". 
sotu_whole %>% 
  filter(president == "Woodrow Wilson") %>%  
  pull(text) %>%
  str_replace_all("\\n", " ") %>% # replace newline
  word(end = 5) 

# Remove whitespaces, because it is counted as a word
sotu_whole %>% 
  filter(president == "Woodrow Wilson") %>%  
  pull(text) %>%
  str_replace_all("\\n", " ") %>% 
  str_squish() %>%  # remove whitespaces
  word(end = 5) 

#### Tokenize ####

# Tokenize the speech table
tidy_sotu <- sotu_whole %>%
  unnest_tokens(word, text)
tidy_sotu

# Word tokenization with punctuation and no lowercasing
sotu_whole %>%
  unnest_tokens(word, text, to_lower = FALSE, strip_punct = FALSE)

# Sentence tokenization
sotu_whole %>%
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>% 
  select(sentence)

# N-gram tokenization as trigrams
sotu_whole %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  select(trigram)

#### Stopwords ####

# Stopwords removal
tidy_sotu_words <- tidy_sotu %>% 
  anti_join(stop_words)
tidy_sotu_words

#### Word Stemming ####
library(SnowballC)

tidy_sotu_words %>%
  mutate(word_stem = wordStem(word))