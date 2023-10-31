# Basic Corpus Table
# List of all reviews in this review

# Load packages
library(dplyr)
library(xtable) # converts tables to latex tables
library(kableExtra)

# Table of List of publications
library(readr)
scopus <- read_csv("scopus.csv")

publications = scopus %>% select(Title,Authors,Year,DOI)
publications = publications %>% arrange(Title)

publications %>%
  kbl(caption = "List of Publications included") %>%
  kable_minimal()

# An empty column is added for 

xtable(publications,auto=TRUE) # makes Latex tables