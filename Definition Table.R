# Definition Table

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

library(xtable) # converts tables to latex tables
library(kableExtra)

definitions = df %>% 
  arrange(Doc_id) %>% 
  select(Abbreviation,texts) %>%
  mutate(texts = str_c('"', texts, '"'))

print(xtable(definitions),include.rownames = F)
