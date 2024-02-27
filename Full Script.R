# Full Script

# Q1: Bibliographic Coupling ####

# Loading required packages
library(bibliometrix) 
library(igraph)
library(stringr)
library(dplyr)
library(xtable) # converts tables to latex tables
library(kableExtra)

# Convert cleaned data to data frame
M <- convert2df(file="savedrecs.txt", dbsource="wos",format="plaintext")

# Reformat the row names (names of the review articles) 
#input_string <- rownames(M)
#output_string <- gsub("([^,]+)\\s[^,]*,\\s(\\d+).*", "\\1 \\2", input_string)
#output_string[26] = "SHARIFI EI 2016"
#output_string[27] = "SHARIFI RSER 2016"
#rownames(M) = output_string

## Descriptive Statistics ####

# Main bibliometric results of the articles
results = biblioAnalysis(M)
summary(results)

# Most cited references
CR = citations(M,field="article",sep=";")
a=data.frame(cbind(CR$Cited[1:10])) # top 20 most cited references
colnames(a)[1] = "Number of Times Cited"
a %>%
  kbl(caption = "Top 10 Most Cited References") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# xtable(a,auto=TRUE) #This produces Latex table

## Bibliographic Coupling Network ####
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, Title = "", type = "auto", size.cex=F, size=3, remove.multiple=FALSE, labelsize=1, edgesize = 10, edges.min=1, label=TRUE, label.cex=F, cluster="leiden", curved=T)
# dotted lines means that there are still connections exist between papers, and the colored lines represent within cluster connections
#summary(net) # Look at the net object

bibnet = net$graph # extract the igraph object and name it bibnet 
V(bibnet)$label == V(bibnet)$name # make sure it is all true. 
nodenames = gsub(" [A-Za-z]+,", "", V(bibnet)$name) # transform the names/labels and make it a bit nicer
V(bibnet)$label = nodenames
V(bibnet)$name = nodenames

bibnetdata = data.frame(V(bibnet)$name,V(bibnet)$community)

summary(bibnet)
V(bibnet)$label.cex = degree(bibnet)/max(degree(bibnet))*1.2
V(bibnet)$size = degree(bibnet)/max(degree(bibnet)) * 3
par(mar=c(0,0,0,0))
plot(bibnet,layout=layout_with_fr,label.cex=degree(bibnet)) # plot it!

#edgelist = as_edgelist(bibnet) # Look at it in an edgelist.
df = net$cluster_res
sum(df$cluster==1) # 17 reviews are in one cluster 

is.weighted(bibnet) #TRUE
is.directed(bibnet) #FALSE
ecount(bibnet) # 1563
vcount(bibnet) # 29

average.path.length(bibnet,weights = E(bibnet)$weights) # 1.852217
transitivity(bibnet) # 0.8496723 (ignoring the weights)
diameter(bibnet, directed = F,weights=E(bibnet)$weights) # 3

#remove the weights
E(bibnet)$weight = count.multiple(bibnet)
bibnet = simplify(bibnet,edge.attr.comb = list(weight="min"))
any(which_multiple(bibnet))
E(bibnet)$weight

transitivity(bibnet,type="barrat",weights = E(bibnet)$weight)
graph.density(bibnet) #0.7980296
mean(degree(bibnet)) # 22.34483
mean(strength(bibnet,weights = E(bibnet)$weights)) # 107.7931

# Q2: Word Clouds ####

# Load required packages
library(tidyverse)
library(googlesheets4)

googlesheets4::gs4_auth(email = "tonyhung.th@gmail.com")

# Load data through Google Sheets
df_wide = read_sheet("https://docs.google.com/spreadsheets/d/1IPjq6CMAEd6HLEo0rBig30nWnEE7vL6d2cskd_bDODQ/edit?usp=sharing", sheet="Definitions")

bibnetdata[26,1] = "sharifi & yagmata 2016"
bibnetdata[25,1] = "sharifi 2016"
colnames(bibnetdata) = c("Abbreviation","Community")

df = df_wide %>% mutate(Abbreviation = tolower(Abbreviation)) %>% left_join(bibnetdata,by = join_by(Abbreviation)) %>% relocate(Community) %>% gather(type,definitions,c(`Def 0`:`Def 35`)) # reshape the data into a long format

df = df %>% drop_na() #drop empty rows
# rename colnames to match the requirements of dataframe source to create corpus
df <- df %>% rename(Doc_id = ID, texts = definitions, Type = type) %>% relocate(texts)

library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

normal_words = c("resilien*","transport*","define*","refer*","water")

# Load packages
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
# library(quanteda.corpora)
library(ggplot2)

set.seed(123)

corp = corpus(df,text_field = "texts") # create corpus

## Cluster 1 (paper=2) (community/urban/non-infra) word cloud ####
clust1 = corpus_subset(corp, Community == 1)

def_tok <- tokens(clust1, remove_punct = TRUE, remove_numbers = TRUE) %>% 
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
  ggtitle("Cluster 1 Resilience Definitions, Top 10")

textplot_wordcloud(dfmat, max_words = 1000, color = viridis::viridis(1000,direction = -1),min_size = 1,max_size = 3.5)

## Cluster 2 (paper = 1) (infra) word cloud ####
clust2 = corpus_subset(corp, Community == 2)

def_tok <- tokens(clust2, remove_punct = TRUE, remove_numbers = TRUE) %>% 
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
  ggtitle("Cluster 1 Resilience Definitions, Top 10")

textplot_wordcloud(dfmat, max_words = 100, color = viridis::plasma(100000),min_size = 1,max_size = 3.5)

