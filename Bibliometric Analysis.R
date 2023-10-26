# Bibliometric Analysis (Findings Part 1)

#### Setting up the script ####
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

#### Descriptive Statistics ####

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

#### Bibliographic Coupling Network ####
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, Title = "Bibliographic Coupling Network", type = "auto", size.cex=TRUE, size=3, remove.multiple=FALSE, labelsize=3, edgesize = 10, edges.min=1, label=TRUE, label.cex=TRUE, cluster="louvain", curved=TRUE)
# dotted lines means that there are still connections exist between papers, and the colored lines represent within cluster connections
summary(net) # Look at the net object

bibnet = net$graph # extract the igraph object and name it bibnet 
V(bibnet)$label == V(bibnet)$name # make sure it is all true. 
nodenames = gsub(" [A-Za-z]+,", "", V(bibnet)$name) # transform the names/labels and make it a bit nicer
V(bibnet)$label = nodenames
V(bibnet)$name = nodenames
plot(bibnet) # plot it!
# summary(bibnet)

#edgelist = as_edgelist(bibnet) # Look at it in an edgelist.
net$cluster_res

# As stated in the slides, it is very interesting to see that with Louvain clustering, there are two clusters grouped just by using the references that each review cites.

# Have to make the cluster table by hand instead...

####  Abstract Co-occurence Network, 1-gram #### 
removeterm = c(stopwords(language = "en", source = "smart"),"resilience","review","research","study","studies","systematic","results","literature","paper")
abstract_terms = termExtraction(M, Field="AB", ngrams=1, verbose=T, stemming = F,remove.terms = removeterm)
NetMatrix <- biblioNetwork(abstract_terms, analysis = "co-occurrences", network = "abstracts")
summary(networkStat(NetMatrix))
net=networkPlot(NetMatrix, Title = "Abstract Co-occurence Network", type = "auto", size.cex=TRUE, size=3, remove.multiple=T, labelsize=2, edgesize = 4, edges.min=4, label=TRUE, label.cex=TRUE, cluster="louvain", curved=T, remove.isolates = TRUE) # 4 clusters seems to be the most stable one.

net$cluster_res
# I'm debating if this should be included... I don't think so, because the previous network already does the summarization job... Also, a lot harder to interpret.

#### Unused Codes ####

# # Abstract Co-occurence Network, 2-grams 
# abstract_terms = termExtraction(M, Field="AB", ngrams=2, verbose=F, stemming = TRUE) # need to first extract terms.
# NetMatrix <- biblioNetwork(abstract_terms, analysis = "co-occurrences", network = "abstracts")
# net=networkPlot(NetMatrix, Title = "Abstract Co-occurence Network", type = "auto", size.cex=TRUE, size=3, remove.multiple=FALSE, labelsize=1.5, edgesize = 10, edges.min=2, label=TRUE, label.cex=TRUE, cluster="louvain", curved=TRUE, remove.isolates = TRUE)
# 
# # Co-word Analysis through Scopus Keyword co-occurences
# NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Scopus Keywords Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=2,label.cex=TRUE,label.n=30,edges.min=1)
# 
# # Historiograph. Better not to use this, because it is rather broken 
# histResults = histNetwork(M, sep=";", min.citations = 1)
# net = histPlot(histResults,size=5,labelsize=4) # a lot of papers are not shown because there aren't any citations? But we know this is not true...
# histResults$histData # shows direct citation results 
# # bibliometrix:::histPlot #shows the source code for histPlot
# 
# # Multiple Correspondence Analysis through abstracts
# suppressWarnings(
#   CS <- conceptualStructure(M, method="MCA", field="AB", minDegree=11, clust="auto", stemming=T, labelsize=10,documents=29)
# ) #minDegree of 10 works the best
# 
# # Metric Multidensional Scaling through abstracts
# suppressWarnings(
#   CS <- conceptualStructure(M, method="MDS", field="AB", minDegree=18, clust="auto", stemming=T, labelsize=10)
# ) #minDegree of 10 works the best
# 
# # Shiny Interface
# biblioshiny() #to use the Shiny interface.

#### Author Keywords Analysis  #### 
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";",remove.terms = "resilience")
net=networkPlot(NetMatrix, Title = "Author Keywords Co-occurence Network", type = "auto", size.cex=TRUE, size=3, remove.multiple=FALSE, labelsize=2, edgesize = 10, edges.min=1, label=TRUE, label.cex=TRUE, cluster="louvain", curved=TRUE,  remove.isolates = TRUE)

net$cluster_res
# Using Louvain clustering, 10/11 clusters were identified. It's interesting to see that that there are three isolated clusters that seems to be by itself. By explaining through these clusters, the "summarization" of the different reviews would be complete. 

####  Co-citation network #### 
# Shows how the different cited references relate to each other
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, Title = "Co-citation Network", type = "auto", size.cex=TRUE, size=3, remove.multiple=FALSE, labelsize=1.5, edgesize = 10, edges.min=5, label=TRUE, label.cex=TRUE, cluster="louvain", curved=TRUE, remove.isolates = TRUE)

net$cluster_res
# This network shows nodes that has at least 5 edges. This filter was chosen in order to reveal the most important cited literature the reviews uses. 
# In a way, this network is the flip side of bibliographic coupling network. 