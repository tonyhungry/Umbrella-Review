# Abstract Co-occurence Network

library(bibliometrix) 
require(quanteda)
require(quanteda.textplots)

set.seed(123)

# Convert cleaned data to data frame using the Bibliometrix Package
M <- convert2df(file="savedrecs.txt", dbsource="wos",format="plaintext")
detach("package:bibliometrix", unload = TRUE)

# Extract the columms that I really need, which are AB(Abstract), DE(Author's Keywords), and TI (Title)
akt.df  = cbind.data.frame(M$AB, M$DE, M$TI)
colnames(akt.df) = c("AB","DE","TI")

# Abstract Only ####

# names = rownames(M)
# abs = cbind.data.frame(names,M$AB)
# colnames(abs) = c("docnames","texts")
# 
# abs.corp = corpus(abs,text_field="texts")
# ndoc(abs.corp) #29
# 
# toks <- abs.corp %>%
#   tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) %>%
#   tokens_tolower() %>% 
#   tokens_remove(pattern = c(stopwords(source="smart"), "purpose", "design", "methodology", "approach", "finding*", "research", "implication*", "conclusion*","review","resilien*","systematic","resilience","review","research","study","studies","systematic","results","literature","paper","article","systematically","due"),padding=F) %>% 
#   tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
#   tokens_remove(pattern=c("article","aim"))
# fcmat <- fcm(toks, context = "window", tri = FALSE)
# feat <- names(topfeatures(fcmat, 100))
# top = fcm_select(fcmat, pattern = feat)
# feata = names(topfeatures(fcmat, 100))
# 
# # Look at the lemma table to see which words came from where
# df = lexicon::hash_lemmas
# # View(df)
# 
# net = as.igraph(top) # convert to igraph object to use it 
# require(igraph)
# net = as.undirected(net) # convert it to undirected network
# summary(net) # Frequency is essentially the weight. 
# # plot(net)
# clusters = cluster_louvain(net)
# numb.clus = length(sizes(clusters)) 
# require(viridis)
# # plot(net, vertex.color=viridis(3,alpha=0.5)[clusters$membership],curved=T)
# detach("package:igraph", unload = TRUE)
# 
# options(ggrepel.max.overlaps = Inf)
# textplot_network(top,vertex_labelsize = 1.3*Matrix::rowSums(top) /min(Matrix::rowSums(top)),vertex_color = viridis(numb.clus,alpha=0.9)[clusters$membership],edge_color="#B2BABB",max.overlaps=Inf)
# 
# # Use this if <= 5 colors...
# require(wesanderson) 
# textplot_network(top,vertex_labelsize = 1.3*Matrix::rowSums(top) /min(Matrix::rowSums(top)),vertex_color = wes_palette("Darjeeling1",numb.clus,type="continuous")[clusters$membership],edge_color="#B2BABB")

# AKT - All THREE TOGETHER <3 ####
akt = as.data.frame(paste(akt.df$AB, akt.df$DE, akt.df$TI))
colnames(akt) = "texts"
names = rownames(M)
abs = cbind.data.frame(names,M$AB)
colnames(abs) = c("docnames","texts")

abs.corp = corpus(abs,text_field="texts")
ndoc(abs.corp) #29

# require(textstem)
# lemma_dictionary_hs <- make_lemma_dictionary(abs.corp, engine = 'hunspell')
# detach("package:textstem", unload = TRUE)
# 
# # The textstem package screws up things so R session needs to restart. 
# library(rstudioapi)
# executeCommand("restartR")
# require(quanteda)
# require(quanteda.textplots)
# set.seed(123)

toks <- abs.corp %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords(source="smart"), "purpose", "design", "methodology", "approach", "finding*", "research", "implication*", "conclusion*","review","resilien*","systematic","resilience","review","research","study","studies","systematic","results","literature","paper","article","systematically","due"),padding=F) %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove(pattern=c("article","aim")) %>% 
  tokens_replace(pattern="assessment",replacement="assess")
fcmat <- fcm(toks, context = "document", tri = FALSE,count="frequency")
feat <- names(topfeatures(fcmat, 100))
top = fcm_select(fcmat, pattern = feat)

net = as.igraph(top) # convert to igraph object to use it 
require(igraph)
net = as.undirected(net) # convert it to undirected network
summary(net) # Frequency is degree centrality of each node
# plot(net)
clusters = cluster_louvain(net)
numb.clus = length(sizes(clusters)) 
# require(viridis)
# plot(net, vertex.color=viridis(3,alpha=0.5)[clusters$membership],curved=T)
detach("package:igraph", unload = TRUE)

options(ggrepel.max.overlaps = Inf)
#textplot_network(top,vertex_labelsize = 1.3*Matrix::rowSums(top) /min(Matrix::rowSums(top)),vertex_color = viridis(numb.clus,alpha=0.9)[clusters$membership],edge_color="#B2BABB",max.overlaps=Inf)

require(wesanderson) 
# plot it!
textplot_network(top,vertex_labelsize = 1.3*Matrix::rowSums(top) /min(Matrix::rowSums(top)),vertex_color = wes_palette("Darjeeling1",numb.clus,type="continuous")[clusters$membership],edge_color="#B2BABB",edge_size = 2)

# feat == feata
# Since there is not difference, then I will take the Title, Abstract Keywords Co-occurrence Network

# Extracting which words go to which cluster
word.clust = cbind.data.frame(clusters$names,clusters$membership)


# Network Characteristics
require(intergraph)
g = as.network(top)
net = asIgraph(g)
detach("package:intergraph", unload = TRUE)

require(igraph)
summary(net)

is.weighted(net) #TRUE
is.directed(net) #FALSE
ecount(net) # 2099
vcount(net) # 100

average.path.length(net,weights = E(net)$weight) # 10.47919
transitivity(net) # 0.5868664 (ignoring the weights)
diameter(net, directed = F,weights=E(net)$weight) # 21
mean(strength(net,weights = E(net)$weights)) # 557.08

#remove the weights
E(net)$weight = count.multiple(net)
net = simplify(net,edge.attr.comb = list(weight="min"))
any(which_multiple(net))
E(net)$weight

# transitivity(net,type="barrat",weights = E(net)$weight) #??? It's the same as above... Investigate. 
graph.density(net) #0.4240404
mean(degree(net)) # 41.98

