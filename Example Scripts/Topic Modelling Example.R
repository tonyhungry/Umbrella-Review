# Source: https://sicss.io/2020/materials/day3-text-analysis/topic-modeling/rmarkdown/Topic_Modeling.html 
# Video: https://www.youtube.com/watch?v=IUAHUEy1V0Q

library(topicmodels)
library(tm)

data("AssociatedPress")

inspect(AssociatedPress[1:5, 1:5])

AP_topic_model <- LDA(AssociatedPress,k=10,control=list(seed=321))

library(tidytext)
library(dplyr)
library(ggplot2)

AP_topics <- tidy(AP_topic_model, matrix = "beta")

ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# From Markdown
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# From the video 
ap_top_terms %>% 
  mutate(term = reorder(term,beta)) %>% 
  ggplot(aes(term,beta,fill=factor(topic))) + geom_col(show.legend = F) + 
  facet_wrap(~ topic, scales = "free") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=18)) + 
  labs(title = "Topic Model of AP News Articles", caption = "Top Terms by Topic (betas)") +
  ylab("") + xlab("") + coord_flip()

# Maybe try a few more different k's so you can find the best number of topics.

#### Structural Topic Modelling ####

google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)

library(stm)

processed <- textProcessor(poliblogs$documents, metadata = poliblogs)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# First STM model
First_STM <- stm(documents = out$documents, vocab = out$vocab, 
                 K = 10, prevalence =~ rating + s(day) ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

plot(First_STM)

# Find exemplary passages based on the topics generated
findThoughts(First_STM, texts = poliblogs$documents,n = 2, topics = 3)

# Find K (this takes a long time!! Approx 7 hours)
findingk <- searchK(out$documents, out$vocab, K = c(10:30),
                    prevalence =~ rating + s(day), data = meta, verbose=FALSE)
plot(findingk)

#### Working with Meta-Data ####
predict_topics<-estimateEffect(formula = 1:10 ~ rating + s(day), stmobj = First_STM, metadata = out$meta, uncertainty = "Global")

# With these plots, we can see how each topic is associated with political leaning
plot(predict_topics, covariate = "rating", topics = c(3, 5, 9),
     model = First_STM, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5','Topic 9'))

plot(predict_topics, covariate = "rating", topics = c(1:10),
     model = First_STM, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5','Topic 9'))

plot(predict_topics, "day", method = "continuous", topics = 3,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

# Interactive dashboard... DAMN!
library(LDAvis)
toLDAvis(First_STM,docs=docs) 
