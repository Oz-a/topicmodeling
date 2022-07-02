library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(tm)
library(ggplot2)
library(forcats)
library(topicmodels)
library(reshape2)

#READ IN TEXTS THAT WERE CLEANED IN PYTHON
#those using the republican calendar
PereDuchene <- read_file("clean_duchene.txt")
JournalOfficiel <- read_file("clean_journofficiel.txt")
LeRappel <- read_file("clean_lerappel.txt")

#those not using the republican calendar
CriDuPeuple <- read_file("clean_cridupeuple.txt")
LeProletariat <- read_file("clean_prolet.txt")
LeFigaro <- read_file("clean_lefigaro.txt")

#SOME MORE CLEANING
#clean digits and punctuation and whitespace with regex
PereDuchene <- PereDuchene %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")
JournalOfficiel <- JournalOfficiel %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")
LeRappel <- LeRappel %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")
CriDuPeuple <- CriDuPeuple %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")
LeProletariat <- LeProletariat %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")
LeFigaro <- LeFigaro %>% str_replace_all('[[:digit:]]+', '') %>% str_replace_all("[[:punct:]]", '') %>% stripWhitespace() %>% str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ")

#Turn data into a tibble
journals <- c(PereDuchene, JournalOfficiel, LeRappel, CriDuPeuple, LeProletariat, LeFigaro)
journal_titles <- c('PereDuchene', 'JournalOfficiel', 'LeRappel', 'CriDuPeuple', 'LeProletariat', 'LeFigaro')
journal_df <- tibble(title = journal_titles, text = journals)

#bring it to tidy one word one journal format and get word counts
tidy_journals <- journal_df %>%  
  unnest_tokens(input = text, output=word) %>%
  count(title, word, sort = TRUE) %>% mutate(RC = case_when(title %in% c('PereDuchene', 'JournalOfficiel', 'LeRappel') ~ "Uses RC", 
                                                        title %in% c('CriDuPeuple', 'LeProletariat', 'LeFigaro') ~ "No RC"))

#get total word counts per book for term frequency analysis
total_n <- tidy_journals %>% 
  group_by(title) %>% 
  summarize(total = sum(n))



#bind the to data frames
tidy_journals_by_book <- left_join(tidy_journals, total_n) #by journal


#Term frequency distribution of journals
ggplot(tidy_journals_by_book, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y")


#Term frequency divided by inverse document frequency
#It's done to find the documents that are common in a document, but not too common across all documents
#TF_IDF by journal
journal_tf_idf <- tidy_journals_by_book %>%
  bind_tf_idf(word, title, n)

top40_by_tfidf <- journal_tf_idf %>% group_by(title) %>% slice_max(tf_idf, n = 40)
## only get the term frequencies
journal_tf_idf %>% group_by(title) %>% select(-total, -idf, -tf_idf) %>% slice_max(tf, n = 40) %>% write.csv("./top_terms_by_frequency.csv")

#A visual comparison of documents with highest tf-idf scores
#Showing terns that are used in one journal but not others
journal_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


### TOPIC MODELING ###
# Going to apply LDA (Latent Drichlet Allocation)
# It treates each document as a mixture of topics and,
# each topic as a mixture of words (Silge and Robinson, "TM in R")

#We need a document-term matrix for that
journals_dtm <- tidy_journals_by_book %>%
  cast_dtm(title, word, n)

#24 Model version
#running the lda function to get a 24-topic model
journals_lda <- LDA(journals_dtm, k = 24, control = list(seed = 1234))

#per-topic per-word possibilities indicated by beta
#the probability of a term being generated from a topic (shown with numbers)
journal_topics <- tidy(journals_lda, matrix = "beta")

#top 10 terms for each topic
top_terms <- journal_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#a visualization of the top 10 terms of each topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#Lets see which topics are associated with which journals
#We use gamma for that
journals_gamma <- tidy(journals_lda, matrix = "gamma")

journals_gamma %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))


#6 Model version
#running the lda function to get a 6-topic model
journals_lda6 <- LDA(journals_dtm, k = 6, control = list(seed = 1234))

#per-topic per-word possibilities indicated by beta
#the probability of a term being generated from a topic (shown with numbers)
journal_topics6 <- tidy(journals_lda6, matrix = "beta")

#top 10 terms for each topic
top_terms6 <- journal_topics6 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#a visualization of the top 10 terms of each topic
top_terms6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#Lets see which topics are associated with which journals
#We use gamma for that
journals_gamma6 <- tidy(journals_lda6, matrix = "gamma")

journals_gamma6 %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

