# Characteristics of creativity: wordclouds
#
# Author: Jason and Flo
#
# Revision history
# - 2016-09-17 Initial

library(data.table)
library(ggplot2)
library(magrittr)
library(tm)
library(wordcloud)


bag_of_words <- fread("data/characteristics_of_creativity.csv", select = 2:4) %>% unlist %>% unname %>% tolower

# for(i in seq_along(bag_of_words)) {
#   bag_of_words[i] <- gsub("risk taker", "risktaker", bag_of_words[i])
#   bag_of_words[i] <- gsub("risk-taker", "risktaker", bag_of_words[i])
#   bag_of_words[i] <- gsub("box", "outsidethebox", bag_of_words[i])
#   bag_of_words[i] <- gsub("square", "outsidethebox", bag_of_words[i])
#   bag_of_words[i] <- gsub("growth mindset", "growthmindset", bag_of_words[i])
#   bag_of_words[i] <- gsub("problem", "problemsolver", bag_of_words[i])
#   bag_of_words[i] <- gsub("open minded", "openminded", bag_of_words[i])
#   bag_of_words[i] <- gsub("open-minded", "openminded", bag_of_words[i])
#   bag_of_words[i] <- gsub("deep thinking", "deepthinker", bag_of_words[i])
#   bag_of_words[i] <- gsub("deep thinker", "deepthinker", bag_of_words[i])
#   bag_of_words[i] <- gsub("thinker", "think", bag_of_words[i])
#   bag_of_words[i] <- gsub("thinks", "think", bag_of_words[i])
#   bag_of_words[i] <- gsub("thinkgs", "think", bag_of_words[i])
#   bag_of_words[i] <- gsub("thinking", "think", bag_of_words[i])
#   bag_of_words[i] <- gsub("ence", "ent", bag_of_words[i])
# }


for(i in seq_along(bag_of_words)) {
  bag_of_words[i] <- gsub("risk take", "risktake", bag_of_words[i])
  bag_of_words[i] <- gsub("-", "", bag_of_words[i])
  bag_of_words[i] <- gsub("out side", "outside", bag_of_words[i])
  bag_of_words[i] <- gsub("out of the box|outside the box|outside the square", "outsidethebox", bag_of_words[i])
  bag_of_words[i] <- gsub("take risks|takes risks", "risktake", bag_of_words[i])
  bag_of_words[i] <- gsub("growth mindset", "growthmindset", bag_of_words[i])
  bag_of_words[i] <- gsub("problem solv", "problemsolv", bag_of_words[i])
  bag_of_words[i] <- gsub("open mind", "openmind", bag_of_words[i])
  bag_of_words[i] <- gsub("deep think", "deepthink", bag_of_words[i])
  bag_of_words[i] <- gsub("thinker", "think", bag_of_words[i])
}


bow_corpus <- Corpus(VectorSource(bag_of_words)) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords('english')) %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, c('the', 'this', 'things', 'able', 'ways', 'outside', 'can', 'takes', 'others', 'solver', 'solving', 'taking', 'teacher', 'etc', 'problemsolvers'))

words <- bow_corpus$content$content %>%
  sapply(strsplit, " ") %>%
  unlist %>%
  unname %>%
  .[. != ""] %>%
  sort

wordcloud(words, random.order = F, random.color = T, scale = c(4, 0.8), use.r.layout = T)
