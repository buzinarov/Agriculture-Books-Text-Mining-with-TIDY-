installed.packages("dplyr")
install.packages("purr")
install.packages("tm.plugin.webmining")
install.packages("tidytext")
install.packages("gutenbergr")
install.packages("ggplot2")

library(dplyr)
library(tm.plugin.webmining)
library(purrr)
library(tidytext)
library(gutenbergr)
library(ggplot2)

##downloading books
agriculture <- gutenberg_download(c(39791, 32158))

## spliting the rows and removing stop words

tidy_agriculture <- agriculture %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word, "illustration", "figure")) %>%
  anti_join(stop_words)






#Counting words
tidy_agriculture %>%
  count(word, sort = TRUE)



##List of most frequent words (over 200)
tidy_agriculture %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + ggtitle("The Most Common Words in Agriculture's books")



##Sentiment Analysis - Count of Words and Sentiment Categorie
bing_word_counts <- tidy_agriculture %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

## Sentiment categories - classifying the words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Words Contribute to sentiment",
       x = NULL) +
  coord_flip()

## Word cloud plot
install.packages("wordcloud")
library(wordcloud)
tidy_agriculture %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Reploting the wordcloud according to the sentiment
install.packages("reshape2")
library(reshape2)
tidy_agriculture %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


##The relationship between the words "pairing words - biagram"
agriculture_bigrams <- agriculture %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
agriculture_bigrams




##Removing stop words
install.packages("tidyr")
library(tidyr)
bigrams_separated <- agriculture_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% c(stop_words$word, "illustration", "figure", "illustration figure")) %>%
  filter(!word2 %in% c(stop_words$word, "illustration", "figure", "illustration figure"))

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united


##Visualizing the new set of biagrams
bigram_tf_idf <- bigrams_united %>%
  count(bigram)
bigram_tf_idf <- bigram_tf_idf %>% filter(n>30)
ggplot(aes(x = reorder(bigram, n), y=n), data=bigram_tf_idf) + geom_bar(stat = 'identity') + ggtitle("The Most Common Bigrams in Agriculture's Books") + coord_flip()

##Visualizing the network of the new set of biagrams
install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

install.packages("ggraph")
library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8) + ggtitle("Common Bigrams in Two agriculture books")


