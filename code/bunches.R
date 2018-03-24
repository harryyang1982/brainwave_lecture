# class 1

## SPSS와 Excel을 대체하는 R

### 기초통계 요약
summary(mtcars)

### t.test
library(tidyverse)

summary(mpg)
t.test(mpg$cty, mpg$hwy)

k <- sample(1:nrow(mpg), size = nrow(mpg) * 0.5)
training_set <- mpg[k,]
test_set <- mpg[-k,]

# training_set <- mpg[mpg$class == "suv",]$cty
# test_set <- mpg[mpg$class == "2seater",]$cty

t.test(training_set$cty, test_set$cty)

### 회귀분석

summary(lm(cty ~ displ, data = mpg))

## 데이터 전처리

### Filter
library(nycflights13)

glimpse(flights)

flights %>% 
  filter(origin == "JFK")


### summarise
flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = T))

### left_join

superheroes <- "
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, skip = 1)

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, skip = 1)

superheroes %>% 
  left_join(publishers, by = "publisher")

scores <- tribble(
  ~name, ~korean, ~english, ~math, 
  "양승훈", 65,      80,       70,  
  "박재범", 50,      100,       90,
  "아이린", 87,      90,       60,
  "아이유", 80,      70,       80
) %>% 
  mutate(total = (korean + english + math)/3)

class_table <- tribble(
  ~name, ~class,
  "양승훈", 1,
  "박재범", 2,
  "아이린", 3,
  "아이유", 1
)

left_join(scores, class_table) %>% 
  group_by(class) %>% 
  summarise(mean_total = mean(total))

### graph 그리기

### 점선그래프

ggplot(mpg) +
  geom_point(aes(cty, displ, color = class)) 

### 선그래프

library(Lahman)
Batting %>% 
  group_by(yearID) %>% 
  summarise(hit = mean(H)) %>% 
  ggplot(aes(yearID, hit)) +
  geom_line()

### 막대 그래프

diamonds %>% 

### SNA

library(tidyverse)
library(tidytext)
library(janeaustenr)

# Tokenizing by N-gram

austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

## Counting and Filtering N-grams

austen_bigrams %>% 
  count(bigram, sort = T)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2", sep = " "))

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigrams_filtered

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = T)

bigram_counts

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigrams_united

trigram_filtered <- austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word)

trigram_filtered

trigram_united <- trigram_filtered %>% 
  unite(trigram, word1, word2, word3, sep = " ")

trigram_united


## Analyzing Bigrams

bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = T)

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(bigram = reorder(bigram, tf_idf)) %>% 
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

trigram_tf_idf <- trigram_united %>% 
  count(book, trigram) %>% 
  bind_tf_idf(trigram, book, n) %>% 
  arrange(desc(tf_idf))

trigram_tf_idf

trigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(trigram = reorder(trigram, tf_idf)) %>% 
  ggplot(aes(trigram, tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## Using Bigrams to Provide Context in Sentiment Analysis

bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = T)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

not_words

not_words %>% 
  mutate(contribution = n * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = F) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = T) %>% 
  ungroup()

negated_words

negated_plots <- negated_words %>% 
  mutate(contribution = n * score,
         word1 = factor(word1)) %>% 
  group_by(word1) %>% 
  top_n(20, abs(contribution)) %>% 
  ungroup()

negated_plots

negated_plots %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = F) +
  xlab("Words preced by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip()

## Visualizing a Network of Bigrams with ggraph

library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Visualizing Bigrams in Other Texts

count_bigrams <- function(dataset) {
  dataset %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = T)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

library(gutenbergr)
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>% 
  count_bigrams()

kjv_bigrams %>% 
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>% 
  visualize_bigrams()

#### 워드 클라우드

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = T)))) %>% 
  ungroup()

original_books

tidy_books <- original_books %>% 
  unnest_tokens(word, text)
tidy_books

data(stop_words)

tidy_books <- tidy_books %>% 
  anti_join(stop_words)

word_count <- tidy_books %>% 
  count(word, sort = T)

word_count


library(wordcloud)
wordcloud(word_count$word, 
          freq=word_count$n,
          max.words = 100,
          min.freq = 10,
          random.order = F,
          rot.per = 0.1,
          colors = brewer.pal(8, "Dark2"))
