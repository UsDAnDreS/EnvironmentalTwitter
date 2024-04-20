library(topicmodels)
library(tidyverse)
library(ggplot2)
library(tidytext)

our.data <- read.csv("FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_UNCHANGED.csv")
head(our.data)


head(our.data)
our.text <- tibble(our.data %>% select(username, description))

bad.words <- tibble(word=c("http", "https", "com", "org", "t.co"))

data(stop_words)
our.tidy <- our.text %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  anti_join(bad.words) %>%
  group_by(username) %>%
  count(word)

head(our.tidy)

our.dtm <- our.tidy %>%
  cast_dtm(username, word, n)

######
## Fitting the LDA (Latent Dirichlet Topic model)
######

n.of.topics <- 20

our.lda <- LDA(our.dtm, k = n.of.topics, control = list(seed = 1234))
our.lda

user.topics <- tidy(our.lda, matrix = "beta")
user.topics

user_top_terms <- user.topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

user_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



## 20 topics

## Topic numbers for:
##  - ACAD: 2, 9, 15
##  - MEDIA: 6, 7, 12, 14, 17,

##  Topic #2 - mix between Acad & Media, but more acad (professor, photographer)
##  Topic #6 - media (meteorologist, anchor, award winning)
##  Topic #7 - likely media (producer)
##  Topic #9 - acad (science, research, environmental, marine, scientist, conservation)
##  Topic #12 - mix betw Acad & Media, but more media (writer, editor, teacher)
##  Topic #14 - media (editor, reporter, journalist)

##  Topic #15 - might be acad (educator, college)
##  Topic #17 - miiiight be media (author)

media.topics <- c(6,7,12, 14,17)
acad.topics <- c(2, 9,15)

user_documents <- tidy(our.lda, matrix = "gamma")
user_documents$our.topic <- ifelse(user_documents$topic %in% media.topics,
                                   "media",
                                   ifelse(user_documents$topic %in% acad.topics,
                                          "acad",
                                          "other"))

topic.model.group.assignments <- user_documents %>%
  group_by(document, our.topic) %>%
  summarise(gamma.sum = sum(gamma)) %>%
  group_by(document) %>%
  summarise(max.our.topic = our.topic[which.max(gamma.sum)])

table(topic.model.group.assignments$max.our.topic)

topic.model.group.assignments$max.our.topic
our.data <- our.data %>%
  left_join(topic.model.group.assignments,
            by=c("username"= "document"))

our.data$hand.label_simplified <- str_remove(our.data$hand.label_simplified,
                                             "-int")
round(prop.table(table(our.data$max.our.topic,
                 our.data$hand.label_simplified),
                 margin=1), 3)
round(prop.table(table(our.data$max.our.topic,
                       our.data$hand.label_simplified),
                 margin=2), 3)


## Adding the basic dictionary labels
dict.label <- read.csv("FINALIZED_DICTIONARY_LABELING_APPROACH.csv")
head(dict.label)

our.data.w.dict <- our.data %>%
  left_join(dict.label %>% select(username, max.class),
            by="username")
# table(dict.label$max.class)


## Just about 17.3% hit rate with "acad" predictions
## 31.5% with "media" predictions

round(prop.table(table(our.data.w.dict$max.class,
                       our.data.w.dict$hand.label_simplified),
                 margin=1), 3)

round(prop.table(table(our.data.w.dict$max.class,
                       our.data.w.dict$hand.label_simplified),
                 margin=2), 3)



### MAIN TAKEAWAY:
##    After a suggestion from a reviewer, we have also played with topic modeling.
##    In particular, we sort of "reverse engineered" the process, trying to see if it could've been helpful
##  for us to use as the "preliminary labeling" strategy.
##    We looked at the user accounts which we have handlabeled, 
## and proceeded to run a 20-topic Latent Dirichlet Analysis on those. 
## It gave us several topics where top terms clearly included things relevant to some of the
## categories of our utmost interest, such as media (words like reporter, journalist, writer, editor) &
##  academia (professor, scientist, conservationist).
## We grouped up all the topics with top terms indicative of media into a "media" bucket,
## those of academica - into "acad" bucket, and then went ahead to check HOW WELL THOSE MATCHED
## WITH OUR HAND-LABLES
##  The performance wasn't necessarily too terrible, but not that great either:
##    * Whenever topic model predicted "media", it was correct about 31.5% of the time; 
##        and whenever the true value was "media", it was correct 32.8% of the time
##    * Whenever topic model predicted "acad", it was correct about 17% of the time;
##        and whenever the true value was "acad", it was correct 27.1% of the time
## Those numbers aren't too terrible, BUT.. they don't compare to the extremely basic DICTIONARY-BASED preliminary labeling,
##  which hit over 50%, and at times around 80%, for these classes.
## Given that we needed to use some knowledge of dictionary terms to even figure out the topics
## resulting from topic modeling (as it only gives us the top terms per topic, without any actual name for the topic)
## utilizing topic modeling likely wouldn't have helped us that much more than the dictionary-based labeling did.
## PLUS, we also had a preliminary idea of which "topics" are important to discern (based on the ad-hoc committee recommendations),
## which made dictionary-based approach more intuitive.
##
## PLUS, a good majority of topic modelling is TRYING TO DISTINGUISH BETWEEN THE "REGULAR CITIZEN"
## CATEGORIES (which is natural, as it's a giant category with a bunch of variability within),
## which isn't what we're interested in. 
## PLUS, it couldn't quite pick out the government, tourbiz


