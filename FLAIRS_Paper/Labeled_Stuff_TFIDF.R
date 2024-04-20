library(tidyverse)

start.time <- as.Date("2018-01-01")
end.time <- as.Date("2023-01-01")

exclude.retweets <- TRUE

# 
what.to.include <- c("labeled_only", "predictions_only", "everything")[3]

load(paste0("ACCOUNTS_EMOJIS_REPLACED_tf_idf_unique_users_",
            what.to.include, "_",
            start.time, "_", end.time,
            ifelse(exclude.retweets, "", "_WITH_RETWEETS" ),
            ".Robj"))

load(paste0("ACCOUNTS_EMOJIS_REPLACED_tf_idf_",
            what.to.include, "_",
            start.time, "_", end.time,
            ifelse(exclude.retweets, "", "_WITH_RETWEETS" ),
            ".Robj"))


## REMOVING THE "INTERNATIONAL" & MISCELLANEOUS
tf.idf.unique.users <- tf.idf.unique.users %>% 
  filter(acc_type %in% c("acad", "gov", "media", "other", "tourbiz"))

print(dim(tf.idf.unique.users))

head(tf.idf.unique.users %>% 
       filter(acc_type %in% c("acad", "gov", "media", "other", "tourbiz")))

# tf.idf

## MENTIONS:

# # TF-IDF
# View(tf.idf %>%
#         group_by(acc_type) %>%
#         slice_max(order_by = tf_idf, n=10) %>%
#        select(-tf,-idf))
# 
# # MOST MENTIONED
# View(tf.idf %>%
#        group_by(acc_type) %>%
#        slice_max(order_by = n, n=10) %>%
#        select(-tf,-idf))

# print(tf.idf %>%
#         group_by(acc_type) %>%
#         slice_max(order_by = tf_idf, n=10),
#       n=60)

#####
## UNIQUE USERS:
#####

# TF-IDF

tf.idf.frame <- tf.idf.unique.users %>%
  filter(is.na(suppressWarnings(as.numeric(word))),
         n>=2) %>%
  # mutate(tf_idf = round(tf_idf, 5)) %>%
  group_by(acc_type) %>%
  slice_max(order_by = tf_idf, n=10
            , with_ties=FALSE
  ) %>%
  select(-tf,-idf)

View(tf.idf.frame)

# "hes" still shows up as a word for some reason
tf.idf.frame <- tf.idf.frame %>%
  filter(!word %in% c("hes", "heres"))

library(wordcloud)
library(plotrix)
#wordcloud(tf.idf.frame$word, tf.idf.frame$tf_idf, min.freq=0)

set.seed(1)
wordcloud(tf.idf.frame$word, tf.idf.frame$tf_idf, # rep(0.5, length(tf.idf.frame$tf_idf)),
          # max.words=max.words,
          min.freq = 0,
          random.order=F,
          # colors=c(1:5),
          # colors=ifelse(tf.idf.frame$word %in% )
          colors=ifelse(tf.idf.frame$acc_type == "acad", "#F8766D",
                        ifelse(tf.idf.frame$acc_type == "gov", "#A3A500",
                               ifelse(tf.idf.frame$acc_type == "media", "#00BF7D",
                                      ifelse(tf.idf.frame$acc_type == "tourbiz", "#E76BF3",
                                             "#00B0F6")))),
          ordered.colors = TRUE,
          
          rot.per=0,
          fixed.asp = FALSE)

# https://www.statology.org/ggplot-default-colors/#:~:text=By%20default%2C%20ggplot2%20chooses%20to,and%20blue%20for%20the%20bars.&text=Here's%20how%20to%20interpret%20the,in%20the%20plot%20is%20%2300BA38.
# "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"

legend("topright", 
       legend = sort(unique(tf.idf.frame$acc_type)), 
       fill=c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6",  "#E76BF3"))

title("TF-IDF Word Cloud\n (Retweets Excluded)")


# MOST MENTIONED
# View(tf.idf.unique.users %>%
#        filter(is.na(suppressWarnings(as.numeric(word))),
#               n>=2) %>%
#        mutate(tf_idf = round(tf_idf, 4)) %>%
#        group_by(acc_type) %>%
#        slice_max(order_by = n, n=10, with_ties=FALSE) %>%
#        select(-tf,-idf))
