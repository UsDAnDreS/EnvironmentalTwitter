#### ISSUE: predictions aren't the same as before

source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

library(tokenizers)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textstem)


# lem.all.users.df <- read.csv("lemmatized_userbio.csv")
# colnames(lem.all.users.df)
# 
# finalized.8K <- read.csv("finalized_8K_accounts_emojis_replaced.csv")
# finalized.biased <- read.csv("finalized_BIASED_accounts_ALL_emojis_replaced.csv")
# 
# write.csv(lem.all.users.df %>% 
#   filter(!username %in% c(finalized.8K$username, finalized.biased$username)) %>%
#     select(username, description),
#   row.names=F,
#   file="UNLABELED_accounts.csv")



## Acad - a BUNCH of bad ones.. "education", "evolutionary" seems to trigger the classification
## similar for Media.. seems like whenever there's "listen" or "update", it triggers false positives

account.labels <- read.csv("UNLABELED_PREDICTED_accounts_W_PROBABILITIES_emojis_replaced.csv")
account.labels.2 <- read.csv("UNLABELED_PREDICTED_accounts_emojis_replaced.csv")

## REMOVE ALL THE OLD ONES
account.labels <- account.labels %>% filter(!username %in% account.labels.2[account.labels.2$hand.label_simplified != "other",]$username )
dim(account.labels)
dim(account.labels.2)

table(account.labels$hand.label_simplified)
account.labels$predicted_label <- account.labels$hand.label_simplified
account.labels$hand.label <- account.labels$hand.label_simplified
account.labels$expert.ind <- "No"

dim(account.labels)

#account.labels <- account.labels %>% filter(hand.label_simplified == "other")
dim(account.labels)
max.minor.ind <- apply(account.labels %>% select(acad_prob, gov_prob, media_prob, tourbiz_prob),
                   1,
                   which.max)
max.minor.ind
account.labels$max.minor.class <- c("acad", "gov", "media",  "tourbiz")[max.minor.ind]

account.labels$max.minor.prob <- apply(account.labels %>% select(acad_prob, gov_prob, media_prob, tourbiz_prob),
                                       1,
                                       max)

write.csv(account.labels[, c("hand.label", "hand.label_simplified", "expert.ind", "max.minor.class", "max.minor.prob",  "username", "description", "description_lemmatized")] %>%
            # filter(hand.label_simplified == "other") %>%
            arrange(max.minor.class, hand.label_simplified, desc(max.minor.prob)),
          file="Major_Predicted_Classes.csv")
