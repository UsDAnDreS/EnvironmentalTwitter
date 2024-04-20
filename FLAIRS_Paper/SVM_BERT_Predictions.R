library(tidyverse)

our.pred <- read.csv("SVM_BERT_unweighted_UNLABELED_PREDICTED_accounts_W_PROBABILITIES_emojis_unchanged.csv")

View(our.pred)
our.pred <- our.pred %>% select(-description_lemmatized)
table(our.pred$hand.label_simplified)

our.pred$description[our.pred$hand.label_simplified == "acad"]

our.pred$description[our.pred$hand.label_simplified == "gov"]

our.pred$description[our.pred$hand.label_simplified == "tourbiz"]

our.pred$description[our.pred$hand.label_simplified == "media"]


write.csv(our.pred %>% select(username, description, hand.label_simplified),
          row.names = F,
          file = "SVM_BERT_Predictions_for_Unlabeled_Accounts_EMOJIS_UNCHANGED.csv")
