library(tidyverse)

## Removing "-int"
our.data <- read.csv("Finalized_Labeled_Accounts_EMOJIS_UNCHANGED.csv")
table(our.data$hand.label_simplified)

our.data$hand.label_simplified <- str_remove(our.data$hand.label_simplified, "-int")
table(our.data$hand.label_simplified)

our.data.pred <- read.csv("SVM_BERT_Predictions_for_Unlabeled_Accounts_EMOJIS_UNCHANGED.csv")
table(our.data.pred$hand.label_simplified)

final.data <- rbind(our.data %>% mutate(Label.Type = "Hand"),
                    our.data.pred %>% mutate(Label.Type = "Predict"))
final.data <- final.data %>% rename(Label = hand.label_simplified)
View(tail(final.data))

write.csv(final.data,
          file="Final_Account_Labels_for_Dashboard.csv",
          row.names = F)
