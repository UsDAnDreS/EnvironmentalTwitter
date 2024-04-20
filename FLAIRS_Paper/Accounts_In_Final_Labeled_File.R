our.data <- read.csv("FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_UNCHANGED.csv")
View(our.data)

table(our.data$hand.label_simplified)

# Acad: 538 (+1 from acad-media)
# Gov: 128
# Media: 1624
# Tourism: 194
# Other: 9441

write.csv(our.data %>% select(username, description, hand.label_simplified),
          row.names = F,
          file = "Finalized_Labeled_Accounts_EMOJIS_UNCHANGED.csv")
