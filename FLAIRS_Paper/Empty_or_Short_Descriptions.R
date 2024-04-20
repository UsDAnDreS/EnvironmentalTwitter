#### ISSUE: predictions aren't the same as before

source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

library(tokenizers)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textstem)

major.pred.classes <- read.csv("Major_Predicted_Classes.csv")
dim(major.pred.classes)
account.labels <- major.pred.classes[-c(1:231, 4091:4121, 4959:5499, 18417:18419),]
dim(account.labels)


# account.labels <- read.csv("UNLABELED_PREDICTED_accounts_W_PROBABILITIES_emojis_replaced.csv")

description.lengths <- sapply(account.labels$description, nchar)
description.lengths


print("Number of FULLY EMPTY descriptions:")
print(sum(description.lengths == 0))

zero.ind <- which(description.lengths == 0)
write.csv(account.labels[zero.ind,], file="Zero_Length_Descriptions.csv",
          row.names=F)

print("Number of short, NON-URL-containing, descriptions")
short.ind <- which(!str_detect(account.labels$description, "https.*|http.*") &
                   (description.lengths < 30) & (description.lengths > 0))
print(length(short.ind))

write.csv(account.labels[short.ind,], file="Short_Length_Descriptions_Below_30_char.csv",
          row.names=F)


View(account.labels[short.ind, ] %>% select(description))

