library(tidyverse)


labeled_8K <- read.csv("finalized_8K_accounts_emojis_replaced.csv")
labeled_biased <- read.csv("finalized_BIASED_accounts_ALL_emojis_replaced.csv")
labeled_major <- read.csv("Major_Predicted_Classes_FINALIZED.csv")
labeled_major <- labeled_major[c(1:231, 4091:4121, 4959:5499, 18417:18419),]
labeled_nonmajor <- read.csv("Non_Major_Predicted_Classes_FINALIZED.csv")
labeled_zero_length <- read.csv("Zero_Length_Descriptions.csv")
labeled_short_length <- read.csv("Short_Length_Descriptions_Below_30_char.csv")

print(dim(labeled_8K))
print(dim(labeled_biased))
print(dim(labeled_major))
print(dim(labeled_nonmajor))
print(dim(labeled_zero_length))
print(dim(labeled_short_length))

our.labeled.user.data <- rbind(labeled_8K %>% select(hand.label, hand.label_simplified, expert.ind, username, description),
      labeled_biased %>% select(hand.label, hand.label_simplified, expert.ind, username, description),
      labeled_major  %>% select(hand.label, hand.label_simplified, expert.ind, username, description), 
      labeled_nonmajor  %>% select(hand.label, hand.label_simplified, expert.ind, username, description),
      labeled_zero_length  %>% select(hand.label, hand.label_simplified, expert.ind, username, description),
      labeled_short_length %>% select(hand.label, hand.label_simplified, expert.ind, username, description))

our.labeled.user.data$label.ind <- TRUE


all_accounts <- read.csv("All_Twitter_Accounts.csv")
all_accounts$hand.label <- "other"
all_accounts$hand.label_simplified <- "other"
all_accounts$expert.ind <- "no"

lab_and_unlab_accounts <- rbind(
  our.labeled.user.data,  
  all_accounts %>% 
    filter(!username %in% our.labeled.user.data$username) %>%
    select(hand.label, hand.label_simplified, expert.ind, username, description) %>%
    mutate(label.ind = FALSE))

write.csv(lab_and_unlab_accounts,
  file="Labeled_and_Unlabeled_ALL_ACCOUNTS.csv")




# full.df <- read.csv("full_df_unique_emojis_replaced.csv")
# dim(full.df)



full.df <- NULL


source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

for (k in 1:6){

  ####
  ## Loading the data
  ####

  # k <- 2  # Which area? 1-6
  # full.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))

  interm.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))

  full.df <- rbind(full.df,
                   interm.df %>% select(id, username, text, text_with_display_links, created_at.x, verified, 
                                        public_metrics.x_retweet_count,
                                        public_metrics.y_followers_count)
  )
}






dim(lab_and_unlab_accounts)
full.df.follower.only <- full.df %>% 
  group_by(username) %>%
  summarise(follower.count = max(public_metrics.y_followers_count))

lab_and_unlab_accounts_augmented <- 
  lab_and_unlab_accounts %>% left_join(full.df.follower.only)



write.csv(lab_and_unlab_accounts_augmented %>%
       arrange(desc(label.ind), desc(follower.count)) %>%
       select(label.ind, follower.count, hand.label, hand.label_simplified, expert.ind, username, description),
  file="Ordered_by_follower_Count_and_Prelabel_Status.csv")

# boxplot(lab_and_unlab_accounts_augmented$follower.count[lab_and_unlab_accounts_augmented$follower.count < 1000000])
