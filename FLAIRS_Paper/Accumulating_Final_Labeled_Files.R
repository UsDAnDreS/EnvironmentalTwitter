## Get the most followers first
## Add the: 8K, full biased, major, non-major => Create "Training file"
## ON TOP OF IT, add: zero

most.follow <- read.csv("Ordered_by_follower_Count_and_Prelabel_Status.csv")
#table(most.follow$description)[which.max(table(most.follow$description))]
#sort(table(most.follow$description), descending=T)[1:5]

most.follow$description <- ifelse(is.na(most.follow$description), "", most.follow$description)
most.follow <- most.follow[c(1:148, 17141:17250), ]
most.follow <- most.follow[!duplicated(most.follow$username), ]

labeled_8K <- read.csv("finalized_8K_accounts_emojis_replaced.csv")
labeled_biased <- read.csv("finalized_BIASED_accounts_ALL_emojis_replaced.csv")
labeled_nonmajor <- read.csv("Non_Major_Predicted_Classes_FINALIZED.csv")
labeled_major <- read.csv("Major_Predicted_Classes_FINALIZED.csv")
labeled_major <- labeled_major[c(1:231, 4091:4121, 4959:5499, 18417:18419),]

training_df <- rbind(most.follow %>% select(hand.label, hand.label_simplified, expert.ind, username, description),
      labeled_8K %>% select(hand.label, hand.label_simplified, expert.ind, username, description) %>% 
        filter(!username %in% most.follow$username))

training_df <- rbind(training_df,
                     labeled_biased %>% select(hand.label, hand.label_simplified, expert.ind, username, description) %>% 
                       filter(!username %in% training_df$username))

training_df <- rbind(training_df,
                     labeled_nonmajor %>% select(hand.label, hand.label_simplified, expert.ind, username, description) %>% 
                       filter(!username %in% training_df$username))

training_df <- rbind(training_df,
                     labeled_major %>% select(hand.label, hand.label_simplified, expert.ind, username, description) %>% 
                       filter(!username %in% training_df$username))

sum(duplicated(training_df$username))

print(dim(training_df))


training_df$description <- ifelse(is.na(training_df$description), "",  training_df$description)
training_df$expert.ind <- tolower(training_df$expert.ind)

View(training_df)

write.csv(training_df,
          file="Training_Data_With_8K_including_Empty_Descriptions.csv")


write.csv(training_df %>% filter(description != ""),
          file="Training_Data_With_8K_EXcluding_Empty_Descriptions.csv",
          row.names=F)

###########
###########

training_df

sum(is.na(training_df$description))
sum(training_df$description == "")


###########
###########


labeled_zero_length <- read.csv("Zero_Length_Descriptions.csv")
labeled_zero_length$description <- ifelse(is.na(labeled_zero_length$description), "", labeled_zero_length$description)

zero_length_df <- labeled_zero_length %>%
  select(hand.label, hand.label_simplified, expert.ind, username, description) %>% 
  filter(!username %in% training_df$username)

zero_length_df$expert.ind <- tolower(zero_length_df$expert.ind)

write.csv(zero_length_df,
  file="Training_Data_Non_8K_Empty_Descriptions.csv",
  row.names=F)

zero_length_ALL_df <- rbind(zero_length_df,
      training_df %>%
        filter(!username %in% zero_length_df$username,
               description == ""))

zero_length_ALL_df$expert.ind <- tolower(zero_length_ALL_df$expert.ind)

write.csv(zero_length_ALL_df,
          file="Training_Data_ALL_Empty_Descriptions.csv",
          row.names=F)



labeled_short_length <- read.csv("Short_Length_Descriptions_Below_30_char.csv")

short_length_df <- labeled_short_length %>% 
       select(hand.label, hand.label_simplified, expert.ind, username, description)

short_length_df$expert.ind <- tolower(short_length_df$expert.ind)

dim(short_length_df)



#######
#######

## ALL AVAILABLE DESCRIPTIONS

all_descriptions_df <- rbind(training_df %>% filter(description != ""),  # Non-empty initial ones
                             short_length_df %>% filter(!username %in% training_df$username),
                             zero_length_ALL_df %>% filter(!username %in% training_df$username))
   sum(duplicated(all_descriptions_df$username))   

all_descriptions_df$expert.ind <- tolower(all_descriptions_df$expert.ind)

write.csv(all_descriptions_df,
          file="Training_Data_ALL_Available_Descriptions.csv",
          row.names=F)

