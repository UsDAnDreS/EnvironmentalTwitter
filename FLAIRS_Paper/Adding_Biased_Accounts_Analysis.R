########
## Coloring the polarized sentiment words
########

source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

library(tokenizers)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textstem)



## Converting all the location names from "Locations.R" into hashtag format
geo.terms <- tolower(c(beaches, cities, counties, lake_list, rivers, bays, towns, other))
geo.terms <- c(geo.terms, paste(tolower(counties), "county"))


geo.terms <- c(geo.terms,
               tolower(c(Tampa.query.words,
                         Pinellas.Clearwater.query.words,
                         StPete.query.words,
                         c(Manatee.query.words, "long boat key"),
                         Sarasota.query.words,
                         Pasco.query.words)))
geo.terms <- str_remove_all(geo.terms, "#")

geo.terms <- unique(c(geo.terms, str_remove_all(geo.terms, " ")))

# geo.terms <- str_remove_all(geo.terms, fixed("."))
# geo.terms <- str_remove_all(geo.terms, fixed("-"))

geo.terms <- unique(geo.terms)
length(geo.terms)
geo.terms



#######
## Political names
#######

polit.terms <- c("democrat", "democratic", "republican", "gop", "demcastfl", 
                 "vote blue", "vote red", "red wave", "blue wave",  
                 "right wing", "left wing", "far right", "far left", "extreme right", "extreme left", 
                 "supremacy", "supremacist", "supremacys", "supremacists", "terrorist", "terrorism", "terrorists",
                 "ron desantis", "desantis", "remove ron", "deathsantis",
                 "rick scott", "red tide rick",
                 "marco rubio", "rubio",
                 "bill nelson",
                 "donald trump", "trump",
                 "mike pence", "pence",
                 "joe biden", "biden",
                 "kamala harris", 
                 "crist", "charlie christ",
                 "andrew gillum", "gillum",
                 "kriseman", "richard kriseman", # Past mayor of St Pete
                 "ken welch",                   # Current mayor of St Pete
                 "george cretekos", "cretekos",  # Mayor of Clearwater
                 "buckhorn", "bob buckhorn",  # Past mayor of Tampa
                 "jane castor", "castor",      # Current mayor of Tampa
                 "john holic", "holic",     # Past mayor of Venice
                 "ron feinsod"               # Current mayor of Venice
) 

polit.terms <- unique(c(polit.terms, str_remove_all(polit.terms, " ")))
polit.terms






max.words <- 50
n.orig.cutoff <- 5  # No. of minimum original content tweets to produce a word cloud

exclude.retweets <- FALSE

## Whether to lemmatize ("run"/"ran"/"running" => "run)
lemmatize <- TRUE

unique.orig.text.only <- FALSE
if (unique.orig.text.only) exclude.retweets <- TRUE


start.time <- as.Date("2018-01-01")
end.time <- as.Date("2023-01-01")


# pdf(# file = paste0("Word_Clouds_UNIGRAMS_Summer_2021", ifelse(lemmatize, "_LEMMATIZED", ""), ".pdf"),
#   file = paste0("SENTIMENT_Word_Clouds_UNIGRAMS_",start.time,"_",end.time, "_NO_PUNCT", ifelse(lemmatize, "_LEMMATIZED", ""), ".pdf"),
#   width=12,
#   height=6.5)

account.labels <- read.csv("finalized_8K_accounts_emojis_replaced.csv")

account.labels <- rbind(account.labels,
                        read.csv("finalized_BIASED_accounts_ALL_emojis_replaced.csv"))
dim(account.labels)

account.labels <- rbind(account.labels %>%
                          select(username, description, hand.label_simplified),
                        read.csv("UNLABELED_PREDICTED_accounts_emojis_replaced.csv") %>%
                          select(username, description, hand.label_simplified))

dim(account.labels)
length(unique(account.labels$username))

full.df <- NULL


for (k in 1:6){
  
  ####
  ## Loading the data
  ####
  
  # k <- 2  # Which area? 1-6
  # full.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
  
  interm.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
  
  full.df <- rbind(full.df,
                   interm.df %>% select(id, username, text, text_with_display_links, created_at.x, verified, public_metrics.x_retweet_count)
  )
}



## Subsetting it
full.df <- full.df %>% left_join(account.labels %>% 
                                   select(username, hand.label_simplified)) %>% 
  filter(!is.na(hand.label_simplified))

dim(full.df)
# table(full.df$hand.label_simplified)


our.df <- full.df %>% filter(created_at.x >= start.time,
                             created_at.x <= end.time,
                             hand.label_simplified != "bot")

if (exclude.retweets){
  our.df <- our.df %>% filter(!str_detect(text, "^RT @"))
}

if (unique.orig.text.only){
  our.df <- our.df %>% filter(!str_detect(text, "^RT @"))
  our.df <- our.df[!duplicated(our.df$text_with_display_links), ]
}






### CLEANING TEXT

# Removing "from https/http till the end of the line
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "https.*")
#our.df$text <- str_remove_all(our.df$text, "https.*")
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "http.*")
#our.df$text <- str_remove_all(our.df$text, "http.*")
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "^RT @[^ ]* ")
#our.df$text <- str_replace_all(our.df$text, "RT @", "@")
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "@[^ ]* ")
#our.df$text <- str_remove_all(our.df$text, "@[^ ]*")
our.df$text_with_display_links <- str_replace_all(full.df$text_with_display_links, "-", " ")
#our.df$text <- str_remove_all(our.df$text, "-")
# Remove trailing empty spaces in the end
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "[ \t]+$")







########
## CALCULATING TWEET LENGTH before too much stuff is removed
## (BELOW)
########


print("Mean tweet length, by account type:")
our.df$tweet.length <- sapply(our.df$text_with_display_links, nchar)
paste.vec <- paste0(round(tapply(our.df$tweet.length, our.df$hand.label_simplified, mean), 2),
                    " (", round(tapply(our.df$tweet.length, our.df$hand.label_simplified, function(x) sd(x)/sqrt(length(x))), 2),
                    ", ", round(tapply(our.df$tweet.length, our.df$hand.label_simplified, sd),2), ")")
names(paste.vec) <- sort(unique(our.df$hand.label_simplified))
print(paste.vec)




CI.df <- NULL

for (categ in sort(unique(our.df$hand.label_simplified))){
  # print(categ)
  # print(as.numeric(t.test(our.df$tweet.length[our.df$hand.label_simplified == categ])$conf.int))
  
  CI.df <- rbind(CI.df,
                 c(mean(our.df$tweet.length[our.df$hand.label_simplified == categ]),
                   as.numeric(t.test(our.df$tweet.length[our.df$hand.label_simplified == categ])$conf.int)))
}

CI.df <- data.frame(CI.df)
CI.df$Type <- sort(unique(our.df$hand.label_simplified))



ggplot(CI.df, aes(x=Type, y=X1, colour=Type)) +
  #geom_errorbar(aes(ymax=Mean+Std, ymin=Mean-Std), width=0) +
  geom_errorbar(aes(ymax=X2, ymin=X3), width=0.4) +
  geom_point(size=3.5) +
  #geom_hline(yintercept=0, linetype=2) +
  xlab("Account Type") +
  ylab("Average Tweet Length") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(factor(CI.df$Type)))) +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0, color="black"),
        axis.text.x = element_text(color="black")) +
  #axis.ticks.x = element_text(color="black")) +
  # theme_bw() +
  ggtitle("95% Confidence Intervals for Average Tweet Length")



### COHEH'S PRACTICAL EFFECT SIZE

sent.list <- list()

for (categ in sort(unique(our.df$hand.label_simplified))){
  print(categ)
  print(as.numeric(t.test(our.df$tweet.length[our.df$hand.label_simplified == categ])$conf.int))
  sent.list[[categ]] <- our.df$tweet.length[our.df$hand.label_simplified == categ]
  #tapply(our.df$tweet.length, our.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int})
}


eff.size.mat <- matrix(0, 
                       nrow=length(unique(our.df$hand.label_simplified)),
                       ncol=length(unique(our.df$hand.label_simplified)))

for (i in 1:length(unique(our.df$hand.label_simplified)))
  for (j in 1:length(unique(our.df$hand.label_simplified))){
    eff.size.mat[i,j] <- (mean(sent.list[[unique(our.df$hand.label_simplified)[i]]]) - 
                            mean(sent.list[[unique(our.df$hand.label_simplified)[j]]]))/(sqrt((sd(sent.list[[unique(our.df$hand.label_simplified)[i]]])^2 + sd(sent.list[[unique(our.df$hand.label_simplified)[j]]])^2/2)))
  }

print("Cohen's D for Tweet Length:")
colnames(eff.size.mat) <- rownames(eff.size.mat) <- unique(our.df$hand.label_simplified)
print(round(eff.size.mat,3))




########
## CALCULATING TWEET LENGTH before too much stuff is removed
## (ABOVE)
########








# Replacing "’" with "'"
our.df$text_with_display_links <- str_replace_all(our.df$text_with_display_links, "’", "'")

# Replacing "'" from where "'s" indicates "is" rather than possessive
legitimate.s <- c("it's", "that's", "there's", "here's", 
                  "he's", "she's", "what's", "where's", "who's", 
                  "let's", "when's", "why's", "how's")
legitimate.s.strip <- str_remove_all(legitimate.s, "'")

for (w.ind in 1:length(legitimate.s)){
  our.df$text_with_display_links <- str_replace_all(tolower(our.df$text_with_display_links),
                                                     legitimate.s[w.ind],
                                                     legitimate.s.strip[w.ind])
} 


our.df$text_with_display_links <- str_replace_all(our.df$text_with_display_links, "('s)([^a-zA-Z0-9])", "\\2")

if (lemmatize == TRUE) our.df$text_with_display_links <- lemmatize_strings(our.df$text_with_display_links)

# full.sentiment <- function(x, polarity_dt=updated_hash_sentiment){
#   # sapply(x, function(y) apply(sentiment(y, polarity_dt=polarity_dt), 2, sum)[4])
#   sapply(x, function(y){
#     sent.table <- sentiment(y, polarity_dt=polarity_dt);
#     w.counts <- as.numeric(sent.table$word_count)
#     if (mean(is.na(w.counts)) == 1) return(0)   # If all "word_count" vales are NA
#     w.counts <- ifelse(is.na(w.counts), 0, w.counts)
#     sum(sqrt(w.counts)*as.numeric(sent.table$sentiment))/sqrt(sum(w.counts))
#   })
#   # apply(sentiment(y, polarity_dt=polarity_dt), 2,
#   #                           function(z)  sum)[4])
# }
# 
# our.df$sentiment <- full.sentiment(as.list(our.df$text_with_display_links))
# 
# save(our.df,
#      file=paste0("ALL_Tweets_WITH_PREDICTED_20K", ifelse(exclude.retweets, "_EXCLUDING_RETWEETS", ""), "_with_Sentiment", ifelse(lemmatize, "_LEMMATIZED", ""), ".Robj"))






### LOADING THE SENTIMENT

# load(paste0("Labeled_8K_PLUS_BIASED_Tweets_with_Sentiment", ifelse(lemmatize, "_LEMMATIZED", ""), ".Robj"))
load(paste0("ALL_Tweets_WITH_PREDICTED_20K_with_Sentiment", ifelse(lemmatize, "_LEMMATIZED", ""), ".Robj"))




print("Total retweets (public metric), by account type")
our.df.no.RT <- our.df[!str_detect(our.df$text, "^RT @"), ]

print(tapply(our.df.no.RT$public_metrics.x_retweet_count, 
             our.df.no.RT$hand.label_simplified, 
             sum))

paste.vec <- paste0(round(tapply(our.df.no.RT$public_metrics.x_retweet_count, 
                                 our.df.no.RT$hand.label_simplified, 
                                 mean),2),
                    " (", round(tapply(our.df.no.RT$public_metrics.x_retweet_count, our.df.no.RT$hand.label_simplified, function(x) sd(x)/sqrt(length(x))),2),
                    ", ", round(tapply(our.df.no.RT$public_metrics.x_retweet_count, our.df.no.RT$hand.label_simplified, sd),2), ")")

names(paste.vec) <- sort(unique(our.df.no.RT$hand.label_simplified))

print(paste.vec)



print("% retweets, by account type")
our.df.no.RT <- our.df[!str_detect(our.df$text, "^RT @"), ]

mini.df <- our.df %>%
  group_by(hand.label_simplified) %>%
  summarise(tot.tweets = n(), 
            tot.retweets = sum(str_detect(text, "^RT @")),
            pct.retweets = tot.retweets/tot.tweets) 

mini.df


CI.df <- NULL

for (categ in sort(unique(mini.df$hand.label_simplified))){
  print(categ)
  print(as.numeric(prop.test(mini.df$tot.retweets[mini.df$hand.label_simplified == categ],
                             mini.df$tot.tweets[mini.df$hand.label_simplified == categ])$conf.int))
  sent.list[[categ]] <- mini.df$pct.retweets[mini.df$hand.label_simplified == categ]
  
  CI.df <- rbind(CI.df,
                 c(mean(mini.df$pct.retweets[mini.df$hand.label_simplified == categ]),
                   as.numeric(prop.test(mini.df$tot.retweets[mini.df$hand.label_simplified == categ],
                                        mini.df$tot.tweets[mini.df$hand.label_simplified == categ])$conf.int)))
  #tapply(mini.df$pct.retweets, mini.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int})
}

CI.df <- data.frame(CI.df)
CI.df$Type <- sort(unique(mini.df$hand.label_simplified))


## Cohen's H for Retweet %:

eff.size.mat <- matrix(0, 
                       nrow=length(unique(mini.df$hand.label_simplified)),
                       ncol=length(unique(mini.df$hand.label_simplified)))

for (i in 1:length(unique(mini.df$hand.label_simplified)))
  for (j in 1:length(unique(mini.df$hand.label_simplified))){
    eff.size.mat[i,j] <- (2*asin(sqrt(sent.list[[unique(mini.df$hand.label_simplified)[i]]])) - 
                            2*asin(sqrt(sent.list[[unique(mini.df$hand.label_simplified)[j]]])))
  }

print("Cohen's H for Tweet Length:")
colnames(eff.size.mat) <- rownames(eff.size.mat) <- unique(mini.df$hand.label_simplified)
print(round(eff.size.mat,3))








# ## Vast majority are <=10, could use a cutoff at about 50
# ggplot(our.df %>% filter(public_metrics.x_retweet_count>5), aes(x=public_metrics.x_retweet_count)) +
#   geom_boxplot()
# 
# ## CHECKING >=100 RETWEETS (some of these are BY THE SAME ACCOUNT)
# table(our.df[our.df$public_metrics.x_retweet_count >= 100, ]$hand.label_simplified)
# 
# ## CHECKING >=100 RETWEETS, the UNIQUE ACCOUNTS
# View(our.df[our.df$public_metrics.x_retweet_count >= 100, ] %>%
#   select(username, hand.label_simplified, public_metrics.x_retweet_count) %>%
#   group_by(username) %>%
#   summarise(type=hand.label_simplified[1],
#             mean.retweets = mean(public_metrics.x_retweet_count),
#             total.viral.tweets = n()))


# our.df[our.df$username == "craigtimes", ]$public_metrics.x_retweet_count



View(our.df %>%
       filter(public_metrics.x_retweet_count >= 50))


### 95% CIs will be based on TERRIBLE ASSUMPTIONS (due to SKEWNESS)
# CI.df <- NULL
# 
# for (categ in sort(unique(our.df$hand.label_simplified))){
#   print(categ)
#   print(as.numeric(t.test(our.df$public_metrics.x_retweet_count[our.df$hand.label_simplified == categ])$conf.int))
#   
#   CI.df <- rbind(CI.df,
#                  c(mean(our.df$public_metrics.x_retweet_count[our.df$hand.label_simplified == categ]),
#                    as.numeric(t.test(our.df$public_metrics.x_retweet_count[our.df$hand.label_simplified == categ])$conf.int)))
#   #tapply(our.df$sentiment, our.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int})
# }





print("Total tweets, by account type:")
print(tapply(our.df$sentiment, our.df$hand.label_simplified, length))

print("Total unique users, by account type:")
print(tapply(our.df$username, our.df$hand.label_simplified, function(x) length(unique(x))))


### SENTIMENT


print("Mean net sentiment, by account type:")
paste.vec <- paste0(round(tapply(our.df$sentiment, our.df$hand.label_simplified, mean),3),
             " (", round(tapply(our.df$sentiment, our.df$hand.label_simplified, function(x) sd(x)/sqrt(length(x))),2),
             ", ", round(tapply(our.df$sentiment, our.df$hand.label_simplified, sd),2), ")")

names(paste.vec) <- sort(unique(our.df$hand.label_simplified))

print(paste.vec)


# print("95% CI sentiment, by account type:")
# print(paste0(round(tapply(our.df$sentiment, our.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int}),3)))

sent.list <- list()



### COHEN'S D

for (categ in sort(unique(our.df$hand.label_simplified))){
  sent.list[[categ]] <- our.df$sentiment[our.df$hand.label_simplified == categ]
}


eff.size.mat <- matrix(0, 
                       nrow=length(unique(our.df$hand.label_simplified)),
                       ncol=length(unique(our.df$hand.label_simplified)))

for (i in 1:length(unique(our.df$hand.label_simplified)))
  for (j in 1:length(unique(our.df$hand.label_simplified))){
    eff.size.mat[i,j] <- (mean(sent.list[[unique(our.df$hand.label_simplified)[i]]]) - 
                            mean(sent.list[[unique(our.df$hand.label_simplified)[j]]]))/(sqrt((sd(sent.list[[unique(our.df$hand.label_simplified)[i]]])^2 + sd(sent.list[[unique(our.df$hand.label_simplified)[j]]])^2/2)))
  }

colnames(eff.size.mat) <- rownames(eff.size.mat) <- unique(our.df$hand.label_simplified)
round(eff.size.mat,3)


######
## 95% CI PLOTS
#######

CI.df <- NULL

for (categ in sort(unique(our.df$hand.label_simplified))){
  print(categ)
  print(as.numeric(t.test(our.df$sentiment[our.df$hand.label_simplified == categ])$conf.int))
  
  CI.df <- rbind(CI.df,
                 c(mean(our.df$sentiment[our.df$hand.label_simplified == categ]),
                   as.numeric(t.test(our.df$sentiment[our.df$hand.label_simplified == categ])$conf.int)))
  #tapply(our.df$sentiment, our.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int})
}

CI.df <- data.frame(CI.df)
CI.df$Type <- sort(unique(our.df$hand.label_simplified))

# 703, 477

library(ggplot2)
set.seed(3)
ggplot(CI.df, aes(x=Type, y=X1, colour=Type)) +
  #geom_errorbar(aes(ymax=Mean+Std, ymin=Mean-Std), width=0) +
  geom_errorbar(aes(ymax=X2, ymin=X3), width=0.4) +
  geom_point(size=3.5) +
  geom_hline(yintercept=0, linetype=2) +
  xlab("Account Type") +
  ylab("Average Sentiment") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(factor(CI.df$Type)))) +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0, color="black"),
        axis.text.x = element_text(color="black")) +
  #axis.ticks.x = element_text(color="black")) +
  # theme_bw() +
  ggtitle("95% Confidence Intervals for Average Sentiment")



ggplot(data = our.df, aes(x=sentiment, y=hand.label_simplified, color=hand.label_simplified)) +
  geom_boxplot() +
  # geom_boxplot(outlier.shape=NA) + 
  #xlim(-0.5,0.5) + 
  coord_flip() +
  theme(legend.position="none") + 
  ggtitle(paste0("Tweets Per User, ", ifelse(exclude.retweets, 
                                             "Excluding retweets",
                                             "Including retweets")))






######
## TWEETS PER USER
######

print("Tweets per user, by account type:")
print(round(tapply(our.df$sentiment, our.df$hand.label_simplified, length)/tapply(our.df$username, our.df$hand.label_simplified, function(x) length(unique(x))),3))

our.df %>%
  group_by(hand.label_simplified, username) %>%
  summarise(tot.tweets = n()) %>%
  group_by(hand.label_simplified) %>%
  summarise(tweet.avg = mean(tot.tweets),
            tweet.median = median(tot.tweets),
            tweet.sd = sd(tot.tweets),
            tweet.se = tweet.sd/n())

mini.df <- our.df %>%
  group_by(hand.label_simplified, username) %>%
  summarise(tot.tweets = n()) %>%
  select(-username)


CI.df <- NULL

for (categ in sort(unique(mini.df$hand.label_simplified))){
  print(categ)
  print(as.numeric(t.test(mini.df$tot.tweets[mini.df$hand.label_simplified == categ])$conf.int))
  
  CI.df <- rbind(CI.df,
                 c(mean(mini.df$tot.tweets[mini.df$hand.label_simplified == categ]),
                   as.numeric(t.test(mini.df$tot.tweets[mini.df$hand.label_simplified == categ])$conf.int)))
  #tapply(mini.df$tot.tweets, mini.df$hand.label_simplified, function(x){t.obj <- t.test(x); t$conf.int})
}

CI.df <- data.frame(CI.df)
CI.df$Type <- sort(unique(mini.df$hand.label_simplified))


ggplot(data = mini.df, aes(x=tot.tweets, y=hand.label_simplified, color=hand.label_simplified)) +
  geom_boxplot() +
  # geom_boxplot(outlier.shape=NA) + 
  #xlim(-0.5,0.5) + 
  coord_flip() +
  theme(legend.position="none") + 
  ggtitle(paste0("Tweets Per User, ", ifelse(exclude.retweets, 
                 "Excluding retweets",
                 "Including retweets")))






# print(paste("N of TRULY distinct tweets:", length(unique(our.df$text_with_display_links))))
# cat("\n")

print(paste("N of distinct tweets with URL links & stuff:", length(unique(our.df$text))))
cat("\n")

print(paste("NET SENTIMENT:", mean(our.df$sentiment)))
cat("\n")

# if (nrow(our.df) > 0){
#   
#   dim(our.df)
#   head(our.df$created_at.x,1)
#   tail(our.df$created_at.x,1)
#   # View(our.df %>% select(id, username, created_at.x, text))
#   
#   if (length(unique(our.df$text_with_display_links)) >= n.orig.cutoff){
#     
#     # our.df.tokenized <- tokenize_words(our.df$text_with_display_links, strip_punct = FALSE)
#     # NEED TO REMOVE ALL THE LOCATION WORDS/PHRASES RIGHT AWAY, e.g. "ST PETE", "ANNA MARIA ISLAND"
#     # ALSO: adding a variation that replaces "'" with "’" (otherwise stuff like "it’s" sneaks in, as it's not "it's")
#     
#     our.stopwords <- unique(stop_words$word)
#     our.stopwords <- c(our.stopwords, "amp")
#     
#     
#     ## Replace the stuff we don't want with "NA"
#     string.for.tokenization <- str_replace_all(tolower(our.df$text_with_display_links),
#                                                paste0("\\b",c(geo.terms, polit.terms,
#                                                               "red tide", "red tides", "karenia brevis", "red algae",
#                                                               "redtide", "redtide's",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"), "\\b", collapse="|"),
#                                                "na")
#     string.for.tokenization <- str_replace_all(string.for.tokenization,
#                                                paste0("\\bna's\\b"),
#                                                "na")
#     
#     ## IF WE WANT LEMMATIZATION
#     head(string.for.tokenization)
#     head(lemmatize_strings(string.for.tokenization))
#     
#     if (lemmatize == TRUE) string.for.tokenization <- lemmatize_strings(string.for.tokenization)
#     
#     
#     ## Replace "STOPPING/CONTEXTUAL PUNCTUATION" with NA:
#     bad.punct <- c("!", "$", "%", "(", ")",  ".",  ":", ";",
#                    "?", ",", "[", "]", "{", "|", "}")
#     
#     head(string.for.tokenization)
#     for (my.char in bad.punct){
#       string.for.tokenization <- str_replace_all(string.for.tokenization, fixed(my.char), " na ")
#     }
#     head(string.for.tokenization)
#     
#     
#     our.df.tokenized <- tokenize_words(string.for.tokenization,
#                                        strip_punct = TRUE,
#                                        stopwords = our.stopwords)
#     our.df.tokenized
#     
#     our.df.terms <- data.frame(tweet_id = our.df$id,
#                                user = our.df$username,
#                                text = sapply(our.df.tokenized, function(x) paste(x, collapse = " ")))
#     
#     
#     
#     # Defining tokenized data set
#     # (removing "na" that result from empty strings)
#     tokenized.term.counts <- our.df.terms %>%
#       unnest_tokens(word, text) %>%
#       filter(word != "na")
#     
#     dim(tokenized.term.counts)
#     
#     
#     # Removing the multi-terms per single tweet
#     # tokenized.term.counts <- tokenized.term.counts[!duplicated(tokenized.term.counts),]
#     
#     ## Getting counts for all the non-location terms
#     term.counts <- tokenized.term.counts %>%
#       filter(!word %in% c(geo.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
#       count(word, sort=TRUE)
#     
#     tf.idf <- tokenized.term.counts %>%
#       filter(!word %in% c(geo.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
#       #  count(tweet_id, word, sort = TRUE) %>%
#       # bind_tf_idf(word, tweet_id, n)
#       count(user, word, sort = TRUE) %>%
#       bind_tf_idf(word, user, n) %>%
#       filter(n>10)
#     
#     
#     
#     head(tf.idf %>%
#            arrange(desc(tf_idf)), 20)
#     
#     
#     
#     # Calculating DISTINCT USERS per term (rather than sheer mentions)
#     tokenized.term.counts.unique.users <- tokenized.term.counts[!duplicated(tokenized.term.counts %>% select(user, word)),]
#     
#     term.unique.user.counts <- tokenized.term.counts.unique.users %>%
#       filter(!word %in% c(geo.terms, "redtide", "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
#       count(word, sort=TRUE)
#     
#     dim(term.counts)
#     dim(term.unique.user.counts)
#     sum(term.counts$word != term.unique.user.counts$word)
#     
#     term.per.user.counts <- term.counts %>%
#       left_join(term.unique.user.counts, by="word") %>%
#       mutate(term.per.user = n.x/n.y) %>%
#       arrange(desc(term.per.user))
#     
#     head(term.per.user.counts %>% filter(n.y >= 3), 20)
#     
#     
#     # print(head(term.counts, 3))
#     
#     if (nrow(term.counts) > 0){
#       
#       
#       if (term.counts$n[1] < 3){
#         # wordcloud("N/A", 1)
#         # title(paste0(start.time, " - ", end.time, "\n Regular Mentions (Max: ",  term.counts$n[1], ")"))
#         # Sys.sleep(2)
#         
#         #  print(as.data.frame(our.df %>% group_by(text_with_display_links) %>% summarise(count = n())))
#         
#       } else {
#         
#         par(mfrow=c(1,2))
#         
#         term.counts <- term.counts %>% 
#           left_join(updated_hash_sentiment_binary,
#                     by=c("word"="x")) %>%
#           mutate(y = ifelse(is.na(y), "neut", y))
#         
#         # "\n Unique Users (Max: ",  term.unique.user.counts$n[1], ")"))
#         # Sys.sleep(2)
#       }
#       
#       # wordcloud2(term.counts,
#       #            size=0.5)
#       
#       
#       
#       
#       # print(head(term.unique.user.counts, 3))
#       
#       if (term.unique.user.counts$n[1] < 3){
#         wordcloud("N/A", 1)
#         title(paste0(start.time, " - ", end.time, "\n Unique Users (Max: ",  term.unique.user.counts$n[1], ")"))
#        # Sys.sleep(2)
#         
#         # print(as.data.frame(our.df %>% group_by(text_with_display_links) %>% summarise(count = n())))
#         
#       } else {
#         
#         term.unique.user.counts <- term.unique.user.counts %>% 
#           left_join(updated_hash_sentiment_binary,
#                     by=c("word"="x")) %>%
#           mutate(y = ifelse(is.na(y), "neut", y))
# 
#       }
#       
#       # wordcloud2(term.unique.user.counts,
#       #            size=0.5)
#       
#       par(mfrow=c(1,1))
#     }
#   } 
# 
# }
# 

}

#}

#dev.off()


