########
## Word clouds for single terms
## Coloring the polarized sentiment words
##
## NOTE: Ctrl+F for "TIME-CONSUMING" to check parts of the code
##      that run quite slow (especially if applied to the entire time frame)
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

geo.terms <- unique(geo.terms)
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

exclude.retweets <- TRUE

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


#####
## COMBINING ALL THE AVAILABLE LABELS:
##    * Hand labels (13K)
##    * Predicted ones (13K)
#####

account.labels <- read.csv("Finalized_Labeled_Accounts_EMOJIS_UNCHANGED.csv")
table(account.labels$hand.label_simplified)

account.labels <- rbind(account.labels,
                        read.csv("SVM_BERT_Predictions_for_Unlabeled_Accounts_EMOJIS_UNCHANGED.csv"))

dim(account.labels)
table(account.labels$hand.label_simplified)



#####
## PULLING THE TWEETS
## (will need the "all_SIMPLE_columns.csv" dataset)
#####


full.df <- NULL

for (k in 1:6){
  
  ####
  ## Loading the data
  ####
  
  interm.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
  
  full.df <- rbind(full.df,
                   interm.df %>% select(id, username, text, text_with_display_links, created_at.x, verified, public_metrics.x_retweet_count)
  )
}


#####
## GETTING TWEETS only for stuff that HAS A LABEL
#####
full.df <- full.df %>% left_join(account.labels %>% 
                                   select(username, hand.label_simplified)) %>% 
  filter(!is.na(hand.label_simplified))

dim(full.df)
# table(full.df$hand.label_simplified)

# Removing complete duplicates (can result from merging across several location-mention datasets)
full.df <- full.df[!duplicated(full.df),]
dim(full.df)

table(full.df$hand.label_simplified)


####
# Only those from 5 categories of interest
#   EXCLUDES "INT" (non-English accounts)... MIGHT WANNA INCLUDE IT LATER
####
full.df <- full.df %>% filter(hand.label_simplified %in% c("acad", "gov", "media", "other", "tourbiz"))
dim(full.df)

our.df <- full.df %>% filter(created_at.x >= start.time,
                             created_at.x <= end.time)

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
our.df$text_with_display_links <- str_replace_all(our.df$text_with_display_links, "-", " ")
#our.df$text <- str_remove_all(our.df$text, "-")
# Remove trailing empty spaces in the end
our.df$text_with_display_links <- str_remove_all(our.df$text_with_display_links, "[ \t]+$")



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

## SOMEWHAT TIME-CONSUMING
if (lemmatize == TRUE) our.df$text_with_display_links <- lemmatize_strings(our.df$text_with_display_links)


# print(paste("N of TRULY distinct tweets:", length(unique(our.df$text_with_display_links))))
# cat("\n")

print(paste("N of distinct tweets with URL links & stuff:", length(unique(our.df$text))))
cat("\n")


if (nrow(our.df) > 0){

  dim(our.df)
  head(our.df$created_at.x,1)
  tail(our.df$created_at.x,1)
  # View(our.df %>% select(id, username, created_at.x, text))

  if (length(unique(our.df$text_with_display_links)) >= n.orig.cutoff){

    # our.df.tokenized <- tokenize_words(our.df$text_with_display_links, strip_punct = FALSE)
    # NEED TO REMOVE ALL THE LOCATION WORDS/PHRASES RIGHT AWAY, e.g. "ST PETE", "ANNA MARIA ISLAND"
    # ALSO: adding a variation that replaces "'" with "’" (otherwise stuff like "it’s" sneaks in, as it's not "it's")

    our.stopwords <- unique(stop_words$word)
    our.stopwords <- c(our.stopwords, "amp")


    ## Replace the stuff we don't want with "NA"
    
    ####
    ## !!! REALLY TIME-CONSUMING !!!!!
    ####
    string.for.tokenization <- str_replace_all(tolower(our.df$text_with_display_links),
                                               paste0("\\b",c(geo.terms, polit.terms,
                                                              "red tide", "red tides", "karenia brevis", "red algae",
                                                              "redtide", "redtide's",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"), "\\b", collapse="|"),
                                               "na")
    string.for.tokenization <- str_replace_all(string.for.tokenization,
                                               paste0("\\bna's\\b"),
                                               "na")

    ## IF WE WANT LEMMATIZATION (which we do)
    if (lemmatize == TRUE) string.for.tokenization <- lemmatize_strings(string.for.tokenization)


    ## Replace "STOPPING/CONTEXTUAL PUNCTUATION" with NA:
    bad.punct <- c("!", "$", "%", "(", ")",  ".",  ":", ";",
                   "?", ",", "[", "]", "{", "|", "}")

    for (my.char in bad.punct){
      string.for.tokenization <- str_replace_all(string.for.tokenization, fixed(my.char), " na ")
    }
    head(string.for.tokenization)


    our.df.tokenized <- tokenize_words(string.for.tokenization,
                                       strip_punct = TRUE,
                                       stopwords = our.stopwords)
    our.df.tokenized

    our.df.terms <- data.frame(tweet_id = our.df$id,
                               user = our.df$username,
                               text = sapply(our.df.tokenized, function(x) paste(x, collapse = " ")))



    # Defining tokenized data set
    # (removing "na" that result from empty strings)
    tokenized.term.counts <- our.df.terms %>%
      unnest_tokens(word, text) %>%
      filter(word != "na")

    dim(tokenized.term.counts)


    # Removing the multi-terms per single tweet
    # tokenized.term.counts <- tokenized.term.counts[!duplicated(tokenized.term.counts),]

    ## Getting counts for all the non-location terms
    term.counts <- tokenized.term.counts %>%
      filter(!word %in% c(geo.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
      count(word, sort=TRUE)

    tf.idf <- tokenized.term.counts %>%
      filter(!word %in% c(geo.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
      #  count(tweet_id, word, sort = TRUE) %>%
      # bind_tf_idf(word, tweet_id, n)
      count(user, word, sort = TRUE) %>%
      bind_tf_idf(word, user, n) %>%
      filter(n>10)



    head(tf.idf %>%
           arrange(desc(tf_idf)), 20)



    # Calculating DISTINCT USERS per term (rather than sheer mentions)
    tokenized.term.counts.unique.users <- tokenized.term.counts[!duplicated(tokenized.term.counts %>% select(user, word)),]

    term.unique.user.counts <- tokenized.term.counts.unique.users %>%
      filter(!word %in% c(geo.terms, "redtide", "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae")) %>%
      count(word, sort=TRUE)

    dim(term.counts)
    dim(term.unique.user.counts)
    sum(term.counts$word != term.unique.user.counts$word)

    term.per.user.counts <- term.counts %>%
      left_join(term.unique.user.counts, by="word") %>%
      mutate(term.per.user = n.x/n.y) %>%
      arrange(desc(term.per.user))

    head(term.per.user.counts %>% filter(n.y >= 3), 20)


    # print(head(term.counts, 3))

    if (nrow(term.counts) > 0){


      if (term.counts$n[1] < 3){
        # wordcloud("N/A", 1)
        # title(paste0(start.time, " - ", end.time, "\n Regular Mentions (Max: ",  term.counts$n[1], ")"))
        # Sys.sleep(2)

        #  print(as.data.frame(our.df %>% group_by(text_with_display_links) %>% summarise(count = n())))

      } else {

        par(mfrow=c(1,2))

        term.counts <- term.counts %>%
          left_join(updated_hash_sentiment_binary,
                    by=c("word"="x")) %>%
          mutate(y = ifelse(is.na(y), "neut", y))

        # "\n Unique Users (Max: ",  term.unique.user.counts$n[1], ")"))
        # Sys.sleep(2)
      }

      ### NOTE: The "term.counts" data frame also has a SENTIMENT column "y". 
      ###       It denotes whether the word is positive, negative or neutral in sentiment.
      wordcloud2(term.counts,
                 size=0.5)




      # print(head(term.unique.user.counts, 3))

      if (term.unique.user.counts$n[1] < 3){
        wordcloud("N/A", 1)
        title(paste0(start.time, " - ", end.time, "\n Unique Users (Max: ",  term.unique.user.counts$n[1], ")"))
       # Sys.sleep(2)

        # print(as.data.frame(our.df %>% group_by(text_with_display_links) %>% summarise(count = n())))

      } else {

        term.unique.user.counts <- term.unique.user.counts %>%
          left_join(updated_hash_sentiment_binary,
                    by=c("word"="x")) %>%
          mutate(y = ifelse(is.na(y), "neut", y))

      }

      ### NOTE: The "term.counts" data frame also has a SENTIMENT column "y". 
      ###       It denotes whether the word is positive, negative or neutral in sentiment.
      
      wordcloud2(term.unique.user.counts,
                 size=0.5)

      par(mfrow=c(1,1))
    }
  }

}



#}

#dev.off()


