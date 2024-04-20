#######
## Testing out HASHTAG WORD CLOUDS, dealing with SPAMMERS, using UNIQUE USER counts
#######

# https://www.tidytextmining.com/tidytext.html

source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

library(tokenizers)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textstem)

# Whether to include predicted accounts
what.to.include <- c("labeled_only", "predictions_only", "everything")[1]




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

exclude.retweets <- TRUE

## Whether to lemmatize ("run"/"ran"/"running" => "run)
lemmatize <- TRUE

unique.orig.text.only <- FALSE
if (unique.orig.text.only) exclude.retweets <- TRUE


start.time <- as.Date("2018-01-01")
end.time <- as.Date("2023-01-01")


# pdf(# file = paste0("Word_Clouds_UNIGRAMS_Summer_2021", ifelse(lemmatize, "_LEMMATIZED", ""), ".pdf"),
#   file = paste0("Word_Clouds_UNIGRAMS_",start.time,"_",end.time, "_NO_PUNCT", ifelse(lemmatize, "_LEMMATIZED", ""), ".pdf"),
#   width=12,
#   height=6.5)

# account.labels <- read.csv("finalized_8K_accounts_emojis_replaced.csv")

## HAND-LABELS
if (what.to.include == "labeled_only"){
  account.labels <- read.csv("FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED_w_DICT_LABELS.csv")  %>% select(username, description, hand.label_simplified)
} else if (what.to.include == "predictions_only"){
  account.labels <- read.csv("SVM_BERT_unweighted_UNLABELED_PREDICTED_accounts_W_PROBABILITIES_emojis_unchanged.csv")  %>% select(username, description, hand.label_simplified)
} else {
  account.labels <- rbind(read.csv("FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED_w_DICT_LABELS.csv") %>% select(username, description, hand.label_simplified),
                          read.csv("SVM_BERT_unweighted_UNLABELED_PREDICTED_accounts_W_PROBABILITIES_emojis_unchanged.csv") %>% select(username, description, hand.label_simplified))
}



# full.df <- NULL
# 
# 
# for (k in 1:6){
#   
#   ####
#   ## Loading the data
#   ####
#   
#   # k <- 2  # Which area? 1-6
#   # full.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
#   
#   interm.df <- read.csv(file=paste0("Data/",names(main.queries)[1], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
#   
#   full.df <- rbind(full.df,
#                    interm.df %>% select(id, username, text, text_with_display_links, created_at.x, verified)
#   )
# }


full.df <- read.csv("full_df_unique_emojis_replaced.csv")
# full.df <- read.csv("full_df_unique.csv")

  
  ## Subsetting it
  full.df <- full.df %>% left_join(account.labels %>% 
                                     select(username, hand.label_simplified)) %>% 
    filter(!is.na(hand.label_simplified))
  
  dim(full.df)
  table(full.df$hand.label_simplified)
  
  
  ### CLEANING TEXT
  
  # Removing "from https/http till the end of the line
  full.df$text_with_display_links <- str_remove_all(full.df$text_with_display_links, "https.*")
  #full.df$text <- str_remove_all(full.df$text, "https.*")
  full.df$text_with_display_links <- str_remove_all(full.df$text_with_display_links, "http.*")
  #full.df$text <- str_remove_all(full.df$text, "http.*")
  full.df$text_with_display_links <- str_remove_all(full.df$text_with_display_links, "^RT @[^ ]* ")
  #full.df$text <- str_replace_all(full.df$text, "RT @", "@")
  full.df$text_with_display_links <- str_remove_all(full.df$text_with_display_links, "@[^ ]* ")
  #full.df$text <- str_remove_all(full.df$text, "@[^ ]*")
  full.df$text_with_display_links <- str_replace_all(full.df$text_with_display_links, "-", " ")
  #full.df$text <- str_remove_all(full.df$text, "-")
  # Remove trailing empty spaces in the end
  full.df$text_with_display_links <- str_remove_all(full.df$text_with_display_links, "[ \t]+$")
  
  # Replacing "’" with "'"
  full.df$text_with_display_links <- str_replace_all(full.df$text_with_display_links, "’", "'")
  
  # Replacing "'" from where "'s" indicates "is" rather than possessive
  legitimate.s <- c("it's", "that's", "there's", "here's", 
                    "he's", "she's", "what's", "where's", "who's", 
                    "let's", "when's", "why's", "how's")
  legitimate.s.strip <- str_remove_all(legitimate.s, "'")
  
  for (w.ind in 1:length(legitimate.s)){
    full.df$text_with_display_links <- str_replace_all(tolower(full.df$text_with_display_links),
                                                       legitimate.s[w.ind],
                                                       legitimate.s.strip[w.ind])
  } 
  
  
  full.df$text_with_display_links <- str_replace_all(full.df$text_with_display_links, "('s)([^a-zA-Z0-9])", "\\2")
  
  
  
  #########
  ## Get a SINGLE ENTRY of CONCATENATED TWEETS per EACH GROUP
  #########
  
  
  # head(full.df$text_with_display_links,10)
  
  # View(full.df %>% select(text))

    
    print(dim(full.df %>% filter(created_at.x >= start.time,
                                 created_at.x <= end.time,
                                 hand.label_simplified != "bot")))
    
    # View(full.df %>% filter(created_at.x >= start.time,
    #                         created_at.x <= end.time) %>% select(id, username, created_at.x, text))
    
    
    
    cat("\n")
    print(paste("Week of", start.time, "-", end.time))
    
    
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
    
    
    
    
    # print(paste("N of TRULY distinct tweets:", length(unique(our.df$text_with_display_links))))
    # cat("\n")
    
    print(paste("N of distinct tweets with URL links & stuff:", length(unique(our.df$text))))
    cat("\n")
    
    ### RE-DEFINE "our.df" to become ONE CONCATENATED TWEET PER CATEGORY
    
    # our.df <- our.df %>% 
    #   filter(hand.label_simplified != "bot") %>%
    #   group_by(hand.label_simplified) %>%
    #   summarise(all.tweets = paste(text_with_display_links, collapse=". "))
    
    # View(our.df)
    
    
        # our.df.tokenized <- tokenize_words(our.df$text_with_display_links, strip_punct = FALSE)
        # NEED TO REMOVE ALL THE LOCATION WORDS/PHRASES RIGHT AWAY, e.g. "ST PETE", "ANNA MARIA ISLAND"
        # ALSO: adding a variation that replaces "'" with "’" (otherwise stuff like "it’s" sneaks in, as it's not "it's")
        
        our.stopwords <- unique(stop_words$word)
        our.stopwords <- c(our.stopwords, "amp")
        
        str_replace("don trump", "\\bdon trump\\b", "na")
        
        ## Replace the stuff we don't want with "NA"
        string.for.tokenization <- str_replace_all(# tolower(our.df$all.tweets),
                                                   tolower(our.df$text_with_display_links),
                                                   paste0("\\b",c(geo.terms, polit.terms,
                                                                  "red tide", "red tides", "karenia brevis", "red algae",
                                                                  "redtide", "redtide's",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"), "\\b", collapse="|"),
                                                   "na")
        string.for.tokenization <- str_replace_all(string.for.tokenization,
                                                   paste0("\\bna's\\b"),
                                                   "na")
        
        ## IF WE WANT LEMMATIZATION
        # head(string.for.tokenization)
        # head(lemmatize_strings(string.for.tokenization))
        
        if (lemmatize == TRUE) string.for.tokenization <- lemmatize_strings(string.for.tokenization)
        
        
        ## Replace "STOPPING/CONTEXTUAL PUNCTUATION" with NA:
        bad.punct <- c("!", "$", "%", "(", ")",  ".",  ":", ";",
                       "?", ",", "[", "]", "{", "|", "}")
        
        head(string.for.tokenization)
        for (my.char in bad.punct){
          string.for.tokenization <- str_replace_all(string.for.tokenization, fixed(my.char), " na ")
        }
        # head(string.for.tokenization)
        
        
        our.df.tokenized <- tokenize_words(string.for.tokenization,
                                           strip_punct = TRUE,
                                           stopwords = our.stopwords)
        dim(our.df.tokenized)
        
        our.df.terms <- data.frame(acc_type = our.df$hand.label_simplified,
                                   user = our.df$username,
                                   text = sapply(our.df.tokenized, function(x) paste(x, collapse = " ")))
        
        dim(our.df.terms)
        
        
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
          filter(!word %in% c(geo.terms, polit.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"),
                 is.na(as.numeric(word))) %>%
          count(word, sort=TRUE)

        tf.idf <- tokenized.term.counts %>%
          filter(!word %in% c(geo.terms, polit.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"),
                 is.na(as.numeric(word))) %>%
          #  count(tweet_id, word, sort = TRUE) %>%
          # bind_tf_idf(word, tweet_id, n)
          count(acc_type, word, sort = TRUE) %>%
          bind_tf_idf(word, acc_type, n) #%>%
          #filter(n>10)
        
        
       save(tf.idf, file=paste0("ACCOUNTS_EMOJIS_REPLACED_tf_idf_",
                                what.to.include, "_",
                                start.time, "_", end.time,
                                ifelse(exclude.retweets, "", "_WITH_RETWEETS" ),
                                ".Robj"))
       
        
        print(tf.idf %>%
                group_by(acc_type) %>%
                slice_max(order_by = tf_idf, n=10),
              n=60)
        
        
         # summarise(top_tfidf = head(desc(tf_idf)))
        
        
        # head(tf.idf %>%
        #        arrange(desc(tf_idf)), 20)
        
        
        
        # Calculating DISTINCT USERS per term (rather than sheer mentions)
        tokenized.term.counts.unique.users <- tokenized.term.counts[!duplicated(tokenized.term.counts %>% select(user, word)),]
        
        term.unique.user.counts <- tokenized.term.counts.unique.users %>%
          filter(!word %in% c(geo.terms, polit.terms, "redtide", "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"),
                 is.na(as.numeric(word))) %>%
          count(word, sort=TRUE)
        
        
        tf.idf.unique.users <- tokenized.term.counts.unique.users %>%
          filter(!word %in% c(geo.terms, polit.terms, "redtide",  "kbrevis", "karenia", "brevis", "kareniabrevis", "redalgae"),
                 is.na(as.numeric(word))) %>%
          #  count(tweet_id, word, sort = TRUE) %>%
          # bind_tf_idf(word, tweet_id, n)
          count(acc_type, word, sort = TRUE) %>%
          bind_tf_idf(word, acc_type, n) #%>%
          #filter(n>10)
        
        head(tf.idf.unique.users)
        
        
       save(tf.idf.unique.users, file= paste0("ACCOUNTS_EMOJIS_REPLACED_tf_idf_unique_users_",
                                              what.to.include, "_",
                                              start.time, "_", end.time,
                                              ifelse(exclude.retweets, "", "_WITH_RETWEETS" ),
                                              ".Robj"))
        
        print(tf.idf.unique.users %>%
          group_by(acc_type) %>%
          slice_max(order_by = tf_idf, n=10),
          n=60)


#dev.off()




