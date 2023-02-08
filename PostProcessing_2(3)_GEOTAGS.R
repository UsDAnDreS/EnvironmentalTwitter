## NOTE: No need for "PostProcessing_2" as it was for the NON-GEOTAG TWEETS, as there are NO RETWEETS IN HERE

#######
## Obtain CONVERSATION THREADS from 
##  1. Original or Retweets (NOT REPLIES, as THOSE CAN'T ORIGINATE A THREAD), with AT LEAST 1 REPLY ("reply_count = 1")
##  2. Keeping in mind that 
##    a. it ONLY PULLS REPLIES (NOT THE ORIGINAL TWEET ITSELF)
##    b. and that THERE ARE POTENTIAL INCONSISTENCIES due to ACCOUNTS/POSTS being deleted, USERS BEING PRIVATE etc etc
##  3. Arrange them by date? NOT by "reply thread has to be clustered together"
## 

source("Project_Functions.R")
source("Project_Objects.R")

load(file="topic.dfs.geotag.expanded.urls.RData")


#######
##  DETAILS ON SOME OF THE ISSUES:
##
## https://twittercommunity.com/t/conversation-id-query-returns-only-replies-other-inconsistencies-with-academic-track-archive-search/181569
##
## YES, it's NOT EXPECTED TO RETURN THE ORIGINAL TWEET 
##
# "It’s hard to say what the root of the issues is: 
#   It could be the delay between gathering the original tweets, and retrieving conversations (if any) 
#   in that time users could have deactivated, or removed tweets, or made new replies. 
#   Counts could be reflecting an older state and not updated in time, or the search index 
#   failed to retrieve data and silently gave back 0 results. There are multiple things."
# 
# edit: oh the other one you mentioned, 1415770645154177035, yes -
#   most likely a private account reply, or deleted reply and counts not updated.
#######




#####
### NEED TO SPECIFY THE START TIME
# https://twittercommunity.com/t/conversation-id-does-not-even-return-original-post/162692/6
# https://twittercommunity.com/t/conversation-full-archive-search/158380
# https://twittercommunity.com/t/twarc2-conversation-zero-returns/172278/6
#
### 
## If NEED TO GO WITH "twarc"
# 
# twarc2 conversations --archive input.txt output.jsonl
# write(conversation_ids[[l]][[j]], "/home/andrey/input.txt")
#####


conversation_ids <- all_queries <- list()

for (l in 1:length(main.queries)){
  
  conversation_ids[[l]] <- all_queries[[l]] <- list()
  
  for (j in 1:length(area.terms)){
    print(j)
    
    # Only including tweets that have NA in "in_reply_to_user_id" field (that would exclude all the replies which can't originate a thread),
    #   and that themselves have a reply_count of at least 1
    
    # For the NON-EMPTY tweet loads
    if (class(topic.dfs.geotag[[l]][[j]])[1] %in% c("data.frame", "list")){
      
    conversation_ids[[l]][[j]] <- topic.dfs.geotag[[l]][[j]]$conversation_id[is.na(topic.dfs.geotag[[l]][[j]]$in_reply_to_user_id)
                                                                                     & (topic.dfs.geotag[[l]][[j]]$public_metrics.x_reply_count > 0)]
    
    } else {
      # If not - empty data frame
      conversation_ids[[l]][[j]] <- data.frame()
    }
    
    # If no conversations - assign "None", next iteration.
    if (length(conversation_ids[[l]][[j]]) == 0){
      all_queries[[l]][[j]] <- "None"
      next;
    }
    
    full.query <-  NULL
    
    
    i <- 0
    
    ## While index is less than the total of conversation id's, keep creating queries
    ##  Each query should be no longer than 1024;
    ##  eventual result ("conversation_ids[[l]][[j]]") will have a VECTOR of QUERIES,
    ##  that exhausts the entirety of all conversation id's
    
    while (i <= length(conversation_ids[[l]][[j]])){
      
      print(i)
      
      flag <- 0
      i <- i+1
      
      interm.query <- paste0("(conversation_id:", conversation_ids[[l]][[j]][i], sep=" ")
      
      repeat{
        i <- i+1
        # If iterations reached the maximum, drop out, mark it with a flag.
        if (i > length(conversation_ids[[l]][[j]])) {flag <- 1; break;}
        # If adding this ID doesn't overlow the "1024" character limit, go ahead. Otherwise, drop out of the loop.
        if ((nchar(paste0(interm.query, paste0("OR conversation_id:", conversation_ids[[l]][[j]][i], ")", sep=" "), collapse=" "))) > 1024) break;
        interm.query <- paste0(interm.query, paste0("OR conversation_id:", conversation_ids[[l]][[j]][i], sep=" "), collapse=" ")
      }
      
      if (flag == 0) i <- i-1
      
      ## Cutting out the final " ", and then replacing it with ")"
      interm.query <- substr(interm.query, 1, nchar(interm.query)-1)
      interm.query <- paste0(interm.query, ")")
      
      # Attach the new interm query to the other available ones. Refresh the "interm.query" variable
      full.query <- c(full.query, interm.query)
      interm.query <- "("
      
    }
    
    all_queries[[l]][[j]] <- full.query
    
  }
}

# paste0("conversation_id:", conversation_ids[[1]], collapse=" ")

all_queries


####
## For each area:
##    1. Go through each conversation_id query
##    2. Add the NON-DUPLICATES to the ORIGINAL "topic.dfs.geotag[[l]][[j]]" set
#####

df <- list()

for (l in 1:length(main.queries)){
  
  print(paste0("Query #", l))
  
  df[[l]] <- list()  
  
  for (j in 1:length(area.terms)){
    
    # If there were no conversation ids:
    if (all_queries[[l]][[j]] == "None"){
      df[[l]][[j]] <- "None";
      next;
    }
    
    
    for (query in all_queries[[l]][[j]]){
      
      print(j)
      
      geo.string <- area.terms[[j]]
      
      params = list(
        `query` = paste(#'lang:en', 
          query
        ), 
        `max_results` = '100',
        `start_time` = '2018-01-01T00:00:00Z',
        `media.fields` = 'url',
        `tweet.fields` = 'attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source',
        `user.fields` = 'created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,verified,withheld',
        `expansions` = 'geo.place_id,author_id'
        ,`place.fields` = 'contained_within,country,country_code,full_name,geo,id,name,place_type'
      )
      
      inter.query <- query_twitter(params,0)
      
      if (is.null(inter.query)){
        if (length(df[[l]]) < j) {
          df[[l]][[j]] <- "None"
        } 
      } else {
        if (length(df[[l]]) < j) {
          df[[l]][[j]] <- inter.query
        } else {
          # Instead of rbind, do full_join, in case of different colnames
          df[[l]][[j]] <- df[[l]][[j]] %>%  full_join(inter.query,
                                                      by=intersect(colnames(df[[l]][[j]]), 
                                                                   colnames(inter.query)))
        }
      }
      
      
      # unique(colnames(df[[l]][[j]]))
      # View(df[[l]][[j]])
      dim(df[[l]][[j]])
    }
  }
}

### Double-checking the conversation ids, after we obtain the actual tweet loads:
##    Printing out:
##      1. How many conversation ids were extracted from the data set to have >0 replies and emanate from a non-reply tweet itself.
##      2. How many conversation ids were returned after ('post') being requested for as a part of tweet load 
##        (making sure it's at least NOT LARGER than the 'prior', and if it is SMALLER - that just means that some "REPLY_COUNT" DATA was off)
##      3. Checking if tweet-load returned conversation ids are a part of the 'prior' set of ids (which they should be, so it should be "1.0")

for (l in 1:length(main.queries)){
  for (j in 1:length(area.terms)){
    ## If not a character (which would've been the case of "None", an empty dataset)
    if (!is.character(df[[l]][[j]])){
      print(paste0("Query: ", l, ";  Area: ", j))
      print(paste0("# of conversation ids prior: ", length(conversation_ids[[l]][[j]])))
      print(paste0("# of conversation ids post : ", length(unique(df[[l]][[j]]$conversation_id))))
      print(mean(sapply(unique(df[[l]][[j]]$conversation_id), function(x) x %in% conversation_ids[[l]][[j]])))
    }
  }
}

for (l in 1:length(main.queries)){
  print(sapply(df[[l]], function(x) dim(x)))
}


### Joining conversations together with the original tweet dataset
##    1. Using the full_join by all intersecting columns, BUT...
##    2. Making sure to subsequently clean up the DUPLICATE IDs, 
##    otherwise it could get messed up due to changes to various engagement metrics
##    (e.g. between the time I got the original tweet load and the conversation_id load)

full.df <- list()

for (l in 1:length(main.queries)){
  
  full.df[[l]] <- list()  
  
  for (j in 1:length(area.terms)){
    print(paste0("Query: ", l, ";  Area: ", j))
    cat("\n")
    
    if (class(topic.dfs.geotag[[l]][[j]])[1] %in% c("data.frame", "list")){
    print(paste0("Dataset size without conversations: ", 
                 nrow(topic.dfs.geotag[[l]][[j]]), ", ", 
                 length(unique(topic.dfs.geotag[[l]][[j]]$id))))
    }
    
    # If there were no conversations added, then just keep as is and move onto the next.
    ## If a character (hence the case of "None", an empty dataset)
    if (is.character(df[[l]][[j]])){
      print(paste0("Conversation size: ", 0, ",", 0))
      full.df[[l]][[j]] <- topic.dfs.geotag[[l]][[j]];
      
      if (class(topic.dfs.geotag[[l]][[j]])[1] %in% c("data.frame", "list")){
      print(paste0("Dataset size with conversations:", 
                   nrow(full.df[[l]][[j]]), ", ", 
                   length(unique(full.df[[l]][[j]]$id))))
      }
      next;
    }
    
    print(paste0("Conversation size: ", 
                 nrow(df[[l]][[j]]), ", ", 
                 length(unique(df[[l]][[j]]$id))))
    
    full.df[[l]][[j]] <- topic.dfs.geotag[[l]][[j]] %>% full_join(df[[l]][[j]],
                                                                          by=intersect(colnames(topic.dfs.geotag[[l]][[j]]),
                                                                                       colnames(df[[l]][[j]])))
    
    ## Getting rid of potential duplicates as a result of joining
    #   Duplicates arise due to potential: edit history, changes in follower numbers, account tweet counts, tweet engagement metrics, etc
    # https://twittercommunity.com/t/edit-history-tweet-ids-field-not-edit-history-tweet-ids/182820
    
    ind.dup <- which(duplicated(full.df[[l]][[j]]$id))
    if (length(ind.dup) > 0){
      # Checking the duplicates (if need be)
      # View(full.df[[l]][[j]][ full.df[[l]][[j]]$id %in% full.df[[l]][[j]]$id[ind.dup[1]], ])
      full.df[[l]][[j]] <- full.df[[l]][[j]][-which(duplicated(full.df[[l]][[j]]$id)), ]
    } 
    print(paste0("Dataset size with conversations:", 
                 nrow(full.df[[l]][[j]]), ", ", 
                 length(unique(full.df[[l]][[j]]$id))))
  }
  
}

for (l in 1:length(main.queries)){
  print(sapply(full.df[[l]], function(x) dim(x)))
}

save(full.df, file="topic.dfs.geotag.full.UNsorted.RData")


#######
## TRYING TO ORDER BY DATE...
######


for (l in 1:length(main.queries)){
  for (j in 1:length(area.terms)){
    ## 
    
    if (class(full.df[[l]][[j]])[1] %in% c("data.frame", "list")){
    print(all_equal(full.df[[l]][[j]] %>% mutate(created_at.x = as.POSIXct(created_at.x, tz = "UTC", "%Y-%m-%dT%H:%M:%OS")),
                    full.df[[l]][[j]] %>% mutate(created_at.x = as.POSIXct(created_at.x, tz = "UTC", "%Y-%m-%dT%H:%M:%OS")) %>% arrange(created_at.x)))
    
    full.df[[l]][[j]] <- full.df[[l]][[j]] %>% mutate(created_at.x = as.POSIXct(created_at.x, tz = "UTC", "%Y-%m-%dT%H:%M:%OS")) %>% arrange(created_at.x)
    }
  }
}

View(full.df[[1]][[5]] %>% select(text, created_at.x))

save(full.df, file="topic.dfs.geotag.full.sorted.RData")

