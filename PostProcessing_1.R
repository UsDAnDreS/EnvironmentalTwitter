#########
## NOTE (SOMETHING TO THINK ABOUT):
##    MIGHT WANT TO CAREFULLY RETHINK THE INCORPORATION OF THE LINKS INTO THE TEXT...
##   (GIVEN THAT SOME OF THEM ARE NON-INFORMATIVE, and could CLOG UP SENTIMENT ANALYSIS/WORD CLOUDS ETC ETC)
##########


###########
##  1. Excluding retweets (for now)
##  2. Identifying tweets with url links, verifying their expanded_urls are OK (with https/http check), incorporating those into the text.
#     (NOTE: If the search terms don't show up in Twitter API's "expanded_url" field, the tweet was NOT matched by link anyway,
#     BUT I still do the cleanup of links to make sure we have the entire context of what the person wanted to convey)
#     If the "expand_urls() function gives NA, we STICK WITH THE ORIGINAL TWITTER API "expanded_url" FIELD
##  3. Deleting links from text itself. Disposing of broken links (NA)
##  4. Deleting the "post-?" chunk of links, that never contains anything truly relevant
##
###   EXPLANDING THE URLS could be SOMEWHAT LENGTHY (at least over 5 years).. but if we smooth out the process where 
####  only a handful of tweets are loaded at once (e.g. just 1 day, or even 6-12 hours), likely won't be an issue.
##########

source("Project_Functions.R")
source("Project_Objects.R")

# load("all.tweets.RData")
load("topic.dfs.RData")

###
# https://github.com/thomasfitzgerald87/tbep_twitter_dashboard/blob/main/twitter_query.rmd
###

######
## This code:
##    1. ONLY DEALS WITH ORIGINAL TWEETS or REPLIES (EXCLUDING RETWEETS)
##
##  LATER we would just: 
##    2. COPY-PASTE the TEXT of the ORIGINAL TWEET into RETWEETS
## (LATER EXCLUDE RETWEETS that were REFERENCING A POST-PROCESSED TWEET)


library(longurl)



#####
## ONLY NON-RETWEETS
#####

## For each topic (l)
##  For each location (i)
##    (See the body of the loop for details of steps being performed)

for (l in 1:length(topic.dfs)){
  print(paste0("Topic #: ",l, ", ", names(topic.dfs)[l]))
  
  for (i in 1:length(area.terms)){
    print(i)
    
    ## Excluding Retweets
    ## (using the "referenced_tweets" field)
    topic.dfs[[l]][[i]] <- topic.dfs[[l]][[i]][!(sapply(topic.dfs[[l]][[i]]$referenced_tweets, function(x) ifelse(length(x) > 0, x['type'][1], "not retweeted")) == "retweeted"),]
    ## That's an ERROR-PRONE way, could get false positives
    # topic.dfs[[l]][[i]] <- topic.dfs[[l]][[i]][-grep("RT ", topic.dfs[[l]][[i]]$text), ]
    
    
    ## There could be a "retweet-only" data chunk, so...
    
    if (nrow(topic.dfs[[l]][[i]]) == 0){
      # Creating an empty data set, but with extended column names
      cnames <- colnames(topic.dfs[[l]][[i]])
      topic.dfs[[l]][[i]] <- data.frame(matrix(ncol=ncol(topic.dfs[[l]][[i]]) + 1, nrow=0))
      colnames(topic.dfs[[l]][[i]]) <- c(cnames, "text_with_display_links")
      next;
    }
    
    ## Identifying tweets with links:
    ##  "regexpr" gives either '-1' if there's no link, or a starting position of the match
    ##  "regmatches" uses regexpr's output to pull up the actual matches
    
    # !!!!!!!!
    # https://developer.twitter.com/en/docs/twitter-api/enterprise/enrichments/overview/expanded-and-enhanced-urls
    # "For requests made to the Full Archive Search API, 
    # expanded URL enrichment data is only available for Tweets 13 months old or newer."
    # !!!!!!!!!!
    
    # The URL enrichment is not available for Tweet links (including quote Tweets), Moments links, and 
    # profile links that are included within a Tweet. 
    
    #topic.dfs[[l]][[i]]$entities.x_urls
    
    
    pattern <- 'https://t.co/[a-zA-Z0-9]*'
    m <- regexpr(pattern, topic.dfs[[l]][[i]]$text)
    # grep(pattern, topic.dfs[[l]][[i]]$text)
    # regmatches(topic.dfs[[l]][[i]]$text, m)
    
    # Where "m != -1", the tweet has a link.
    # sum(sapply(topic.dfs[[l]][[i]][m != -1,]$entities.x_urls, is.null))
    # sum(sapply(topic.dfs[[l]][[i]][m == -1,]$entities.x_urls, function(x) !is.null(x)))
    
    ## There's tweets with MULTIPLE url links... something I DON'T THINK I'VE ACCOUNTED FOR BEFORE...
    ##  Some have SEVERAL REPEATED urls.. not sure why, but I just used "UNIQUE()" to clean that up
    #
    
    
    
    
    ## Extracting all DISTINCT pairs of "display_url" and "expanded_url"
    
    display.and.expanded.urls.list <- lapply(topic.dfs[[l]][[i]][m != -1,]$entities.x_urls, function(x) x[, c("display_url", "expanded_url")])
    display.and.expanded.urls.list
    
    # If there are no tweets with a URL link, just add a "text_with_display_links" column, as a copy of "text" column
    if  (length(display.and.expanded.urls.list) == 0){
      topic.dfs[[l]][[i]]$text_with_display_links <- topic.dfs[[l]][[i]]$text
      next;
    }
    
    display.and.expanded.urls.df <- data.frame()
    
    for (list.ind in 1:length(display.and.expanded.urls.list)){
      display.and.expanded.urls.df <- rbind(display.and.expanded.urls.df,
                                            display.and.expanded.urls.list[[list.ind]])
    }
    
    # Disposing of any duplicates across tweets; initializing "actual expanded url" field, that will containt the properly expanded tweets
    unique.display.and.expanded.urls.df <- display.and.expanded.urls.df[!duplicated(display.and.expanded.urls.df),]
    unique.display.and.expanded.urls.df$actual_expanded_url <- unique.display.and.expanded.urls.df$expanded_url
    
    # Indices where expanded urls are just slapping "https://" or "http://" on top of the display one, nothing else
    shaky.ind <- which(unique.display.and.expanded.urls.df$expanded_url %in% c(paste0("https://", unique.display.and.expanded.urls.df$display_url),
                                                                               paste0("http://", unique.display.and.expanded.urls.df$display_url)))
    unique.display.and.expanded.urls.df$actual_expanded_url[shaky.ind] <- expand_urls(unique.display.and.expanded.urls.df$expanded_url[shaky.ind])$expanded_url
    # View(unique.display.and.expanded.urls.df[shaky.ind, ] %>% select(expanded_url, actual_expanded_url))
    
    
    ## Checking if there are differences between "expand_urls()" function return vs Twitter API's "expanded_url" field
    ## for tweets where EXPANDED URL DOES MORE than just slap a "https://" or "http://"
    #  Yeah - there are some differences here and there, but Twitter API's "expanded_url" is actually probably better if anything. 
    #   Might be outdated, but was more relevant at the time of the post.
    
    # expansion.likely.not.needed <- expand_urls(unique.display.and.expanded.urls.df$expanded_url[-shaky.ind])$expanded_url
    # View(data.frame(
    #   expanded_url = unique.display.and.expanded.urls.df$expanded_url[-shaky.ind],
    #   actual_expanded_url = expansion.likely.not.needed)[which(unique.display.and.expanded.urls.df$expanded_url[-shaky.ind] != expansion.likely.not.needed),])
    # 
    # sum(unique.display.and.expanded.urls.df$expanded_url[-shaky.ind] != expansion.likely.not.needed,
    #     na.rm=T)
    
    
    ## Replacing all the bad expanded links for proper expanded links (via "left_join" part)
    ## Disposing of the "post ?" stuff in the links, which seems mostly to be analytics ("utm" stuff; via "strsplit(...)" part)
    # hop <- topic.dfs[[l]][[i]]$entities.x_urls
    topic.dfs[[l]][[i]]$entities.x_urls <- lapply(topic.dfs[[l]][[i]]$entities.x_urls, 
                                                  function(x) if (!is.null(x)){
                                                    x %>% left_join(unique.display.and.expanded.urls.df, 
                                                                    by=intersect(colnames(x), colnames(unique.display.and.expanded.urls.df))) %>% 
                                                      mutate(expanded_url = sapply(strsplit(ifelse(is.na(actual_expanded_url), expanded_url, actual_expanded_url), split="\\?"),
                                                                                   function(x) x[1])) %>% 
                                                      select(-actual_expanded_url)
                                                  } else {
                                                    NULL
                                                  }
    )
    
    
    # Stripping original text of display links, so that we could replace those with more meaningful expanded ones
    # Still keeping the old text around in the "text_with_display_links" column
    topic.dfs[[l]][[i]]$text_with_display_links <- topic.dfs[[l]][[i]]$text
    topic.dfs[[l]][[i]]$text <- gsub(pattern, "", topic.dfs[[l]][[i]]$text)
    
    # Accumulating all the links per each tweet into one string
    all_expanded_links_in_one_string <- sapply(topic.dfs[[l]][[i]]$entities.x_urls, function(x) paste0(sapply(unique(x$expanded_url),
                                                                                                              function(x) ifelse(is.na(x), "", paste(unlist(strsplit(x, split="\\W")), collapse=" "))),
                                                                                                       collapse =". "))
    # View(data.frame(all_expanded_links_in_one_string))
    
    # l <- 1; i <- 1
    # which(sapply(topic.dfs[[l]][[i]]$entities.x_urls, function(x) any(is.na(x$expanded_url))))
    # topic.dfs[[l]][[i]]$entities.x_urls[which(sapply(topic.dfs[[l]][[i]]$entities.x_urls, function(x) any(is.na(x$expanded_url))))]
    # all_expanded_links_in_one_string[928]
    
    
    # Attaching the link-string to the original text of the tweet (from which the display_url has been stripped already)
    topic.dfs[[l]][[i]]$text <- paste(topic.dfs[[l]][[i]]$text, 
                                      all_expanded_links_in_one_string,
                                      sep =". ")
    
    dim(topic.dfs[[l]][[i]])
    
    # View(data.frame(topic.dfs[[l]][[i]]$text))
    # View(data.frame(topic.dfs[[l]][[i]]$text[m == -1]))
    # topic.dfs[[l]][[i]]$text[m == -1]
    # topic.dfs[[l]][[i]][928,]
    
    # What about unwound? Nah, it's pointless.. no obvious rhyme or reason as to when it's available vs not, while "expanded_url" is always available
    # display.and.expanded.urls.list.extra <- lapply(topic.dfs[[l]][[i]][m != -1,]$entities.x_urls, function(x){
    #   if (is.null(x$unwound_url)){
    #     x[, c("display_url", "expanded_url")]
    #     } else {
    #       x[, c("display_url", "expanded_url", "unwound_url")]
    #     }})
    # display.and.expanded.urls.list.extra
    
    
  }
  
}


for (l in 1:length(main.queries)){
  print(sapply(topic.dfs[[l]], function(x) dim(x)))
}


## Checking if all tweet IDs are unique
## (if all good - nothing is printed out)
print("Checking if all tweet IDs are unique:")
for (l in 1:length(main.queries)){
  for (j in 1:length(area.terms)){
    if (nrow(topic.dfs[[l]][[j]]) != length(unique(topic.dfs[[l]][[j]]$id))){
      print(paste0("Query: ", l, ";  Area: ", j, " ;  FALSE"))
    }
  }
}



save(topic.dfs, file="topic.dfs.non.retweets.expanded.urls.RData")



