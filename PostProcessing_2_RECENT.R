#######
## ISSUE: the RETWEETS for GOOD ORIGINAL TWEETS - TEXT DOESN'T SEEM TO CHANGE (STILL CUTS OFF)
#######

##########
###   1. Cleaning out the tweets that got matched to a region based on SOLELY:
###       * A SHAKY LINK (e.g. patch.com, bradenton.com)
###       * Bad text reference (e.g. "CBS Tampa", "Tampa Bay Times", not necessarily about Tampa/Hillsborough)
###
###   2. Bringing back retweets, copy-pasting text/url info from the original tweet.
##########

source("Project_Functions.R")
source("Project_Objects_RECENT.R")

library(tidyverse)


load(file="recent.search.dfs.non.retweets.expanded.urls.RData")





for (l in 1:length(main.queries)){
  print(sapply(topic.dfs[[l]], function(x) dim(x)))
}




area.terms <- list(
  Tampa.query.chunk,
  Pinellas.Clearwater.query.chunk,
  StPete.query.chunk,
  Manatee.query.chunk,
  Sarasota.query.chunk,
  Pasco.query.chunk
)


## Get the WORD BOUNDARIES
# grep("\\b(tampa bay)\\b", "tampa bay hey", ignore.case = T)
query.terms <- lapply(area.words, function(x) paste("\\b(", x, ")\\b", sep=""))
query.terms




#####
## Getting rid of area-specific bad links (e.g. bradenton.com, stpete.org,)
#####

# Tampa.query.chunk 
##    https://www.tampabay.com/news/tampa/   -  is probably OK, mostly pertinent to Tampa.
##    https://www.cbsnews.com/tampa/ - not OK, GOTTA POST-PROCESS, has "Sarasota FLA" entries (e.g. https://www.cbsnews.com/tampa/news/elevated-levels-of-red-tide-present-in-the-gulf)
#
# Pinellas.Clearwater.query.chunk
#     https://www.abcactionnews.com/news/region-pinellas/   - is likely OK, every article starts either with "PINELLAS CO" or "ST PETE" etc
#     "pinellas.gov" - VERY RARE, BUT.. I guess that could be OK as, while it's not directly indicative of red tide being in Pinellas, 
#                there's a REASON why "pinellas.gov" is being referenced in the tweet (to tailor towards Pinellas residents, 
#                because red tide might be coming around that area)
#     https://www.wfla.com/news/pinellas-county/ - seems OK, all articles pertain to Pinellas specifically
#     https://www.tampabay.com/news/pinellas/ - seems relevant, especially lengthy links
# 	  https://pinellas.gov/red-tide, https://pinellas.floridahealth.gov/  - same as all other "gov", seems OK
#     https://www.wtsp.com/article/news/local/pinellascounty/ - seems OK, local to Pinellas mostly


# StPete.query.chunk
#     stpete.org - I guess that could be OK as, while it's not directly indicative of red tide being in StPete, 
#                 there's a REASON why "stpete.org" is being referenced in the tweet (to tailor towards StPete residents, 
#                 because red tide might be coming around that area)
#             


# Manatee.query.chunk 
##      "bradenton.com" - yep. Bradenton Herald, doesn't mean that it's about bradenton specifically, 
##                      plus the reach of the publication goes beyond the area.
##    	https://www.newsbreak.com/florida/bradenton/news/ - might be OK, but seems outdated.. just keep as is.


# Sarasota.query.chunk
#       https://www.wfla.com/news/sarasota-county - totally OK, every article starts with "SARASOTA CO" 
#       https://www.abcactionnews.com/news/region-sarasota-manatee/  - can be a mix, especially in mixing up Sarasota & Manatee - PROBABLY WORTH POST-FILTERING...
#       https://sarasota.floridahealth.gov - just like other "gov" should be fine
#    	  https://www.wtsp.com/article/news/local/sarasotacounty/ - SEEMS RELEVANT.. gotta include "SARASOTACOUNTY" WEBSITE MATCHES (same as PINELLASCOUNTY)
#       https://www.yoursun.com/englewood/ OR venice  -  ALL GOOD, for the VAST MAJORITY those are LOCAL



# There's a PHANTON "space" between "CBS" & Tampa that's not readable in R.. had to extract it separately.
load("CBS_Tampa.RData")

# CBS.tampa.string <- substr(topic.dfs[[l]][[j]]$text[hey$actual.inds[1]], 51,59)
# save(CBS.tampa.string, file = "CBS_Tampa.RData")


## Unreliable tweet text terms for each location

shaky.terms <- list(Tampa = c("Tampa Bay Times",        # Yep, gotta post-filter.
                              "Tampa Bay Waterkeeper",  # Yep, post-filter. Even though the organization about the Tampa Bay, it doesn't point to LOCATION of Tampa Bay directly
                              "CBS Tampa", CBS.tampa.string),             # Yep, post-filter. Doesn't point DIRECTLY to Tampa, has a "Sarasota FLA" article, e.g. "https://www.cbsnews.com/tampa/news/elevated-levels-of-red-tide-present-in-the-gulf/"
                    PinellasClearwater = c(),
                    PinellasStPete = c(
                      #"St Pete Catalyst", "stpetecatalyst      # These are mostly local, should be fine
                    ),  
                    Manatee = c("Bradenton Herald"),   # NEED TO MIX IN with the LINKS ABOVE RIGHT AWAY (otherwise it would mistakenly retain a tweet)
                    Sarasota = c("Sarasota Magazine", "Sarasota Herald", "sarasotamagazine"),
                    Pasco = c()
)


## Unreliable links for each location
shaky.links <- list(Tampa = c("https://www.baynews9.com/fl/tampa/", "https://www.cbsnews.com/tampa/"),
                    PinellasClearwater = c(),
                    PinellasStPete = c(),
                    Manatee = c("bradenton.com", "https://www.abcactionnews.com/news/region-sarasota-manatee/"),
                    Sarasota = c("https://www.abcactionnews.com/news/region-sarasota-manatee/"),
                    Pasco = c())


#######
## For each location:
##    Checking if there are no problematic terms/links. If there's not - move on to the next location.
##    If there are problematic terms/links:
##        Applying the "postprocess" function to all those terms/links, 
##        where we strip the tweet of those, and then look for whether there are still any mentions of the relevant area remaining.
##    Remove the tweets where the mention of the area happens to be solely by virtue of shaky links/terms.


for (l in 1:length(topic.dfs)){
  print(paste0("Topic #: ", l, ", ", names(topic.dfs)[l]))
  
  
  for (j in 1:length(shaky.links)){
    
    # If there's no tweets
    if (nrow(topic.dfs[[l]][[j]]) == 0) next;
    
    # If there's no shaky links/terms
    if (length(shaky.links[[j]]) == 0 & length(shaky.terms[[j]]) == 0) next;
    
    # for (link in shaky.links[[j]]){
    
    # link.pattern <- ifelse(is.na(link), "", paste(unlist(strsplit(shaky.links[[j]], split="\\W")), collapse=" "))
    link.pattern <- sapply(shaky.links[[j]], function(x) paste(unlist(strsplit(x, split="\\W")), collapse=" "))
    names(link.pattern) <- NULL
    link.pattern
    
    term.pattern <- shaky.terms[[j]]
    
    hey <- postprocess.badterm.cleanup(topic.dfs[[l]][[j]]$text, bad.terms = c(link.pattern,term.pattern), grep.terms = query.terms[[j]])
    length(hey)
    names(hey$actual.inds) <- NULL
    names(hey$ind.drop) <- NULL
    
    # If there are no "bad" matches
    if (length(hey$ind.drop) == 0) next;
    
    bad.ind <- hey$actual.inds[hey$ind.drop]
    good.ind <- hey$actual.inds[!hey$ind.drop]
    bad.ind
    good.ind
    #  View(data.frame(topic.dfs[[l]][[j]]$text[bad.ind]))
    #  View(data.frame(topic.dfs[[l]][[j]]$text[good.ind]))
    
    topic.dfs[[l]][[j]]$actual_url_link[good.ind]
    
    if (length(bad.ind) > 0) topic.dfs[[l]][[j]] <- topic.dfs[[l]][[j]][-bad.ind, ]
    
  }
}



#######
## For each location:
##      Getting rid of a general pattern of bad links (e.g. patch.com/florida/[city name]),
##      PRETTY MUCH THE SAME AS ABOVE.
#####

always.shaky.in.text <- c("patch com florida [a-zA-Z0-9]* ")
always.shaky.in.text


for (l in 1:length(topic.dfs)){
  print(paste0("Topic #: ", l, ", ", names(topic.dfs)[l]))
  
  for (j in 1:length(query.terms)){
    
    # If there's no tweets
    if (nrow(topic.dfs[[l]][[j]]) == 0) next;
    
    for (link in always.shaky.in.text){
      link.pattern <- link
      
      hey <- postprocess.badterm.cleanup(topic.dfs[[l]][[j]]$text, bad.terms = link.pattern, grep.terms = query.terms[[j]])
      length(hey)
      names(hey$actual.inds) <- NULL
      names(hey$ind.drop) <- NULL
      
      # If there are no "bad" matches
      if (length(hey$ind.drop) == 0) next;
      
      bad.ind <- hey$actual.inds[hey$ind.drop]
      good.ind <- hey$actual.inds[!hey$ind.drop]
      bad.ind
      good.ind
      # View(data.frame(topic.dfs[[l]][[j]]$text[bad.ind]))
      # View(data.frame(topic.dfs[[l]][[j]]$text[good.ind]))
      topic.dfs[[l]][[j]]$actual_url_link[good.ind]
      
      if (length(bad.ind) > 0) topic.dfs[[l]][[j]] <- topic.dfs[[l]][[j]][-bad.ind, ]
      
    }
  }
}



######
## Bringing in ALL TWEETS, !!! INCLUDING THE RETWEETS !!!
######

### Obtaining IDs of all the "clean" tweets, for each query, for each location.
good.tweet.ids <- list()
for (l in 1:length(main.queries)){
  good.tweet.ids[[l]] <- lapply(topic.dfs[[l]], function(x) x$id)
}

### Saving the full dataset with clean tweets to an "all.cleaned" object
### (because we'll be loading a similarly-named object from memory via 'load()' operation)

all.cleaned.topic.dfs <- topic.dfs

## Loading the full data set, with retweets and "dirty" tweets
## It now takes place of "topic.dfs" object
load("recent.search.dfs.RData")



### For retweets that correspond to a "good" original tweet, we
###   1. Create a list of their corresponding good original tweet IDs
###   2. Left-join that list with "id, text, text_with_display_links" of the cleaned-up data set
###   3. That gives us a properly-ordered "fill-in" of text/url-info, 
##      we proceed to FILL IT into the retweet data that corresponds to good original tweets.
##     (instead of their original data)
##    4. Append the retweets to the cleaned non-retweets, arrange by "created_at.x"


both.tweets.and.retweets <- list()

for (l in 1:length(topic.dfs)){
  
  both.tweets.and.retweets[[l]] <- list()
  
  for (j in 1:length(area.terms)){
    dim(topic.dfs[[l]][[j]])
    
    good.ind <- which(((sapply(topic.dfs[[l]][[j]]$referenced_tweets, function(x) ifelse(length(x) > 0, x['type'][1], "not retweeted")) == "retweeted") & 
                         (sapply(topic.dfs[[l]][[j]]$referenced_tweets, function(x) ifelse(length(x) > 0, x['id'][1], "none")) %in% good.tweet.ids[[l]][[j]])))
    length(good.ind)
    
    # If there are no good retweets, just record the original tweets
    if (length(good.ind) == 0) {
      both.tweets.and.retweets[[l]][[j]] <- all.cleaned.topic.dfs[[l]][[j]]
      next;
    }
    
    all.good.retweets <- topic.dfs[[l]][[j]][good.ind,]
    
    hoi <- data.frame(id=unlist(sapply(all.good.retweets$referenced_tweets, function(x) ifelse(length(x) > 0, x['id'][1], "none"))))
    #dim(hoi)
    hey <- hoi %>% left_join(all.cleaned.topic.dfs[[l]][[j]] %>% dplyr::select(id, text, text_with_display_links),
                             by="id")
    #dim(hey)
    # View(hey)
    # View(all.good.retweets)
    
    ## Retaining the "RT @...:" part, but replacing the following chunk with the full text of the original tweet
    ##    (First we split away the "RT @... :" part
    rt.at.part <- sapply(all.good.retweets$text, function(x) str_split(x, ":")[[1]][1])
    ##    (Second we paste it with 1) the unwound-url text; 2) the "display url" text;)
    all.good.retweets$text <- apply(cbind(rt.at.part, hey$text), 1, function(x) paste(x[1], x[2], sep = ": "))
    all.good.retweets$text_with_display_links <- apply(cbind(rt.at.part, hey$text_with_display_links), 1, function(x) paste(x[1], x[2], sep = ": "))
    View(all.good.retweets)
    
    # nrow(all.good.retweets)
    # length(unique(all.good.retweets$id))
    #  nrow(all.cleaned.topic.dfs[[l]][[j]])
    #  length(unique(all.cleaned.topic.dfs[[l]][[j]]$id))
    
    all.cleaned.topic.dfs[[l]][[j]]$created_at.x
    
    ## HAD TO SWITCH "rbind()" OUT FOR "bind_rows()"
    ## Because the former didn't work for fancier data frames with variables that take on list values
    both.tweets.and.retweets[[l]][[j]] <- bind_rows(all.cleaned.topic.dfs[[l]][[j]],
                                                    all.good.retweets)
    
    both.tweets.and.retweets[[l]][[j]] <- arrange(both.tweets.and.retweets[[l]][[j]], 
                                                  desc(created_at.x))
    
    
    
  }
}


for (l in 1:length(main.queries)){
  print(sapply(both.tweets.and.retweets[[l]], function(x) dim(x)))
}


## Checking if all tweet IDs are unique
## (if all good - nothing is printed out)
print("Checking if all tweet IDs are unique:")
for (l in 1:length(main.queries)){
  for (j in 1:length(area.terms)){
    if (nrow(both.tweets.and.retweets[[l]][[j]]) != length(unique(both.tweets.and.retweets[[l]][[j]]$id))){
      print(paste0("Query: ", l, ";  Area: ", j, " ;  FALSE"))
    }
  }
}


save(both.tweets.and.retweets, file="recent.search.dfs.all.cleaned.tweets.RData")

