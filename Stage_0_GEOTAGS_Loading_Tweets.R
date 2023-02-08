###########
## THERE IS INDEED A PROBLEM with "RED TIDE" VERACITY OF MATCHES...
##    * Tons of POLITICAL "RED TIDES" for REPUBLICAN PARTY... ESPECIALLY FOR TAMPA/HILLSBOROUGH TWEETS (almost ALL GEOTAGS are POLITICS-RELATED)
##      NOT REALLY AN ISSUE for CLEARWATER, 
##      For ST PETE - NOT AN ISSUE... just a COUPLE tweets about POLITICAL RED TIDE around the ELECTION TIME in 2018
##      For MANATEE - NOT AN ISSUE whatsoever, even during the election periods
##      For SARASOTA - A COUPLE, but STILL RELEVANT TO RED TIDE (e.g. "VOTE THEM OUT, BLUE WAVE #Redtide")
##      For PASCO - just 5 tweets total, ONE OF THEM is about RED TIDE IN POLITICAL SENSE (2018)
###########


source("Project_Functions.R")
source("Project_Objects.R")


######
## Collecting the data:
##    For each topic (query). Within each topic, hitting on every geo location
##    Each location is determined by GEOTAG BOXES where users EXPLICITLY TAGGED THEMSELVES during TWEETING
######

#######
## REALLY WEIRD API BEHAVIOR: For the SAME QUERY, keeps pulling NULL pages, with NON-NULLS thrown IN-BETWEEN
#######

# [1] "Page 1 returned null."
# [1] "Page 2 returned NON-null."
# [1] "Page 3 returned null."
# [1] "Page 4 returned null."
# [1] "Page 5 returned null."
# [1] "Page 6 returned null."
# [1] "Page 7 returned null."
# [1] "Page 8 returned null."
# [1] "Page 9 returned null."
# [1] "Page 10 returned null."
# [1] "Page 11 returned null."
# [1] "Page 12 returned null."
# [1] "Page 13 returned null."
# [1] "Page 14 returned null."
# [1] "Page 15 returned null."
# [1] "Page 16 returned NON-null."
# [1] "Page 17 returned null."
# [1] "Page 18 returned NON-null."
# [1] "Page 19 returned null."
# [1] "Page 20 returned null."
# [1] "Page 21 returned null."
# [1] "Page 22 returned null."



topic.dfs.geotag <- list()

for (l in 1:length(main.queries)){
  
  print(paste("Query #",l, sep=""))
  print(main.queries[[l]])
  
  geo.dfs.within.topic <- list()
  
  for (j in 1:length(area.tags)){
    print(j)
    
    geo.string <- area.tags[[j]]
    
    # Going from 2018, to incorporate the big red tide year
    params = list(
      `query` = paste(main.queries[[l]], 
                      geo.string
      ), 
      `max_results` = '100',
      `start_time` = '2018-01-01T00:00:00Z',
      `end_time` = '2023-01-01T00:00:00Z', 
      `media.fields` = 'url',
      `tweet.fields` = 'attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source',
      `user.fields` = 'created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,verified,withheld',
      `expansions` = 'geo.place_id,author_id'
      ,`place.fields` = 'contained_within,country,country_code,full_name,geo,id,name,place_type'
    )
    
    
    # response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', 
    #                       httr::add_headers(.headers=headers), query = params)
    # response
    # 
    # # fas_body$errors
    # fas_body <-
    #   content(
    #     response,
    #     as = 'parsed',
    #     type = 'application/json',
    #     simplifyDataFrame = TRUE
    #   )
    # 
    # fas_body$meta$next_token
    # 
    # fas_body
    # dim(fas_body$data)
    # 
    # ## Checking TWEET CONTENTS
    # # fas_body$data$text
    # View(data.frame(fas_body$data$text, fas_body$data$created_at))
    
    interm.df <- query_twitter(params,0)
    if (!is.null(interm.df)){
      geo.dfs.within.topic[[j]] <- interm.df
    } else {
      geo.dfs.within.topic[[j]] <- "None"
    }
    unique(colnames(geo.dfs.within.topic[[j]]))
    # View(geo.dfs.within.topic[[j]])
    dim(geo.dfs.within.topic[[j]])
  }
  
  topic.dfs.geotag[[l]] <- geo.dfs.within.topic
  
}


for (l in 1:length(main.queries)){
  print(sapply(topic.dfs.geotag[[l]], function(x) dim(x)))
}

names(topic.dfs.geotag) <- names(main.queries)


## Checking if all tweet IDs are unique
## (if all good - nothing is printed out)
print("Checking if all tweet IDs are unique:")
for (l in 1:length(main.queries)){
  for (j in 1:length(area.tags)){
    if (class(topic.dfs.geotag[[l]][[j]])[1] %in% c("data.frame", "list")){
      if (nrow(topic.dfs.geotag[[l]][[j]]) != length(unique(topic.dfs.geotag[[l]][[j]]$id))){
        print(paste0("Query: ", l, ";  Area: ", j, " ;  FALSE"))
      }
    }
  }
}


save(topic.dfs.geotag, file="topic.dfs.geotag.RData")
