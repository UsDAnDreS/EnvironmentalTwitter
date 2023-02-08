source("Project_Functions.R")
source("Project_Objects.R")

# NOTES:
##    1. Need a potentially AUTOMATED WAY to POST-PROCESS/PARSE OUT THESE KINDS OF TWEETS:
##    "Not surprising the avaricious Glazers tried to exploit off this catastrophe, pure greed. #GlazersOut https://t.co/kPmqVMfOHc"
##      https://thepewterplank.com/2019/05/27/tampa-bay-buccaneers-attempted-receive-compensation-2010-oil-spill/
##


######
## Collecting the data:
##    For each topic (query). Within each topic, hitting on every geo location.
######

topic.dfs <- list()

for (l in 1:length(main.queries)){
  
  print(paste("Query #",l, sep=""))
  print(main.queries[[l]])
  
  geo.dfs.within.topic <- list()
  
  for (j in 1:length(area.terms)){
    print(j)
    
    geo.string <- area.terms[[j]]
    
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
    
    
    geo.dfs.within.topic[[j]] <- query_twitter(params,0)
    unique(colnames(geo.dfs.within.topic[[j]]))
    View(geo.dfs.within.topic[[j]])
    dim(geo.dfs.within.topic[[j]])
  }
  
  topic.dfs[[l]] <- geo.dfs.within.topic
  
}


for (l in 1:length(main.queries)){
  print(sapply(topic.dfs[[l]], function(x) dim(x)))
}

names(topic.dfs) <- names(main.queries)


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



save(topic.dfs, file="topic.dfs.RData")
