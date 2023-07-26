source("Project_Functions.R")
source("Project_Objects_RECENT.R")

# NOTES:
##    1. Need a potentially AUTOMATED WAY to POST-PROCESS/PARSE OUT THESE KINDS OF TWEETS:
##    "Not surprising the avaricious Glazers tried to exploit off this catastrophe, pure greed. #GlazersOut https://t.co/kPmqVMfOHc"
##      https://thepewterplank.com/2019/05/27/tampa-bay-buccaneers-attempted-receive-compensation-2010-oil-spill/
##



## IF WE JUST WANT TO CHECK THE TWEET COUNTS PRIOR TO PULLING
## (Using the "COUNT" ENDPOINT)

# main.queries <- main.queries["AllAlgae"]
# 
# for (l in 1:length(main.queries)){
#   
#   print(paste("Query #",l, sep=""))
#   print(main.queries[[l]])
#   
#   for (j in 1:length(area.terms)){
#     print(j)
#     
#     geo.string <- area.terms[[j]]
#     
#     # TAKING FROM HERE:
#     # https://github.com/twitterdev/Twitter-API-v2-sample-code/blob/main/Recent-Tweet-Counts/recent_tweet_counts.r
#     
#     params = list(
#       `query` = paste(main.queries[[l]], 
#                       geo.string
#       ),
#       `granularity` = 'day'
#     )
#     
#     flag <- 0
#     
#     while (flag == 0){
#     response <- httr::GET(url = 'https://api.twitter.com/2/tweets/counts/recent', httr::add_headers(.headers=headers), query = params)
#     
#     body <-
#       content(
#         response,
#         as = 'parsed',
#         type = 'application/json',
#         simplifyDataFrame = TRUE
#       )
#     
#     if ((length(body$title) > 0)){
#       if (body$title == "Too Many Requests"){
#         Sys.sleep(5)
#       } else {
#         flag <- 1
#       }
#     } else {
#       flag <- 1
#     }
#     
#     }
#     
#     print(body$meta$total_tweet_count)
# 
#   }
#   
# }






######
## Collecting the data:
##    Only focusing on ALGAE-RELATED STUFF. 
######

topic.dfs <- list()

main.queries <- main.queries[c("RedTide", "AllAlgae")]
main.queries

for (l in 2:length(main.queries)){

  print(paste("Query #",l, sep=""))
  print(main.queries[[l]])
  
  if (length(main.queries[[l]]) == 0) stop()
  
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
      `media.fields` = 'url',
      `tweet.fields` = 'attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source',
      `user.fields` = 'created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,verified,withheld',
      `expansions` = 'geo.place_id,author_id'
      ,`place.fields` = 'contained_within,country,country_code,full_name,geo,id,name,place_type'
    )
    
    resp <- query_twitter(params,0, type="recent")
    resp
    if (is.null(resp)){
      geo.dfs.within.topic[[j]] <- data.frame()
    } else {
      geo.dfs.within.topic[[j]] <- resp
    }
    
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



save(topic.dfs, file="recent.search.dfs.RData")
