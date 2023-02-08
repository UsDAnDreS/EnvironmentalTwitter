source("Bearer_Token.R")

library(httr)
library(tidyverse)

# https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user
# https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/place

headers = c(
  `Authorization` = sprintf('Bearer %s', bearer_token)
)


# The following block includes all functions used in this document.
#
# set_parameters:             Formats input for query_twitter or query_twitter_count  Stores default values for all but query text and start/end date.
# query_twitter:              Retrieves tweets matching search criteria. (queries.___)
#   fix_page_data:            Sub-function for query_twitter  
#   *location_filter_tweets:  Meant to filter by user location.  MAY NOT WORK IN CURRENT VERSION.
#   *combine_annotations:     Meant to merge annotations.  MAY NOT WORK IN CURRENT VERSION.
#   query_twitter_count:      Returns a count of tweets matching criteria.  Granularity can be set to 'day','hour','minute', default day.
# query_text_comparison:      Returns 4 dataframes:
#                               [1] Results from 1st query
#                               [2] Results from 2nd query
#                               [3] Overlapping results from both queries
#                               [4] Dataframe comparing both queries based on word.list input.
# query_graph_comparison:     Prints & returns a plotly line plot of tweet counts over time between two queries.
#   prep_for_plot:            Sub-function for query_graph_comparison
#   mark_events:              Sub-function for query_graph_comparison comparison.

#'set_parameters 
#' @summary Formats input parameters for query_twitter and query_twitter_count functions.
#'
#' @param query.text    max length 1024 characters
#' @param start.date    'YYYY-MM-DD' OR 'YYYY-MM-DDThh:mm:ssZ' 
#' @param end.date      'YYYY-MM-DD' OR 'YYYY-MM-DDThh:mm:ssZ' 
#' @param max.results   Results per page, 1-100.
#' @param tweet.fields  String list of fields.  'id,text' do not need to be included.
#' @param user.fields   Expanded from author_id.  'author_id,name,username' do not need to be included.
#' @param expansions    Joins tweet and user results by author_id.
#'
#' @return Formatted list of parameters.
#'
#' @examples
#' set_parameters('karenia_brevis','2022-09-01') Results for 9/1/2022
#' set_parameters('karenia_brevis','2022-09-01','2022-09-02') Results for 9/1/2022 and 9/2/2022
#' set_parameters('karenia_brevis','2022-09-01T12:00:00Z','2022-09-01T16:30:00Z') Results on 9/1/2022 from 12:00 noon-4:30 PM
#' set_parameters('karenia_brevis','2022-09-01',tweet.fields='',expansions='',user.fields='') Required columns only from 9/1/2022
set_parameters<-function(query.text,
                         start.date,
                         end.date,
                         max.results='100',
                         tweet.fields='author_id,created_at,lang',
                         user.fields='location',
                         expansions='author_id'){
  if(nchar(start.date)==10) start.date<-paste(start.date,'T00:00:00Z',sep='')
  if(missing(end.date)) end.date<-substr(start.date,1,10)
  if(nchar(end.date)==10) end.date<-paste(end.date,'T23:59:59Z',sep='')
  
  #print(start.date)
  #print(end.date)
  
  return(list(
    `query` = query.text,
    `max_results` = max.results,
    `start_time` = start.date,
    `end_time` = end.date,
    `tweet.fields` = tweet.fields,
    `user.fields` = user.fields,
    `expansions` = expansions
  ))}





#' #' fix_page_data
#' #' @summary Helper function for tweet_queries.  Some tweet fields may return compound variable types (lists, dataframes, etc.) This function unpacks those compound variables into multiple columns and returns the page data
#' #' @param page.data Results from query iteration within query_twitter function.
#' #'
#' #' @return page.data (usually with more columns)
#' fix_page_data<-function(page.data){
#'   #This function takes page.data from the query_twitter function and re-formats (most) list variables
#'   
#'   #Separate compound list variables
#'   for(j in 1:ncol(page.data)){
#'     if(typeof(page.data[,j])=="list"){
#'       #print(colnames(page.data)[j])
#'       if(colnames(page.data)[j] %in% c("context_annotations","referenced_tweets")){
#'         #cbind(page.data[,j][[1]][1],page.data[,j][[1]][2])
#'       } else {
#'         #print(paste(j,colnames(page.data)[j]),sep=' ')
#'         
#'         #Check if variable includes multiple columns
#'         var.col.count<-ncol(page.data[,j])
#'         if(is.null(var.col.count)) var.col.count<-1
#'         
#'         for(q in 1:var.col.count){
#'           #print(paste(j, q,colnames(page.data[,j][q])),sep=' ')
#'           column.name<-paste(colnames(page.data[j]),'_',colnames(page.data[,j][q]),sep='')
#'           page.data[column.name]<-page.data[,j][q]
#'           #colnames(ncol(page.data))<-colnames(page.data[,j][q])
#'         }
#'       }
#'     }
#'   }
#'   
#'   #Clean up any remaining compound list variables
#'   deletion.vec<-c()
#'   for(j in 1:ncol(page.data)){
#'     #print(paste(j,typeof(page.data[,j]))) 
#'     if(!is.null(ncol(page.data[,j][1]))){
#'       deletion.vec<-append(deletion.vec,j)
#'     }
#'   }
#'   if(!is.null(deletion.vec)) page.data<-page.data[-deletion.vec]
#'   return(page.data)
#' }






#' fix_page_data
#' @summary Helper function for tweet_queries.  Some tweet fields may return compound variable types (lists, dataframes, etc.) This function unpacks those compound variables into multiple columns and returns the page data
#' @param page.data Results from query iteration within query_twitter function.
#'
#' @return page.data (usually with more columns)
fix_page_data<-function(page.data){
  #This function takes page.data from the query_twitter function and re-formats (most) list variables
  
  #Separate compound list variables
  # 1. Checking if it's a dataframe (other lists - keep them as is)
  # 2. Separating the columns out.
  
  for(j in 1:ncol(page.data)){
    if((class(page.data[,j])=="data.frame")){
      #print(colnames(page.data)[j])
      #print(paste(j,colnames(page.data)[j]),sep=' ')
      
      #Check if variable includes multiple columns
      var.col.count<-ncol(page.data[,j])
      if(is.null(var.col.count)) var.col.count<-1
      
      for(q in 1:var.col.count){
        #print(paste(j, q,colnames(page.data[,j][q])),sep=' ')
        column.name<-paste(colnames(page.data[j]),'_',colnames(page.data[,j][q]),sep='')
        page.data[column.name]<-page.data[,j][q]
        #colnames(ncol(page.data))<-colnames(page.data[,j][q])
      }
    }
  }
  
  #Clean up any remaining compound list variables
  ## THOMAS' stuff: I don't think that's really needed.
  ##    It actually removes relevant stuff, including UNWOUND URLs !!!
  
  # deletion.vec<-c()
  # for(j in 1:ncol(page.data)){
  #   #print(paste(j,typeof(page.data[,j]))) 
  #   if(!is.null(ncol(page.data[,j][1]))){
  #     deletion.vec<-append(deletion.vec,j)
  #   }
  # }
  # if(!is.null(deletion.vec)) page.data<-page.data[-deletion.vec]
  return(page.data)
}
























#'query_twitter
#' @summary This function takes a list of tweet query parameters and returns the results as a dataframe.
#'
#' @param params A list of parameters, in format list(`query`=query.text , max_results='100',...) etc.  See set_parameters function.
#' @param iter.limit Maximum pages to request.  Page limit is 100, so maximum return is 100*iter.limit.  '0' For no limit. Default 10.
#'    Note: return per page is rarely 100, 92-97 is typical.  All results should still return.
#'
#' @return A dataframe with all requested tweets within parameters.
#' @dependencies fix_page_data()
#'
#' @examples
#' query_twitter(set_parameters('dogs cats','2022-09-25'),iter.limit=10,safety=TRUE) #9/25 only, stops after 10 pages.
#' query_twitter(set_parameters('dogs cats','2022-09-25','2022-09-30'),iter.limit=0,safety=FALSE) #9/25-9/30, may run forever.
#' 
#'
query_twitter<-function(params,iter.limit=10,safety=FALSE){
  #Initialize
  next.list<-NA
  output.df<-NULL
  if(iter.limit==0) iter.limit<-.Machine$integer.max
  
  #Get tweets
  for (i in 1:iter.limit){
    #print(paste('Requesting page:',i,sep=' '))
    if(!is.na(next.list)){
      params.extended<-append(params,next.list)
    } else {
      params.extended<-params
    }
    
    #Request Tweets
    if(safety){
      response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', 
                            httr::add_headers(.headers=headers), 
                            query = params.extended)
      fas_body <-
        content(
          response,
          as = 'parsed',
          type = 'application/json',
          simplifyDataFrame = TRUE
        )
      
      #Check if request limit was reached
      if(exists("fas_body$title")){
        if(fas_body$title=="Invalid Request"){
          print(paste('Invalid Request received on page ',i,sep=''))
          return(output.df)
        }
        if(fas_body$title=="Too Many Requests"){
          print(paste('Request limit reached on page ',i,sep=''))
          return(output.df)
        }
      }
    } else {
      #This version continues looping until page request is fulfilled.  May stall out if too many requests.
      flag <- 0
      
      while (flag == 0){
        try(response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all',
                                  httr::add_headers(.headers=headers),
                                  query = params.extended))
        fas_body <-
          content(
            response,
            as = 'parsed',
            type = 'application/json',
            simplifyDataFrame = TRUE
          )
        
        if ((length(fas_body$detail) > 0)){
          if (fas_body$detail == "Too Many Requests") {Sys.sleep(runif(1,.5,1.5)); next;}
        }
        
        flag <- 1
      }
    }
    
    #Check if expansions are included & write results to page.data
    # Using left_join (NOT inner_join), just so that we don't lose the tweet in case there's no user info.
    
    if(is.null(fas_body$includes)){
      page.data<-fas_body$data
    } else {
      page.data<-left_join(fas_body$data,fas_body$includes$users,by=c("author_id"="id"))
    }
    
    
    #Adds new page data to output.df, if both have valid results.
    #'NOTE: There was an ISSUE with "NULL" page.data... Evidently sometimes "page.data" can be NULL, be it from a:
    #           * 0-return query, OR
    #           * a query with "next" token, yet leading to 0 tweets in that "next" load?
    # Got handled via "is.null()"
    
    if(!is.null(page.data)){
      #print(dim(page.data))
      #print(colnames(page.data))
      
      page.data<-fix_page_data(page.data)
      
      #print(dim(page.data))
      #print(colnames(page.data))
      
      print(paste('Page ',i,' returned NON-null.',sep=''))
      if(is.null(output.df)){
        output.df <- page.data
      } else{
        output.df <- full_join(output.df,
                               page.data,
                               by=intersect(colnames(output.df),colnames(page.data)))
      }
    } else {
      print(paste('Page ',i,' returned null.',sep=''))
    }
    
    #Check if we have/need a next token
    if(!is.null(fas_body$meta$next_token)){
      next.list <- list(
        #`since_id` = fas_body$meta$newest_id, #Required based on twitter API documentation for v2, unnecessary based on testing.
        `next_token` = fas_body$meta$next_token
      )}else{
        break
      }
  }
  #Finalize
  if (!is.null(output.df))  output.df <- output.df %>% select(text, everything())
  
  return(output.df)
}



#' query_twitter_count
#' @summary Returns the number of tweets matching a query over a specified date range.  Returns count only.
#' 
#' @param params  A list of parameters, in format list(`query`=query.text , max_results='100',...) etc.  See set_parameters function.
#' @param iter.limit Maximum pages to request.   Page size is based on granularity (30 for 'day', 1440 for 'hour, etc.)  Not typically an issue with counts.
#' @param granularity.input granularity of results.  Options: 'day','hour','minute'
#'
#' @return A dataframe with start times, end times, and tweet counts
#' @export
#'
#' @examples
query_twitter_count<-function(params,iter.limit=.Machine$integer.max,granularity.input='day'){
  #Clean params
  params<-params[names(params) %in% c('query','start_time','end_time')]
  params<-append(params,list(`granularity`=granularity.input))
  if(!all(c('query','start_time','end_time','granularity') %in% names(params))) print('WARNING: Required query fields missing, results may not be accurate.')
  
  next.list<-NA
  output.df<-NULL
  
  for (i in 1:iter.limit){
    #print(paste('Requesting page:',i,sep=' '))
    if(!is.na(next.list)){
      params.extended<-append(params,next.list)
    } else {
      params.extended<-params
    }
    
    response <- httr::GET(url = 'https://api.twitter.com/2/tweets/counts/all', httr::add_headers(.headers=headers), query = params.extended)
    fas_body <-
      content(
        response,
        as = 'parsed',
        type = 'application/json',
        simplifyDataFrame = TRUE
      )
    page.data<-fas_body$data
    
    output.df<-rbind(output.df,page.data)
    
    #Check if we have/need a next token
    if(!is.null(fas_body$meta$next_token)){
      next.list <- list(
        #`since_id` = fas_body$meta$newest_id, #Required based on twitter API documentation for v2, unnecessary based on testing.
        `next_token` = fas_body$meta$next_token
      )}else{
        break
      }
    
    Sys.sleep(runif(1,.1,.5)) #
  }
  #print(sum(output.df$tweet_count))
  return(output.df)
}

#' location_filter_tweets
#' This function may not work.  Meant to filter results based on user's "location" field.  "location" is an open text entry field and generally unreliable.
#' Use not recommended.
#' 
#' @param df output dataframe from tweet_queries
#'
#' @return filtered dataframe
location_filter_tweets<-function(df){
  #Takes a dataframe of tweets with the 'location' column (from user.fields extension)
  #Adds a 'relevence' column that returns TRUE if that tweet's user 'location' contains a string in location.list, otherwise FALSE
  location.list<-c('tampa','petersburg','palmetto','bradenton','hillsborough','manatee','sarasota',
                   'siesta key','longboat key','fruitville')
  location.exclusion.list<-c('russia','palmetto state','ohio')
  user.location.list<-tolower(df$location)
  df$relevence<-0
  
  for(i in 1:length(user.location.list)){
    for(j in 1:length(location.list)){
      #print(grepl(location.list[j],user.location.list[i]),fixed=TRUE)
      if(grepl(location.list[j],user.location.list[i])==TRUE){
        df$relevence[i]<-1
        break
      }    
    }
  }
  return(df)
}

combine_annotations<-function(df){
  #Function incomplete, does not run
  df<-df.2
  #Debug Line
  full.list<-NA
  annot<-df$context_annotations
  
  for(i in 1:nrow(df)){
    if(!is.null(annot[[i]])){
      test<-as.data.frame(cbind(domain_id = as.data.frame(annot[[i]][[1]])$id,
                                domain_name = as.data.frame(annot[[i]][[1]])$name,
                                entity_id = as.data.frame(annot[[i]][[2]])$id,
                                entity_name = as.data.frame(annot[[i]][[2]])$name,
                                entity_desc = as.data.frame(annot[[i]][[2]])$description
      ))
      full.list<-rbind(full.list,test)
    }
  }
  full.list<-na.omit(full.list)
  return(full.list)
}

#' query_text_comparison
#'
#' @param start.date    'YYYY-MM-DD' OR 'YYYY-MM-DDThh:mm:ssZ' 
#' @param end.date      'YYYY-MM-DD' OR 'YYYY-MM-DDThh:mm:ssZ' 
#' @param query.text.1  max length 1024 characters
#' @param query.text.2  max length 1024 characters
#' @param topic_1       String describing 1st query (optional)
#' @param topic_2       String describing 2nd query (optional)
#' @param term.list     List of terms to check for in each query.  Not case sensitive.
#'
#' @return 
#' @export
#'
#' @examples
query_text_comparison<-function(start.date,end.date=start.date,query.text.1,query.text.2,term.list,topic_1,topic_2){
  #Default topic name if none applied
  if(missing(topic_1)) topic_1='Query_1'
  if(missing(topic_2)) topic_2='Query_2'
  df.1<-NULL
  df.2<-NULL
  df.both<-NULL
  
  #Get tweets from query 1, if any.
  params.1 <- set_parameters(query.text.1,start.date,end.date)
  df.1.counts<-query_twitter_count(params.1)
  if(sum(df.1.counts$tweet_count)>0){
    df.1<-query_twitter(params.1,0)}
  
  Sys.sleep(runif(1,.1,1)) #Delay to prevent too many queries in short timeframe
  
  #Get tweets from query 2, if any
  params.2 <- set_parameters(query.text.2,start.date,end.date)
  df.2.counts<-query_twitter_count(params.2)
  if(sum(df.2.counts$tweet_count)>0){
    df.2<-query_twitter(params.2,0)}
  
  #Make sure one of the tweets had results.
  if(sum(df.1.counts$tweet_count)==0 & sum(df.2.counts$tweet_count)==0){
    print('Neither query returned any results.  Please check parameters and try again.')
    return(list(NA,NA,NA,NA))
    
  }
  
  #Check if results returned from both.  This is to prevent divide by zero errors later.
  if(sum(df.1.counts$tweet_count)==0){
    print(paste(topic_1,' returned 0 results.',sep=''))
    df.1.counts<-df.2.counts
    df.1.counts$tweet_count<-0}
  if(sum(df.2.counts$tweet_count)==0){
    print(paste(topic_2,' returned 0 results.',sep=''))
    df.2.counts<-df.1.counts
    df.2.counts$tweet_count<-0}
  
  #Move text to first column for ease of viewing
  if(!is.null(df.1)) df.1 <- df.1 %>% select(text, everything())
  if(!is.null(df.2)) df.2 <- df.2 %>% select(text, everything())
  
  if(!is.null(df.1)&!is.null(df.2)) df.both<-subset(df.1,df.1$id %in% df.2$id)
  #Set up df.display
  df.display <- data.frame(matrix(ncol=4,nrow=0))
  
  colnames(df.display) <- c('Term',topic_1,'Both',topic_2)
  df.display <- rbind(df.display,c('Total Tweets',
                                   length(setdiff(df.1$id,df.2$id)),
                                   length(intersect(df.1$id,df.2$id)),
                                   length(setdiff(df.2$id,df.1$id))))
  
  #Return query results only if no term list supplied.
  if(missing(term.list)){
    colnames(df.display)<-c('Term','Query1','Both','Query2')
    df.display<-subset(df.display,Query1>0|Query2>0)
    df.list<-list(df.1,df.2,df.both,df.display)
    return(df.list)
  }
  term.list<-tolower(term.list)
  
  #Check for presence of keywords in term.list, store in df.display as proportion.
  for(i in 1:length(term.list)){
    if(!is.null(df.1)){
      df.1[term.list[i]]<-grepl(term.list[i],tolower(df.1$text))
      str.1 <- round(sum(df.1[term.list[i]])/nrow(df.1),4)
    } else {
      str.1 <- NA
    }
    if(!is.null(df.both)){
      df.both[term.list[i]]<-grepl(term.list[i],tolower(df.both$text))
      str.2 <- round(sum(df.both[term.list[i]])/nrow(df.both),4)
    } else {
      str.2 <- NA
    }
    if(!is.null(df.2)){
      df.2[term.list[i]]<-grepl(term.list[i],tolower(df.2$text))
      str.3 <- round(sum(df.2[term.list[i]])/nrow(df.2),4)
    } else {
      str.3 <- NA
    }
    df.display <- rbind(df.display,c(term.list[i],str.1,str.2,str.3))
  }
  
  #Clean up irrelevent rows from df.display (where 0 tweets contain keyword.)
  colnames(df.display)<-c('Term','Query1','Both','Query2')
  df.display<-subset(df.display,Query1>0|Query2>0)
  
  #Create output DFs
  df.list<-list(df.1,df.2,df.both,df.display)
  return(df.list)
}

query_graph_comparison<-function(start.date,end.date,query.text.1,query.text.2,topic_1,topic_2,events){
  
  if(nchar(start.date)==10) start.date<-paste(start.date,'T00:00:00Z',sep='')
  if(missing(end.date) | start.date==end.date) end.date<-substr(start.date,1,10)
  if(nchar(end.date)==10) end.date<-paste(end.date,'T23:59:59Z',sep='')
  
  if(missing(topic_1)) topic_1='Query_1'
  if(missing(topic_2)) topic_2='Query_2'
  
  if(missing(query.text.2)){
    topic_2 <- topic_1
    query.text.2 <- query.text.1
  }
  
  #Get counts for each query
  df.1.counts<-query_twitter_count(set_parameters(query.text.1,start.date,end.date))
  df.2.counts<-query_twitter_count(set_parameters(query.text.2,start.date,end.date))
  
  #If no results for one query, return only the remaining one.  Error message if both null.
  if(is.null(df.1.counts) & !is.null(df.2.counts)){
    print(paste(topic_1,' did not return any results, returning ',topic_2,' only.',sep=''))
    df.1.counts <- df.2.counts
    topic_1 <- topic_2
  } else if (!is.null(df.1.counts) & is.null(df.2.counts)){
    print(paste(topic_2,' did not return any results, returning ',topic_1,' only.',sep=''))
    df.2.counts <- df.1.counts
    topic_2 <- topic_1
  } else if (is.null(df.1.counts) & is.null(df.2.counts)) {
    print('Neither query returned any tweets.  Please check parameters.')
    return(NA)
  }
  
  #Combine count results & prep for plot
  df.1.counts<-prep_for_plot(df.1.counts)
  df.2.counts<-prep_for_plot(df.2.counts)
  df.2.counts$tweet_count_2<-df.2.counts$tweet_count
  df.counts<-cbind(df.1.counts,df.2.counts$tweet_count_2)
  colnames(df.counts)<-c('date','tweet_count','disp_date','tweet_count_2')
  
  #Plot tweet counts
  fig<-plot_ly(data=df.counts) %>%
    add_trace(x = ~date, y = ~tweet_count, type='scatter',mode='lines+markers',name=topic_1) %>%
    add_trace(x = ~date, y = ~tweet_count_2, type='scatter',mode='lines+markers',name=topic_2) %>%
    layout(title='Tweets over Time',
           xaxis = list(title='Date',
                        ticks='inside'),
           yaxis = list(title='Number of Tweets'),
           bargap=0)
  
  #Add event markers, if event DF present
  if(!missing(events)) fig<-mark_events(fig,df.counts,events)
  
  #Display plot & Return
  fig
  return(fig)
}

#' prep_for_plot
#'  Helper function for query_graph_comparison
#'
#' @param df.counts -Output from query_twitter_counts function.
#'
#' @return Cleaned up dataframe
#' @export
#'
#' @examples
prep_for_plot<-function(df.counts){
  df.counts<-df.counts %>% 
    mutate(date = as.Date(substr(start,1,10))) %>%
    mutate(month = format(date,"%b")) %>%
    mutate(day = format(date,"%d")) %>%
    mutate(disp_date = ifelse(day %in% c('01','07','14','21'), paste(month,day,sep=' '),'')) %>%
    select(date,tweet_count,disp_date)
  df.counts <- arrange(df.counts,date)
  return(df.counts)
}

#' mark_events
#' Helper function for query_graph_comparison.  Marks when events took place using transparent vertical bars.
#' 
#'
#' @param fig       Plotly plot from query_graph_comparison
#' @param df.counts Output from query_twitter_counts
#' @param events    Data Frame of events, in format Col1: Name_of_Event (no spaces), Col2: start date, Col3: end date.  Same date format as function.
#'
#' @return          Plotly plot with events overlayed
#'
#' @examples
mark_events<-function(fig,df.counts,events){
  col.adjust<-ncol(df.counts)
  for(i in 1:nrow(events)){
    df.counts<-df.counts %>% mutate(event = ifelse(between(date,as.Date(events[i,2]),as.Date(events[i,3])),
                                                   max(max(df.counts$tweet_count)*1.05,max(df.counts$tweet_count_2)*1.05),0))
    colnames(df.counts)[i+col.adjust]<-events[i,1]
    
    X<-df.counts$date
    Y<-df.counts[,i+col.adjust]
    NAME<-events[i,1]
    fig<-fig %>% add_trace(
      x= X,
      y= Y,
      type='bar',
      opacity = .25,
      name = NAME) %>%
      layout(bargap=0)
  }
  
  return(fig)
}

#Notes regarding extraction from context_annotations field
#for(i in 1:nrow(df)){
# i<-1
# if(!is.null(df$context_annotations[[i]])){
#   df$context_annotations[[i]]<-
#     as.data.frame(cbind(domain_id = as.data.frame(annot[[i]][[1]])$id,
#                         domain_name = as.data.frame(annot[[i]][[1]])$name,
#                         entity_id = as.data.frame(annot[[i]][[2]])$id,
#                         entity_name = as.data.frame(annot[[i]][[2]])$name,
#                         entity_desc = as.data.frame(annot[[i]][[2]])$description
#     ))
# }
# #}
# df
# ```
# 
# ```{r}
# #annotation extraction
# test<-as.data.frame(cbind(domain_id = page.data$context_annotations[[1]]$domain$id,
#                           domain_name = page.data$context_annotations[[1]]$domain$name,
#                           domain_desc = page.data$context_annotations[[1]]$domain$description,
#                           entity_id = page.data$context_annotations[[1]][2]$entity$id,
#                           entity_name = page.data$context_annotations[[1]][2]$entity$name,
#                           entity_desc = page.data$context_annotations[[1]][2]$entity$description
# ))
# ```



##########
## This function checks for tweets that contain certain "bad.terms"
## (terms that usually indicate concepts not related either to the phenomena of red tide, 
##  or the metro area of interest)
## and further processes them to see if there are any other parts of the tweet
## that are pertinent to our search.
##
## For example:
##   A lot of tweets that contain "Red Tide Rick" are matched, but they mostly talk about 
##  politics rather than the actual red tide (e.g. "Don't vote for Red Tide Rick! Go Blue!")
##  But, at the same time, if tweet has "Red Tide Rick" reference, it can still have
## a part of it that references the actual red tide, e.g.
##  "Red Tide Rick couldn't do a thing about red tide, in fact he contributed to it by not doing X-Y-X blah blah blah"
##
##  This function allows us to see that second type of tweets as pertinent, and not simply 
##  dispose of every single tweet mentioning "Red Tide Rick".
#########

postprocess.badterm.cleanup <- function(tweets, bad.terms, grep.terms=NULL, agrep.terms=NULL){
  all.terms <- c(grep.terms, agrep.terms)
  ind <- sapply(tweets, function(x) any(sapply(bad.terms, function(y) grepl(y,x,ignore.case = T)))) 
  #| sapply(result.df$tweet_urls, function(x) any(sapply(bad.terms, function(y) grepl(y,x,ignore.case = T))))
  actual.inds <- which(ind)
  
  if (length(actual.inds) == 0) return(NULL)
  
  cleaned.up.tweet_full_contents <- tweets
  #  cleaned.up.tweet_urls <- result.df$tweet_urls
  for (bad.term in bad.terms){
    cleaned.up.tweet_full_contents <- gsub(bad.term, "", cleaned.up.tweet_full_contents, ignore.case=T)
    # cleaned.up.tweet_urls <- gsub(bad.term, "", cleaned.up.tweet_urls, ignore.case=T)
  }
  
  if (is.null(agrep.terms)) ind.agrep <- rep(FALSE, length(actual.inds))
  if (!is.null(agrep.terms)){
    ind.agrep <- sapply(cleaned.up.tweet_full_contents[actual.inds], 
                        function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
    #ind.agrep <- ind.agrep | sapply(cleaned.up.tweet_urls[actual.inds], 
    #                                function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
  }
  
  if (is.null(grep.terms)) ind.grep <- rep(FALSE, length(actual.inds))
  if (!is.null(grep.terms)){
    ind.grep <- sapply(cleaned.up.tweet_full_contents[actual.inds], 
                       function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
    # ind.grep <- ind.grep | sapply(cleaned.up.tweet_urls[actual.inds], 
    #                              function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
  }
  
  ind.drop <- !(ind.agrep | ind.grep)
  actual.inds[ind.drop]
  
  return(list(actual.inds=actual.inds,
              ind.drop=ind.drop))
}



## Breaking down an array of potentially HUGE geoboxes into sub-boxes
## according to the 25mi x 25mi requirement 

Mile.requirement.func <- function(bound.box.char){
  
  # To contain the final set of 25mi x 25mi boxes
  full.char.box <- NULL
  
  # Going through each huge box in array
  for (j in 1:length(bound.box.char)){
    
    # Getting the numerical value for its left/rightmost longitudes, top/bottom latitudes
    y <- gsub("bounding_box:[", "", bound.box.char[j], fixed=T); 
    y <- gsub("]", "", y, fixed=T);
    y <- as.numeric(strsplit(y, split = " ")[[1]]);
    
    long_1 <- y[1]; long_2 <- y[3];  lat_1 <- y[2]; lat_2 <- y[4]
    
    ## Figuring out how many 25mi intervals are in the width and height of that huge box
    n.rect.long <- ceiling((long_2 - long_1)*69/25)
    n.rect.lat <- ceiling((lat_2 - lat_1)*69/25)
    
    ## Creating lists of well spaced out left-most and right-most longitudes, and top-most/bottom-most latitudes
    lat_1_list <- lat_1 + c(0:(n.rect.lat - 1))*((lat_2 - lat_1)/n.rect.lat)
    if (n.rect.lat > 1){
      lat_2_list <- c(lat_1 + c(1:(n.rect.lat - 1))*((lat_2 - lat_1)/n.rect.lat), lat_2)
    } else {
      lat_2_list <- lat_2
    }
    
    long_1_list <- long_1 + c(0:(n.rect.long - 1))*((long_2 - long_1)/n.rect.long)
    if (n.rect.long > 1){
      long_2_list <- c(long_1 + c(1:(n.rect.long - 1))*((long_2 - long_1)/n.rect.long), long_2)
    } else {
      long_2_list <- long_2
    }
    
    # Rounding for prettyness
    long_1_list <- round(long_1_list, 6); long_2_list <- round(long_2_list, 6);
    lat_1_list <- round(lat_1_list, 6); lat_2_list <- round(lat_2_list, 6);
    
    
    # Making intermediate char box, for that single HUGE box being broken into the 25mi x 25mi ones
    char.box <- NULL
    count <- 0
    for (i in 1:n.rect.long){
      for (j in 1:n.rect.lat){
        count <- count + 1
        char.box[count] <- paste0("bounding_box:[", 
                                  long_1_list[i], " ", lat_1_list[j], " ", 
                                  long_2_list[i], " ", lat_2_list[j], "]")
      }
    }
    
    full.char.box <- c(full.char.box, 
                       char.box)
  }
  
  return(paste0("(", paste0(full.char.box, collapse=" OR "), ")"))
}

