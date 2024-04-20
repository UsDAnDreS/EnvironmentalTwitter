# NOTE: "RESEARCH" - NOT A GREAT WORD FOR THE "RESEARCH/ACADEM" CATEGORY!!!
#  Need to add "JournalisM" to the media dictionary
#  assistant prof, ast prof, asst prof, associate prof, assoc prof - to ACAD
# geoscientist, oceanographer
# !! vacation rentals, beachside, beachfront, gulffront, waterfront, fishery, fisheries !! for tourbiz
# remove WRITER from MEDIA

source("Project_Functions.R")
source("Project_Objects.R")
source("Locations.R")

library(tokenizers)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textstem)

## If one needs to do lemmatization first (takes time)
# all.users.df <- read.csv("userbio.csv")
# lem.all.users.df <- all.users.df %>%
#   mutate(descr.lemmatized = lemmatize_strings(description))
# 
# write.csv(file="lemmatized_userbio.csv", lem.all.users.df)

lem.all.users.df <- read.csv("lemmatized_userbio.csv")

our.stopwords <- unique(intersect(intersect(stop_words$word[stop_words$lexicon == "SMART"],
                                            stop_words$word[stop_words$lexicon == "onix"]),
                                  stop_words$word[stop_words$lexicon == "snowball"]))

our.stopwords <- c(our.stopwords, "amp")


tokenized.descr <- tokenize_words(lem.all.users.df$description,
               stopwords = our.stopwords,
               strip_punct = TRUE)


media_list2 <- unique(lemmatize_strings(c("news", "journalist", "journalism",
                                          "reporter", "editor", #"writer",
               "radio", "tv", "television", "magazine", "newspaper",
               "press", "media", "correspondent", "columnist", "anchor", "host",
               "producer", "reporting", "report", "coverage", "meteorologist", "newscasts", "journo")))
gov_list2 <- unique(lemmatize_strings( c("government", "politician", "senator", "congressman", "commissioner", "mayor")))
acad_list2 <- unique(lemmatize_strings( c("professor", "research", "researcher", "scientist", "adjunct", "prof")))
other_list2 <- unique(lemmatize_strings(c("retired", "former", "ex")))
tourbiz_list2 <- unique(lemmatize_strings(c("tourism", "tourist", "restaurant", "hotel", "hospitality", "fisherman", "angler",
                                            "waterfront", "beachside", "beachfront", "gulffront", "fishery", "fisheries", "rentals")))

#vacation rentals, beachside, beachfront, gulffront, waterfront, fishery, fisheries

lem.all.users.df$media_matches <- sapply(tokenized.descr, 
                                         function(x) sum(x %in% media_list2))
lem.all.users.df$gov_matches <- sapply(tokenized.descr, 
                                         function(x) sum(x %in% gov_list2))
lem.all.users.df$acad_matches <- sapply(tokenized.descr, 
                                       function(x) sum(x %in% acad_list2))
lem.all.users.df$tourbiz_matches <- sapply(tokenized.descr, 
                                       function(x) sum(x %in% tourbiz_list2))

View(lem.all.users.df %>%
       filter(media_matches > 0))

lem.all.users.df$max.class <- 
apply(lem.all.users.df[,  str_detect(colnames(lem.all.users.df), "matches")],
      1,
      function(x){
        if (max(x) > 0){
          paste(str_remove_all(colnames(lem.all.users.df)[str_detect(colnames(lem.all.users.df), "matches")][x == max(x)], fixed("_matches")), collapse=", ")
        } else {
          "other"
        }
      }
)

lem.all.users.df$other.term <- sapply(tokenized.descr, 
                                      function(x) sum(x %in% other_list2))

## How many of each match
c(sapply(lem.all.users.df[,  str_detect(colnames(lem.all.users.df), "matches")],
         sum),
  other=sum(apply(lem.all.users.df[,  str_detect(colnames(lem.all.users.df), "matches")],
        1,
        function(x) all(x == 0))))

table(lem.all.users.df$max.class)

write.csv(lem.all.users.df %>% select(username, description, max.class),
          file = "FINALIZED_DICTIONARY_LABELING_APPROACH.csv",
          row.names = F)


####
## Picking 8,000 accounts, randomly
####

set.seed(1)

n <- nrow(lem.all.users.df)
n

our.samp.ind <- sample(n, size=8000)
our.samp.ind

samp.lem.all.users.df <- lem.all.users.df[our.samp.ind,]

c(sapply(samp.lem.all.users.df[,  str_detect(colnames(lem.all.users.df), "matches")],
         sum),
  other=sum(apply(samp.lem.all.users.df[,  str_detect(colnames(lem.all.users.df), "matches")],
                  1,
                  function(x) all(x == 0))))


colnames(lem.all.users.df[c(1:n)[-our.samp.ind], ])

# save(lem.all.users.df[c(1:n)[-our.samp.ind], ])

write.csv(file="UNLABELED_accounts.csv",
          row.names = F,
          lem.all.users.df[c(1:n)[-our.samp.ind], ] %>% select(username, description))
          


## Arrange them by:
##    1. media_matches
##    2. acad_matches
##    3. gov_matches
##    4. tourbiz_matches

table(samp.lem.all.users.df$max.class)

# View(samp.lem.all.users.df %>%
#   mutate(comma.count = str_detect(max.class, fixed(","))) %>%
#   arrange(comma.count, max.class) %>%
#   select(-comma.count))

# write.csv(file="ordered_sampled.accounts_short.csv",
#           samp.lem.all.users.df %>%
#             mutate(hand.label = max.class, expert.ind = "no", comma.count = str_detect(max.class, fixed(","))) %>%
#             arrange(comma.count, max.class, desc(verified)) %>%
#             select(-comma.count) %>%
#             select(hand.label, expert.ind, max.class, verified, username, description))


View(samp.lem.all.users.df %>%
  mutate(hand.label = max.class, expert.ind = "no", comma.count = str_detect(max.class, fixed(","))) %>%
  arrange(comma.count, max.class, desc(verified)) %>%
  select(-comma.count) %>%
  select(hand.label, expert.ind, max.class, verified, username, description))




# hey <- read.csv("ordered_sampled.accounts_short.csv")



# 
# table(hey$hand.label)
# 
# View(hey %>%
#   filter(str_detect(hand.label, "na") | !expert.ind %in% c("yes", "no")))
#            #str_detect(expert.ind, "maybe")))
# 
# write.csv(file="unclear_ordered_sampled.accounts_short.csv",
#           hey %>%
#             filter(str_detect(hand.label, "na") | !expert.ind %in% c("yes", "no")))
#                      #str_detect(expert.ind, "maybe")))




#######
## FROM THE REMAINING 20+K ACCOUNTS
##    * Pick the ones MATCHING WITH AT LEAST ONE VOCABULARY
##    * HAND-LABEL THOSE
#######

rest.samp.lem.all.users.df <- lem.all.users.df[-our.samp.ind,]
dim(rest.samp.lem.all.users.df)

# dim(rest.samp.lem.all.users.df %>% filter(max.class != "other"))

# write.csv(file="ordered_BIASED_accounts.csv",
#           rest.samp.lem.all.users.df %>%
#             filter(max.class != "other") %>%
#             mutate(hand.label = max.class, expert.ind = "no", comma.count = str_detect(max.class, fixed(","))) %>%
#             arrange(comma.count, max.class, desc(verified)) %>%
#             select(-comma.count) %>%
#             select(hand.label, expert.ind, max.class, verified, username, description))

library(tidyverse)


# hey <- read.csv("ordered_sampled.accounts_short.csv")
# 
# table(hey$hand.label)
# 
# ## Putting together the finalized 8K accounts, with simplified categories
# hey$hand.label_simplified <- hey$hand.label
# hey$hand.label_simplified[hey$hand.label %in% c("tourbiz-agg", "tourbiz-media", "tourbiz-real", "real")] <- "tourbiz"
# hey$hand.label_simplified[hey$hand.label %in% c("acad-gov", "acad-media", "acad-tourbiz")] <- "acad"
# hey$hand.label_simplified[hey$hand.label %in% c("media-acad", "media-gov", "media-tourbiz")] <- "media"
# hey$hand.label_simplified[hey$hand.label %in% c("gov-acad", "gov-media", "gov-tourbiz")] <- "gov"
# hey$hand.label_simplified[hey$hand.label %in% c("other-agg")] <- "media"
# hey$hand.label_simplified[hey$hand.label %in% c("other-agg-bot", "bot")] <- "bot"
# 
# View(hey)
# write.csv(file="finalized_8K_accounts.csv",
#           hey,
#           row.names = FALSE)

#table(hey$hand.label_simplified)


hey <- read.csv("ordered_BIASED_accounts.csv")

# tourbiz: 42;
# media: 876
# gov: 33
# acad: 213

# table(hey$hand.label)

View(hey %>%
  filter(str_detect(hand.label, "na") | !expert.ind %in% c("yes", "no")))
           #str_detect(expert.ind, "maybe")))

write.csv(file="unclear_ordered_BIASED_accounts.csv",
          row.names = F,
          hey %>%
            filter(str_detect(hand.label, "na") | !expert.ind %in% c("yes", "no")))
                     #str_detect(expert.ind, "maybe")))



View(hey %>% 
       filter(#max.class == "media", #verified == FALSE, 
             # X > 1300, 
        str_detect(hand.label, "acad-na"),
            #  expert.ind != "yes",
         #!expert.ind %in% c("yes","no"),
             #str_detect(hand.label, "media") & !str_detect(max.class, "media"),
            # !str_detect(hand.label, "media") & max.class != "media",
              # str_detect(tolower(description), "assignment")
              #str_detect(tolower(description), "court rep")
              
         #!str_detect(hand.label, "media"),
          #   str_detect(tolower(description), "produc")
             # str_detect(tolower(description), "documentar") | str_detect(tolower(description), "docuserie")
            
           #  !str_detect(tolower(description), "meteo"),
           #  !str_detect(tolower(description), "enviro"),
           #   !str_detect(tolower(description), "climat"),
           # 
           #  !str_detect(tolower(description), "microbio"),
           #  !str_detect(tolower(description), "marine"),
           #   !str_detect(tolower(description), "ecolog"),
           #   !str_detect(tolower(description), "biolog"),
           # !str_detect(tolower(description), "geolog"),
           #  
           #  !str_detect(tolower(description), "water"),
           #  !str_detect(tolower(description), "ocean"),
           #  !str_detect(tolower(description), "aquat"),
           # !str_detect(tolower(description), "natural"),
           #  
           #   str_detect(tolower(description), "weather")
              ))
#%>%
     #  select(-username))
