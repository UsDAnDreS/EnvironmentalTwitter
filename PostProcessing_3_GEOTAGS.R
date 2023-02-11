######
## Creating .csv data set files
##    Simply disposing of the "list"/"data.frame" style variables (retaining only SIMPLE columns),
##    while still keeping those around in the R object ("topics.df.full.sorted.RData")
##
## The "topics.df" object structure is:
##    - It's a LIST of LISTS
##    - First layer corresponds to the 5 ENVIRONMENTAL ISSUES, hence 5 elements (titled "RedTide", "BlueGreen", etc)
##    - Second layer corresponds to the 6 LOCATIONS, hence 6 elements (titled "Tampa", "Pinellas.Clearwater", etc)
######

source("Project_Functions.R")
source("Project_Objects.R")

# Before unwounding the reply urls
# load("topic.dfs.geotag.full.sorted.RData")

# With unwounding the reply urls
load("topic.dfs.geotag.full.sorted.url.unwound.RData")


for (l in 1:length(main.queries)){
  print(names(main.queries)[l])
  # print(sapply(full.df[[l]], function(x) dim(x)))
  print(sum(sapply(full.df[[l]], function(x) ifelse(is.data.frame(x), dim(x)[1], 0))))
}


# Assigning proper names to the all-encompassing "topics.df" object:

names(full.df) <- names(main.queries)
for (l in 1:length(full.df)) names(full.df[[l]]) <- names(area.terms)


# "created_at" has a DOUBLE-TYPE of LENGTH 2 ("POSIXct" "POSIXt")
dir.create("Data")
save(full.df, file="Data/topic.dfs.geotag.full.sorted.url.unwound.RData")


for (l in 1:length(full.df)){
  for (k in 1:length(full.df[[l]])){
    if (!is.character(full.df[[l]][[k]])){
      write.csv(full.df[[l]][[k]][, which(unlist(sapply(full.df[[l]][[k]], function(x) !(class(x)[1] %in% c("data.frame", "list")))))],
                paste0("Data/Geotag_",names(main.queries)[l], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
    }
  }
}

