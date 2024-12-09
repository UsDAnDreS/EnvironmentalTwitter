######
## Appending the most recent data to the already-existing historical database, saving it to R object
##  "Data/ALL.recent.search.dfs.full.sorted.RData", which is
##    - A LIST of LISTS
##    - First layer corresponds to the 2 ENVIRONMENTAL ISSUES, hence 2 elements (titled "RedTide", "Algae")
##    - Second layer corresponds to the 6 LOCATIONS, hence 6 elements (titled "Tampa", "Pinellas.Clearwater", etc)
##
##
## Creating .csv data set files:
##    Simply disposing of the "list"/"data.frame" style variables (retaining only SIMPLE columns),
##    (while still keeping those around in the aforementioned R object)
##
######

source("Project_Functions.R")
source("Project_Objects_RECENT.R")

# Before unwinding the reply urls
load("recent.search.dfs.full.sorted.RData")

# With unwinding the reply urls
# load("recent.search.dfs.full.sorted.url.unwound.RData")


for (l in 1:length(main.queries)){
  print(names(main.queries)[l])
  print(sapply(full.df[[l]], function(x) dim(x)))
  print(sum(sapply(full.df[[l]], function(x) dim(x)[1])))
}


# Assigning proper names to the all-encompassing "topics.df" object:

names(full.df) <- names(main.queries)
for (l in 1:length(full.df)) names(full.df[[l]]) <- names(area.terms)

#####
## Adding to the existing recent search data
#####

# Initial run
# dir.create("Data")
# all.full.dfs <- full.df
# save(all.full.dfs, file = "Data/ALL.recent.search.dfs.full.sorted.RData")

# Subsequent runs

## The warning is fine, it's just due to double-designation for "Posix" date/time variable

load("Data/ALL.recent.search.dfs.full.sorted.RData")
for (l in 1:length(all.full.dfs)){
  for (j in 1:length(all.full.dfs[[l]])){

    if (nrow(all.full.dfs[[l]][[j]]) != 0){
      if (nrow(full.df[[l]][[j]]) != 0){
      all.full.dfs[[l]][[j]] <- bind_rows(fix_page_data(full.df[[l]][[j]]),
                                          fix_page_data(all.full.dfs[[l]][[j]]))
      } 
    } else {
      all.full.dfs[[l]][[j]] <- fix_page_data(full.df[[l]][[j]])
    }

    
    
    if (nrow(all.full.dfs[[l]][[j]]) != 0){
      all.full.dfs[[l]][[j]] <- all.full.dfs[[l]][[j]][!duplicated(all.full.dfs[[l]][[j]]$id), ]
      all.full.dfs[[l]][[j]] <- all.full.dfs[[l]][[j]] %>% arrange(desc(created_at.x))
    }
  }
}

lapply(full.df[[1]], dim)
lapply(full.df[[2]], dim)

lapply(all.full.dfs[[1]], dim)
lapply(all.full.dfs[[2]], dim)

# View(all.full.dfs[[1]][[1]])

save(all.full.dfs, file = "Data/ALL.recent.search.dfs.full.sorted.RData")




# "created_at" has a DOUBLE-TYPE of LENGTH 2 ("POSIXct" "POSIXt")

# save(full.df, file="Data/topic.dfs.full.sorted.url.unwound.RData")


for (l in 1:length(all.full.dfs)){
  for (k in 1:length(all.full.dfs[[l]])){
    if (!is.character(all.full.dfs[[l]][[k]])){
      write.csv(all.full.dfs[[l]][[k]][, which(unlist(sapply(all.full.dfs[[l]][[k]], function(x) !(class(x)[1] %in% c("data.frame", "list")))))],
                paste0("Data/ALL_RECENT_SEARCH_",names(main.queries)[l], "_", names(area.terms)[k], "_", "all_SIMPLE_columns.csv"))
    }
  }
}

# View(full.df[[1]][[1]][, which(unlist(sapply(full.df[[1]][[1]], function(x) !(class(x)[1] %in% c("data.frame", "list")))))])

