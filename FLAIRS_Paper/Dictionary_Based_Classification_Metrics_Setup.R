dict.label <- read.csv(file = "FINALIZED_DICTIONARY_LABELING_APPROACH.csv")
FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED <- read.csv("FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED.csv")

# Doing random 50-50 pick for all the double predictions
set.seed(1)
double.label.length <- length(dict.label$max.class[str_detect(dict.label$max.class, ",")])
dict.label$single.label <- dict.label$max.class
# dict.label$single.label[str_detect(dict.label$single.label, ",")] <- str_split(dict.label$single.label[str_detect(dict.label$single.label, ",")], ", ")[sample(1:2, double.label.length, replace=T)]

dict.label$single.label[str_detect(dict.label$single.label, ",")] <- 
sapply(str_split(dict.label$single.label[str_detect(dict.label$single.label, ",")], ", "),
       function(x) x[[sample(1:2, 1)]])

FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS <- 
  FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED %>%
  select(username, hand.label_simplified) %>%
  left_join(dict.label)

head(FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS)
dim(FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS)

#FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS <-
#  FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS %>%
# filter(!(str_detect(max.class, ",") | str_detect(hand.label_simplified, "-") | str_detect(hand.label_simplified, "bot")))
dim(FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS)

table(FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS %>%
        select(single.label, hand.label_simplified))

write.csv(FINALIZED_Training_Data_ALL_Available_Descriptions_WITH_DICT_LABELS,
          file= "FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED_w_DICT_LABELS.csv",
          row.names=F)
