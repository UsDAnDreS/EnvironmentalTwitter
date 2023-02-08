########
## Most critical reused objects
########

source("Project_Functions.R")


######
## Main search queries
######

main.queries <- list(
  RedTide = '("red tide" OR "red tides" OR "red algae" OR #redtide OR "karenia brevis" OR kbrevis OR #kareniabrevis)',
  BlueGreen = '("blue green algae" OR "blue green algal" OR #bluegreenalgae OR "toxic green algae" OR cyanobacteria OR cyanotoxins OR Lyngbya OR Lyngbia OR Lingbia OR Lingbya OR Lynbia OR Lynbya OR Linbia OR Linbya OR Dapis)',
  OilSpill = '((oil OR crude OR petroleum OR tar_ball OR tar_balls) (leak OR leaks OR leaked OR leaking OR leakage OR spill OR spills OR spilled OR spilling OR spillage) (ocean OR beach OR beaches OR bay OR gulf OR sea OR lake OR river OR creek OR waterway))',
  SewageSpill = '((sewer OR sewers OR sewage OR septic OR stormwater OR storm_water) (untreated OR raw OR overflow OR discharge OR pump OR pumps OR pumping OR pumped OR leak OR leaks OR leaked OR leaking OR leakage OR spill OR spills OR spilled OR spilling OR spillage OR dump OR dumps OR dumped OR dumping) (ocean OR beach OR beaches OR bay OR gulf OR sea OR lake OR river OR creek OR waterway))',
  IndustrialSpill = '((wastewater OR contaminants OR contamination OR contaminating OR chemical OR chemicals) (leak OR leaks OR leaked OR leaking OR leakage OR spill OR spills OR spilled OR spilling OR spillage) (ocean OR beach OR beaches OR bay OR gulf OR sea OR lake OR river OR creek OR waterway))',
  AllAlgae = '((algae OR algal OR #algaebloom OR #algalbloom OR #toxicalgae OR #harmfulalgae) OR ("red tide" OR "red tides" OR #redtide OR "karenia brevis" OR kbrevis OR #kareniabrevis) OR (#bluegreenalgae OR cyanobacteria OR cyanotoxins OR Lyngbya ORLyngbia OR Lingbia OR Lingbya OR Lynbia OR Lynbya OR Linbia OR Linbya OR Dapis))',
  HarmfulAlgaeBlooms = '(((toxic OR toxins OR harmful OR bloom OR blooms OR blooming) (algae OR algal) OR #algaebloom OR #algalbloom) OR (\"blue green algae\" OR \"blue green algal\" OR #bluegreenalgae OR \"toxic green algae\" OR cyanobacteria OR cyanotoxins OR Lyngbya OR Lyngbia OR Lingbia OR Lingbya OR Lynbia OR Lynbya OR Linbia OR Linbya OR Dapis) OR (\"red algae\" OR \"red tides\" OR \"red tide\" OR #redtide OR \"karenia brevis\" OR kbrevis OR #kareniabrevis))'
)



#####
## Geographical reference terms, by area
#####

Tampa.query.chunk <- '(Tampa OR Tampas OR #TampaBay OR "TB area" OR ((Hillsborough OR HillsboroughCounty) (FL OR Florida))
OR "Apollo Beach" OR #ApolloBeach OR Wimauma 
OR ((Gibsonton OR Ruskin OR "Sun City") (FL OR Florida)) OR "Hillsborough Bay"
OR "Davis Islands" OR "Alafia River" OR "McKay Bay" 
OR "lake thonotosassa")'
nchar(Tampa.query.chunk)


Pinellas.Clearwater.query.chunk <- '(Pinellas OR PinellasCounty OR ((Clearwater OR Dunedin) (FL OR Florida)) OR "Clearwater Beach" OR #ClearwaterBeach
OR "Indian Rocks Beach" OR #IndianRocksBeach OR "Tarpon Springs" OR #TarponSprings OR Belleair OR "Palm Harbor" OR #PalmHarbor OR "Safety Harbor" OR #SafetyHarbor
OR "Honeymoon Island" OR "Sand Key" 
OR Caladesi OR "Lake Tarpon")'
nchar(Pinellas.Clearwater.query.chunk)


# -\"Redington Beach\" -\"Redington Shores\" => "Redington" could just be added to Treasure Island/Tierra Verde in "FL OR Florda" chunk
# Tierra Verde could be dropped if need be for space. Not super-popular, but included just in case.
StPete.query.chunk <- '(StPetersburg OR "St Petersburg" OR "St Pete" OR StPete OR #StPeteBeach
OR "Madeira Beach" OR #MadeiraBeach OR (("Treasure Island" OR "Tierra Verde") (FL OR Florida)) OR "Sunshine Skyway"
OR "Fort De Soto" OR "Fort DeSoto" OR "Redington Beach" OR "Redington Shores" OR "Pass a grille" OR "Boca Ciega Bay"
OR "Egmont Key" OR "Weedon Island")'
nchar(StPete.query.chunk)



# The last few - Terra Ceia, Palma Sola Bay, Bishop Harbor - have been really low mentions. 
# Could easily drop the off for space if need be.
Manatee.query.chunk <- '("Manatee county" OR "Manatee counties" OR ManateeCounty OR Bradenton OR Bradentons OR #BradentonBeach
OR "Anna Maria Island" OR #AnnaMariaIsland OR "Longboat Key" OR #LongboatKey OR "Holmes Beach" OR #HolmesBeach
OR "Manatee River" OR #ManateeRiver OR "Port Manatee" OR ("Coquina Beach" -NC -Carolina)
OR ("Terra Ceia" (FL OR Florida)) 
OR "Palma Sola Bay" OR "Bishop Harbor" 
OR "lake manatee")'
nchar(Manatee.query.chunk)



# #ManasotaKey, #CaseyKey, #LemonBay hashes are not that popular, could drop for space.
# Watch for 'Sarasota Herald-Tribune' in post-processing: seemingly the only reference to the area.
Sarasota.query.chunk <- '(Sarasota OR Sarasotas OR SarasotaCounty
OR "Siesta Key Beach" OR #SiestaKeyBeach
OR ((Venice OR Englewood OR "North Port" OR #NorthPort OR "Lido Beach") (FL OR Florida)) OR "Casey Key" OR #CaseyKey OR Nokomis OR "Lemon Bay" OR #LemonBay OR "St Armands" OR #StArmands
OR "Manasota Key" OR #ManasotaKey OR "Manasota Beach" OR "Englewood Beach" OR "Lido Key"
OR "Caspersen Beach" OR "Stump Pass" OR #SarasotaBay)'
nchar(Sarasota.query.chunk)


# That last line of places (Werner-Boys, Three Rooker, Beacon) - very low mentions.. could easily drop if space is needed.
# Elfers & Shady Hills are both pretty low on mentions - could also drop.
# Pithlachascotee is really low mentions, but at least it's a water body (river).. could still drop it though.
Pasco.query.chunk <- '(((Pasco (county OR counties OR Florida OR FL)) OR PascoCounty 
OR "Port Richey" OR #PortRichey
OR (("Bayonet Point" OR Anclote OR Elfers OR "Shady Hills") (FL OR Florida))
OR "Cotee River" OR Pithlachascotee OR "Jasmine Estates" OR "Key Vista" OR Aripeka 
OR "Werner Boyce" OR "Three Rooker Island" OR "Beacon Square")
-@Lou_Port_Richey)'
nchar(Pasco.query.chunk)


area.terms <- list(
  Tampa = Tampa.query.chunk,
  Pinellas.Clearwater = Pinellas.Clearwater.query.chunk,
  Pinellas.StPete = StPete.query.chunk,
  Manatee = Manatee.query.chunk,
  Sarasota = Sarasota.query.chunk,
  Pasco = Pasco.query.chunk
)


#####
## !!! LANG:EN - DELETES TWEETS THAT ARE ONLY LINKS or ONlY MEDIA....
## https://twittercommunity.com/t/unkown-language-code-qht-returned-by-api/172819/2
##
## lang:qam 3 for tweets with mentions only (works for tweets since 2022-06-14)
## lang:qct 1 for tweets with cashtags only (works for tweets since 2022-06-14)
## ... etc
## SOLUTION: WILL HAVE TO USE "(x OR y OR z)" format.. at least for ONE of the (main, geographical) query chunks
####



Tampa.query.words <- c("Tampa", "Tampas", "#TampaBay", "TB area",
                       "Hillsborough", "HillsboroughCounty",
                        "Apollo Beach", "#ApolloBeach", "Wimauma",
                        "Gibsonton", "Ruskin", "Sun City",  "Hillsborough Bay",
                        "Davis Islands", "Alafia River", "McKay Bay",
                        "lake thonotosassa")


Pinellas.Clearwater.query.words <- c("Pinellas", "PinellasCounty", "Clearwater", "Dunedin", "Clearwater Beach", "#ClearwaterBeach",
"Indian Rocks Beach",  "#IndianRocksBeach",  "Tarpon Springs",  "#TarponSprings",  "Belleair",  "Palm Harbor",  "#PalmHarbor",  "Safety Harbor",  "#SafetyHarbor",
"Honeymoon Island",  "Sand Key", "Caladesi",  "Lake Tarpon")


# -\"Redington Beach\" -\"Redington Shores\" => "Redington" could just be added to Treasure Island/Tierra Verde in "FL OR Florda" chunk
# Tierra Verde could be dropped if need be for space. Not super-popular, but included just in case.
StPete.query.words <-  c("StPetersburg",  "St Petersburg",  "St Pete",  "StPete",  "#StPeteBeach",
 "Madeira Beach",  "#MadeiraBeach",  "Treasure Island", "Tierra Verde",  "Sunshine Skyway",
 "Fort De Soto",  "Fort DeSoto",  "Redington Beach",  "Redington Shores",  "Pass a grille",  "Boca Ciega Bay",
 "Egmont Key",  "Weedon Island")



# The last few - Terra Ceia, Palma Sola Bay, Bishop Harbor - have been really low mentions. 
# Could easily drop the off for space if need be.
Manatee.query.words <- c("Manatee county",  "Manatee counties",  "ManateeCounty",  "Bradenton",  "Bradentons",  "#BradentonBeach",
 "Anna Maria Island",  "#AnnaMariaIsland",  "Longboat Key",  "#LongboatKey",  "Holmes Beach",  "#HolmesBeach",
 "Manatee River",  "#ManateeRiver",  "Port Manatee",  "Coquina Beach", 
 "Terra Ceia",
 "Palma Sola Bay", "Bishop Harbor",
 "lake manatee")



# #ManasotaKey, #CaseyKey, #LemonBay hashes are not that popular, could drop for space.
# Watch for 'Sarasota Herald Tribune' in post processing: seemingly the only reference to the area.
Sarasota.query.words <-  c("Sarasota",  "Sarasotas",  "SarasotaCounty",
 "Siesta Key Beach",  "#SiestaKeyBeach",
 "Venice", "Englewood", "North Port", "#NorthPort", "Lido Beach", "Casey Key",  "#CaseyKey",  "Nokomis",  "Lemon Bay",  "#LemonBay",  "St Armands",  "#StArmands",
 "Manasota Key",  "#ManasotaKey",  "Manasota Beach",  "Englewood Beach",  "Lido Key",
 "Caspersen Beach",  "Stump Pass",  "#SarasotaBay")


# That last line of places (Werner Boys, Three Rooker, Beacon)   very low mentions.. could easily drop if space is needed.
# Elfers & Shady Hills are both pretty low on mentions   could also drop.
# Pithlachascotee is really low mentions, but at least it's a water body (river).. could still drop it though.
Pasco.query.words <-  c("Pasco county", "Pasco counties", "PascoCounty",
 "Port Richey",  "#PortRichey",
 "Bayonet Point", "Anclote", "Elfers", "Shady Hills",
 "Cotee River",  "Pithlachascotee",  "Jasmine Estates",  "Key Vista",  "Aripeka", 
 "Werner Boyce",  "Three Rooker Island",  "Beacon Square")


area.words <- list(
  Tampa.query.words,
  Pinellas.Clearwater.query.words,
  StPete.query.words,
  Manatee.query.words,
  Sarasota.query.words,
  Pasco.query.words
)



###########
#### GEOTAG chunks
##########


#### Explicit geo-tags via:

# https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/build-a-query#list

#
# bounding_box:
#   
#   Available alias: geo_bounding_box:
#   
#   Matches against the place.geo.coordinates object of the Tweet when present, and in Twitter, against a place geo polygon, where the place polygon is fully contained within the defined region.
# 
# bounding_box:[west_long south_lat east_long north_lat]
# 
# west_long south_lat represent the southwest corner of the bounding box where west_long is the longitude of that point, and south_lat is the latitude.
# east_long north_lat represent the northeast corner of the bounding box, where east_long is the longitude of that point, and north_lat is the latitude.
# 
# Width and height of the bounding box must be less than 25mi
# Longitude is in the range of ±180
# Latitude is in the range of ±90
# All coordinates are in decimal degrees.
# Rule arguments are contained within brackets, space delimited.
# You can only pass a single geo polygons per bounding_box: operator. 
# 
# Note: This operator will not match on Retweets, since Retweet's places are attached to the original Tweet. It will also not match on places attached to the original Tweet of a Quote Tweet.
# 
# 			(BOTTOM-LEFT 	    to 		TOP-RIGHT)
# Example: bounding_box:[-105.301758 39.964069 -105.178505 40.09455]
# 
# 
# PASCO: 
# 	BOTTOM-LEFT: 28.173438, -82.911389
# 	TOP-RIGHT:   28.479579, -82.054455
# 	
#   bounding_box:[-82.911389 28.173438 -82.054455 28.479579]
#

Pasco.query.tag <- Mile.requirement.func("bounding_box:[-82.911389 28.173438 -82.054455 28.479579]")
nchar(Pasco.query.tag)
Pasco.query.tag

#
# PINELLAS (CLEARWATER):
# 	
# 	BOTTOM-LEFT: 27.884506, -82.882183  (TOUCHES GOOD CHUNK OF WHAT'S SUPPOSED TO BE "ST PETE'S TERRITORY" - Feather Sound, Bardmoor, Seminole)
#   TOP-RIGHT:   28.172837, -82.650652
#  
#  bounding_box:[-82.882183 27.884506 -82.650652 28.172837]

Pinellas.Clearwater.query.tag <- Mile.requirement.func("bounding_box:[-82.882183 27.884506 -82.650652 28.172837]")
nchar(Pinellas.Clearwater.query.tag)
Pinellas.Clearwater.query.tag


# PINELLAS (ST PETE):
#   
#   BOTTOM-LEFT: 27.609497, -82.923815
# TOP-RIGHT:   27.934536, -82.581179

# bounding_box:[-82.923815 27.609497 -82.581179 27.934536]

StPete.query.tag <- Mile.requirement.func("bounding_box:[-82.923815 27.609497 -82.581179 27.934536]")
nchar(StPete.query.tag)
StPete.query.tag

# 
# HILLSBOROUGH:
#   TOP-HALF:
#   BOTTOM-LEFT: 27.933701, -82.651060
# TOP-RIGHT:   28.173745, -82.053338
# 
# BOTTOM-HALF:
#   BOTTOM-LEFT: 27.643030, -82.571997
# TOP-RIGHT:   27.933701, -82.054366

# bounding_box:[-82.651060 27.933701 -82.053338 28.173745]
# bounding_box:[-82.571997 27.643030 -82.054366 27.933701]


Tampa.query.tag <- Mile.requirement.func(c("bounding_box:[-82.651060 27.933701 -82.053338 28.173745]",
                                         "bounding_box:[-82.571997 27.643030 -82.054366 27.933701]"))
nchar(Tampa.query.tag)
Tampa.query.tag


# 
# 
# MANATEE:
#   TOP-THIRD:
#   BOTTOM-LEFT: 27.542987, -82.672178
# TOP-RIGHT:   27.648263, -82.052137
# 
# MIDDLE-THIRD:
#   BOTTOM-LEFT: 27.385798	-82.790968
# TOP-RIGHT:   27.542987  -82.052137
# 
# BOTTOM-THIRD:
#   BOTTOM-LEFT: 27.208464, -82.254526
# TOP-RIGHT:   27.385798  -82.052137
# 
# bounding_box:[-82.672178 27.542987 -82.052137 27.648263]
# bounding_box:[-82.790968 27.385798 -82.052137 27.542987]
# bounding_box:[-82.254526 27.208464 -82.052137 27.385798]

Manatee.query.tag <- Mile.requirement.func(c("bounding_box:[-82.672178 27.542987 -82.052137 27.648263]",
                                           "bounding_box:[-82.790968 27.385798 -82.052137 27.542987]",
                                           "bounding_box:[-82.254526 27.208464 -82.052137 27.385798]"))
nchar(Manatee.query.tag)
Manatee.query.tag



# 
# SARASOTA:
#   TOP-THIRD:
#   BOTTOM-LEFT: 27.208464, -82.719214
# TOP-RIGHT:   27.389918, -82.248862
# 
# MIDDLE-THIRD:
#   BOTTOM-LEFT: 27.031464, -82.723334
# TOP-RIGHT:   27.209919, -82.054541
# 
# BOTTOM-THIRD:
#   BOTTOM-LEFT: 26.943813, -82.723334
# TOP-RIGHT:   27.034981, -82.254012
# 
# bounding_box:[-82.719214 27.208464 -82.248862 27.389918]
# bounding_box:[-82.723334 27.031464 -82.054541 27.209919]
# bounding_box:[-82.723334 26.943813 -82.254012 27.034981]

Sarasota.query.tag <- Mile.requirement.func(c("bounding_box:[-82.719214 27.208464 -82.248862 27.389918]",
                                              "bounding_box:[-82.723334 27.031464 -82.054541 27.209919]",
                                              "bounding_box:[-82.723334 26.943813 -82.254012 27.034981]"))
nchar(Sarasota.query.tag)
Sarasota.query.tag




# LEFT COORDINATE is HEIGHT, RIGHT COORDINATE is WIDTH

# bounding_box:[left_long bot_lat right_long top_lat]


area.tags <- list(
  Tampa = Tampa.query.tag,
  Pinellas.Clearwater = Pinellas.Clearwater.query.tag,
  Pinellas.StPete = StPete.query.tag,
  Manatee = Manatee.query.tag,
  Sarasota = Sarasota.query.tag,
  Pasco = Pasco.query.tag
)

