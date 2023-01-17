########
## Most critical reused objects
########


######
## Main search queries
######

main.queries <- list(
  RedTide = '(red-algae OR red-tides OR red-tide OR #redtide OR karenia-brevis OR kbrevis OR #kareniabrevis)',
  BlueGreen = '(blue-green-algae OR blue-green-algal OR #bluegreenalgae OR cyanobacteria OR cyanotoxins)',
  OilSpill = '(oil OR crude OR petroleum OR tar_ball OR tar_balls) -(-leak -leaks -leaked -leaking -spill -spills -spilled -spilling) -(-ocean -beach -beaches -bay -gulf -sea -lake -river -creek -waterway)',
  SewageSpill = '(sewer OR sewers OR sewage OR septic OR stormwater OR storm_water) -(-untreated -raw -overflow -discharge -pump -pumps -pumping -pumped -leak -leaks -leaked -leaking -spill -spills -spilled -spilling -dump -dumps -dumped -dumping) -(-ocean -beach -beaches -bay -gulf -sea -lake -river -creek -waterway)',
  IndustrialSpill = '(wastewater OR contaminants OR contamination OR contaminating OR chemical OR chemicals) -(-(-leak -leaks -leaked -leaking) -(-spill -spills -spilled -spilling)) -(-ocean -beach -beaches -bay -gulf -sea -lake -river -creek -waterway)'
)


#####
## Geographical reference terms, by area
#####

Tampa.query.chunk <- '-(-Tampa -Tampas -#TampaBay -TB-area -((Hillsborough OR HillsboroughCounty) (FL OR Florida))
-Apollo-Beach -#ApolloBeach -Wimauma 
-((Gibsonton OR Ruskin OR Sun-City) (FL OR Florida)) -Hillsborough-Bay
-Davis-Islands -Alafia-River -McKay-Bay
-lake-thonotosassa)'
nchar(Tampa.query.chunk)


Pinellas.Clearwater.query.chunk <- '-(-Pinellas -PinellasCounty -((Clearwater OR Dunedin) (FL OR Florida)) -Clearwater-Beach -#ClearwaterBeach
-Indian-Rocks-Beach -#IndianRocksBeach -Tarpon-Springs -#TarponSprings -Belleair -Palm-Harbor -#PalmHarbor -Safety-Harbor -#SafetyHarbor
-Honeymoon-Island -Sand-Key
-Caladesi -Lake-Tarpon)'
nchar(Pinellas.Clearwater.query.chunk)


# -\"Redington Beach\" -\"Redington Shores\" => "Redington" could just be added to Treasure Island/Tierra Verde in "FL OR Florda" chunk
# Tierra Verde could be dropped if need be for space. Not super-popular, but included just in case.
StPete.query.chunk <- '-(-StPetersburg -St-Petersburg -St-Pete -StPete -#StPeteBeach
-Madeira-Beach -#MadeiraBeach -((Treasure-Island OR Tierra-Verde) (FL OR Florida)) -Sunshine-Skyway
-Fort-De-Soto -Fort-DeSoto -Redington-Beach -Redington-Shores -Pass-a-grille -Boca-Ciega-Bay
-Egmont-Key -Weedon-Island)'
nchar(StPete.query.chunk)



# The last few - Terra Ceia, Palma Sola Bay, Bishop Harbor - have been really low mentions. 
# Could easily drop the off for space if need be.
Manatee.query.chunk <- '-(-Manatee-county -Manatee-counties -ManateeCounty -Bradenton -Bradentons -#BradentonBeach
-Anna-Maria-Island -#AnnaMariaIsland -Longboat-Key -#LongboatKey -Holmes-Beach -#HolmesBeach
-Manatee-River -#ManateeRiver -Port-Manatee -(Coquina-Beach -NC -Carolina)
-(Terra-Ceia (FL OR Florida)) 
-Palma-Sola-Bay -Bishop-Harbor
-lake-manatee)'
nchar(Manatee.query.chunk)



# #ManasotaKey, #CaseyKey, #LemonBay hashes are not that popular, could drop for space.
# Watch for 'Sarasota Herald-Tribune' in post-processing: seemingly the only reference to the area.
Sarasota.query.chunk <- '-(-Sarasota -Sarasotas -SarasotaCounty
-Siesta-Key-Beach -#SiestaKeyBeach
-((Venice OR Englewood OR North-Port OR #NorthPort OR Lido-Beach) (FL OR Florida)) -Casey-Key -#CaseyKey -Nokomis -Lemon-Bay -#LemonBay -St-Armands -#StArmands
-Manasota-Key -#ManasotaKey -Manasota-Beach -Englewood-Beach -Lido-Key
-Caspersen-Beach -Stump-Pass -#SarasotaBay)'
nchar(Sarasota.query.chunk)


# That last line of places (Werner-Boys, Three Rooker, Beacon) - very low mentions.. could easily drop if space is needed.
# Elfers & Shady Hills are both pretty low on mentions - could also drop.
# Pithlachascotee is really low mentions, but at least it's a water body (river).. could still drop it though.
Pasco.query.chunk <- '-(-(Pasco (county OR counties OR Florida OR FL)) -PascoCounty 
-Port-Richey -#PortRichey
-((Bayonet-Point OR Anclote OR Elfers OR Shady-Hills) (FL OR Florida))
-Cotee-River -Pithlachascotee -Jasmine-Estates -Key-Vista -Aripeka 
-Werner-Boyce -Three-Rooker-Island -Beacon-Square
) -@Lou_Port_Richey'
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

