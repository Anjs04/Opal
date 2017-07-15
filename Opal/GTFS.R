# List of packages for session
.packages = c("dplyr", "stringr", "tidyr", "sqldf", #For structure
              "ggplot2", "ggthemes", "ggmap",       #For mapping
              "devtools", "tibble")                 #For experimental packages

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#Import data--------------------------------------------------
stops = read.csv("Input/stops.txt", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
names(stops) = gsub(pattern = "?..", replacement = "", names(stops))

#QGIS-filtered GTFS stops data
Uni <- read.csv("Input/20170131_Uni_Public_Transport_Stops_170523.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
LGA <- read.csv("Input/20170131_All_Public_Transport_Stops_LGA_SPATIALJOIN_170523.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)

Uni$stop_id = trimws(as.character(Uni$stop_id))
LGA$stop_id = trimws(as.character(LGA$stop_id))

#Opal data
Opal_multi = read.csv("Input/Opal Data/trips.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
Opal_single = read.csv("Input/Opal Data/trip.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)

#Binding single trips to multi trips (keeping all transfers from multi trips) ------------------------------------------------

#Create type of transport column
Opal_single$JRNY_STRT_TSTYPCD =
  if_else(str_detect(Opal_single$ORIGIN.LOCATION, "Station"), "Train",
          if_else(str_detect(Opal_single$ORIGIN.LOCATION, "Wharf"), "Ferry",
                  if_else(str_detect(Opal_single$ORIGIN.LOCATION, "Light Rail"), "Light Rail",
                          if_else(Opal_single$ORIGIN.TSN %in% stops$stop_id, "Bus", "None"
                          )
                  )
          )
  )

#Reformat hour columns to match Opal_multi
sapply(Opal_multi, class)
sapply(Opal_single, class)
Opal_single$TRIP = as.numeric(Opal_single$TRIP)

#Append leading zeros so that all times are of the same width
table(nchar(Opal_multi$JRNY_STRT_TM))
#Length two means "-1", unknown time/tsn
#Length 3 means the hour was "00" and the min was single digit
#Length 4 means the hour was "00" and the min was double digit
#Length 5 requires one leading zero
#Length 6 is the proper length
#formatC(varname, width = 6, format = "d", flag = "0") #For Opal_multi
#formatC(varname, width = 2, format = "d", flag = "0") #For Opal_single

Opal_single$ORIGIN.HOUR = formatC(Opal_single$ORIGIN.HOUR, width = 2, format = "d", flag = "0")
Opal_single$DESTINATION.HOUR = formatC(Opal_single$DESTINATION.HOUR, width = 2, format = "d", flag = "0")
Opal_single$ORIGIN.HOUR = paste(Opal_single$ORIGIN.HOUR, "0000", sep = "")
Opal_single$DESTINATION.HOUR = paste(Opal_single$DESTINATION.HOUR, "0000", sep = "")

Opal_multi$JRNY_STRT_TM  = str_pad(Opal_multi$JRNY_STRT_TM, width = 6, side = "left", pad = "0")
Opal_multi$T1_TAGOFF_TM  = str_pad(Opal_multi$T1_TAGOFF_TM, width = 6, side = "left", pad = "0")
Opal_multi$T2_TAGON_TM   = str_pad(Opal_multi$T2_TAGON_TM, width = 6, side = "left", pad = "0")
Opal_multi$T2_TAGOFF_TM  = str_pad(Opal_multi$T2_TAGOFF_TM, width = 6, side = "left", pad = "0")
Opal_multi$T3_TAGON_TM   = str_pad(Opal_multi$T3_TAGON_TM, width = 6, side = "left", pad = "0")
Opal_multi$T3_TAGOFF_TM  = str_pad(Opal_multi$T3_TAGOFF_TM, width = 6, side = "left", pad = "0")
Opal_multi$T4_TAGON_TM   = str_pad(Opal_multi$T4_TAGON_TM, width = 6, side = "left", pad = "0")
Opal_multi$T4_TAGOFF_TM  = str_pad(Opal_multi$T4_TAGOFF_TM, width = 6, side = "left", pad = "0")

#Rename Opal_single columns to where they will be in Opal_multi
names(Opal_multi)
names(Opal_single)

names(Opal_single) = c("JRNY_STRT_DT",
                       "CARD_TYP_CD",
                       "JRNY_STRT_TM",
                       "JRNY_STRT_TSN",
                       "JRNY_STRT_TSN_NM",
                       "T1_TAGOFF_TM",
                       "T1_TAGOFF_TSN",
                       "T1_TAGOFF_NM",
                       "TRIP",
                       "JRNY_STRT_TSTYPCD"
)

#Add trip count column that is not in Opal_multi
Opal_multi$TRIP = 1

Opal_full = bind_rows(Opal_multi, Opal_single)

Opal_full =
  Opal_full %>%
  mutate_at(ends_with("TSN"), as.character)

#Get the last non-NA value, in other words, the last leg of the journey, which is a TAPOFF, even if UNKNOWN
#tail() has 3 variables, following the TAPOFF structure, which differs from the TAGON structure
tail = data.frame(
  t(
    apply(Opal_full, 1, function(x) tail(na.omit(x), 4L))
  )
)

names(tail) = c("END_TM", "END_TSN", "END_NM", "TRIP")

Opal_full = cbind(Opal_full, tail)

Opal_full$END_NM = trimws(as.character(Opal_full$END_NM))
Opal_full$END_TM = trimws(as.character(Opal_full$END_TM))
Opal_full$END_TSN = trimws(as.character(Opal_full$END_TSN))

Opal_full$JRNY_STRT_TM  = paste(Opal_full$JRNY_STRT_DT, Opal_full$JRNY_STRT_TM, sep = "")
Opal_full$T1_TAGOFF_TM  = paste(Opal_full$JRNY_STRT_DT, Opal_full$T1_TAGOFF_TM, sep = "")
Opal_full$T2_TAGON_TM   = paste(Opal_full$JRNY_STRT_DT, Opal_full$T2_TAGON_TM, sep = "")
Opal_full$T2_TAGOFF_TM  = paste(Opal_full$JRNY_STRT_DT, Opal_full$T2_TAGOFF_TM, sep = "")
Opal_full$T3_TAGON_TM   = paste(Opal_full$JRNY_STRT_DT, Opal_full$T3_TAGON_TM, sep = "")
Opal_full$T3_TAGOFF_TM  = paste(Opal_full$JRNY_STRT_DT, Opal_full$T3_TAGOFF_TM, sep = "")
Opal_full$T4_TAGON_TM   = paste(Opal_full$JRNY_STRT_DT, Opal_full$T4_TAGON_TM, sep = "")
Opal_full$T4_TAGOFF_TM  = paste(Opal_full$JRNY_STRT_DT, Opal_full$T4_TAGOFF_TM, sep = "")
Opal_full$END_TM        = paste(Opal_full$JRNY_STRT_DT, Opal_full$END_TM, sep = "")

Opal_full$TRIP = NULL
Opal_full$TRIP = as.numeric(Opal_full$TRIP)

#Join stops with Uni and LGA data---------------------------------------------------------
stops_Uni_LGA = 
  left_join(stops,
            LGA %>%
              select(stop_id, LGA_CODE_2, LGA_NAME_2),
            by = c("stop_id" = "stop_id")) %>%
  left_join(.,
            Uni %>%
              select(stop_id, University),
            by = c("stop_id" = "stop_id"))  

stops_Uni_LGA = distinct(stops_Uni_LGA)


#Gather Opal_full for Tableau-------------------------------------------------------------

Opal_full$path_id = 1:dim(Opal_full)[1] #When gathered, this will tell us which original row the entry belonged to

#Essential variable:
Opal_full$from_Blacktown = 
  if_else(
    Opal_full$JRNY_STRT_TSN %in% Opal_full$TSN %in% stops_Uni_LGA[stops_Uni_LGA$LGA_NAME_2 == "Blacktown (C)", "stop_id"] |
    Opal_full$JRNY_STRT_TSN_NM == "Blacktown Station", 1, 0
  )

#Unessential variables
Opal_full$to_Blacktown  = 
  if_else(
    Opal_full$END_TSN %in% Opal_full$TSN %in% stops_Uni_LGA[stops_Uni_LGA$LGA_NAME_2 == "Blacktown (C)", "stop_id"] |
    Opal_full$END_NM == "Blacktown Station", 1, 0
  )
Opal_full$to_or_from_Blacktown =
  if_else(Opal_full$from_Blacktown == 1 |
          Opal_full$to_Blacktown == 1, 1, 0
  )

#Manual renaming
names = read.csv("Input/Opal_names.csv")
names(Opal_full) = names$var2

#Back to gathering...
Opal_Tableau =
  Opal_full %>%
  select(-JRNY_STRT_DT,
         -J_ID,
         -CARD_TYP_CD) %>%
  gather(key = "order",
         value = "TM/TYP/TSN/NM",
         ends_with("TSN"),
         ends_with("NM"),
         ends_with("TM"),
         contains("TYP")
         ) %>%
  separate(col = order,
          into = c("order", "on/off", "var"),
          sep = "_") %>%
  spread(key = var,
         value = `TM/TYP/TSN/NM`)

#For checking join, assign id to each row
Opal_Tableau$id = 1:dim(Opal_Tableau)[1]

#Join with stops_Uni_LGA to get coordinates, University, and LGA for each stop
Opal_join_code =
  left_join(Opal_Tableau %>%
              filter(TSN %in% stops_Uni_LGA[,"stop_id"]),
            stops_Uni_LGA[,c("stop_id", "University", "stop_lat", "stop_lon", "LGA_NAME_2")],
            by = c("TSN" = "stop_id")
            )

Opal_join_name =
  left_join(Opal_Tableau %>%
              filter(!TSN %in% stops_Uni_LGA[,"stop_id"]),
            distinct(stops_Uni_LGA[,c("stop_name", "University", "stop_lat", "stop_lon", "LGA_NAME_2")]),
            by = c("NM" = "stop_name")
            )

Opal_join = rbind(Opal_join_code, Opal_join_name)



#Aggregate by LGA for journeys to particular places -------------------------------------
#First join LGAs to Opal_full T1_TAGON_TSN
tmp =
  left_join(Opal_full %>%
              filter(T1_TAGON_TSN %in% stops_Uni_LGA[,"stop_id"]),
            distinct(stops_Uni_LGA[,c("stop_id", "LGA_NAME_2")]),
            by = c("T1_TAGON_TSN" = 'stop_id')
            )
tmp2 =
  left_join(Opal_full %>%
              filter(!T1_TAGON_TSN %in% stops_Uni_LGA[,"stop_id"]),
            distinct(stops_Uni_LGA[,c("stop_name", "LGA_NAME_2")]),
            by = c("T1_TAGON_NM" = "stop_name")
            )
Opal_full_LGA_start = rbind(tmp, tmp2)

#Join Uni tags to the END_TAGOFF_TSN and END_TAGOFF_NM
tmp3 =
  left_join(Opal_full_LGA_start %>%
              filter(END_TAGOFF_TSN %in% stops_Uni_LGA[,"stop_id"]),
            distinct(stops_Uni_LGA[,c("stop_id", "University")]),
            by = c("END_TAGOFF_TSN" = "stop_id")
  )

tmp4 =
  left_join(Opal_full_LGA_start %>%
              filter(!END_TAGOFF_TSN %in% stops_Uni_LGA[,"stop_id"]),
            distinct(stops_Uni_LGA[,c("stop_name", "University")]),
            by = c("END_TAGOFF_NM" = "stop_name")
            )

Opal_full_Uni_LGA = rbind(tmp3, tmp4)

#Aggregate Opal_full_Uni_LGA to LGA and University counts of TRIP
new_aggregate =
  Opal_full_Uni_LGA %>%
  group_by(LGA_NAME_2, University) %>%
  summarise(sum(TRIP))

#Filter for Parra in end NM
parra =
  Opal_full_LGA_start[grep("Parramatta Station", Opal_full_LGA_start$END_TAGOFF_NM),] %>%
  group_by(LGA_NAME_2) %>%
  summarise(sum(TRIP))

parra_station =
  Opal_full_LGA_start[Opal_full_LGA_start$END_TAGOFF_NM == "Parramatta Station",] %>%
  group_by(LGA_NAME_2) %>%
  summarise(sum(TRIP))

parra_full = full_join(parra, parra_station, by = "LGA_NAME_2")
names(parra_full) = c("LGA", "TRIPS", "TRIPS_STATION")
parra_full$TRIPS_BUS = parra_full$TRIPS - parra_full$TRIPS_STATION

richmond = 
  Opal_full_LGA_start[Opal_full_LGA_start$END_TAGOFF_NM == "Richmond Station",] %>%
  group_by(LGA_NAME_2) %>%
  summarise(sum(TRIP))

east_richmond = 
  Opal_full_LGA_start[Opal_full_LGA_start$END_TAGOFF_NM == "East Richmond Station",] %>%
  group_by(LGA_NAME_2) %>%
  summarise(sum(TRIP))

#write.csv(Opal_full_LGA_start[Opal_full_LGA_start$END_TAGOFF_NM == "Richmond Station" | 
#                              Opal_full_LGA_start$END_TAGOFF_NM ==  "East Richmond Station",], 
#          "Richmond Aggregate/For_Checking.csv")

#Test for journeys that go over to the next day -----------------------------------------
table(as.POSIXct(strptime(Opal_full_Uni_LGA$END_TAGOFF_TM, 
                          format = "%Y%m%d%H%M%S")) 
      > as.POSIXct(strptime(Opal_full_Uni_LGA$T1_TAGON_TM, 
                            format = "%Y%m%d%H%M%S"))
)/130306 #As a percentage

#View the false ones
View(
  Opal_full_Uni_LGA[
     ! as.POSIXct(strptime(Opal_full_Uni_LGA$END_TAGOFF_TM, format = "%Y%m%d%H%M%S")) 
     >= as.POSIXct(strptime(Opal_full_Uni_LGA$T1_TAGON_TM, format = "%Y%m%d%H%M%S")),
     c("T1_TAGON_TM", "END_TAGOFF_TM")]
)

#Attach the date 20170330 to the END_TAGOFF_TM for journeys shown above
Opal_full_Uni_LGA[
  !is.na(
    !  as.POSIXct(strptime(Opal_full_Uni_LGA$END_TAGOFF_TM, format = "%Y%m%d%H%M%S")) 
    >= as.POSIXct(strptime(Opal_full_Uni_LGA$T1_TAGON_TM, format = "%Y%m%d%H%M%S"))
  ),
  "END_TAGOFF_TM"] =
   paste("20170330", !is.na(str_sub(Opal_full_Uni_LGA$END_TAGOFF_TM, start = -6)), sep = "")

Opal_full_Uni_LGA$time = 
  difftime(
    as.POSIXct(strptime(paste(Opal_full_Uni_LGA$JRNY_STRT_DT, Opal_full_Uni_LGA$END_TAGOFF_TM, sep = ""), format = "%Y%m%d%H%M%S")),
    as.POSIXct(strptime(paste(Opal_full_Uni_LGA$JRNY_STRT_DT, Opal_full_Uni_LGA$T1_TAGON_TM, sep = ""), format = "%Y%m%d%H%M%S")),
    units = "mins"
  )

#Back to gathering...
Opal_Tableau_Uni_LGA =
  Opal_full_Uni_LGA %>%
  select(-JRNY_STRT_DT,
         -J_ID,
         -CARD_TYP_CD) %>%
  gather(key = "order",
         value = "TM/TYP/TSN/NM",
         ends_with("TSN"),
         ends_with("NM"),
         ends_with("TM"),
         contains("TYP")
  ) %>%
  separate(col = order,
           into = c("order", "on/off", "var"),
           sep = "_") %>%
  spread(key = var,
         value = `TM/TYP/TSN/NM`)

#For checking join, assign id to each row
Opal_Tableau_Uni_LGA =
Opal_Tableau_Uni_LGA %>%
  mutate(id = 1:dim(.)[1]) %>%
  rename(University_END = University,
         LGA_START = LGA_NAME_2)

Opal_Tableau_Uni_LGA_join_code = 
  left_join(Opal_Tableau_Uni_LGA,
            distinct(stops_Uni_LGA[,c("stop_id", "University", "LGA_NAME_2", "stop_lat", "stop_lon")]),
            by = c("TSN" = "stop_id")
  )
  
Opal_Tableau_Uni_LGA_join_name = 
  left_join(Opal_Tableau_Uni_LGA %>%
              filter(!TSN %in% stops_Uni_LGA$stop_id),
            distinct(stops_Uni_LGA[,c("stop_name", "University", "LGA_NAME_2", "stop_lat", "stop_lon")]),
            by = c("NM" = "stop_name")
  )
            
Opal_Tableau_Uni_LGA_join = 
  rbind(Opal_Tableau_Uni_LGA_join_code, Opal_Tableau_Uni_LGA_join_name
  )

#Test all rows are still there after the join
dim(Opal_Tableau_Uni_LGA_join)[1] == dim(Opal_Tableau_Uni_LGA_join_code)[1]+dim(Opal_Tableau_Uni_LGA_join_name)[1]

#Add GSC Districts to the LGA Origin
GSC = read.csv("Input/20170131_GSC_to_LGA_Concordance_2011_20170206.csv")

Opal_Tableau_Uni_LGA_join_GSC =
left_join(Opal_Tableau_Uni_LGA_join, 
          GSC,
          by = c("LGA_START" = "LGA_NAME_2"))

#Add new definitions of Western Sydney
Opal_Tableau_Uni_LGA_join_GSC$District = 
  if_else(
    Opal_Tableau_Uni_LGA_join_GSC$GSC_District %in% c("West",
                                                      "West Central",
                                                      "South West"
                                                      ),
          "Western Sydney",
          if_else(
            Opal_Tableau_Uni_LGA_join_GSC$GSC_District %in% c("Central",
                                                              "North",
                                                              "South"
                                                              ),
            "Greater Sydney",
            "Other"
            
          )
  )
