library(dplyr)
library(stringr)
library(tidyr)
library(sqldf)

setwd("O:/Synergy/Projects/2017/20170131 Blacktown CC - Future Tertiary Educational Facilities in Blacktown/Data")

#It's a good idea, before importing, to manually change the data types of each column to the simplest type that still contains all the data
#Note: I didn't do this, and you will se the implications of this later on.
trips = read.csv("Input/trips.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
trip = read.csv("Input/trip.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
All_Bus_New <- read.csv("All_bus_stops.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
Blacktown_LGA_TSN <- read.csv("Input/Blacktown_LGA_TSN.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
Uni_Bus <- read.csv("Input/20170131_Uni_Public_Transport_Stops_170523.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
Bus_LGA <- read.csv("Input/20170131_All_Public_Transport_Stops_LGA_SPATIALJOIN_170523.csv", stringsAsFactors = FALSE, na.strings = c("", " "))
uni_train_list =
  c("Redfern Station",
    "Macquarie University Station",
    "Parramatta Station",
    "Rydalmere Station",
    "Macarthur Station",
    "Strathfield Station",
    "Revesby Station",
    "East Richmond Station",
    "Richmond Station",
    "Quakers Hill Station",
    "Central Station",
    "North Sydney Station",
    "Kingswood Station",
    "Werrington Station"
  )

#Show stop_id is unique
length(unique(All_Bus_New$stop_id))

#Note: Date for all data is 20170329 (29th March 2017), and card type for all data is "Concession"

#Aggregate of journeys with single trips
q_single =
  trip %>%
  group_by(ORIGIN.TSN,           
           ORIGIN.LOCATION,     
           DESTINATION.TSN,      
           DESTINATION.LOCATION
  ) %>%
  summarise(sum(TRIP)) #Aggregate trip by day, dropping hours data


#Get the last non-NA value, in other words, the last leg of the journey, which is a TAPOFF, even if UNKNOWN
#tail() has 3 variables, following the TAPOFF structure, which differs from the TAGON structure
tail = data.frame(
  t(
    apply(trips, 1, function(x) tail(na.omit(x), 3L))
  )
)

names(tail) = c("END_TM", "END_TSN", "END_NM")

trips = cbind(trips, tail)


#Aggregate of journeys with multiple trips
q_multi = 
  trips %>%
  group_by(JRNY_STRT_TSN,
           JRNY_STRT_TSN_NM,
           END_TSN,
           END_NM
  ) %>%
  summarise(count = n())

#Create a TSN code of the start-to-end TSN combination, to combine data for single and multi aggregations
q_single =
  q_single %>%
  mutate(id = paste(ORIGIN.TSN, DESTINATION.TSN, sep = "_"))

q_multi =
  q_multi %>%
  mutate(id = paste(JRNY_STRT_TSN, END_TSN, sep = "_"))

#Join on newly created id
q = merge(q_single, q_multi, by = "id", all = TRUE)

#Confirm the id is unique to each row
View(table(q$id)) #Sort by Freq and see there is nothing above 1.

#Join the new counts, by taking the last 5 non-NA values
test = data.frame(
  t(
    apply(q, 1, function(x) tail(na.omit(x), 5L))
  )
)

#Drop meaningless count data
test$X5 = NULL

#Create id column of combined TSNs again
test$id = paste(test$X1, test$X3, sep = "_")

#Join q_single$sum(TRIP) and q_multi$count
test2 = merge(test, q_single[,c("id", "sum(TRIP)")], by = "id", all = TRUE)
test2 = merge(test, q_single, by = "id", all = TRUE)

inner_join = #merge(q_single, q_multi, by = "id", all = FALSE)
  sqldf("SELECT * FROM q_single
         INNER JOIN q_multi
         ON q_single.id = q_multi.id")
inner_join$num_of_trips = as.integer(inner_join[,"sum(TRIP)"]) + as.integer(inner_join[,"count"])

outer_single = 
  sqldf("SELECT * FROM q_single
         LEFT JOIN q_multi
         ON q_single.id = q_multi.id
         WHERE q_multi.id IS NULL")
outer_single$num_of_trips = outer_single$`sum(TRIP)`
  
outer_multi = 
  sqldf("SELECT * FROM q_multi
         LEFT JOIN q_single
         ON q_single.id = q_multi.id
         WHERE q_single.id IS NULL")
outer_multi$num_of_trips = outer_multi$count

final_join = 
  rbind(inner_join, outer_single, outer_multi)
final_join$id = NULL
final_join$id = NULL #Required twice, as two "id" columns introduced during joins!

#Clean up large swathes of NA
clean_join = data.frame(
  t(
    apply(final_join, 1, function(x) tail(na.omit(x), 6L))
  )
)
clean_join$X5 = NULL
names(clean_join) = c("START_TSN", 
                      "START_NM",
                      "END_TSN",
                      "END_NM",
                      "SUM_TRIPS")

clean_join$START_BLACKTOWN_LGA = if_else(clean_join$START_TSN %in% Blacktown_LGA_TSN$stop_code |
                                         clean_join$START_NM == "Blacktown Station", 1, 0)

#Second attempt at same join, at this point I have run "write.csv(clean_join, "20170524_QGIS_Aggregate.csv")"...
#...manually opened clean_join in Excel, and changed END_TSN to Numeric with no decimal places, but including negative numbers
clean_join_manual = read.csv("Output/20170524_QGIS_Aggregate.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", " "))
clean_join_manual

code_join =
  inner_join(
    clean_join_manual %>%
      filter(END_TSN %in% Uni_Bus$stop_code),
    Uni_Bus %>%
      select(stop_code, University),
    by = c("END_TSN" = "stop_code")
  )

name_join =
  left_join(
    clean_join_manual %>%
      filter(!END_TSN %in% Uni_Bus$stop_code),
    Uni_Bus %>%
      select(stop_name, University),
    by = c("END_NM" = "stop_name")
  )

names(code_join) == names(name_join)

full_join = rbind(code_join, name_join)

#List of included uni locations for filtering full join(to be edited if needed)
uni_list =
  c(
    "ACU-North Sydney",
    "ACU-Strathfield",
    "Central",
    "Kingswood-Werrington",
    "Macquarie Univeristy",
    "North Sydney",
    "University of Sydney",
    "UNSW",
    "UTS",
    "WSU-Bankstown",
    "WSU-Campbelltown",
    "WSU-Hawkesbury",
    "WSU-Nirimba",
    "WSU-Parramatta",
    "WSU-Penrith")


full_join_filtered =
  full_join %>%
  filter(START_BLACKTOWN_LGA == 1,
         University %in% uni_list) %>%
  select(-START_BLACKTOWN_LGA, -X)

write.csv(full_join_filtered, "Output/20170131_QGIS_Blacktown_to_All_Unis_20170524.csv", row.names = FALSE)

#Now joining all START_TSN to their LGA-------------------------------------------------------------------

code_join_LGA =
inner_join(full_join %>%
             select(-X),
           Bus_LGA %>%
             select(stop_code, LGA_CODE_2, LGA_NAME_2),
           by = c("START_TSN" = "stop_code")
           )

name_join_LGA =
left_join(full_join %>%
            filter(!START_TSN %in% Bus_LGA$stop_code) %>%
            select(-X),
          Bus_LGA %>%
            select(stop_code, LGA_CODE_2, LGA_NAME_2),
          by = c("START_NM" = "stop_name")
          )

#Joining 3 stop datasets


stops_Uni_LGA = left_join(stops,
                          Uni_Bus %>%
                            select(stop_id, University),
                          by = c("stop_id" = "stop_id"))
