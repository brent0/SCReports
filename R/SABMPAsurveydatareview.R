#This script creates the required tables for the SABMPA post activity report
#---- set up-----

MPA.SAB.data = function (current_year) {
  
  ######NEED TO MODIFY THE REST SIMILAR TO MPA.gully.data############

getwd()
setwd("C:/Users/GlassA/Desktop/covid copies/SABMPA")

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# ----get catch data for all tows inside the MPA ----
require(ROracle)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)
#select data from the 14 tows in the SABMPA
sabdat=dbGetQuery(con, (" SELECT trip.trip_id, trip.trip,
trip.board_date, st.set_no, st.station, pr.latitude, pr.longitude,
 st.est_catch, ca.speccd_id, ca.est_num_caught, ca.est_discard_wt
FROM istrips trip, isgears gr, isfishsets st, iscatches ca, issetprofile pr
WHERE trip.tripcd_id = 7061
AND trip.trip_id = gr.trip_Id
AND pr.PNTCD_ID=2
AND (trip.trip_id = st.trip_Id
    AND gr.gear_id = st.gear_id)
AND (st.fishset_id = pr.fishset_Id
    AND st.set_no = pr.set_no)
AND (st.fishset_id = ca.fishset_id
AND st.haulccd_id = '1')
and st.station in ('016', '017', '018', '029', '031', '032', '034', '091', '093', '205', '511', '609', '611', '801')  
order by board_date, station, speccd_id"))
sab<-sabdat
str(sab)
#separate date
sab$BOARD_DATE <- as.Date(sab$BOARD_DATE, format = "%Y-%m-%d hh:mm:ss")
sab = sab %>% 
  mutate(BOARD_DATE = ymd(BOARD_DATE)) %>% 
  mutate_at(vars(BOARD_DATE), funs(year, month, day))
summary(sab)

#number of distinct species by year
sab %>%
  group_by(year) %>%
  summarise(unique = n_distinct(SPECCD_ID))

#change speccd_id to name and group certain species
data1=sab

data1$SPECCD_ID[data1$SPECCD_ID == 10] <- "Atlantic Cod"
data1$SPECCD_ID[data1$SPECCD_ID == 11] <- "Haddock"
data1$SPECCD_ID[data1$SPECCD_ID == 12] <- "White Hake"
data1$SPECCD_ID[data1$SPECCD_ID == 13] <- "Red Hake"
data1$SPECCD_ID[data1$SPECCD_ID == 14] <- "Silver Hake"
data1$SPECCD_ID[data1$SPECCD_ID == 15] <- "Cusk"
data1$SPECCD_ID[data1$SPECCD_ID == 16] <- "Pollock"
data1$SPECCD_ID[data1$SPECCD_ID == 17] <- "Tom Cod"
data1$SPECCD_ID[data1$SPECCD_ID == 23] <- "Redfish NS"
data1$SPECCD_ID[data1$SPECCD_ID == 30] <- "Halibut"
data1$SPECCD_ID[data1$SPECCD_ID == 31] <- "Turbot"
data1$SPECCD_ID[data1$SPECCD_ID == 40] <- "American Plaice"
data1$SPECCD_ID[data1$SPECCD_ID == 41] <- "Witch Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 42] <- "Yellowtail Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 43] <- "Winter Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 44] <- "Gulfstream Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 50] <- "Striped Atlantic Wolffish"
data1$SPECCD_ID[data1$SPECCD_ID == 51] <- "Spotted Wolffish"
data1$SPECCD_ID[data1$SPECCD_ID == 52] <- "Northern Wolffish"
data1$SPECCD_ID[data1$SPECCD_ID == 60] <- "Herring"
data1$SPECCD_ID[data1$SPECCD_ID == 61] <- "American Shad"
data1$SPECCD_ID[data1$SPECCD_ID == 62] <- "Alewife"
data1$SPECCD_ID[data1$SPECCD_ID == 64] <- "Capelin"
data1$SPECCD_ID[data1$SPECCD_ID == 70] <- "Mackerel"
data1$SPECCD_ID[data1$SPECCD_ID == 112] <- "Longfin Hake"
data1$SPECCD_ID[data1$SPECCD_ID == 114] <- "Fourbeard Rockling"
data1$SPECCD_ID[data1$SPECCD_ID == 118] <- "Greenland Cod"
data1$SPECCD_ID[data1$SPECCD_ID == 142] <- "Four-spot Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 143] <- "Windowpane Flounder"
data1$SPECCD_ID[data1$SPECCD_ID == 201] <- "Thorny Skate"
data1$SPECCD_ID[data1$SPECCD_ID == 202] <- "Smooth Skate"
data1$SPECCD_ID[data1$SPECCD_ID == 211] <- "Skates NS"
data1$SPECCD_ID[data1$SPECCD_ID == 241] <- "Northern Hagfish"
data1$SPECCD_ID[data1$SPECCD_ID == 300] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 301] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 303] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 304] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 306] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 311] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 317] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 320] <- "Sea Raven"
data1$SPECCD_ID[data1$SPECCD_ID == 323] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 350] <- "Atlantic Sea Poacher"
data1$SPECCD_ID[data1$SPECCD_ID == 351] <- "Alligatorfish"
data1$SPECCD_ID[data1$SPECCD_ID == 400] <- "Monkfish"
data1$SPECCD_ID[data1$SPECCD_ID == 410] <- "Marlinspike Grenadier"
data1$SPECCD_ID[data1$SPECCD_ID == 500] <- "Snailfish Sp."
data1$SPECCD_ID[data1$SPECCD_ID == 501] <- "Lumpfish"
data1$SPECCD_ID[data1$SPECCD_ID == 502] <- "Spiny Lumpsucker"
data1$SPECCD_ID[data1$SPECCD_ID == 565] <- "Barracudina NS"
data1$SPECCD_ID[data1$SPECCD_ID == 590] <- "Sand Lances NS"
data1$SPECCD_ID[data1$SPECCD_ID == 619] <- "Eelpout NS"
data1$SPECCD_ID[data1$SPECCD_ID == 622] <- "Snakeblenny"
data1$SPECCD_ID[data1$SPECCD_ID == 623] <- "Daubed Shanny"
data1$SPECCD_ID[data1$SPECCD_ID == 625] <- "Radiated Shanny"
data1$SPECCD_ID[data1$SPECCD_ID == 626] <- "Fourline Snakeblenny"
data1$SPECCD_ID[data1$SPECCD_ID == 627] <- "Eelpout NS"
data1$SPECCD_ID[data1$SPECCD_ID == 630] <- "Wrymouth"
data1$SPECCD_ID[data1$SPECCD_ID == 640] <- "Ocean Pout"
data1$SPECCD_ID[data1$SPECCD_ID == 642] <- "Eelpout NS"
data1$SPECCD_ID[data1$SPECCD_ID == 644] <- "Blennies,Shannies,Gunnels"
data1$SPECCD_ID[data1$SPECCD_ID == 647] <- "Eelpout NS"
data1$SPECCD_ID[data1$SPECCD_ID == 712] <- "Barracudina NS"
data1$SPECCD_ID[data1$SPECCD_ID == 720] <- "Atlantic Saury"
data1$SPECCD_ID[data1$SPECCD_ID == 880] <- "Sculpin NS"
data1$SPECCD_ID[data1$SPECCD_ID == 1100] <- "Eggs Unid."
data1$SPECCD_ID[data1$SPECCD_ID == 1224] <- "Skate Eggs NS"
data1$SPECCD_ID[data1$SPECCD_ID == 1510] <- "Whelk Eggs NS"
data1$SPECCD_ID[data1$SPECCD_ID == 1821] <- "Sea Squirts"
data1$SPECCD_ID[data1$SPECCD_ID == 1823] <- "Sea Potato"
data1$SPECCD_ID[data1$SPECCD_ID == 2000] <- "Crustacea C."
data1$SPECCD_ID[data1$SPECCD_ID == 2100] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2211] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2212] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2313] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2316] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2411] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2413] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2415] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2416] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2511] <- "Jonah Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2513] <- "Atlantic Rock Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2521] <- "Lesser Toad Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2523] <- "Northern Stone Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2526] <- "Snow Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2527] <- "Toad Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2559] <- "Hermit Crab"
data1$SPECCD_ID[data1$SPECCD_ID == 2565] <- "Shrimp NS"
data1$SPECCD_ID[data1$SPECCD_ID == 2990] <- "Barnacles"
data1$SPECCD_ID[data1$SPECCD_ID == 3100] <- "Bristle Worm"
data1$SPECCD_ID[data1$SPECCD_ID == 3200] <- "Sea Mouse"
data1$SPECCD_ID[data1$SPECCD_ID == 4200] <- "Snails and Slugs"
data1$SPECCD_ID[data1$SPECCD_ID == 4210] <- "Whelk NS"
data1$SPECCD_ID[data1$SPECCD_ID == 4310] <- "Clam NS"
data1$SPECCD_ID[data1$SPECCD_ID == 4312] <- "Northern Propeller Clam"
data1$SPECCD_ID[data1$SPECCD_ID == 4321] <- "Sea Scallop"
data1$SPECCD_ID[data1$SPECCD_ID == 4322] <- "Icelandic Scallop"
data1$SPECCD_ID[data1$SPECCD_ID == 4340] <- "Cockles NS"
data1$SPECCD_ID[data1$SPECCD_ID == 4400] <- "Sea Slug"
data1$SPECCD_ID[data1$SPECCD_ID == 4430] <- "Canoe Shells"
data1$SPECCD_ID[data1$SPECCD_ID == 4511] <- "Shortfin Squid"
data1$SPECCD_ID[data1$SPECCD_ID == 4514] <- "Squid NS"
data1$SPECCD_ID[data1$SPECCD_ID == 4521] <- "Octopus"
data1$SPECCD_ID[data1$SPECCD_ID == 4522] <- "Bobtail Squid"
data1$SPECCD_ID[data1$SPECCD_ID == 5100] <- "Sea Spider"
data1$SPECCD_ID[data1$SPECCD_ID == 6100] <- "Star NS"
data1$SPECCD_ID[data1$SPECCD_ID == 6113] <- "Star NS"
data1$SPECCD_ID[data1$SPECCD_ID == 6115] <- "Mud Star"
data1$SPECCD_ID[data1$SPECCD_ID == 6117] <- "Horse Star"
data1$SPECCD_ID[data1$SPECCD_ID == 6119] <- "Blood Star"
data1$SPECCD_ID[data1$SPECCD_ID == 6121] <- "Purple Sunstar"
data1$SPECCD_ID[data1$SPECCD_ID == 6123] <- "Spiny Sunstar"
data1$SPECCD_ID[data1$SPECCD_ID == 6125] <- "Star NS"
data1$SPECCD_ID[data1$SPECCD_ID == 6128] <- "Star NS"
data1$SPECCD_ID[data1$SPECCD_ID == 6200] <- "Brittle Star"
data1$SPECCD_ID[data1$SPECCD_ID == 6300] <- "Basket Star"
data1$SPECCD_ID[data1$SPECCD_ID == 6400] <- "Sea Urchin"
data1$SPECCD_ID[data1$SPECCD_ID == 6411] <- "Sea Urchin"
data1$SPECCD_ID[data1$SPECCD_ID == 6413] <- "Sea Urchin"
data1$SPECCD_ID[data1$SPECCD_ID == 6500] <- "Sand Dollar"
data1$SPECCD_ID[data1$SPECCD_ID == 6511] <- "Sand Dollar"
data1$SPECCD_ID[data1$SPECCD_ID == 6600] <- "Sea Cucumbers"
data1$SPECCD_ID[data1$SPECCD_ID == 8300] <- "Anemones NS"
data1$SPECCD_ID[data1$SPECCD_ID == 8313] <- "Anemones NS"
data1$SPECCD_ID[data1$SPECCD_ID == 8318] <- "Sea Pen"
data1$SPECCD_ID[data1$SPECCD_ID == 8332] <- "Coral NS"
data1$SPECCD_ID[data1$SPECCD_ID == 8500] <- "Jellyfish NS"
data1$SPECCD_ID[data1$SPECCD_ID == 8520] <- "Jellyfish NS"
data1$SPECCD_ID[data1$SPECCD_ID == 8600] <- "Sponges NS"
data1$SPECCD_ID[data1$SPECCD_ID == 9300] <- "Seaweeds NS"

datacheck<-data1 %>%  filter(SPECCD_ID == "Sea Urchin", year == 2006)

# separate date for new file
data1$BOARD_DATE <- as.Date(data1$BOARD_DATE, format = "%Y-%m-%d hh:mm:ss")
data1 = data1 %>% 
  mutate(BOARD_DATE = ymd(BOARD_DATE)) %>% 
  mutate_at(vars(BOARD_DATE), funs(year, month, day))
summary(data1)

# table of total species caught per year 
table1 <- data1 %>%
  group_by(year, SPECCD_ID) %>%
  summarise(numcaught = sum(EST_NUM_CAUGHT), wt = sum(EST_DISCARD_WT)) 
table1
write.table(table1,file="SABMPAspectableallyears.csv", sep=",")

table1a<-data1 %>% # individuals by spec by year
  group_by(SPECCD_ID, year) %>% 
  summarise(N = sum(EST_NUM_CAUGHT)) %>% 
  spread(year, N)
table1a
write.table(table1a,file="SABMPAspectableallyears_by_spec.csv", sep=",")


specplot <- ggplot(table1 %>% filter(numcaught > 99)) +
  geom_point(aes(x = year, y = numcaught, col = SPECCD_ID), size = 4, shape = "triangle") + 
  geom_line(aes(x = year, y = numcaught, col = SPECCD_ID)) +
  xlab("Year") +
  ylab("Number Captured") + 
  theme_bw(12) +
  scale_x_continuous(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  ggtitle("St Anns Bank MPA Captured Species (2004-2020)")
specplot


# species caught per year, for current year
table2 <-data1 %>%
  filter(year == "2019") %>%
  group_by(SPECCD_ID) %>%
  summarise(total = sum(EST_NUM_CAUGHT), wt = sum(EST_DISCARD_WT))
table2

write.table(table2,file="SABMPAspectable.csv", sep=",")

#----stomach sample details----
require(ROracle)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)
sabstom=dbGetQuery(con, ("SELECT stom.trip, stom.board_date, stom.set_no, sets.station, 
stom.speccd_id, stom.fish_no
FROM sncrabsets sets, snstomachdetails stom
WHERE sets.trip = stom.trip
AND sets.trip = stom.trip
And sets.set_no = stom.set_no
AND sets.setcd_id = '11'
order by trip, board_date, station, speccd_id"))



str(sabstom)
#separate date
sabstom$BOARD_DATE <- as.Date(sabstom$BOARD_DATE, format = "%Y-%m-%d hh:mm:ss")
sabstom = sabstom %>% 
  mutate(BOARD_DATE = ymd(BOARD_DATE)) %>% 
  mutate_at(vars(BOARD_DATE), funs(year, month, day))
summary(sabstom)

t1<-distinct(sabstom)# removes duplicates if there are any


# number of stomach samples per year, for current year, month is selected to only
#get the SABMPA samples
table3 <-t1 %>%
  filter(year == '2019', month == '9') %>%
  group_by(STATION) %>%
  count(SPECCD_ID)
table3
write.table(table3,file="SABMPAstombystation_spec_table.csv", sep=",")

# to get total SABMPA stomachs for current year
table4 <-t1 %>%
  filter(year == '2019', month == '9') %>%
  count(year) 
table4
write.table(table4,file="SABMPAstom_total.csv", sep=",")

#to get total number of stomach samples inside the SABMPA
table5in <-t1 %>%
  filter(year == '2019', month == '9', STATION %in% c('016', '018', '029', '032', '034', '091', '093', '511', '609')) %>%
  count(STATION)
table5in
write.table(table5in,file="SABMPAstom_in_by_tow_table.csv", sep=",")

table5in2 <-t1 %>%
  filter(year == '2019', month == '9', STATION %in% c('016', '018', '029', '032', '034', '091', '093', '511', '609')) %>%
  count(year)
table5in2
write.table(table5in2,file="SABMPAstom_in_total_table.csv", sep=",")

#to get total number of stomach samples taken adjecent to the SABMPA
table5out <-t1 %>%
  filter(year == '2019', STATION %in% c('036', '051', '204', '206', '507')) %>%
  count(STATION)
table5out
write.table(table5out,file="SABMPAstom_out_by_tow_table.csv", sep=",")

table5out2 <-t1 %>%
  filter(STATION %in% c('036', '051', '204', '206', '507'), year == '2019') %>%
  count(year)
table5out2
write.table(table5out2,file="SABMPAstom_out_total_table.csv", sep=",")

}
