# ---- SET UP ----
V2LSCF.temp.data = function (current_year) {
getwd()
setwd("S:/TemperatureData")

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)



# ---- Import Data from OTN ----

t201620<-read.csv("V2LSCF_2020/V2LSCF_temp_data_2016_to_2020.csv")
str(t201620)
summary(t201620)



#1.1 ---- Evaluate and format temperature file ----
names(t201620)
levels(t201620$date)
levels(t201620$description)
levels(t201620$deploy_date)
levels(t201620$station_name)


# to make OTN station names consistent since they differ from year to year
t201620$station_name[t201620$station_name == "V2LSCF-N1"] <- "N101"
t201620$station_name[t201620$station_name == "V2LSCF-N2"] <- "N102"
t201620$station_name[t201620$station_name == "V2LSCF-N3"] <- "N103"
t201620$station_name[t201620$station_name == "V2LSCF-N4"] <- "N104"
t201620$station_name[t201620$station_name == "V2LSCF-N5"] <- "N105"
t201620$station_name[t201620$station_name == "V2LSCF-N6"] <- "N106"
t201620$station_name[t201620$station_name == "V2LSCF-N7"] <- "N107"
t201620$station_name[t201620$station_name == "V2LSCF-N8"] <- "N108"
t201620$station_name[t201620$station_name == "V2LSCF-N9"] <- "N109"
t201620$station_name[t201620$station_name == "V2LSCF-N10"] <- "N110"
t201620$station_name[t201620$station_name == "V2LSCF-N11"] <- "N111"
t201620$station_name[t201620$station_name == "V2LSCF-N12"] <- "N112"
t201620$station_name[t201620$station_name == "V2LSCF-N13"] <- "N113"
t201620$station_name[t201620$station_name == "V2LSCF-N14"] <- "N114"
t201620$station_name[t201620$station_name == "V2LSCF-N15"] <- "N115"
t201620$station_name[t201620$station_name == "V2LSCF-N16"] <- "N116"
t201620$station_name[t201620$station_name == "V2LSCF-N17"] <- "N117"
t201620$station_name[t201620$station_name == "V2LSCF-N18"] <- "N118"
t201620$station_name[t201620$station_name == "V2LSCF-N19"] <- "N119"
t201620$station_name[t201620$station_name == "V2LSCF-N20"] <- "N120"
t201620$station_name[t201620$station_name == "V2LSCF-N21"] <- "N121"
t201620$station_name[t201620$station_name == "V2LSCF-N22"] <- "N122"
t201620$station_name[t201620$station_name == "V2LSCF-N23"] <- "N123"
t201620$station_name[t201620$station_name == "V2LSCF-S1"] <- "S101"
t201620$station_name[t201620$station_name == "V2LSCF-S2"] <- "S102"
t201620$station_name[t201620$station_name == "V2LSCF-S3"] <- "S103"
t201620$station_name[t201620$station_name == "V2LSCF-S4"] <- "S104"
t201620$station_name[t201620$station_name == "V2LSCF-S5"] <- "S105"
t201620$station_name[t201620$station_name == "V2LSCF-S6"] <- "S106"
t201620$station_name[t201620$station_name == "V2LSCF-S7"] <- "S107"
t201620$station_name[t201620$station_name == "V2LSCF-S8"] <- "S108"
t201620$station_name[t201620$station_name == "V2LSCF-S9"] <- "S109"
t201620$station_name[t201620$station_name == "V2LSCF-S10"] <- "S110"
t201620$station_name[t201620$station_name == "V2LSCF-S11"] <- "S111"
t201620$station_name[t201620$station_name == "V2LSCF-S12"] <- "S112"
t201620$station_name[t201620$station_name == "V2LSCF-S13"] <- "S113"
t201620$station_name[t201620$station_name == "V2LSCF-S14"] <- "S114"
t201620$station_name[t201620$station_name == "V2LSCF-S15"] <- "S115"
t201620$station_name[t201620$station_name == "V2LSCF-S16"] <- "S116"
t201620$station_name[t201620$station_name == "V2LSCF-S17"] <- "S117"
t201620$station_name[t201620$station_name == "V2LSCF-S18"] <- "S118"
t201620$station_name[t201620$station_name == "V2LSCF-S19"] <- "S119"
t201620$station_name[t201620$station_name == "V2LSCF-S20"] <- "S120"
t201620$station_name[t201620$station_name == "V2LSCF-S21"] <- "S121"
t201620$station_name[t201620$station_name == "V2LSCF-S22"] <- "S122"
t201620$station_name[t201620$station_name == "V2LSCF-S23"] <- "S123"
t201620$station_name[t201620$station_name == "N22"] <- "N122"
t201620$station_name[t201620$station_name == "N23"] <- "N123"



levels(t201620$station_name)
levels(t201620$description)

t201620$date <- as.Date(t201620$date, format = "%Y-%m-%d")
#t201620$recover_date <- as.Date(t201620$recover_date, format = "%Y-%m-%d")
#t201620$deploy_date <- as.Date(t201620$deploy_date, format = "%Y-%m-%d")

str(t201620)

summt201620<- t201620 %>% 
  filter(date > '2018-08-06') %>% 
  group_by(station_name, description) %>% 
  summarise(N=n())

#reassign description name for consistency and ability to pull out daily temperatures
t201620$description[t201620$description == "ambient_deg_c"] <- "Temperature"
t201620$description[t201620$description == "Average temperature"] <- "Temperature"
t201620$description[t201620$description == "ambient_mean_deg_c"] <- "Temperature"
t201620$description[t201620$description == "ambient_temperature_deg_c"] <- "Temperature"

summary(t201620)# look for outliers, odd location data, missing data (NA)
glimpse(t201620)



#----to determine how much data is available----
x<-t201620 %>%
group_by(station_name, description = "Temperature") %>% 
  summarise(N=n())#select Temperature or ambient_deg_c
  



# ---- to select annual data to load to SCTemperature.  average daily temperature per receiver----

temp2020data <- data2020 %>% 
  group_by(station_name, date, receiver, description, units, rcv_serial_no, deploy_date, recover_date, recover_ind, dep_lat, dep_long, the_geom, catalognumber) %>% 
  summarise(avg_temp = mean(data))
            
View(temp2020data)


col_order<- c("station_name", "date", "receiver", "description", "avg_temp", "units", "rcv_serial_no", "deploy_date", "recover_date", "recover_ind", "dep_lat", "dep_long", "the_geom", "catalognumber")
V2LSCFdata2020 <- temp2020data[, col_order]

names(V2LSCFdata2020)[5]<-"data"

write.csv(V2LSCFdata2020,'S:/TemperatureData/Entry 2020/V2LSCFdata2020.csv')

#V2LSCFdata2020<-read.csv("V2LSCFdata2020.csv")

#V2LSCFdata2020<-read.csv("V2LSCFdata2020.csv")
summary(V2LSCFdata2020)

#---- to correct station naming based on location----

recdata2020<-read.csv("V2LSCF_2020/v2lscf_2020-08_receiver_metadata.csv")
summary(recdata2020)

names(recdata2020)[2]<-"station_name"
names(recdata2020)[6]<-"dep_lat"
names(recdata2020)[7]<-"dep_long"
names(recdata2020)[11]<-"rcv_serial_no"


recdata2020$deploy_date <- as.Date(recdata2020$deploy_date, "%m/%d/%Y")

#----Map of station locations by OTN names and by CORRECTED_STATION_NO----

library(googleway) 
library(ggrepel) 
library(ggspatial)   
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

loc<-tdata %>% 
  group_by(CORRECTED_STATION_NO, dep_lat.y, dep_long.y) %>% 
  summarise(N=n())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

map<-ggplot(data = world) +#this one plots location of receivers by OTN station name.  This confirms that the station names are not consistent with locations
  geom_sf() +
  geom_point(data = recdata2020, aes(x = dep_long, y = dep_lat, color= factor(station_name)), size = 2, 
             shape = 1) +
  coord_sf(xlim = c(-59.3, -58.75), ylim = c(45.99, 46.3), expand = FALSE)+
  labs(x="Longitude", y="Latitude", caption = "Figure.")+
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.title = element_blank())+
  theme(plot.caption = element_text(size = 12))
map

mapfix<-ggplot(data = world) +
  geom_sf() +
  geom_point(data = loc, aes(x = dep_long.y, y = dep_lat.y, color= factor(CORRECTED_STATION_NO)), size = 1, 
             shape = 1) +
  coord_sf(xlim = c(-59.3, -58.75), ylim = c(45.99, 46.3), expand = FALSE)+
  labs(x="Longitude", y="Latitude", caption = "Figure.")+
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.title = element_blank())+
  theme(plot.caption = element_text(size = 12))
mapfix

#----join two files to get reassigned station name with temperature data by station

testnull<- t201620%>% #to see what records don't match between files
  anti_join(recdata2020, by = c("station_name" = "station_name", "rcv_serial_no" = "rcv_serial_no"))

summary(testnull)
levels(testnull$station_name)

tdata<- t201620 %>%# to join the receiver and data files
  left_join(recdata2020, by = c("station_name" = "station_name", "rcv_serial_no" = "rcv_serial_no"))

tdata <- as_tibble(tdata)
str(tdata)
summary(tdata)

# ---- write formatted file if necessary ----


data2020<- tdata %>% 
  filter(description == "Temperature", date > "2018-06-08") %>% 
  group_by(CORRECTED_STATION_NO, date, dep_lat.y, dep_long.y) %>% 
  summarise(mean = mean(data))
 
dailyavg<- tdata %>% # to join the receiver and data files
  filter(description == "Temperature", date > "2018-06-08") %>% 
  left_join(data2020, by = c("CORRECTED_STATION_NO" = "CORRECTED_STATION_NO", "date" = "date", "dep_lat.y" = "dep_lat.y", "dep_long.y" = "dep_long.y"))


dailyavg<- dailyavg %>% 
  select(CORRECTED_STATION_NO, date, receiver, description, mean, units, rcv_serial_no, deploy_date.y, recover_date, recover_ind, dep_lat.y, dep_long.y,  the_geom, catalognumber)


names(dailyavg)[1]<-"station_name"
names(dailyavg)[8]<-"deploy_date"
names(dailyavg)[11]<-"dep_lat"
names(dailyavg)[12]<-"dep_long"
names(dailyavg)[5]<-"data"

#data2020$date <- as.Date(data2020$date, "%m/%d/%Y %H:%M:%S")

d2020<-distinct(dailyavg) #check for duplicates
d2020<-d2020 %>% filter(date > '2018-06-10')

d2020<- d2020 %>% filter(date < '2020-08-22')
d2020<- d2020 %>% filter(description == 'Temperature')
d2020<- d2020 %>% filter(date != '2019-08-07')

summary(d2020)
glimpse(d2020)

check<-d2020 %>% 
  group_by(station_name, date) %>% 
  summarize(n())

write.csv(d2020,'V2LSCFdata2020.csv', row.names = F)

#V2LSCFdata2020<-read.csv("V2LSCFdata2020.csv")
#############################################################
#V2LSCFdata2020<-read.csv("S:/TemperatureData/Entry 2020/V2LSCFdata2020.csv")
#V2LSCFdata2020$deploy_date <- as.Date(V2LSCFdata2020$deploy_date, format = "%Y-%m-%d")
#V2LSCFdata2020$recover_date <- as.Date(V2LSCFdata2020$recover_date, format = "%Y-%m-%d")
#V2LSCFdata2020$date <- as.Date(V2LSCFdata2020$date, format = "%Y-%m-%d")

#write.csv(V2LSCFdata2020,'V2LSCFdata2020.csv', row.names = F)
#write.csv(V2LSCFdata2020,'S:/TemperatureData/Entry 2020/V2LSCFdata2020.csv', row.names = F)




#----various checks for data to ensure using correct records----
x1<-tdata %>%
  group_by(CORRECTED_STATION_NO, description = "Temperature") %>% 
  summarise(N=n())#select Temperature or ambient_deg_c

check<-tdata %>% 
  group_by(CORRECTED_STATION_NO, LINE, description) %>% 
summarise(N=n())
check

check3<-tdata %>% 
  filter(date < '2020-08-22')

summary(check3)

check2<- plotdata %>% 
  filter(CORRECTED_STATION_NO == "N123")

plot <- ggplot(check2) +
  geom_point(aes(x = date, y = avg_temp, col = CORRECTED_STATION_NO))
plot

# ---- to select data by station and by line (North, South) by month ----

tdata<- tdata %>% #removes records with temp over 10
  filter(data < 10)


plotdata<- tdata %>% 
  select(LINE, CORRECTED_STATION_NO, description, date, data) %>% 
  group_by(LINE, CORRECTED_STATION_NO, description, date) %>% 
  filter(description == 'Temperature') %>% 
  summarise(avg_temp = mean(data))


levels(plotdata)
summary(plotdata)

plotdata2<- plotdata %>% 
 group_by(LINE, date) %>% 
  summarise(avg_temp = mean(avg_temp))


#---create line column in case required----
str(plotdata)
#these lines are unnecessary when using the receiver file to assign line
#plotdata$line[plotdata$line == "N101"] <- "N"
#plotdata$line[plotdata$line == "N102"] <- "N"
#plotdata$line[plotdata$line == "N103"] <- "N"
#plotdata$line[plotdata$line == "N104"] <- "N"
#plotdata$line[plotdata$line == "N105"] <- "N"
#plotdata$line[plotdata$line == "N106"] <- "N"
#plotdata$line[plotdata$line == "N107"] <- "N"
#plotdata$line[plotdata$line == "N108"] <- "N"
#plotdata$line[plotdata$line == "N109"] <- "N"
#plotdata$line[plotdata$line == "N110"] <- "N"
#plotdata$line[plotdata$line == "N111"] <- "N"
#plotdata$line[plotdata$line == "N112"] <- "N"
#plotdata$line[plotdata$line == "N113"] <- "N"
#plotdata$line[plotdata$line == "N114"] <- "N"
#plotdata$line[plotdata$line == "N115"] <- "N"
#plotdata$line[plotdata$line == "N116"] <- "N"
#plotdata$line[plotdata$line == "N117"] <- "N"
#plotdata$line[plotdata$line == "N118"] <- "N"
#plotdata$line[plotdata$line == "N119"] <- "N"
#plotdata$line[plotdata$line == "N120"] <- "N"
#plotdata$line[plotdata$line == "N121"] <- "N"
#plotdata$line[plotdata$line == "N122"] <- "N"
#plotdata$line[plotdata$line == "N123"] <- "N"
#plotdata$line[plotdata$line == "S101"] <- "S"
#plotdata$line[plotdata$line == "S102"] <- "S"
#plotdata$line[plotdata$line == "S103"] <- "S"
#plotdata$line[plotdata$line == "S104"] <- "S"
#plotdata$line[plotdata$line == "S105"] <- "S"
#plotdata$line[plotdata$line == "S106"] <- "S"
#plotdata$line[plotdata$line == "S107"] <- "S"
#plotdata$line[plotdata$line == "S108"] <- "S"
#plotdata$line[plotdata$line == "S109"] <- "S"
#plotdata$line[plotdata$line == "S110"] <- "S"
#plotdata$line[plotdata$line == "S111"] <- "S"
#plotdata$line[plotdata$line == "S112"] <- "S"
#plotdata$line[plotdata$line == "S113"] <- "S"
#plotdata$line[plotdata$line == "S114"] <- "S"
#plotdata$line[plotdata$line == "S115"] <- "S"
#plotdata$line[plotdata$line == "S116"] <- "S"
#plotdata$line[plotdata$line == "S117"] <- "S"
#plotdata$line[plotdata$line == "S118"] <- "S"
#plotdata$line[plotdata$line == "S119"] <- "S"
#plotdata$line[plotdata$line == "S120"] <- "S"
#plotdata$line[plotdata$line == "S121"] <- "S"
#plotdata$line[plotdata$line == "S122"] <- "S"
#plotdata$line[plotdata$line == "S123"] <- "S"


#---create monthly plots by line or station----

plot1 <- ggplot(plotdata) + #basic plot
  geom_point(aes(x = date, y = avg_temp, col = LINE))
plot1

monthplotdata = plotdata %>% 
  mutate_at(vars(date), funs(year, month, day)) %>% 
  group_by(LINE, year, month) %>% 
  summarise(avg_temp = mean(avg_temp))

monthplotdata2 <- monthplotdata %>%
  group_by(LINE, year, month) %>%
  mutate(Date = format(lubridate::ymd(paste(year,month,"01"))))
monthplotdata2$Date<-as.Date(as.character(monthplotdata2$Date))

monthlineplot<-ggplot(monthplotdata2, aes( y=avg_temp, x= Date, col = LINE)) + 
  geom_line() +
  geom_point()+
 # ggtitle("Average monthly temperature for North and South lines in the V2LSCF receiver array.") +
  ylab(NULL)+
  xlab(NULL)+
  scale_color_manual(values=c('#0072B2','#D55E00'))

monthlineplot+scale_x_date(name=NULL, date_breaks="6 month", minor_breaks=NULL, date_labels="%b-%Y")+
  theme(legend.title = element_blank(), legend.text = element_text(size = 14), legend.position= c(.93,.93))


#by station by month---
monthplotdata3 <- plotdata %>% 
  mutate_at(vars(date), funs(year, month, day)) %>% 
  group_by(CORRECTED_STATION_NO, year, month) %>% 
  summarise(avg_temp = mean(avg_temp))

monthplotdata4 <- monthplotdata3 %>%
  group_by(CORRECTED_STATION_NO, year, month) %>%
  mutate(Date = format(lubridate::ymd(paste(year,month,"01"))))

monthplotdata4$Date<-as.Date(as.character(monthplotdata4$Date))

monthtempplot<-ggplot(monthplotdata4, aes( y=avg_temp, x= Date, col = CORRECTED_STATION_NO)) + 
  geom_line() +
  ggtitle("Average monthly temperature .") +
  ylab(NULL)+
  xlab(NULL)

monthtempplot+scale_x_date(name=NULL, date_breaks="3 month", minor_breaks=NULL, date_labels="%b-%Y")+
  theme(legend.title = element_blank(), legend.text = element_text(size = 14))
monthtempplot  


}