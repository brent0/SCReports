# ---- SET UP ----
V2LSCF.SCtemp.data = function (current_year) {

dir = file.path("S:", "TemperatureData")

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)
library(googleway) 
library(ggrepel) 
library(ggspatial)   
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tibble)
library(zoo)



# ---- Import Data from SCTemperature ----

require(ROracle)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)

#----select data using a modified sql view script----
dat=dbGetQuery(con, ("SELECT *
FROM SABMPA_TEMPERATURE
WHERE PROJECT = 'V2LSCF'
"))


#1.1 ---- Evaluate and format temperature data ----
temp<-dat

temp$T_DATE <- as.Date(temp$T_DATE, format = "%Y-%m-%d")
temp = temp %>% 
  mutate(T_DATE = ymd(T_DATE)) %>% 
  mutate_at(vars(T_DATE), funs(year, month, day))
summary(temp)

temp$yearmon <- as.yearmon(paste(temp$year, temp$month), "%Y %m")

temp <-add_column(temp, 'Line', .after = "PID")

names(temp)[names(temp) == '"Line"'] <- 'LINE'

temp$LINE <- ifelse(temp$LAT_DD > 46.1, 'North', ifelse(temp$LAT_DD < 46.1, 'South', 'check'))
temp$LINE = as.factor(as.character(temp$LINE))

levels(temp$LINE)

names(temp)
temp$PID = as.factor(as.character(temp$PID))
levels(temp$PID) # to check PID to be sure data can be grouped

#----to determine how much data is available----

summ<- temp %>% 
  group_by(PID) %>% 
  dplyr::summarise(N=n())


summary(temp)# look for outliers, odd location data, missing data (NA)
glimpse(temp)



#----Map of station locations by OTN names and by PID which has been corrected for location----



loc<-temp %>% 
  group_by(LINE, PID, LAT_DD, LON_DD) %>% 
  dplyr::summarise(N=n())



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

map<-ggplot(data = world) +#this one plots location of receivers by OTN station name.  This confirms that the station names are not consistent with locations
  geom_sf() +
  geom_point(data = temp, aes(x = LON_DD, y = LAT_DD, color= factor(PID)), size = 2, 
             shape = 1) +
  coord_sf(xlim = c(-59.3, -58.75), ylim = c(45.99, 46.3), expand = FALSE)+
  labs(x="Longitude", y="Latitude", caption = "Figure.")+
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.title = element_blank())+
  theme(plot.caption = element_text(size = 12))
map




# ---- write daily average and monthly average file if necessary ----


dailymeandat<- temp %>% 
  dplyr::group_by(LINE, T_DATE) %>% 
  dplyr::summarise(DAILY_AVG = mean(TEMP))
 
monthlymeandat<- temp %>% 
  dplyr::group_by(LINE, yearmon) %>% 
  dplyr::summarise(monthly_AVG = mean(TEMP))


dailymeandat<-distinct(dailymeandat) #check for duplicates
monthlymeandat<-distinct(monthlymeandat) #check for duplicates

#write.csv(dailymeandat,'V2LSCF_dailyaveragetemp.csv', row.names = F)

# ----create daily and monthly plots by line or station----

dayplot <- ggplot(dailymeandat) +
  geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE))
dayplot

monthplot <- ggplot(monthlymeandat) +
  geom_point(aes(x = yearmon, y = monthly_AVG, col = LINE))+
  geom_line(aes(x = yearmon, y = monthly_AVG, col = LINE))
monthplot


#plot1 <- ggplot(dailymeandat) + #basic plot
#  geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE))
#plot1


monthlineplot<-ggplot(monthlymeandat, aes( y=monthly_AVG, x= yearmon, col = LINE)) + 
  geom_line() +
  geom_point()+
 # ggtitle("Average monthly temperature for North and South lines in the V2LSCF receiver array.") +
  ylab(NULL)+
  xlab(NULL)+
  scale_color_manual(values=c('#0072B2','#D55E00'))
monthlineplot

# ----by station by month----
sm <- temp %>% 
  dplyr::group_by(PID, yearmon) %>% 
  dplyr::summarise(avg_temp = mean(TEMP))


#monthly temperature by station number
monthtempplot<-ggplot(sm, aes( y=avg_temp, x= yearmon, col = PID)) + 
  geom_line() +
  ggtitle("Average monthly temperature .") +
  ylab(NULL)+
  xlab(NULL)
monthtempplot

}


