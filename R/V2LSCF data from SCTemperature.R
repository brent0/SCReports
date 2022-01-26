# ---- SET UP ----
V2LSCF.SCtemp.data = function (current_year) {
  
  dir = file.path("S:", "TemperatureData")
  fig.fn = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "acoustic")
  if(!dir.exists(fig.fn))(dir.create(fig.fn, recursive = TRUE))
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
  
  
  library(marmap)
  library(oce)
  library(ocedata)
  library(mapdata)
  # get bathymetry data
  b = getNOAA.bathy(lon1 = -66.1, lon2 = -56.6, lat1 = 42.8, lat2 = 47.4, resolution = 1)
  data("coastlineWorldFine")
  #bathyLon = as.numeric(rownames(b))
  #bathyLat = as.numeric(colnames(b))
  #bathyZ = as.numeric(b)
  #dim(bathyZ) = dim(b)
  # convert bathymetry to data frame
  bf = fortify.bathy(b)
  
  # get regional polygons
  reg = map_data("world2Hires")
  reg = subset(reg, region %in% c('Canada', 'USA'))
  
  # convert lat longs
  reg$long = (360 - reg$long)*-1
  
  
  
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
    dplyr::mutate(T_DATE = ymd(T_DATE)) %>% 
    dplyr::mutate_at(vars(T_DATE), funs(year, month, day))
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
    dplyr::group_by(PID) %>% 
    dplyr::summarise(N=n())
  
  
  summary(temp)# look for outliers, odd location data, missing data (NA)
  glimpse(temp)
  
  
  
  #----Map of station locations by OTN names and by PID which has been corrected for location----
  
  
  
  loc<-temp %>% 
    dplyr::group_by(LINE, PID, LAT_DD, LON_DD) %>% 
    dplyr::summarise(N=n())
  
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  class(world)
  
  map<-ggplot(data = world) +#this one plots location of receivers by OTN station name.  This confirms that the station names are not consistent with locations
    # add 100m contour
    geom_contour(data = bf, 
                 aes(x=x, y=y, z=z),
                 breaks=c(-100),
                 size=c(0.3),
                 colour="grey")+
    
    # add 250m contour
    geom_contour(data = bf, 
                 aes(x=x, y=y, z=z),
                 breaks=c(-250),
                 size=c(0.6),
                 colour="grey")+
    
    # add coastline
    geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
                 fill = "darkgrey", color = NA) + 
    
    geom_point(data = temp, aes(x = LON_DD, y = LAT_DD, color= factor(PID)), size = 2, 
               shape = 1) +
    coord_sf(xlim = c(-60.3, -58), ylim = c(45.5, 46.5), expand = TRUE)+
    labs(x="Longitude", y="Latitude")+
    theme(plot.caption = element_text(hjust = 0))+
    theme(legend.title = element_blank())+
    theme(plot.caption = element_text(size = 8))+
    theme_bw()
  map
  
  fn = file.path(fig.fn, "SAB.locations.pdf")
  ggsave(filename = fn, device = "pdf", width = 12, height = 12)
  
  
  
  
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
  fn = file.path(fig.fn, "day.plot.all.pdf")
  
  dayplot <- ggplot(dailymeandat) +
    geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE))
  dayplot
  
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  
  
  
  fn = file.path(fig.fn, "day.plot.currentyear.pdf")
  startTime <- as.Date(paste(current_year-1, "-01-01", sep = ""))
  endTime <- as.Date(paste(current_year, "-01-01", sep = ""))
  start.end.c <- c(startTime,endTime)
  
  dayplot <- ggplot(dailymeandat) +
    geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE)) +
    (scale_x_date(limits=start.end.c))
  dayplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  fn = file.path(fig.fn, "day.plot.prevyear.pdf")
  startTime <- as.Date(paste(current_year-2, "-01-01", sep = ""))
  endTime <- as.Date(paste(current_year-1, "-01-01", sep = ""))
  start.end.p <- c(startTime,endTime)
  
  dayplot <- ggplot(dailymeandat) +
    geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE)) +
    (scale_x_date(limits=start.end.c))
  dayplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  fn = file.path(fig.fn, "monthplot.plot.all.pdf")
  monthplot <- ggplot(monthlymeandat) +
    geom_point(aes(x = yearmon, y = monthly_AVG, col = LINE))+
    geom_line(aes(x = yearmon, y = monthly_AVG, col = LINE))
  monthplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  #plot1 <- ggplot(dailymeandat) + #basic plot
  #  geom_point(aes(x = T_DATE, y = DAILY_AVG, col = LINE))
  #plot1
  
  monthlymeandat.c <- monthlymeandat %>%
    filter(yearmon >= as.yearmon(start.end.c[1]) & yearmon <= as.yearmon(start.end.c[2]))
  
  fn = file.path(fig.fn, "monthplot.plot.all.2.c.pdf")
  monthlineplot<-ggplot(monthlymeandat.c, aes( y=monthly_AVG, x= yearmon, col = LINE)) + 
    geom_line() +
    geom_point()+
    # ggtitle("Average monthly temperature for North and South lines in the V2LSCF receiver array.") +
    ylab(NULL)+
    xlab(NULL)+
    scale_color_manual(values=c('#0072B2','#D55E00'))
  
  monthlineplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  monthlymeandat.c <- monthlymeandat %>%
    filter(yearmon >= as.yearmon(start.end.p[1]) & yearmon <= as.yearmon(start.end.p[2]))
  
  fn = file.path(fig.fn, "monthplot.plot.all.2.p.pdf")
  monthlineplot<-ggplot(monthlymeandat.c, aes( y=monthly_AVG, x= yearmon, col = LINE)) + 
    geom_line() +
    geom_point()+
    # ggtitle("Average monthly temperature for North and South lines in the V2LSCF receiver array.") +
    ylab(NULL)+
    xlab(NULL)+
    scale_color_manual(values=c('#0072B2','#D55E00'))
  
  monthlineplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  
  fn = file.path(fig.fn, "by.station.month.pdf")
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
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
}


