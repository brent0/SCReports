# ---- SET UP ----
V2LSCF.summary.data = function (current_year, redo = F) {
  dir = file.path("S:", "TemperatureData")
  fig.fn = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "acoustic")
  if(!dir.exists(fig.fn))(dir.create(fig.fn, recursive = TRUE))
  getwd()
  setwd("S:/Research Projects/MPA/SABsummary/summary data/qualified detections")
  
  library(dplyr)
  
  library(ggplot2)
  library(ggforce)
  library(tidyr)
  library(data.table)
  library(scales)
  library(tidyverse) 
  library(ggrepel) 
  library(formattable)
  library(viridis)
  library(hrbrthemes)
  library(zoo)
  library(xtable)   
  library(stargazer) 
  library(pander) 
  library(tables) 
  
  if(redo){
    # ---- Import Data ----
    # Import complete file if not updating data
    
    d2015<-read.csv("v2lscf_qualified_detections_2015redo.csv")
    d2016<-read.csv("v2lscf_qualified_detections_2016update2.csv")
    d2017<-read.csv("v2lscf_qualified_detections_2017update.csv")
    d2018<-read.csv("v2lscf_qualified_detections_2018update.csv")
    d2019<-read.csv("v2lscf_qualified_detections_2019update.csv")
    d2020<-read.csv("v2lscf_qualified_detections_2020update.csv")
    d2021<-read.csv("v2lscf_qualified_detections_2021.csv")
    past<-read.csv("pastworktocompare2.csv")
    
    # past1<- past %>% 
    #   select(catalognumber, fieldnumber, Project, Common.Name)
    
    #names(past1)[3]<-"Common.Name2"
    
    
    
    #1.1 ---- Evaluate catch file ----
    names(d2015)
    names(d2016)
    names(d2017)
    names(d2018)
    names(d2019)
    names(d2020)
    
   # names(d2020)[1]<-"basisofrecord"
    
    if(any(grepl("ï..basisofrecord", names(d2015))))names(d2015)[which(names(d2015) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2016))))names(d2016)[which(names(d2016) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2017))))names(d2017)[which(names(d2017) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2018))))names(d2018)[which(names(d2018) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2019))))names(d2019)[which(names(d2019) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2020))))names(d2020)[which(names(d2020) == "ï..basisofrecord")] = "basisofrecord"
    if(any(grepl("ï..basisofrecord", names(d2021))))names(d2021)[which(names(d2021) == "ï..basisofrecord")] = "basisofrecord"
    
    
   
    
    test<-rbind(setDT(past),setDT(d2015), setDT(d2016), setDT(d2017), setDT(d2018), setDT(d2019), setDT(d2020), setDT(d2021), fill=TRUE)
    names(test)
    
    levels(test$collectornumber)
    
    
    
    glimpse(test) # evaluate structure
    
    
    
    #----trying to keep past tag identification work by merging files---- 
    
   complete = test
    
    
    
    distinct(complete) #check for duplicates
    str(complete)
    summary(complete)
    
    glimpse(complete)
    head(complete)
    tail(complete)
    
    #write.csv(test,'S:/Research Projects/MPA/SABsummary/summary data/V2LSCF_compiled_up_to_2022.csv')
    
    
    write.csv(complete,'S:/Research Projects/MPA/SABsummary/summary data/V2LSCF_complete_up_to_Jan2022.csv')
  }
  
  options(dplyr.summarise.inform = FALSE)
  #----start here to load data that is complete and formatted----
  complete<-read.csv('S:/Research Projects/MPA/SABsummary/summary data/V2LSCF_complete_up_to_Jan2022.csv')
  
  #---- prepare tables ----
  #summarize to get count of species
  if(any(grepl("trackercode", names(complete))))names(complete)[which(names(complete) == "trackercode")] = "Project"
  complete$scientificname[which(complete$scientificname == "Gadus morhua")] = "Atlantic Cod"
  complete$scientificname[which(complete$scientificname == "Anarhichas lupus")] = "Atlantic Striped Wolffish"
  complete$scientificname[which(complete$scientificname == "Prionace glauca")] = "Blue Shark"
  complete$scientificname[which(complete$scientificname == "Lamna nasus")] = "Porbeagle Shark"
  complete$scientificname[which(complete$scientificname == "Microgadus tomcod")] = "Atlantic Tomcod"
  complete$scientificname[which(complete$scientificname == "Amblyraja radiata")] = "Thorny Skate"
  if(any(grepl("scientificname", names(complete))))names(complete)[which(names(complete) == "scientificname")] = "Common.Name"
  
  write.csv(complete,'S:/Research Projects/MPA/SABsummary/summary data/V2LSCF_complete_cleanup.csv')
  complete<-read.csv('S:/Research Projects/MPA/SABsummary/summary data/V2LSCF_complete_cleanup.csv')
  
  
  table1 <- complete %>% dplyr::group_by(Project) %>%
    dplyr::summarise(INDIVIDUALS= n_distinct(fieldnumber))
  
  
  table1$Project <- as.character(table1$Project)
  table1$Project[table1$Project == "Unknown"] <- "UNKNOWN (BDK, PBN, PBU)"
  
  table1 = formattable(table1, align = c("l", rep("r", NCOL(table1) - 1)))
  
  table1
   
  
  table2<-complete %>%
    dplyr::group_by(Project, Common.Name) %>%
    dplyr::summarise(Individuals =n_distinct(fieldnumber))
  
  table2a <- as_tibble(table2)
  table2a$Project <- as.character(table2a$Project)
  table2a$Individuals <- as.character(table2a$Individuals)
  
  table2b<-table2a %>% 
    add_row(Project = "BDK -Bras d'Or Atlantic Salmon", Common.Name = "Atlantic Salmon",  Individuals = "*") %>% 
    add_row(Project = "PBN - NOAA Penobscot Salmon Tracking", Common.Name = "Atlantic Salmon",  Individuals = "*") %>% 
    add_row(Project = "PBU -USGS Salmon", Common.Name = "Atlantic Salmon",  Individuals = "*")
  
  #detailed table of project and individuals
  table2b$Project <- as.character(table2b$Project)
  table2b$Project[table2b$Project == "ASF"] <- "ASF - Atlantic Salmon Federation"
  table2b$Project[table2b$Project == "BDK"] <- "BDK -Bras d'Or Atlantic Salmon"
  table2b$Project[table2b$Project == "BQEEL"] <- "BQEEL - Bay of Quinte Eel Tracking"
  table2b$Project[table2b$Project == "BFTTAG"] <- "BFTTAG-Estimating Bluefin tuna post release mortality**"
  table2b$Project[table2b$Project == "JPBS"] <- "JPBS - DFO Porbeagle and Blue Shark"
  table2b$Project[table2b$Project == "KNE"] <- "KNE -NOAA Kennebec Salmon Tracking"
  table2b$Project[table2b$Project == "MARSS"] <- "MARSS -Maritimes Region Atlantic Salmon"
  table2b$Project[table2b$Project == "MFSAR"] <- "MFSAR-Shark Spatial Ecology and Life History in NL Waters**"
  table2b$Project[table2b$Project == "MMFSRP"] <- "MMFSRP - MA Maine Fisheries Shark Research Program"
  table2b$Project[table2b$Project == "NARR"] <- "NARR -Survival of Stocked Smolts Maine USA"
  table2b$Project[table2b$Project == "NSBS"] <- "NSBS -OTN Blue Shark"
  table2b$Project[table2b$Project == "PBN"] <- "PBN - NOAA Penobscot Salmon Tracking"
  table2b$Project[table2b$Project == "PBU"] <- "PBU -USGS Salmon"
  table2b$Project[table2b$Project == "QMS"] <- "QMS-Quebec MDDEFP Atlantic Sturgeon Tagging**"
  table2b$Project[table2b$Project == "SABMPA"] <- "SABMPA -St. Anns Bank Species Tracking Study"
  table2b$Project[table2b$Project == "SGS"] <- "SGS -OTN Canada Sable Island Grey Seal Bioprobes"
  table2b$Project[table2b$Project == "SPI"] <- "SPI - Shippagan Cod Tagging"
  table2b$Project[table2b$Project == "TAG"] <- "TAG -Tag a Giant"
  table2b$Project[table2b$Project == "ZSC"] <- "ZSC- Scotian Shelf Snow Crab Tagging**"
  table2b$Project[table2b$Project == "ESRF"] <- "ESRF - Atlantic Salmon Offshore Tracking Project"
  table2b$Project[table2b$Project == "ACT.PROJ179"] <- "ACT.PROJ179 - Massachusetts White Shark Research Program"
  table2b$Project[table2b$Project == "ACT.PROJ129"] <- "ACT.PROJ129 - New York Juvenile White Shark Study"
  table2b$Project[table2b$Project == "FACT.CCOCE"] <- "FACT.CCOCE - Movements and migration of sharks in the Western North Atlantic Ocean"
  table2b$Project[table2b$Project == "BDLSPG"] <- "Apoqnmatulti’k - Use of Mi'kmaw, Local, and Western knowledge to study aquatic species in Bras d'Or Lake"
  table2b$Project[table2b$Project == "MIGRAMAR.LBTPCR"] <- "Leatherback Turtle Tracking in Pacuare Wildlife Reserve, CR"
  table2b$Project[table2b$Project == "V2LNSSA"] <- "Nova Scotia Salmon Association, Sheet Harbour"
  table2b$Project[table2b$Project == "V2LWSAMP"] <- "White Shark Acoustic Monitoring Program"
  names(table2b)[1]<-"PROJECT"
  names(table2b)[2]<-"SPECIES"
  names(table2b)[3]<-"INDIVIDUALS"
  formattable(table2b, align = c("l", rep("r", NCOL(table2b) - 1)))
  
  table3<-complete %>% # species detected
    dplyr::group_by(Common.Name) %>%
    dplyr::summarise(n_distinct(fieldnumber))
  
  names(table3)[1]<-"SPECIES"
  names(table3)[2]<-"INDIVIDUALS"
  formattable(table3, align = c("l", rep("r", NCOL(table3) - 1)))
  
  
  table4<-complete %>% #species by project and the number of detections
    dplyr::group_by(Common.Name, Project) %>%
    dplyr::summarise(N = n())
  names(table4)[1]<-"SPECIES"
  names(table4)[2]<-"PROJECT"
  names(table4)[3]<-"DETECTIONS"
  formattable(table4, align = c("l", rep("r", NCOL(table4) - 1)))
  
  
  
  table5<-complete %>% # individuals by project by year
    dplyr::group_by(Project, yearcollected) %>% 
    dplyr::summarise(N = n_distinct(fieldnumber)) %>% 
    spread(yearcollected, N)
  
  names(table5)[1]<-"PROJECT"
  formattable(table5, align = c("l", rep("r", NCOL(table5) - 1)))
  
  names(table5) = names(table5)
  table5a <- as_tibble(table5)
  #table5a$Project <- as.character(table5a$Project)
  table5a$PROJECT[table5a$PROJECT == "Unknown"] <- "UNKNOWN (BDK, PBN, PBU)"
  formattable(table5a, align = c("l", rep("r", NCOL(table5a) - 1)))
  
  names(table5a)[1]<-"PROJECT"
  
  table6<-complete %>% # returning individuals by project by year
    dplyr::group_by(fieldnumber, Common.Name, Project, yearcollected) %>% 
    dplyr::summarise(N = n()) %>% 
    mutate(freq = n_distinct(yearcollected)) %>% 
    filter(freq > 1) %>% 
    select(Common.Name, Project, yearcollected, N) %>% 
    spread(yearcollected, N)
  formattable(table6)
  names(table6)[3]<-"PROJECT"
  names(table6)[2]<-"SPECIES"
  names(table6)[1]<-"TAG ID"
  
  
  
  
  #---- charts/plots-----
  
  piedata <- complete %>%
    dplyr::group_by(Common.Name) %>%
    dplyr::summarise(count = n_distinct(fieldnumber)) %>% 
    arrange(desc(count))
  
  piedata$Label <-paste(round(((piedata$count/sum(piedata$count))*100),2),"%")
  
  ####
  piedata <- piedata %>% 
    mutate(end = 2 * pi * cumsum(count)/sum(count),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  fn = file.path(fig.fn, "pie.data.pdf")
  
  plot1<-ggplot(piedata) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Common.Name)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Label,
                  hjust = hjust, vjust = vjust)) +
    coord_fixed() +
    scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL)+
    theme(legend.title = element_blank())
  
  plot1
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  fn = file.path(fig.fn, "bar.data.pdf")
  ####second way to show data
  
  plot2 <- ggplot(data = piedata, aes(x = Common.Name, y = count)) +
    geom_col(aes(fill = Common.Name) , show.legend = FALSE) +
    ggtitle(paste("Species Frequency")) +
    coord_flip() +
    geom_label(aes(label = paste(count, percent(count/sum(count)), sep = "\n"),
                   y = -15, fill = Common.Name),
               show.legend = FALSE,
               size = 3.25, label.padding = unit(0.1, "lines")) +
    expand_limits(y = -15) +
    scale_fill_brewer(palette = "Set3", direction = -1) 
  
  plot2
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
  
  #histogram of species by month
  x1 <- complete %>%
    dplyr::group_by(Common.Name, yearcollected, monthcollected) %>%
    dplyr::summarise(count = n_distinct(fieldnumber))
  
  x1$monthcollected[x1$monthcollected == "1"] <- "01"
  x1$monthcollected[x1$monthcollected == "2"] <- "02"
  x1$monthcollected[x1$monthcollected == "3"] <- "03"
  x1$monthcollected[x1$monthcollected == "4"] <- "04"
  x1$monthcollected[x1$monthcollected == "5"] <- "05"
  x1$monthcollected[x1$monthcollected == "6"] <- "06"
  x1$monthcollected[x1$monthcollected == "7"] <- "07"
  x1$monthcollected[x1$monthcollected == "8"] <- "08"
  x1$monthcollected[x1$monthcollected == "9"] <- "09"
  x1$yearcollected <- as.character(as.numeric(x1$yearcollected))
  
  
  x1$Date <- as.yearmon(paste(x1$yearcollected, x1$monthcollected), "%Y %m")
  
  
  x1test <- x1 %>%
    dplyr::group_by(Common.Name, yearcollected, monthcollected) %>%
    dplyr::summarise(count) %>% 
    mutate(Date = format(lubridate::ymd(paste(yearcollected,monthcollected,"01"))))
  
  x1test$Date<-as.Date(as.character(x1test$Date))
  
  fn = file.path(fig.fn, "month.data.pdf")
  library(RColorBrewer)
  nb.cols <- length(unique(x1test$Common.Name))
  mycolors <- colorRampPalette(brewer.pal(5, "Set3"))(nb.cols)
  speciesplot<-ggplot(x1test, aes(fill=Common.Name, y=count, x= Date)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(values = mycolors)  +
    ggtitle("Individual species occurrances by month for the duration of\n      the deployment of the V2LSCF receiver line.") +
    labs(x="Year", y="Individuals Detected")+
    theme_bw() 
  speciesplot  
  ggsave(speciesplot, filename = fn, device = "pdf", width = 7, height = 5)
  
  fn = file.path(fig.fn, "month.data2.pdf")
  
  speciesplot+scale_x_date(name=NULL, date_breaks="3 month", minor_breaks=NULL, date_labels="%b-%Y")+
    theme(legend.title = element_blank(), legend.text = element_text(size = 14))
  speciesplot
  ggsave(filename = fn, device = "pdf", width = 7, height = 5)
}
