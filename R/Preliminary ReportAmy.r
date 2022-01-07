
Amy.Preliminary.survey.data = function (current_year) {
require(chron)
require(oce)

wd="C:/Rsaves/2020"
dir.create(wd)
setwd(wd)

###### For January 2018- add length freq's and a measure of trends for bycatch rather than just a snapshot
#--------------------------------------------
#Create a map of survey stations for past year

# Call variable required by PBS Mapping
source("S:/R/Ben.Scripts/functions/snow.crab.mapping.functions.r")
source("S:/R/Ben.Scripts/functions/ben.mapplots.r")

#All sets
require(ROracle)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)

all=dbGetQuery(con, ("SELECT * FROM SNOWCRAB.SNCRABSETS SNCRABSETS WHERE (SNCRABSETS.HAULCCD_ID=1)"))

require(lubridate)

all$yr=NA
all$yr=lubridate::year(all$BOARD_DATE-2592000) #takes 30 days off date before determing tear- January survey finishes
names(all)=tolower(names(all))
all$X=-all$start_long       
all$Y=all$start_lat
all$EID=1:nrow(all)
all=as.EventData(all, projection="LL")


#Create a folder for current year (if one doesn't exist)

iy=as.character(max(as.numeric(all$yr)))
q=paste("C:/Rsaves/survey/bycatch", iy,sep="/")
dir.create(q)
wd=q
setwd(q)






# All crab with bcd and positional information
#bcd=dbGetQuery(con,("SELECT SNCRABDETAILS.TRIP_ID, SNCRABDETAILS.TRIP, SNCRABDETAILS.BOARD_DATE,
# SNCRABDETAILS.SET_NO, SNCRABDETAILS.EST_NUM_CAUGHT, SNCRABDETAILS.EST_DISCARD_WT,
#  SNCRABDETAILS.FISH_NO, SNCRABDETAILS.SEXCD_ID, SNCRABDETAILS.FISH_LENGTH,
#   SNCRABDETAILS.MEASURED_WGT, SNCRABDETAILS.CALC_WGT, SNCRABDETAILS.FEMALE_ABDOMEN,
#    SNCRABDETAILS.CHELA_HEIGHT, SNCRABDETAILS.MATURITY_CD, SNCRABDETAILS.SHELLCOND_CD,
#     SNCRABDETAILS.GONADE_CD, SNCRABDETAILS.EGGCOLOR_CD, SNCRABDETAILS.EGGPERCENT_CD,
#      SNCRABDETAILS.DUROMETRE, SNCRABDETAILS.BCD, SNCRABDETAILS.MISSING_LEGS, SNCRABSETS.START_LAT,
#       SNCRABSETS.START_LONG FROM SNOWCRAB.SNCRABDETAILS SNCRABDETAILS, SNOWCRAB.SNCRABSETS SNCRABSETS
#WHERE SNCRABDETAILS.TRIP = SNCRABSETS.TRIP AND SNCRABDETAILS.SET_NO = SNCRABSETS.SET_NO AND ((SNCRABDETAILS.BCD=1))"))


require(chron)



#Differentiate Sets with no snow crab in catch
none=all[all$est_catch==0.0000001,]


yr=max(all$yr)
      
    last=all[all$yr==(yr-1),]
    a=all[all$yr==yr,]
    n=none[none$yr==yr,]

# Creates map of all survey stations for a given year.  2017 has ly plotted under this year to show the stations not completed.

          # PDF  
            filename=paste(yr," ", "All Survey Stations", ".pdf", sep="")
            pdf(file=filename)
              makemap(a, area="all", title=paste(yr, "Snow Crab Survey", sep=" "))
           #   addPoints(data=last, col='coral2', pch=16, cex=.7)
            #  points(x=-58.5, y=42.8,col='coral2', pch=16, cex=.9)
             # text( paste(yr,"Stations Not Completed",sep=' '), x=-58.5, y=42.8, cex=0.65, pos=4)
              
              addPoints(data=a, col="green", pch=16, cex=.9)
              points(x=-58.5, y=43.2,col="green", pch=16, cex=.9)
              text( paste(yr,"Stations Completed",sep=' '), x=-58.5, y=43.2, cex=0.65, pos=4) 
              
              addPoints(data=n, col="black", pch=16, cex=.9)
              points(x=-58.5, y=43,col="black", pch=16, cex=.9)
              text(paste(yr, "Station w/ No Snow Crab"), x=-58.5, y=43, cex=0.65, pos=4) 
              
            
          dev.off()
          print(paste("Find file here: ", wd, "/",filename,sep=""))

          # Windows Metafile
          filename=paste(yr," ", "All Survey Stations", ".emf", sep="")
            win.metafile(file=filename, width=10)
            makemap(a, area="all", title=paste(yr, "Snow Crab Survey", sep=" "))
           
            #addPoints(data=last, col='coral2', pch=16, cex=.7)
            #points(x=-58.5, y=42.8,col='coral2', pch=16, cex=.9)
            #text( paste(yr,"Stations Not Completed",sep=' '), x=-58.5, y=42.8, cex=0.65, pos=4) 
            
            addPoints(data=a, col="green", pch=16, cex=.9)
            points(x=-58.5, y=43.2,col="green", pch=16, cex=.9)
            text( paste(yr,"Stations Completed",sep=' '), x=-58.5, y=43.2, cex=0.65, pos=4) 
            
            addPoints(data=n, col="black", pch=16, cex=.9)
            points(x=-58.5, y=43,col="black", pch=16, cex=.9)
            text(paste(yr, "Station w/ No Snow Crab"), x=-58.5, y=43, cex=0.65, pos=4) 
          
          dev.off()
          print(paste("Find file here: ", wd, "/",filename,sep=""))



#----------------------------------------
#Retrieve all survey bycatch information
#----------------------------------------
        

     
x= dbGetQuery(con, "
SELECT C.*, S.START_LAT, S.START_LONG, N.COMMON, N.SCIENTIFIC 
FROM SNCRABSETS S, SNTRAWLBYCATCH C, ISSPECIESCODES N
WHERE C.TRIP = S.TRIP
AND C.SET_NO = S.SET_NO
AND C.SPECCD_ID=N.SPECCD_ID" ) 

names(x) = c("trip_id", "trip", "date", "set", "lat", "long", "temp", "est_catch", "spec", "num", "wtkg","slat", "slong" , "common", "scientific")
x$name=NA
x$name=paste(x$common,x$spec, sep=":")
x$year=as.character(lubridate::year(x$date-2592000))
x$yr=as.factor(x$year)

#To plot most recent year

yrs=unique(x$year)
i=as.character(max(as.numeric(yrs)))
z=which(x$year==i)
y=x[z,]


#write.csv to save a copy of that year's bycatch record from survey in a csv file
write.csv(y, file=paste(y$year[1], "All Survey Bycatch.csv", sep=" "))

#remove all species names and then repopulate with desired name
# allows us to remove all species not of interest at end or at others to species of interest at a later date

y$name=NA
y$name[which(y$spec==10)]="Atlantic Cod" 
y$name[which(y$spec==40)]="American Plaice" 
y$name[which(y$spec==2511)]="Jonah Crab" 
y$name[which(y$spec==2211)]="Northern Shrimp" 
y$name[which(y$spec==2521)]="Lesser Toad Crab" 
y$name[which(y$spec==2513)]="Rock Crab" 
y$name[which(y$spec==41)]="Witch Flounder" 
y$name[which(y$spec==201)]="Thorny Skate" 
y$name[which(y$spec==30)]="Halibut"
y$name[which(y$spec==2523)]="Northern Stone Crab"
y$name=as.factor(y$name)
y=y[is.finite(y$name),]
y$name=as.character(y$name)

# Call variable required by PBS Mapping
source("S:/R/Ben.Scripts/functions/snow.crab.mapping.functions.r")
source("S:/R/Ben.Scripts/functions/ben.mapplots.r")

y$X=-y$long
y$Y=y$lat
y$EID=1:nrow(y)

#This displays values of selected species  for current year to allow a quick check

spec=unique(y$spec)

for (s in spec) {
    z=y[y$spec==s,]
    maxkg=max(as.numeric(z$wtkg))
    big=z[z$wtkg==maxkg,]
    print(paste(big$name, "(",big$spec,")", ": max kg =", maxkg, "on trip", big$trip, "in set", big$set))
    gc()
}

#------------------------
# The following code creates summary of catch by species by year (for past 10 years)
# This allows the creation of summary plots of catch by species

iyear=as.numeric(iy)
x=x[as.numeric(x$year)>(iyear-10),] #takes last 10 years of data
x$yr=as.factor(as.numeric(x$year)) #creates an ordered factor of year
x$tripset=paste(x$trip, x$set)

yearsum=as.data.frame(xtabs(wtkg~yr+spec, data=x)) #total weight by species by year
names(yearsum)=c("yr", "spec","totwtkg")
yearsum$sets=NA

setsum=aggregate(tripset~ year, x, function(x) length(unique(x))) #determines number of unique sets by year
###########################################
#BRENT -The following line wasnt generating the a station count for all entries for me. Only
#       the first 10
#yearsum$sets=setsum$tripset[as.character(yearsum$yr)==as.character(setsum$year)] #adds a column for numer of sets to yearsums df
#      -Changed to the loop below
##########################################
for(j in 1:length(setsum$year)){
  yearsum$sets[which(yearsum$yr == setsum$year[j])] = setsum$tripset[j]
}


yearsum$kgtow=yearsum$totwtkg/yearsum$sets

#-----------------------------------
# Following Code produces maps of various bycatch species (dealing with extreme highs) from survey for inclusion in industry report
spec=unique(y$spec)
###########################################
#BRENT - The function is calling yplot variable that are only being set from the yplot in memory.
#        Need to pass the yplot into the function through the x variable.
#p.inset=function(x){
#  plotInset(xleft=-66.8,ybottom = 45.85, xright=-63.5, ytop=47.45,
#            expr=plot(as.character(yplot$yr), yplot$kgtow, type="o", pch=21, lwd=2,ylab="kg/tow", xlab="", cex.lab= 0.8, cex.axis=.6, mgp=c(1.5, 0.4, 0)), 
#            mar = c(1, 0.75, 1, 0),debug = getOption("oceDebug"))
#}
##########################################
p.inset=function(x){
  plotInset(xleft=-66.8,ybottom = 45.85, xright=-63.5, ytop=47.45,
            expr=plot(as.character(x$yr), x$kgtow, type="o", pch=21, lwd=2,ylab="kg/tow", ylim=c(0, max(x$kgtow)),xlab="", cex.lab= 0.8, cex.axis=.6, mgp=c(1.5, 0.4, 0)), 
            mar = c(1, 0.75, 1, 0),debug = getOption("oceDebug"))
}
###########################################
#BRENT -
# Moved the sourceing of the r scripts out of the loop as we only need to put 
# them into memory one time
########################################
source("S:/R/Ben.Scripts/functions/snow.crab.mapping.functions.r")
source("S:/R/Ben.Scripts/functions/ben.mapplots.r")


for (s in spec) {
zz=y[y$spec==s,]

      filename=paste(zz$year[1]," ",zz$name[1], ".emf", sep="")
      #png(file=filename, width=869, height=823)
      win.metafile(file=filename, width=10)

    
      makemap(zz, area="all", title=paste(zz$year[1], zz$name[1], sep=" "))
      
      #determine weight at 98th quantile and separate df into 2 df's
      #One all value below 98th, one equal to or above
      high=quantile(zz$wtkg, .98, names=FALSE)
      z=zz[zz$wtkg<high,]
      top=zz[zz$wtkg>=high,]
      
      #plot most values (graduated)
      draw.bubble(z$X, z$Y,(z$wtkg+1), maxradius=0.12, pch=21, bg='red')
      max.size.point=0.15
      cex.max <- 2 * max.size.point/par("cxy")[2]/0.375
      
      #plot high outliers (fixed size)
      points(top$X, top$Y, pch=21, bg="red", cex=cex.max)
      
      #Draw legend
      rect(-57.5, 42.6, -56.3, 44.15, col='white') 
      text(x=-56.9, y=44.05, "Weight (kg)", col="black", cex=0.7)
      text(x=-56.9, y=43.9, paste("Max =", round(max(top$wtkg)), "kg",sep=" "), col="black", cex=0.5)
      
      legend.bubble(x=-57.2, y=43.65, maxradius=0.15, z=round(max(z$wtkg)), n=1,  
      txt.cex=0.001, pch=21, pt.bg='red', mab=1.5, bty="n")
      text(paste(">", round(high), sep=""), x=-56.65, y=43.65, cex=.8)
      
      legend.bubble(x=-57.2, y=43.3, maxradius=0.12, z=round(max(z$wtkg)), n=1,  
      txt.cex=0.001, pch=21, pt.bg='red', mab=1.5, bty="n")
      text(round(max(z$wtkg)), x=-56.6, y=43.3, cex=.8)
      
      if (as.numeric(round(max(z$wtkg)))>1){
          legend.bubble(x=-57.2, y=43.0, maxradius=0.08, z=round(.66*(max(z$wtkg))), n=1,  
          txt.cex=0.001, pch=21, pt.bg='red', mab=1.5, bty="n")
          text(round(.66*max(z$wtkg)), x=-56.6, y=43, cex=.8)
            }
      
      if (as.numeric(round(.66*max(z$wtkg)))>1){
          legend.bubble(x=-57.2, y=42.75, maxradius=0.04, z=round(.33*(max(z$wtkg))), n=1,  
          txt.cex=0.001, pch=21, pt.bg='red', mab=1.5, bty="n")
          text(round(.33*max(z$wtkg)), x=-56.6, y=42.75, cex=.8)
            }
      
      yplot=yearsum[yearsum$spec==s,]
      
      #add inset plot showing yearly kg/tow for the species of interest
      rect(-67.29, 45.75, -63.25, 47.45, col='white')
      
      #######
      #BRENT need to pass unique data to iset function
      #p.inset()
      p.inset(x=yplot)
     
      #par(fig =c(0,1,0,1), mar =c(1, 1, 1, 1))# to reset plot boundaries
      graphics.off()
      #dev.off()
      #dev.off()
      print(paste("Find file here: ", wd,"/", filename,sep=""))
}






###    Total Number and Total Weight of Each Bycatch Species by Year

compute.sums = function (x, var, index) {
 for (i in index) x[,i] = as.factor(x[,i])
 res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { sum(q, na.rm=T)}, simplify=T))
 names(res) = c(index, var)
 for (i in index) { res[,i] = as.character( res[,i] ) }
return(res) }

count=compute.sums(x=x,var="num", index=c("name", "year"))
weight=compute.sums(x=x,var="wtkg", index=c("name", "year"))

summation=merge(count, weight)
summation=summation[is.finite(summation$num),]

source("S:/R/Ben.Scripts/functions/bycatch.species.names.r")

yy=max(as.numeric(summation$year))
bycatch=summation[summation$year==yy,]
write.csv(bycatch, file=paste(yy, "Survey Bycatch.csv"))


#Crab Counts by year

allcrab=dbGetQuery(con, ("SELECT * from SNCRABDETAILS "))

require(chron)
allcrab$yr=NA
allcrab$yr=as.numeric(lubridate::year(allcrab$board_date-2592000))
names(allcrab)=tolower(names(allcrab))
xx=as.numeric(max(allcrab$yr))-12
late=allcrab[allcrab$yr>xx,] #remove early years
late=late[is.finite(as.numeric(late$fish_no)),]  #remove any "false crab" created by view

years=unique(late$yr)

out=data.frame(Year=NA, Total.Crab=NA, Male.Crab=NA, Female.Crab=NA, Comm.num=NA)

for (i in (1:length(years))){
    
      b=allcrab[allcrab$yr==years[i],]
      male=b[b$sexcd_id==1,]
      fem= b[b$sexcd_id==2,]
      large=male[male$fish_length>94 & is.finite(male$fish_length),]
      out[i,"Male.Crab"]=length(male$yr)
      out[i,"Female.Crab"]=length(fem$yr)
      out[i,"Total.Crab"]=length(male$yr)+ length(fem$yr)
      out[i,"Year"]=years[i]
      out[i,"Comm.num"]=length(large$yr)
      
}

out$perc.male= NA 
out$perc.male= paste(100*round(out$Male.Crab/out$Total.Crab, 2))

out$perc.comm= NA
out$perc.comm= paste(100* round(out$Comm.num/out$Male.Crab, 2))

names(out)=c("Year", "Total Crab", "Male Crab", "Female Crab", "Commercial Size", "% Male", "% Commercial") 
out

#send the out table to clipboard
write.table(out, "clipboard")
print("Catch summary is now on clipboard")

#setwd("c://Rsaves")

# --------------------------------------
# Query database


con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)


sets=dbGetQuery(con, ("select* FROM SNOWCRAB.SNCRABSETS") )
names(sets)=tolower(names(sets))

save(sets, file="C:\\Rsaves\\allsets.rdata", compress=T)
load("C:\\Rsaves\\allsets.rdata")


good=which(sets$haulccd_id<2)
sets=sets[good,]

sets$X=-(sets$start_long)
sets$Y=(sets$start_lat)
sets$EID=as.numeric(paste(1:nrow(sets),  sep=""))
sets$year=years(sets$board_date)

year="2018"
iyear=which(sets$year==paste(year))
setsyear=sets[iyear,]

require(PBSmapping)

x=setsyear
x=as.EventData(x, projection= "LL")






#we created a new map to show all stations and the ones with no crab in a different color
#Create maps with pbsMapping
#--------------------------------------------

source("S:/R/Ben.Scripts/functions/snow.crab.mapping.functions.r")


win.graph(width=10.5, height=7.5)
makemap(x, area="all", title=paste(x$yr[1], "Snow Crab Survey", sep=" "))

#need to plot last year's stations

addPoints(data=x, col="black", bg="green", pch=21, cex=0.8)  #chart showing all stations
points(x=-58, y=43.2,  col="black", bg="green", pch=21, cex=1.1) # add legend like key
text("Snow Crab", x= -57.9, y=43.2, font=2, cex=.85, pos=4)

nocrab=setsyear[setsyear$est_catch<.0000002,]
y=as.EventData(nocrab, projection= "LL")
addPoints(data=y, col="black", bg="black", pch=21, cex=0.8)  #add black points where no crab caught
points(x=-58, y=43, col="black", bg="black", pch=21, cex=1.1)  #add black points where no crab caught
text("No Snow Crab", x= -57.9, y=43, font=2, cex=.85, pos=4)

}

