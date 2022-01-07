Preliminary.survey.data = function (current_year) {

require(chron)
wd="C:/Rsaves/survey/bycatch/"
setwd(wd)

###### For January 2018- add length freq's and a measure of trends for bycatch rather than just a snapshot


#--------------------------------------------
#Create a map of survey stations for past year

# Call variable required by PBS Mapping
source("C:/Scripts/functions/snow.crab.mapping.functions.r")
source("C:/Scripts/functions/ben.mapplots.r")

#All sets
Require(ROracle)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)

all=dbGetQuery(con, ("SELECT * FROM SNOWCRAB.SNCRABSETS SNCRABSETS WHERE (SNCRABSETS.HAULCCD_ID=1)"))

require(chron)
all$yr=NA
all$yr=as.numeric(as.character((years(as.chron(all$BOARD_DATE)))))
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


#Differentiate Sets with no snow crab in catch
none=all[all$est_catch==0.0000001,]


# All crab with bcd and positional information
bcd=dbGetQuery(con, ("SELECT SNCRABDETAILS.TRIP_ID, SNCRABDETAILS.TRIP, SNCRABDETAILS.BOARD_DATE,
 SNCRABDETAILS.SET_NO, SNCRABDETAILS.EST_NUM_CAUGHT, SNCRABDETAILS.EST_DISCARD_WT,
  SNCRABDETAILS.FISH_NO, SNCRABDETAILS.SEXCD_ID, SNCRABDETAILS.FISH_LENGTH,
   SNCRABDETAILS.MEASURED_WGT, SNCRABDETAILS.CALC_WGT, SNCRABDETAILS.FEMALE_ABDOMEN,
    SNCRABDETAILS.CHELA_HEIGHT, SNCRABDETAILS.MATURITY_CD, SNCRABDETAILS.SHELLCOND_CD,
     SNCRABDETAILS.GONADE_CD, SNCRABDETAILS.EGGCOLOR_CD, SNCRABDETAILS.EGGPERCENT_CD,
      SNCRABDETAILS.DUROMETRE, SNCRABDETAILS.BCD, SNCRABDETAILS.MISSING_LEGS, SNCRABSETS.START_LAT,
       SNCRABSETS.START_LONG FROM SNOWCRAB.SNCRABDETAILS SNCRABDETAILS, SNOWCRAB.SNCRABSETS SNCRABSETS
WHERE SNCRABDETAILS.TRIP = SNCRABSETS.TRIP AND SNCRABDETAILS.SET_NO = SNCRABSETS.SET_NO AND ((SNCRABDETAILS.BCD=1))"))

require(chron)
bcd$yr=NA
names(bcd)=tolower(names(bcd))
bcd$yr=as.numeric(as.character((years(as.chron(bcd$board_date)))))
bcd$X=-bcd$start_long
bcd$Y=bcd$start_lat
bcd$EID=1:nrow(bcd)
bcd=as.EventData(bcd, projection="LL")


yr=max(bcd$yr)
      
    b=bcd[bcd$yr==yr,]
    a=all[all$yr==yr,]
    n=none[none$yr==yr,]

# Creates map of all survey stations for a given year
  filename=paste(yr," ", "All Survey Stations", ".pdf", sep="")
          #PDF
              pdf(file=filename)
              makemap(a, area="all", title=paste(yr, "Snow Crab Survey", sep=" "))
              addPoints(data=a, col="black", pch=20, cex=.8)
          dev.off()
          print(paste("Find file here: ", wd, filename,sep=""))
          
          #Windows Metafile
           filename=paste(yr," ", "All Survey Stations", ".emf", sep="")
              win.metafile(file=filename, width=10)
              makemap(a, area="all", title=paste(yr, "Snow Crab Survey", sep=" "))
              addPoints(data=a, col="black", pch=20, cex=1)
          dev.off()
          print(paste("Find file here: ", wd, filename,sep=""))


# Creates map of all BCD survey stations for a given year
          # PDF  
            filename=paste(yr," ", "Bitter Crab", ".pdf", sep="")
            pdf(file=filename)
              makemap(a, area="all", title=paste(yr, "Survey Locations with Bitter Crab Disease", sep=" "))
                addPoints(data=a, col="green", pch=20, cex=.6)
                points(x=-59, y=43.2,col="green", pch=20, cex=.6)
                text("Survey Station", x=-59, y=43.2, cex=0.65, pos=4) 
                addPoints(data=n, col="black", pch=20, cex=.6)
                points(x=-59, y=43,col="black", pch=20, cex=.6)
                text("Station w/ No Snow Crab", x=-59, y=43, cex=0.65, pos=4) 
                addPoints(data=b, col="red", pch=20, cex=.6)
                points(x=-59, y=42.8,col="red", pch=20, cex=.6)
                text("Station w/ Bitter Crab", x=-59, y=42.8, cex=0.65, pos=4) 
          
          dev.off()
          print(paste("Find file here: ", wd, "/",filename,sep=""))

          # Windows Metafile
          filename=paste(yr," ", "Bitter Crab", ".emf", sep="")
            win.metafile(file=filename, width=10)
              makemap(a, area="all", title=paste(yr, "Survey Locations with Bitter Crab Disease", sep=" "))
                addPoints(data=a, col="green", pch=20, cex=1.2)
                points(x=-58.5, y=43.2,col="green", pch=20, cex=1.2)
                text("Survey Station", x=-58.4, y=43.2, cex=0.8, pos=4) 
                addPoints(data=n, col="black", pch=20, cex=1.2)
                points(x=-58.5, y=43,col="black", pch=20, cex=1.2)
                text("Station w/ No Snow Crab", x=-58.4, y=43, cex=0.8, pos=4) 
                addPoints(data=b, col="red", pch=20, cex=1.2)
                points(x=-58.5, y=42.8,col="red", pch=20, cex=1.2)
                text("Station w/ Bitter Crab", x=-58.4, y=42.8, cex=0.8, pos=4) 
          
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

names(x) = c("trip_id", "trip", "date", "set", "est_catch", "spec", "num", "wtkg", "lat", "long", "common", "scientific")
x$name=NA
x$name=paste(x$common,x$spec, sep=":")
x$year=as.character(years(x$date))


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
source("C:/Scripts/functions/snow.crab.mapping.functions.r")
source("C:/Scripts/functions/ben.mapplots.r")


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

#-----------------------------------
# Following Code produces maps of various bycatch species (dealing with extreme highs) from survey for inclusion in industry report
spec=unique(y$spec)

for (s in spec) {

zz=y[y$spec==s,]

filename=paste(zz$year[1]," ",zz$name[1], ".emf", sep="")
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
dev.off()
print(paste("Find file here: ", wd,"/", filename,sep=""))
gc()
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

source("C:/Scripts/functions/bycatch.species.names.r")

yy=max(as.numeric(summation$year))
bycatch=summation[summation$year==yy,]
write.csv(bycatch, file=paste(yy, "Survey Bycatch.csv"))


#Crab Counts by year

allcrab=dbGetQuery(con, ("SELECT * from SNCRABDETAILS "))

require(chron)
allcrab$yr=NA
allcrab$yr=as.numeric(as.character((years(as.chron(allcrab$BOARD_DATE)))))
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

sets=dbGetQuery(con, ("select * FROM SNOWCRAB.SNCRABSETS") )
names(sets)=tolower(names(sets))

save(sets, file="C:\\Rsaves\\allsets.rdata", compress=T)
load("C:\\Rsaves\\allsets.rdata")


good=which(sets$haulccd_id<2)
sets=sets[good,]

sets$X=-(sets$start_long)
sets$Y=(sets$start_lat)
sets$EID=as.numeric(paste(1:nrow(sets),  sep=""))
sets$year=years(sets$board_date)

year="2011"
iyear=which(sets$year==paste(year))
setsyear=sets[iyear,]

require(PBSmapping)

x=setsyear
x=as.EventData(x, projection= "LL")


# Create maps with pbsMapping
#--------------------------------------------

source("C:/Scripts/snow.crab.mapping.functions.r")


win.graph(width=10.5, height=7.5)
makemap(x, area="all", title=paste(x$year[1], "Snow Crab Survey", sep=" "))

addPoints(data=x, col="black", bg="green", pch=21, cex=0.8)  #chart showing all stations
points(x=-58, y=43.2,  col="black", bg="green", pch=21, cex=1.1) # add legend like key
text("Snow Crab", x= -57.9, y=43.2, font=2, cex=.85, pos=4)

nocrab=setsyear[setsyear$est_catch<.0000002,]
y=as.EventData(nocrab, projection= "LL")
addPoints(data=y, col="black", bg="black", pch=21, cex=0.8)  #add black points where no crab caught
points(x=-58, y=43, col="black", bg="black", pch=21, cex=1.1)  #add black points where no crab caught
text("No Snow Crab", x= -57.9, y=43, font=2, cex=.85, pos=4)

# Get BCD (+) crab from db
#-------------------------------------

##con=odbcconnect("BANK.CANSO3", uid="snowcrab", pwd="0507wel")
bcd=dbGetQuery(con, ("SELECT SNCRABDETAILS.*,   SNCRABSETS.START_LAT,  SNCRABSETS.START_LONG,  SNCRABDETAILS.BCD
FROM SNCRABDETAILS
INNER JOIN SNCRABSETS
ON SNCRABDETAILS.TRIP    = SNCRABSETS.TRIP
AND SNCRABDETAILS.SET_NO = SNCRABSETS.SET_NO
WHERE SNCRABDETAILS.BCD  = 1") )

names(bcd)=tolower(names(bcd))
bcd$year=years(bcd$board_date)

bcd$X=-(bcd$start_long)
bcd$Y=(bcd$start_lat)
bcd$EID=as.numeric(paste(1:nrow(bcd),  sep=""))
bcd$year=years(bcd$board_date)


iyear=which(bcd$year==paste(year))
bcdyear=bcd[iyear,]

z=bcdyear
z=as.EventData(z, projection= "LL")

addPoints(data=z, col="black", bg="red", pch=21, cex=.8)  #add black points where no crab caught
points(x=-58, y=42.8,  col="black", bg="red", pch=21, cex=1.1) # add legend like key
text("Bitter Snow Crab", x= -57.9, y=42.8, font=2, cex=.85, pos=4)

}
