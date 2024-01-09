require(bio.utilities)
require(aegis)
require(devEMF)
require(bio.utilities)
con= dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)

lic = ROracle::dbGetQuery(con, "select * from MARFISSCI.LICENCE_AREAS")

north = which( lic$AREA_ID %in% c(304, 305, 306, 615, 619) )
cfa23 = which( lic$AREA_ID %in% c(307, 790, 791, 792, 921, 1058) )
cfa24 = which( lic$AREA_ID %in% c(794, 795, 1059, 308, 793, 616) )
cfa4X = which( lic$AREA_ID %in% c(27, 620) )

lic$cfa0 = NA
lic$cfa0 [north] = "north"
lic$cfa0 [cfa23] = "cfa23"
lic$cfa0 [cfa24] = "cfa24"
lic$cfa0 [cfa4X] = "cfa4X"

lic0 = lic[ which(!is.na(lic$cfa0)) , ]
lic0 = lic0[ - which(duplicated( lic0$LICENCE_ID) ) ,]



obs = ROracle::dbGetQuery(con, ("SELECT * FROM SNCRABSETS_OBS where BOARD_DATE > TO_DATE('01-10-2018', 'DD-MM-YYYY')"))


#remove sets that are not from standard sampling
# these include forced C&P trips, moving traps, etc
obs=obs[as.character(obs$SETCD_ID)=="1",]


#----------------------------------------
# create columns for area and CFA

obs$lat = obs$LATITUDE
obs$lon = obs$LONGITUDE
obs$lon = obs$lon*-1

cfa=c("cfanorth", "cfa23", "cfa24", "cfa4x")
obs$cfa=NA
for  (a in cfa){
  rowindex= filter.region.polygon(obs,recode.areas(a))
  obs$cfa[rowindex]=a
}

#Force some as bad positional data
obs$cfa[which(obs$TRIP == "J21-0008")] = "cfa24"
obs$cfa[which(obs$TRIP == "J22-0014A")] = "cfa23"
obs$cfa[which(obs$TRIP == "J20-0261A")] = "cfa24"
obs$cfa[which(obs$TRIP == "J19-0207E")] = "cfa23"
obs$cfa[which(obs$TRIP == "J19-0024B")] = "cfa24"

yrs=as.character(sort(as.numeric(unique(as.character(obs$year)))))
obs$year=factor(obs$year,levels=yrs, labels=yrs)


#Following lines force 4X into proper dataset
# i.e. Winter 2011 gets grouped under 2010 "Season"

i4x=which(obs$cfa %in% "4X")

obs$BOARD_DATE[i4x] = obs$BOARD_DATE[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
obs$year=as.character(lubridate::year(obs$BOARD_DATE))        #determine year
obs$BOARD_DATE[i4x] = obs$BOARD_DATE[i4x] + lubridate::days(91) #add the days back


#Choose most recent year and create directory
iy=max(as.numeric(as.character(obs$year)))

# --------------------------------------
# divide into north, south, cfa 23, cfa 24, and 4X as required

yrs=c(iy, iy-1, iy-2, iy-3, iy-4)

cfas=c("cfanorth", "cfasouth", "cfa23", "cfa24", "cfa4x")
out=NULL

for (y in yrs){
  for (c in cfas){
    
    cc=c
    if (c=="cfasouth") c=c("cfa23", "cfa24")
    
    iyear=which(obs$year==y)
    iarea= which(obs$cfa %in% c)
    
    j=obs[intersect(iyear,iarea) ,]
    
    # determine total mt observed by area
    observedmt= (sum(j$EST_KEPT_WT, na.rm=T)/1000)
    observedmt
    
    # determine # of traps sampled
    sampledtraps= length(j$SET_NO)
    sampledtraps
    
    # --------------------------------------
    # determine # of traps observed (bycatch, landings, etc)
    
    observedtraps= sum(j$NUM_HOOK_HAUL, na.rm=T)
    observedtraps
    
   out=rbind(out,c(y,cc, observedmt, sampledtraps, observedtraps))
  }
}
out=as.data.frame(out)



names(out)=c("year", "cfa", "Observed Mt", "Traps Sampled", "Traps Observed")



logbook= ROracle::dbGetQuery(con, " SELECT * FROM MARFISSCI.MARFIS_CRAB MARFIS_CRAB WHERE year > 2017 AND (MARFIS_CRAB.TARGET_SPC=705)")


logs = merge(logbook, lic0, by="LICENCE_ID", all.x=T, all.y=F, sort=F)
logs = logs[ ! is.na( logs$cfa0 ),]
logs$lbspertrap=logs$PRO_RATED_SLIP_WT_LBS/logs$NUM_OF_TRAPS

logs$q=quarters(logs$DATE_LANDED)
logs=logs[logs$q!="QNA",]
logs$q=factor(logs$q)

#Following lines force 4X into proper dataset
# i.e. Winter 2011 gets grouped under 2010 "Season"
i4x=which(logs$cfa0=="cfa4X")

logs$logs$DATE_LANDED[i4x] = logs$DATE_LANDED[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
logs$YEAR=as.character(lubridate::year(logs$logs$DATE_LANDED))        #determine year
logs$DATE_LANDED[i4x] = logs$logs$DATE_LANDED[i4x] + lubridate::days(91) #add the days back
logs$year_q=paste(logs$YEAR, logs$q, sep=":")

areas = unique(logs$cfa0)

year.land=as.data.frame(xtabs(logs$PRO_RATED_SLIP_WT_LBS~logs$YEAR+logs$cfa0), stringsAsFactors=F)
names(year.land)=c("year","cfa", "lbs")
year.land$mt=NA
year.land$mt=(year.land$lbs)/2204.626
year.land$cfa[which(year.land$cfa == "north")] = "cfanorth"
year.land$cfa[which(year.land$cfa == "cfa4X")] = "cfa4x"

#to limit temporal extent
yrs=c(iy, iy-1, iy-2, iy-3, iy-4)
#
year.land= year.land[year.land$year %in% yrs,]
uniyear = unique(year.land$year)
for(i in 1:length(uniyear)){


  tlbs = as.numeric(year.land$lbs[which(year.land$year == uniyear[i] & year.land$cfa == "cfa23")]) + as.numeric(year.land$lbs[which(year.land$year == uniyear[i] & year.land$cfa == "cfa24")])

  tkgs = as.numeric(year.land$mt[which(year.land$year == uniyear[i] & year.land$cfa == "cfa23")]) + as.numeric(year.land$mt[which(year.land$year == uniyear[i] & year.land$cfa == "cfa24")])

  year.land = rbind(year.land, c(uniyear[i], "cfasouth", tlbs, tkgs) )
  
}

merged = merge(out, year.land, by = c("year","cfa"))



#merged$Observed=round(as.numeric(as.character(out$Observed)))
merged$Percent=as.numeric(merged$'Observed Mt')/as.numeric(merged$mt)*100
merged$lbs = NULL
names(merged)=c("Year", "Area", "Observed (mt)", "Traps Sampled", "Traps Observed", "Landings (mt)", "Percent Observed")

merged=merged[order(merged$Year, merged$Area),]
write.csv(merged, "observed.percents.csv")
