par.old = par()$mar
#NOTE:  This script has been modified on the fly to create tables..will need to be cleaned up for January meetings
January.industry.meeting.data = function (current_year) {
  
  if(!exists("current_year"))  current_year = year(now())
  
  #Create a folder to store all figures and tables
  wd=file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "fishery") 
  if(!dir.exists(wd))dir.create(wd, recursive = T)
  #  setwd(q)
  print(paste("All figures will be stored at: ",wd))
  
  
  #Only do grab database refresh if more than 24 hours since last query.
  rawfile=file.path(wd, paste("logbook.",current_year,".rdata", sep=""))
  update = F
  if(!file.exists(rawfile)) update = T
  if(!update)if(lubridate::hour(lubridate::as.period(lubridate::now() - file.info(rawfile)$mtime) ) > 24) update = T
  if(update){
    
    con= ROracle::dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, oracle.server)
    logbook= ROracle::dbGetQuery(con, " SELECT * FROM MARFISSCI.MARFIS_CRAB MARFIS_CRAB WHERE (MARFIS_CRAB.TARGET_SPC=705)")
    lic = ROracle::dbGetQuery(con, "select * from MARFISSCI.LICENCE_AREAS")
    obs = ROracle::dbGetQuery(con, ("SELECT * FROM SNCRABSETS_OBS"))
    setsobs=ROracle::dbGetQuery(con, ("SELECT * FROM SNCRABSETS_OBS, SNCRABDETAILS_OBS 
  WHERE SNCRABDETAILS_OBS.TRIP_ID = SNCRABSETS_OBS.TRIP_ID AND SNCRABDETAILS_OBS.SET_NO = SNCRABSETS_OBS.SET_NO"))
    
    lfobsquery=ROracle::dbGetQuery(con, ("SELECT SNCRABDETAILS_OBS.TRIP_ID, SNCRABDETAILS_OBS.TRIP,
    SNCRABDETAILS_OBS.BOARD_DATE, SNCRABSETS_OBS.PRODCD_ID, SNCRABDETAILS_OBS.SET_NO,
                              SNCRABDETAILS_OBS.FISH_NO, SNCRABDETAILS_OBS.SEXCD_ID,
                              SNCRABDETAILS_OBS.FISH_LENGTH,
                              SNCRABDETAILS_OBS.FEMALE_ABDOMEN,
                              SNCRABDETAILS_OBS.CHELA_HEIGHT,
                              SNCRABDETAILS_OBS.SHELLCOND_CD,
                              SNCRABDETAILS_OBS.DUROMETRE, SNCRABSETS_OBS.LATITUDE,
                              SNCRABSETS_OBS.LONGITUDE FROM SNOWCRAB.SNCRABDETAILS_OBS SNCRABDETAILS_OBS,
                              SNOWCRAB.SNCRABSETS_OBS SNCRABSETS_OBS WHERE SNCRABDETAILS_OBS.TRIP_ID = SNCRABSETS_OBS.TRIP_ID AND
                              SNCRABDETAILS_OBS.SET_NO = SNCRABSETS_OBS.SET_NO AND
                              (SNCRABDETAILS_OBS.FISH_NO Is Not Null) AND (SNCRABDETAILS_OBS.SHELLCOND_CD Is Not Null)"))
    
    all=ROracle::dbGetQuery(con, ("SELECT * FROM SNOWCRAB.SNCRABSETS SNCRABSETS WHERE (SNCRABSETS.HAULCCD_ID=1)"))
    
    
    names(logbook) = tolower( names(logbook) )
    names(lic) = tolower( names(lic) )
    
    save(logbook, file=file.path(wd, paste("logbook.",current_year,".rdata", sep="")), compress=T)
    save(lic, file=file.path(wd, paste("lic.",current_year,".rdata", sep="")), compress=T)
    save(obs, file=file.path(wd, paste("obs.",current_year,".rdata", sep="")), compress=T)
    save(setsobs, file=file.path(wd, paste("setsobs.",current_year,".rdata", sep="")), compress=T)
    save(lfobsquery, file=file.path(wd, paste("lfobsquery.",current_year,".rdata", sep="")), compress=T)
    save(all, file=file.path(wd, paste("all.",current_year,".rdata", sep="")), compress=T)
  }
  
  # load saved temporary files
  load(file=file.path(wd, paste("logbook.",current_year,".rdata", sep="")))
  load(file=file.path(wd, paste("lic.",current_year,".rdata", sep="")))
  load(file=file.path(wd, paste("obs.",current_year,".rdata", sep="")))
  load(file=file.path(wd, paste("setsobs.",current_year,".rdata", sep="")))
  load(file=file.path(wd, paste("lfobsquery.",current_year,".rdata", sep="")))
  load(file=file.path(wd, paste("all.",current_year,".rdata", sep="")))
  
  
  # State focus year
  #chooses most recent "full" year. Avoids being tripped up by a few 4X dates.
  #iy=max(logbook$year[which(logbook$cfa=="23")])
  # print(paste("The fishing season to be examined is: ",iy))
  iy = current_year
  
  ######
  # Removed for 2015 due to NA's in database
  
  ## manual pro-rate:
  #
  #      # fix problems with slip weights
  #      new.slip.sums =  as.data.frame.table( tapply( X=logbook$slip_weight_lbs, INDEX=logbook$doc_id, FUN   = function(q) { sum(unique(q)) },  simplify = T ))
  #      names(new.slip.sums ) = c("doc_id", "new.slip.wgt")
  #      new.slip.sums$doc_id = as.character(new.slip.sums$doc_id )
  #
  #      logbook = merge( x=logbook, y=new.slip.sums, by="doc_id", sort=F, all.x=T, all.y=F)
  #      i = which(( logbook$new.slip.wgt - logbook$slip_weight_lbs) != 0 )
  #      j = which( duplicated( logbook$log_efrt_std_info_id,  ))
  #      bad.data = intersect ( i,j )
  #	logbook = logbook[-bad.data ,]
  #
  #      logbook$slip_weight_lbs = logbook$new.slip.wgt
  #      logbook$new.slip.wgt = NULL
  #
  #      # counts
  #      pr.count = as.data.frame.table( tapply( X=logbook$log_efrt_std_info_id, INDEX = logbook$doc_id, FUN = function(q) { length(unique(q)) },  simplify =T  ))
  #	names(pr.count) = c("doc_id", "pr.count")
  #      pr.count$doc_id = as.character(pr.count$doc_id )
  #
  #      # sums
  #      pr.sum = as.data.frame.table( tapply( X=logbook$est_weight_log_lbs, INDEX = logbook$doc_id,
  #	FUN   = function(q) { sum(q) }, simplify = T ))
  #      names(pr.sum) = c("doc_id", "pr.sum")
  #      pr.sum$doc_id = as.character(pr.sum$doc_id )
  #
  #      # merge
  #      pr = merge(x=pr.count, y=pr.sum, bu="doc_id", sort=F, all=F)
  #      pr$doc_id = as.character(pr$doc_id )
  #
  #    	logbook = merge(x=logbook, y=pr, by="doc_id", all.x=T, all.y=F, sort=F)
  #	logbook$pr.fraction = logbook$est_weight_log_lbs / logbook$pr.sum
  #    	logbook$pro_rated_slip_wt_lbs = logbook$pr.fraction * logbook$slip_weight_lbs
  #
  #      # second pro-rating of data with missing values/nulls, etc
  #	na.logs = which (!is.finite ( logbook$pr.sum ) )
  #    	logbook$pro_rated_slip_wt_lbs[ na.logs ] = logbook$slip_weight_lbs[na.logs ] / logbook$pr.count [  na.logs]
  #     	bad.usage.codes = which ( !logbook$catch_usage_code%in% 10   )
  #    	good.usage.codes = which ( is.na(logbook$log_efrt_std_info_id )  )
  #      toDump = 	 setdiff(bad.usage.codes,good.usage.codes)
  #      logbook = logbook[-toDump, ]
  #
  #
  
  
  # licence information
  logs = logbook
  logs=merge(logs, lic, by="licence_id", all.x=T, all.y=T, sort=F)
  
  #Not sure why this is here but will keep in case one day it becomes apparent
  q308.i = which(logs$area_id==308)
  if (length (q308.i) >0) {
    q308 = logs[ q308.i,]
    r = which(q308$licence_id %in% c("100201", "100202", "301303"))
    q4x = q308[r,]
    lic.in.4X = unique(q4x$licence_id)
    lic.in.4X = setdiff( lic.in.4X, "152740" )
  }
  
  north = which( lic$area_id %in% c(304, 305, 306, 615, 619) )
  cfa23 = which( lic$area_id %in% c(307, 790, 791, 792, 921, 1058) )
  cfa24 = which( lic$area_id %in% c(794, 795, 1059, 308, 793, 616) )
  cfa4X = which( lic$area_id %in% c(27, 620) )
  
  lic$cfa0 = NA
  lic$cfa0 [north] = "north"
  lic$cfa0 [cfa23] = "cfa23"
  lic$cfa0 [cfa24] = "cfa24"
  lic$cfa0 [cfa4X] = "cfa4X"
  
  lic0 = lic[ which(!is.na(lic$cfa0)) , ]
  lic0 = lic0[ - which(duplicated( lic0$licence_id) ) ,]
  
  logs = merge(logbook, lic0, by="licence_id", all.x=T, all.y=F, sort=F)
  logs = logs[ ! is.na( logs$cfa0 ),]
  logs$lbspertrap=logs$pro_rated_slip_wt_lbs/logs$num_of_traps
  
  logs$q=quarters(logs$date_landed)
  logs=logs[logs$q!="QNA",]
  logs$q=factor(logs$q)
  
  #Following lines force 4X into proper dataset
  # i.e. Winter 2011 gets grouped under 2010 "Season"
  i4x=which(logs$cfa0=="cfa4X")
  
  logs$date_landed[i4x] = logs$date_landed[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
  logs$year=as.character(lubridate::year(logs$date_landed))        #determine year
  logs$date_landed[i4x] = logs$date_landed[i4x] + lubridate::days(91) #add the days back
  logs$year_q=paste(logs$year, logs$q, sep=":")
  
  save(logs, file=file.path(wd, paste("logs.",iy,".rdata", sep="")), compress=T)
  load(file=file.path(wd, paste("logs.",iy,".rdata", sep="")))
  
  #logs$area=logs$cfa0
  #logs$area[logs$cfa0=="cfa23"]="S-ENS"
  #logs$area[logs$cfa0=="cfa24"]="S-ENS"
  
  tables<-logs %>%
    dplyr::group_by(cfa0, year) %>%
    dplyr::summarise(landings = sum(pro_rated_slip_wt_lbs), effort = sum(num_of_traps))
  
  write.table(tables,file=file.path(wd, "tables.csv"), sep=",")
  
  #########################################
  # Landings
  #########################################
  
  #Calculate total landings by year by area
  #For some reason, Licence_ID "100198" gets sorted into 4X, need to force it into cfa24
  
  land=logs
  land$cfa0 [land$licence_id=="100198"]="cfa24"
  #
  land$cfa0 = as.character(land$cfa0)
  areas = unique(land$cfa0)
  land$julian = as.integer(julian(land$date_landed))
  
  year.land=as.data.frame(xtabs(land$pro_rated_slip_wt_lbs~land$year+land$cfa0), stringsAsFactors=F)
  names(year.land)=c("year","cfa", "lbs")
  year.land$mt=NA
  year.land$mt=(year.land$lbs)/2204.626
  
  year.land$cfa[which(year.land$cfa=="north")]="N-ENS"
  year.land$cfa[which(year.land$cfa=="cfa24")]="CFA 24"
  year.land$cfa[which(year.land$cfa=="cfa23")]="CFA 23"
  year.land$cfa[which(year.land$cfa=="cfa4X")]="CFA 4X"
  
  #to limit temporal extent
  yrs=c(iy, iy-1, iy-2, iy-3, iy-4, iy-5, iy-6, iy-7, iy-8, iy-9, iy-10, iy-11, iy-12, iy-13, iy-14, iy-15, iy-16, iy-17, iy-18)
  #
  year.land= year.land[year.land$year %in% yrs,]
  
  
  # For a given year, calculate landings by cfa and week of season
  #--------------------------------------
  
  iyear=which(logs$year==iy)
  landings= logs[iyear,]
  
  #To determine date of last reported landings by area for the year:
  cfas=unique(landings$cfa0)
  for (c in cfas){
    z=landings[landings$cfa0==c,]
    last=max(z$date_landed)
    print(paste(c,last, sep=":"))
  }
  
  #For some reason, Licence_ID "100198" gets sorted into 4X, need to force it into cfa24
  landings$cfa0 [landings$licence_id=="100198"]="cfa24"
  landings$cfa0 = as.character(landings$cfa0)
  areas = unique(landings$cfa0)
  landings$julian = as.integer(julian(landings$date_landed))
  landings$dayofseason= ((landings$julian)-(min(landings$julian)))+1
  landings$weekofseason=ceiling(landings$dayofseason/7)
  
  #Look at the landings by licence to compare to quota reports in case of inconsistencies between our landings and CDD's
  tot.lic=as.data.frame(xtabs(landings$pro_rated_slip_wt_lbs~landings$licence_id+landings$cfa0), stringsAsFactors=F)
  names(tot.lic)=c("licence","cfa", "total.lbs")
  tot.lic$tot.mt=NA
  tot.lic$tot.mt=tot.lic$total.lbs/2204.626
  tot.lic=tot.lic[(tot.lic$tot.mt)>0,]
  write.table(tot.lic,file=file.path(wd, "Landings by Licence.csv"), sep=",")
  
  totals=as.data.frame(xtabs(tot.lic$tot.mt~tot.lic$cfa), stringsAsFactors=F)
  names(totals)=c("cfa","mt")
  
  cfa23mt=totals$mt[totals$cfa=="cfa23"]
  cfa24mt=totals$mt[totals$cfa=="cfa24"]
  northmt=totals$mt[totals$cfa=="north"]
  cfa4xmt=totals$mt[totals$cfa=="cfa4x"]
  
  
  #Sum landings by area on a weekly basis
  #each area gets a unique starting point as season dates are not the same
  
  landings$weekfromstart = NA
  for (a in areas) {
    q = which (landings$cfa0 == a)
    t0 = min(landings$weekofseason[q], na.rm=T)
    landings$weekfromstart[q] = landings$weekofseason[q] - t0 +1 }
  
  crablandings = compute.sums( x=landings, var="pro_rated_slip_wt_lbs", index=c("weekfromstart", "cfa0")  )
  crablandings$totallandings=NA
  areaweeks=unique(crablandings$cfa0)
  crablandings = crablandings[ is.finite(crablandings$pro_rated_slip_wt_lbs) ,]
  
  for (a in areaweeks){
    q = which(crablandings$cfa0 == a)
    crablandings$totallandings [q]=cumsum(crablandings$pro_rated_slip_wt_lbs [q])
  }
  
  crablandings$mt=NA
  crablandings$mt=crablandings$totallandings/2204.626
  crablandings$year=median(as.numeric(landings$year))
  
  write.table(crablandings,file=file.path(wd, "Landings by Week.csv"), sep=",")
  
  
  #Create a plots showing landings by week for each area
  #------------------------------------------------------------------------------------------------
  
  #remove 4X
  #i=crablandings$cfa0%in%c("cfa23", "cfa24", "north", "cfa4x")
  #crablandings=crablandings[i,]
  
  crablandings$cfa0[crablandings$cfa0=="north"]="NENS"
  crablandings$cfa0[crablandings$cfa0=="cfa23"]="CFA23"
  crablandings$cfa0[crablandings$cfa0=="cfa24"]="CFA24"
  crablandings$cfa0[crablandings$cfa0=="cfa4x"]="CFA4X"
  
  
  cfa=unique(crablandings$cfa0)
  
  #create metafiles:
  
  for (c in cfa)   {
    filename=paste(c,"_weekly_landing", ".emf", sep="")
    win.metafile(file=file.path(wd, filename), width=10)
    
    toplot=crablandings[crablandings$cfa0==c,]
    plot=barplot(toplot$mt, main= paste(toplot$year[1],toplot$cfa0[1],"Landings", sep=" "),cex.main=1.2, xlab= "Week of Season",
                 ylab="Cumulative Landings (mt)", names.arg=toplot$weekfromstart, ylim=c(0,1.2*(max(toplot$mt))), col="deepskyblue")
    #text(plot, 0, round(toplot$mt,0), cex=1, pos=3) # Can remove "#" if you want values shown on barplot
    dev.off()
    print(paste(filename, " created", sep=""))
  }
  
  #create PDFs:
  
  for (c in cfa)   {
    filename=paste(c,"_weekly_landing", ".pdf", sep="")
    pdf(file=file.path(wd, filename))
    toplot=crablandings[crablandings$cfa0==c,]
    plot=barplot(toplot$mt, main= paste(toplot$year[1],toplot$cfa0[1],"Landings", sep=" "),cex.main=1.2, xlab= "Week of Season",
                 ylab="Cumulative Landings (mt)", names.arg=toplot$weekfromstart, ylim=c(0,1.2*(max(toplot$mt))), col="deepskyblue")
    #text(plot, 0, round(toplot$mt,0), cex=1, pos=3) # Can remove "#" if you want values shown on barplot
    dev.off()
    print(paste(filename, " created", sep=""))
  }
  
  # Calculate the percentage of spring landings for each area by year
  #--------------------------------------
  
  lbyq = compute.sums( x=logs, var="pro_rated_slip_wt_lbs", index=c("q", "year", "cfa0")  )
  
  names(lbyq)=c("q", "year", "cfa", "lbs")
  lbyq=lbyq[(as.numeric(lbyq$year)>2006),]
  #lbyq=lbyq[lbyq$cfa %in% c("north", "cfa23", "cfa24"),]
  lbyq$lbs[which(!is.finite(lbyq$lbs))]=0
  
  lan=compute.sums(x=lbyq, var="lbs", index=c("cfa", "year"))
  
  lbyq=merge(lbyq, lan, by=c("year", "cfa"))
  names(lbyq)=c("year", "cfa", "q", "lbs", "total")
  lbyq$perc=NA
  lbyq$perc=(lbyq$lbs/lbyq$total)*100
  
  
  spring=lbyq[lbyq$q=="Q2",] #April, May, June
  spring$sorter=NA
  spring$sorter[spring$cfa=="north"]=1
  spring$sorter[spring$cfa=="cfa23"]=2
  spring$sorter[spring$cfa=="cfa24"]=3
  
  spring=orderBy(~cfa, spring)
  spring=spring[order(spring$cfa),]
  
  # Save as Metafile
  
  filename=paste(iy, "_percent_spring_landings", ".emf", sep="")
  emf(file=file.path(wd, filename), bg="white")
  cf=unique(spring$cfa)
  cf = cf[order(cf)]
  cols=c("blue", "red", "black", "green4")
  point=c(1,2,4,3)
  
  
  plot(spring$year, spring$perc, type="n",
       ylab="Percent of Total", xlab="Year", ylim=c(0,100), lty=1, col="red", pch=19, bg="white")
  
  for (y in 1:length(cf)) {
    sprc=spring[spring$cfa==cf[y],]
    lines(x=sprc$year, y=sprc$perc, col=cols[y], lwd=2 )
    points(x=sprc$year, y=sprc$perc, col=cols[y], pch=point[y])
    
  }
  legend("bottomright",paste(cf), bty="n", col=cols, pch=point, inset=c(0,.1))
  legend("bottomright",paste(cf), bty="n", col=cols, lwd = 1.5, lty=1, inset=c(0,.1))
  
  dev.off()
  print(paste(filename, " created", sep=""))
  
  
  # Save as PDF
  
  filename=paste("percent_spring_landings.pdf", sep="")
  
  pdf(file=file.path(wd, filename))
  par(mar = c(5, 4, 1, 1))
  cf=unique(spring$cfa)
  cf = cf[order(cf)]
  cols=c("blue", "red","black","green4")
  point=c(1,2,4,15)
  
  
  plot(spring$year, cex.main = .1, spring$perc, type="n", ylab="Percent of Total", xlab="Year", ylim=c(0,100), lty=1, col="red", pch=19, bg="white")
  
  for (y in 1:length(cf)) {
    sprc=spring[spring$cfa==cf[y],]
    lines(x=sprc$year, y=sprc$perc, col=cols[y], lwd=2 )
    points(x=sprc$year, y=sprc$perc, col=cols[y], pch=point[y])
    
  }
  legend("bottomright",paste(cf), bty="n", col=cols, pch=point, inset=c(0,.1))
  legend("bottomright",paste(cf), bty="n", col=cols, lwd = 1.5, lty=1, inset=c(0,.1))
  par(mar=par.old)
  dev.off()
  
  print(paste(filename, " created", sep=""))
  
  #########################################
  #Annual Catch Rates
  #########################################
  
  # Clean for Catch Rate Data
  landings=logs
  cleanlogsna=landings[!is.na(landings$num_of_traps),]
  cleanlogscr=cleanlogsna[which(cleanlogsna$lbspertrap < 800),]
  #c=cleanlogscr
  
  logs.fixed = cleanlogscr
  #logs.fixed = cleanlogsna
  areas = unique(na.omit(logs.fixed$cfa0))
  
  for (a in areas) {
    q = which (logs.fixed$cfa0 == a)
    catchrate = sum(logs.fixed$pro_rated_slip_wt_lbs[q], na.rm=T)/(sum(logs.fixed$num_of_traps[q], na.rm=T))
    output = rbind( data.frame(area=a, catchrate=catchrate)) 
  }
  logs.fixed$area=logs.fixed$cfa0
  logs.fixed$area[logs.fixed$cfa0=="cfa23"]="S-ENS"
  logs.fixed$area[logs.fixed$cfa0=="cfa24"]="S-ENS"
  
  
  fixedtable<-logs.fixed %>%
    dplyr::group_by(area, year) %>%
    dplyr::summarise(landings_total_lbs = sum(pro_rated_slip_wt_lbs), effort_total = sum(num_of_traps))
  
  days_fished <- logs.fixed %>%
    dplyr::group_by(year, cfa0 ) %>%
    dplyr::count(cfa0)
  
  write.table(days_fished,file=file.path(wd, "days_fished.csv"), sep=",")
  
  #####
  # calculate mean catch rates for the season by Area
  
  cpue = compute.catchrate( x=logs.fixed, var="catchrate", index=c("year","area" ) )
  names(cpue)=c("year", "area", "cpuelbs")
  cpue$cpuekg=(cpue$cpuelbs/2.204626)
  
  i=cpue$area%in%c("S-ENS", "north", "cfa4X")
  cpue=cpue[i,]
  cpue=cpue[is.finite(cpue$cpuelbs),]
  
  # calculate mean catch rates for the season by CFA
  cpu = compute.catchrate( x=logs.fixed, var="catchrate", index=c("year","cfa0" ) )
  names(cpu)=c("year", "cfa", "cpuelbs")
  cpu$cpuekg=(cpu$cpuelbs/2.204626)
  
  
  i=cpu$cfa%in%c("cfa23", "cfa24", "cfa4X", "north")
  cpu=cpu[i,]
  cpu=cpu[is.finite(cpu$cpuelbs),]
  test=cpu[cpu$cfa=="cfa23",]
  
  topyear=max(as.numeric(test$year))
  cpu=cpu[(as.numeric(cpu$year))<=topyear,]
  
  cpu$cfa[cpu$cfa=="north"]="N-ENS"
  cpu$cfa[cpu$cfa=="cfa23"]="CFA 23"
  cpu$cfa[cpu$cfa=="cfa24"]="CFA 24"
  cpu$cfa[cpu$cfa=="cfa4X"]="4X"
  
  
  # Produce windows metafile of annual catch rates
  
  filename=paste(iy,"_annual_cpue_kg", ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  aland(cpu)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  # produce pdf of annual catch rates
  filename=paste("annual_cpue_kg", ".pdf", sep="")
  pdf(file=file.path(wd, filename))
  par(mar = c(5, 4, 1.5, 1.5))
  aland(cpu)
  par(mar=par.old)
  dev.off()  
  
  print(paste(filename, " created", sep=""))
  
  # calculate mean catch rates for the season by CFA
  cpu.quarter = compute.catchrate.quarter( x=logs.fixed, var="catchrate", index=c("q", "year", "cfa0" ) )
  names(cpu.quarter) = c("q", "year", "cfa", "cpuelbs" )
  cpu.quarter=cpu.quarter[is.finite(cpu.quarter$cpuelbs),]
  cpu.quarter$cpuekg = as.numeric(cpu.quarter$cpuelbs)*.45359
  for(i in c("Q1", "Q2", "Q3", "Q4")){
    
    cq = cpu.quarter[which(cpu.quarter$q == i),]
    filename=paste("annual_cpue_kg_",i, ".pdf", sep="")
    
    
    pdf(file=file.path(wd, filename))
    par(mar = c(5, 4, 1, 1))
    aland(cq)
    par(mar=par.old)
    dev.off()    
    
    print(paste(filename, " created", sep=""))
    
  }
  
  #########################################
  #Following script calculates weekly catch rates by area (in lbs)
  #uses a three week running average of lbs and traps to smooth
  #########################################
  
  cleanlogsna=logs[! is.na(logs$num_of_traps),]
  cleanlogscr=cleanlogsna[cleanlogsna$lbspertrap < 800,]
  logs.fixed = cleanlogscr
  rm(cleanlogscr)
  rm(cleanlogsna)
  
  ##using this to remove one log record in 2020 that is causing problems
  logs.fixed2<-logs.fixed[!(logs.fixed$vr_number=="105873" & logs.fixed$year=="2020"),]
  
  
  #use log.fixed2 for the rest of this section to replot data without the log error for this record
  
  logs.fixed2$julian = as.integer(julian(logs.fixed2$date_landed))
  logs.fixed2$dayofseason= ((logs.fixed2$julian)-(min(logs.fixed2$julian)))+1
  logs.fixed2$weekofseason=ceiling(logs.fixed2$dayofseason/7)
  
  nweek <- function(x, format="%Y-%m-%d", origin){
    if(missing(origin)){
      as.integer(format(strptime(x, format=format), "%W"))
    }else{
      x <- as.Date(x, format=format)
      o <- as.Date(origin, format=format)
      w <- as.integer(format(strptime(x, format=format), "%w"))
      2 + as.integer(x - o - w) %/% 7
    }
  }
  
  logs.fixed2$weekofyear=NA
  logs.fixed2$weekofyear=nweek(logs.fixed2$date_fished)
  logs.fixed2$weekofyear=factor(logs.fixed2$weekofyear,levels=c(40:52, 1:39),ordered=TRUE)
  logs.fixed2$cfa0 = as.character(logs.fixed2$cfa0)
  areas = unique(logs.fixed2$cfa0)
  
  weekly=as.data.frame(xtabs(logs.fixed2$pro_rated_slip_wt_lbs~logs.fixed2$weekofyear+logs.fixed2$cfa0+logs.fixed2$year), stringsAsFactors=F)
  effort=as.data.frame(xtabs(logs.fixed2$num_of_traps~logs.fixed2$weekofyear+logs.fixed2$cfa0+logs.fixed2$year),stringsAsFactors=F)
  
  names(weekly)=c("week", "area", "year", "tot_lbs")
  names(effort)=c("week", "area", "year", "tot_traps")
  
  effortsum<-effort %>%
    dplyr::group_by(area, year) %>%
    dplyr::summarise(total = sum(tot_traps))
  
  wk=merge(weekly, effort)
  
  wk$run_lbs=NA
  wk$run_traps=NA
  wk$week=as.numeric(wk$week)
  
  lags =c(-1,0,1)
  areas=unique(wk$area)
  
  for (j in 1:nrow(wk)){
    w=wk$week[j]
    y=wk$year[j]
    a=wk$area[j]
    wks=w+lags
    i=which(wk$week %in% wks & wk$year==y & wk$area==a)
    wk$run_lbs[j]=sum(wk$tot_lbs[i])
    wk$run_traps[j]=sum(wk$tot_traps[i])
  }
  
  wk$cpue=wk$run_lbs/wk$run_traps
  wk$cpuekg=wk$cpue/2.204626
  wk$time=as.numeric((as.numeric(wk$year)+ (wk$week/52-.0001)))
  wk$area[which(wk$area=="north")]="N-ENS"
  wk$area[which(wk$area=="cfa23")]="CFA 23"
  wk$area[which(wk$area=="cfa24")]="CFA 24"
  wk$area[which(wk$area=="cfa4X")]="4X"
  
  #Separate out last 3 years
  back=c(-2,-1, 0)
  past=as.character(max(as.numeric(logs.fixed2$year)[which(logs.fixed2$cfa0=="cfa23")])+ back)
  i=which(wk$year %in% past)
  recent=wk[i,]
  recent=recent[is.finite(recent$cpue),]
  
  #Produce pdf file
  
  fn=file.path(wd, "weekly_cpue_smoothed2.pdf")
  #pdf(file=file.path(wd, filename), width = 10)
  par(mar = c(4, 4, 1, 1))
  retx = smoothcatch(recent)
  ggsave(filename = fn, device = "pdf", width = 12, height = 8)
  
  par(mar=par.old)
  # dev.off()
  
  print(paste(filename, " created", sep=""))
  
  #########################################
  #Following script calculates monthly catch rates by area (in lbs)
  #########################################
  
  ###coerce 4X into data set
  i4x=which(logs$cfa0=="cfa4X")
  
  logs$date_landed[i4x] = logs$date_landed[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
  logs$year=as.character(lubridate::year(logs$date_landed))        #determine year
  logs$date_landed[i4x] = logs$date_landed[i4x] + lubridate::days(91) #add the days back
  
  cyear=which(logs$year==iy)
  #landings= logs[iyear,]
  iyear = which(logs$year > iy-4  & logs$year <= current_year)
  landings= logs[iyear,]
  
  
  cleanlogsna=landings[! is.na(landings$num_of_traps),]
  cleanlogscr=cleanlogsna[cleanlogsna$lbspertrap < 800,]
  logs.fixed.old = logs.fixed
  logs.fixed = cleanlogscr
  
  logs.fixed$cfa0 = as.character(logs.fixed$cfa0)
  areas = unique(logs.fixed$cfa0)
  logs.fixed$julian = as.integer(julian(logs.fixed$date_landed))
  logs.fixed$dayofseason= ((logs.fixed$julian)-(min(logs.fixed$julian)))+1
  logs.fixed$weekofseason=ceiling(logs.fixed$dayofseason/7)
  logs.fixed$month=months(logs.fixed$date_fished)
  logs.fixed$month=factor(logs.fixed$month,levels=c("November","December","January","February","March",
                                                    "April","May","June","July","August","September",
                                                    "October"),ordered=TRUE)
  logs.fixed$weekfromstart = NA
  
  
  monthlycpue = compute.means( x=logs.fixed[which(logs.fixed$year == iy),], var="lbspertrap", index=c("month", "cfa0")  )
  monthlycpue$lbspertrap[!is.finite(monthlycpue$lbspertrap)]=0
  monthlycpue$kg=NA
  monthlycpue$kg=monthlycpue$lbspertrap/2.204626
  
  monthlycpue=monthlycpue[monthlycpue$month %in% c("April", "May", "June", "July", "August", "September"),]
  
  
  north=monthlycpue[monthlycpue$cfa0=="north",]
  north$cfa="N-ENS"
  c23=monthlycpue[monthlycpue$cfa0=="cfa23",]
  c23$cfa="CFA 23"
  c24=monthlycpue[monthlycpue$cfa0=="cfa24",]
  c24$cfa="CFA 24"
  c4x=monthlycpue[monthlycpue$cfa0=="cfa4X",]
  c4x$cfa="4X"
  
  
  
  #########################################
  #### Generate Monthly Catch Rates by Area by month for RAP
  #########################################
  
  #Produce emf file
  filename=paste(iy,"_monthly_catch_rates",".emf", sep="")
  emf(file=file.path(wd, filename), bg="white")
  plotmonthlycpue(c23, c24, north, monthlycpue, iy)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  #Produce pdf file
  
  
  filename=paste("monthly_catch_rates",".pdf", sep="")
  pdf(file=file.path(wd, filename))
  par(mar = c(5, 4, 1, 1))
  plotmonthlycpue(c23, c24, north, monthlycpue, iy)
  par(mar=par.old)
  dev.off()  
  
  print(paste(filename, " created", sep=""))
  
  ###########################################
  # Look at catch rates within an area with more detail
  ###########################################
  
  n=logs.fixed[logs.fixed$cfa0 %in% "north",] #ie.N-ENS
  n = n[which(n$year == iy),]
  catchrates= compute.means( x=n, var="lbspertrap", index=c("year", "licence_id" ))
  catchrates=catchrates[is.finite(catchrates$lbspertrap),]
  catchrates$kg=catchrates$lbspertrap/2.204626
  
  anes=unique(catchrates$year)
  out=NULL
  for (a in anes){
    i=which(catchrates$year==a)
    cr=catchrates[i,]
    mn=mean(cr$kg)
    stdev=sd(cr$kg)
    out=rbind(out,c(a,mn,stdev))
    #  print(paste(a, mn, stdev))
  }
  
  out =as.data.frame(out)
  out =toNums(out,c(1,2,3))
  out$cv = out[,3]/out[,2]
  
  
  
  #########################################
  #Calculate Active Vessels per year for each CFA
  #########################################
  
  for (y in yrs){
    vessels=logs[logs$year==y,]
  }
  
  boats = compute.vessels( x=logs, var="vessels", index=c("year", "cfa0")  )
  boats=boats[is.finite(boats$vessels) & boats$year>2004,]
  
  areas=unique(boats$cfa0)
  cols= c("cornflowerblue", "red", "black", "green4")
  point=c(16, 17, 8, 15)
  
  lim=c(1:4)
  iye=max(boats$year[boats$cfa0=="cfa23"])
  
  filename=paste(iye,"_vessels_per_year", ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  plot(boats$year, boats$vessels, type="n", ylim=c(0,(max(boats$vessels)+1)), ylab="Vessels",
       main="Vessels Active by Year", xlab="Year" )
  for (l in lim){
    q = which(boats$cfa0 == areas[l] )
    lines(boats$year[q],boats$vessels[q], col=cols[l],lty=1, pch=point[l], type="o" )
  }
  legend("topright",paste(areas), bty="n", col=cols, pch=point, lty=1, ncol=2)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  filename=paste("vessels_per_year", ".pdf", sep="")
  
  pdf(file=file.path(wd, filename))
  par(mar = c(5, 4, 1, 1))
  plot(boats$year, boats$vessels, type="n", ylim=c(0,(max(boats$vessels)+1)), ylab="Vessels", xlab="Year" )
  
  
  for (l in lim){
    q = which(boats$cfa0 == areas[l] )
    lines(boats$year[q],boats$vessels[q], col=cols[l],lty=1, pch=point[l], type="o" )
  }
  legend("topright",paste(areas), bty="n", col=cols, pch=point, lty=1, ncol=2)
  par(mar=par.old)
  dev.off()
  
  print(paste(filename, " created", sep=""))
  
  
  #Need to add something that looks at the homeport of vessels being within a fishing area or not
  #May be tough as licence holders may live in non-adjacent ports (i.e. Glace Bay vs. CFA 23)
  #Dec 2016- Cannot find a way to link VR # to actual homeport
  
  #########################################
  # Mapping
  #########################################
  
  ###Convert Positions for plotting
  
  load(file.path(wd, paste("logs.",iy,".rdata", sep="")))
  
  logs$lat=logs$latitude/10000
  logs$lon=logs$longitude/10000
  
  logs=convert.degmin2degdec(logs)
  logs=logs[!is.na(logs$lat),]
  logs=logs[!is.na(logs$lon),]
  
  # Format dataframe to be able to convert to EventData
  logs$X=-(logs$lon)
  logs$Y=logs$lat
  logs$EID=as.numeric(paste(1:nrow(logs),  sep=""))
  
  
  # Create maps with pbsMapping
  #--------------------------------------------
  i=which(logs$cfa0=="cfa23")
  #iy=max(as.numeric(logs$year[i]))
  iy1=as.numeric(iy)-1
  iyear=which(logs$year==iy)
  iyear1=which(logs$year==iy1)
  
  spring=which(logs$q=="Q2")
  summer=which(logs$q=="Q3")
  
  iyearspring=intersect(iyear, spring)
  iyearsum=intersect(iyear, summer)
  iyear1spring=intersect(iyear1, spring)
  iyear1sum=intersect(iyear1, summer)
  
  #------------------------------------------------------------------
  # Plot last two seasons' fishing positions on same map
  
  areas=c("cfa23", "cfa24zoom", "nens", "sens", "cfa4x")
  
  # Create EMF's
  for (a in areas){
    filename=paste(a,"_past_two_years_fishing_positions.emf", sep="")
    win.metafile(file=file.path(wd, filename), width=10)
    fishmap(a = logs[iyear,], b= logs[iyear1,], area = a, cy = iy)
    dev.off()
    print(paste(filename, " created", sep=""))
  }
  
  # Create PDF's
  
  for (a in areas){
    filename=paste(a,"_past_two_years_fishing_positions.pdf", sep="")
    
    if(any(c("cfa23", "cfa24zoom") %in% a)){
      pdf(file=file.path(wd, filename), width = 6)
    }else{
      pdf(file=file.path(wd, filename), width = 8)
    }
    fishmap(a = logs[iyear,], b= logs[iyear1,], area = a, cy = iy)
    dev.off()
    print(paste(filename, " created", sep=""))
  }
  
  
  
  # Plot Previous 2 Seasons Fishing Positions by Season (Q)
  
  
  #Spring
  
  areas=c("cfa23", "cfa24zoom", "nens", "sens")
  
  #Create EMF's
  for (a in areas){
    filename=paste(a,"_spring_fishing_positions", ".emf", sep="")
    win.metafile(file=file.path(wd, filename))
    seasonmap(a = logs[iyearspring,], b = logs[iyear1spring,], area = a, cy = iy, season = "Spring")
    dev.off()
  }
  
  #Create PDF's
  for (a in areas){
    filename=paste(a,"_spring_fishing_positions", ".pdf", sep="")
    if(grepl("cfa", a)){
      pdf(file=file.path(wd, filename), width = 6)
    }else{
      pdf(file=file.path(wd, filename), width = 8)
    }
    seasonmap(a = logs[iyearspring,], b = logs[iyear1spring,], area = a, cy = iy, season = "Spring")
    dev.off()
  }
  
  #Summer
  
  areas=c("cfa23", "cfa24zoom", "nens", "sens")
  
  # Create EMF's
  for (a in areas){
    filename=paste(a,"_summer_fishing_positions", ".emf", sep="")
    win.metafile(file=file.path(wd, filename))
    seasonmap(a = logs[iyearsum,], b = logs[iyear1sum,], area = a, cy = iy, season = "Summer")
    dev.off()
  }
  
  # Create PDF's
  for (a in areas){
    filename=paste(a,"_summer_fishing_positions", ".pdf", sep="")
    if(grepl("cfa", a)){
      pdf(file=file.path(wd, filename), width = 6)
    }
    else{
      pdf(file=file.path(wd, filename), width = 8)
    }
    seasonmap(a = logs[iyearsum,], b = logs[iyear1sum,], area = a, cy = iy, season = "Summer")
    dev.off()
  }
  
  #########################################
  # Observer Data
  #########################################
  
  #------------------------------------
  # get all info from observed crab sets
  
  
  
  
  h=names(obs)
  h[h=="LATITUDE"] = "lat"
  h[h=="LONGITUDE"] = "lon"
  names(obs) = h
  obs$lon=-obs$lon
  obs$year=NA
  obs$year=year(obs$BOARD_DATE)
  #remove sets that are not fom standard sampling
  # these include forced C&P trips, moving traps, etc
  
  obs=obs[as.character(obs$SETCD_ID)=="1",]
  
  
  #----------------------------------------
  # create columns for area and CFA
  x=obs
  
  cfa=c("cfanorth", "cfa23", "cfa24", "cfa4x")
  x$cfa=NA
  for  (a in cfa){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$cfa[rowindex]=a
  }
  
  area=c("cfanorth", "cfasouth", "cfa4x")
  
  x$area=NA
  for (a in area){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$area[rowindex]=a
  }
  
  
  x$area=factor(x$area, levels=c("cfa4x","cfasouth","cfanorth"), labels=c("4X","South","North"))
  x$cfa=factor(x$cfa, levels=c("cfa4x","cfa24","cfa23","cfanorth"), labels=c("4X","CFA 24","CFA 23","North"))
  
  yrs=as.character(sort(as.numeric(unique(as.character(x$year)))))
  x$year=factor(x$year,levels=yrs, labels=yrs)
  
  
  #Following lines force 4X into proper dataset
  # i.e. Winter 2011 gets grouped under 2010 "Season"
  
  i4x=which(x$cfa %in% "4X")
  
  x$BOARD_DATE[i4x] = x$BOARD_DATE[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
  x$year=as.character(lubridate::year(x$BOARD_DATE))        #determine year
  x$BOARD_DATE[i4x] = x$BOARD_DATE[i4x] + lubridate::days(91) #add the days back
  
  
  #Choose most recent year and create directory
  i=which(x$cfa=="CFA 23")
  # iy=max(as.numeric(as.character(x$year)))
  
  # --------------------------------------
  # divide into north, south, cfa 23, cfa 24, and 4X as required
  
  yrs=c(iy, iy-1, iy-2, iy-3, iy-4)
  
  cfas=c("North", "South", "CFA 23", "CFA 24", "4X")
  out=NULL
  
  for (y in yrs){
    for (c in cfas){
      
      cc=c
      if (c=="South") c=c("CFA 23", "CFA 24")
      
      iyear=which(x$year==y)
      iarea= which(x$cfa %in% c)
      
      j=x[intersect(iyear,iarea) ,]
      
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
      
      #print(paste(y,c,"Observed mt=", observedmt, "Traps Sampled=", sampledtraps, "Observed Traps=", observedtraps, sep=" "))
      out=rbind(out,c(y,cc, observedmt, sampledtraps, observedtraps))
    }
  }
  out=as.data.frame(out)
  a=out
  out=a
  
  names(out)=c("Year", "Area", "Observed", "Traps Sampled", "Traps Observed")
  out$Area=as.character(out$Area)
  out$Area[which(out$Area=="North")]="N-ENS"
  out$Area[which(out$Area=="South")]="S-ENS"
  out=out[order(out$Area),]
  
  out=out[out$Area!="S-ENS",]
  
  ys=yrs
  
  out$Landings=NA
  cfa=unique(out$Area)
  for (y in ys){
    for (c in cfa){
      if(!any(year.land$year==y & year.land$cfa==c)){
        out$Landings[out$Area==c & out$Year==y]=NA
      }
      else{  
        out$Landings[out$Area==c & out$Year==y]=round(year.land$mt[year.land$year==y & year.land$cfa==c],0)
      }
    }
  }
  
  out$Observed=round(as.numeric(as.character(out$Observed)))
  out$Percent=round(out$Observed/out$Landings*100,1)
  names(out)=c("Year", "Area", "Observed (mt)", "Traps Sampled", "Traps Observed", "Landings (mt)", "Percent")
  out$Area=factor(out$Area, levels= c("N-ENS", "CFA 23", "CFA 24", "4X" ))
  out=out[order(out$Area),]
  
  out$Landings=NA
  cfa=unique(out$Area)
  for (y in ys){
    for (c in cfa){
      if(!any(year.land$year==y & year.land$cfa==c)){
        out$Landings[out$Area==c & out$Year==y]=NA
      }
      else{
        out$Landings[out$Area==c & out$Year==y]=round(year.land$mt[year.land$year==y & year.land$cfa==c],0)
      }
    }
  }
  out$Observed=round(as.numeric(as.character(out$Observed)))
  out$Percent=round(out$Observed/out$Landings*100,1)
  names(out)=c("Year", "Area", "Observed (mt)", "Traps Sampled", "Traps Observed", "Landings (mt)", "Percent")
  out$Area=factor(out$Area, levels= c("N-ENS", "CFA 23", "CFA 24", "4X" ))
  out=out[order(out$Area),]
  #out=out[out$Area != "4X",]
  
  
  output=out[out$Year==iy,]
  #the next line adds previous years observer coverage % in brackets to Percent observed column
  output$Percent=paste(output$Percent ,"(", out$Percent[out$Year==iy-1], ")", sep="")
  colnames(output)[colnames(output)=="Percent"] = "Percent Observed"
  output=output[,c(-8, -9)]
  softsum=out[,c(-8, -9)]
  
  #Save a pdf version of this table
  put=output[,c(-1)]
  put$`Traps Sampled` = as.numeric(put$`Traps Sampled`)
  put$`Traps Observed` = as.numeric(put$`Traps Observed`)
  put$`Percent Observed`[which(put$`Percent Observed` == 'NA(NA)')] = '-'
  #source("C:/Scripts/functions/tab.4.tex.r")
  kbl(x = put, row.names = F, "latex", booktabs = T) %>%
    kable_classic() %>%
    kable_styling(latex_options = c("striped", "scale_down")) %>%
    as_image(file = file.path(wd,  "observersummary2.pdf"))
  
  
  
  
  pdf(file = file.path(wd, "observersummary.pdf"))
  
  grid.table(put, theme=ttheme_default(), rows= NULL)
  dev.off()
  
  # --------------------------------------
  # Use the script below to create the map of soft crab sampling locations
  
  
  a=setsobs
  
  # setting "hardlines" below lets you switch between multiple durometer levels to be considered hard
  hl=68
  
  
  # --------------------------------------
  # convert lat's and long's to recognizable format for recode.areas
  a=setsobs
  h=names(a)
  h[h=="LATITUDE"] = "lat"
  h[h=="LONGITUDE"] = "lon"
  names(a) = h
  a$lon=-a$lon
  
  #if no landing date, populate with board date
  a$LANDING_DATE[is.na(a$LANDING_DATE)]=a$BOARD_DATE[is.na(a$LANDING_DATE)]
  
  
  # Add a column for year
  #--------------------------------------------
  a$year=NA
  a$year=as.character(lubridate::year(a$LANDING_DATE))
  
  a$date=a$LANDING_DATE
  
  # Add month
  #--------------------------------------------
  a$month=NA
  a$month=as.character(lubridate::month(a$LANDING_DATE, label = T))
  
  # Add season (Q1 =winter, Q2=spring........)
  #--------------------------------------------
  a$season=NA
  a$season=as.character(quarters(a$date))
  a$season[which(a$season=="Q1")]="winter"
  a$season[which(a$season=="Q2")]="spring"
  a$season[which(a$season=="Q3")]="summer"
  a$season[which(a$season=="Q4")]="fall"
  
  
  # --------------------------------------
  # create a column unique to each set called tripset
  
  a$tripset= paste(a$TRIP,a$SET_NO,sep="~")
  a$kgpertrap= (a$EST_CATCH*1000)/(a$NUM_HOOK_HAUL)
  a=a[!is.na(a$kgpertrap),]
  
  
  
  # --------------------------------------
  # create field for soft/hard
  
  a= a[is.finite(a$DUROMETRE),]
  a$dummy= 1
  a$hardness= ifelse(a$DUROMETRE>=hl, 1, 0)
  
  # --------------------------------------
  # calculate counts of hard and total crab for each trip set
  
  b=xtabs(as.integer(a$hardness)~as.factor(a$tripset))
  b=as.data.frame(b)
  names(b)= c("tripset", "no.hard")
  c=xtabs(as.integer(a$dummy)~as.factor(a$tripset))
  c=as.data.frame(c)
  names(c)= c("tripset", "total")
  
  # --------------------------------------
  # merge two new data frames
  
  d=merge(x=b, y=c, by="tripset", all.x=F, all.y=T, sort=F)
  d$percenthard=(d$no.hard/d$total)*100
  d$percentsoft=100-d$percenthard
  
  
  # -a-------------------------------------
  # import catch rate from a
  
  e=tapply(X=a$kgpertrap, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  e=as.data.frame(e)
  f=data.frame(kgpertrap=as.vector(e$e), tripset=dimnames(e)[[1]] )
  f$tripset =  as.character(f$tripset)
  
  g=merge(x=d, y=f, by="tripset", all.x=F, all.y=T, sort=F)
  
  # --------------------------------------
  # year from a
  
  
  ee=tapply(X=a$year, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  ee=as.data.frame(ee)
  ff=data.frame(year=as.vector(ee$ee), tripset=dimnames(ee)[[1]] )
  ff$tripset =  as.character(ff$tripset)
  ff$year = as.character(ff$year)
  
  
  gg=merge(x=g, y=ff, by="tripset", all.x=F, all.y=T, sort=F)
  
  
  # --------------------------------------
  # import latitude from a
  
  h=tapply(X=a$lat, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  h=as.data.frame(h)
  i=data.frame(lat=as.vector(h$h), tripset=dimnames(h)[[1]] )
  i$tripset =  as.character(i$tripset)
  
  j=merge(x=gg, y=i, by="tripset", all.x=F, all.y=T, sort=F)
  
  # --------------------------------------
  # import longitude from a
  
  k=tapply(X=a$lon, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  k=as.data.frame(k)
  l=data.frame(lon=as.vector(k$k), tripset=dimnames(k)[[1]] )
  l$tripset =  as.character(l$tripset)
  
  mm=merge(x=j, y=l, by="tripset", all.x=F, all.y=T, sort=F)
  
  # --------------------------------------
  # import season from a
  
  kk=tapply(X=a$season, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  kk=as.data.frame(kk)
  ll=data.frame(season=as.vector(kk$kk), tripset=dimnames(kk)[[1]] )
  ll$tripset =  as.character(ll$tripset)
  
  mmm=merge(x=mm, y=ll, by="tripset", all.x=F, all.y=T, sort=F)
  
  # --------------------------------------
  # import month from a
  
  kkk=tapply(X=a$month, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  kkk=as.data.frame(kkk)
  lll=data.frame(month=as.vector(kkk$kkk), tripset=dimnames(kkk)[[1]] )
  lll$tripset =  as.character(lll$tripset)
  
  m=merge(x=mmm, y=lll, by="tripset", all.x=F, all.y=T, sort=F)
  # --------------------------------------
  # import estimated catch from a
  
  n=tapply(X=a$EST_CATCH, INDEX= a$tripset, FUN= function(q){unique(q)[1]})
  n=as.data.frame(n)
  p=data.frame(EST_CATCH=as.vector(n$n), tripset=dimnames(n)[[1]] )
  p$tripset =  as.character(p$tripset)
  
  x=merge(x=m, y=p, by="tripset", all.x=F, all.y=T, sort=F)
  
  x$discardkg=(x$EST_CATCH*1000)*(x$percentsoft/100)
  
  
  # --------------------------------------
  # divide into north, south, and 4X by positions
  
  cfa=c("cfanorth", "cfa23", "cfa24", "cfa4x")
  
  x$cfa=NA
  for  (a in cfa){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$cfa[rowindex]=a
  }
  
  area=c("cfanorth", "cfasouth", "cfa4x")
  
  x$area=NA
  for (a in area){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$area[rowindex]=a
  }
  
  #4X fishing activity on 4X line doesn't get recoded properly by recode.areas
  #force thes into 4X
  x$long=NA
  x$long=-(as.numeric(x$lon))
  i4x=which(x$month %in% c("Nov", "Dec", "Jan", "Feb") & x$long>63.3)
  x$cfa[i4x]="cfa4x"
  
  #set up month and year as ordered factors
  x$area=factor(x$area, levels=c("cfa4x","cfasouth","cfanorth"), labels=c("4X","South","North"))
  x=x[!is.na(x$area),]
  
  x$cfa=factor(x$cfa, levels=c("cfa4x","cfa24","cfa23","cfanorth"), labels=c("4X","CFA 24","CFA 23","North"))
  
  ####### Need to resolve month script to become an ordered factor
  x$month=factor(x$month, levels=c("Jan","Feb","Mar","Apr","May", "Jun", "Jul", "Aug", "Sep","Oct","Nov", "Dec"),
                 labels=c("1","2","3","4","5","6", "7","8", "9", "10", "11", "12"))
  
  yrs=as.character(sort(as.numeric(unique(x$year))))
  x$year=factor(x$year,levels=yrs, labels=yrs)
  
  x$areaseason=NA
  x$areaseason=paste(x$area ,":", x$season)
  
  xnorth=x[x$area=="North", ]
  xsouth=x[x$area=="South",]
  x23=x[x$cfa=="CFA 23",]
  x24=x[x$cfa=="CFA 24",]
  
  jj=x
  
  
  softbymonth=xtabs(percentsoft~month+year+cfa, data=x)
  #softbymonth
  
  #-----------------------------------------------
  # Use below to plot percent soft by month by year by cfa
  
  z=compute.sums(x=x, var=c("discardkg"), index=c("month","year","cfa"))
  z=na.omit(z)
  
  v=compute.sums(x=x, var=c("EST_CATCH"), index=c("month","year","cfa"))
  v=na.omit(v)
  
  w=merge(x=z, y=v, by=c("month","year","cfa"), all.x=T, all.y=F, sort=F)
  
  w$totalkg=NA
  w$totalkg=w$EST_CATCH*1000
  w$percsoft=NA
  w$percsoft=(w$discardkg/w$totalkg)*100
  w$month=as.numeric(as.character(w$month))
  w$year=as.numeric(as.character(w$year))
  
  baddata=which(w$cfa=="4X" & w$month %in% c(5,6,7,8,9,10))
  w$bad=1
  w$bad[baddata]=0
  w=w[w$bad>0,]
  
  
  #-----------------------------------------------
  # Use below to plot soft by month for years for all areas by month
  w=w[w$year>(iy-5),]
  w$year=factor(w$year)
  
  
  #Save as metafile
  filename=paste("soft_crab_by_month.emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  default.options = options()
  setup.lattice.options()
  
  plot_obj <- xyplot(
    percsoft~month|year+cfa, data=w,  main=paste("Percent Soft Shell by Month", sep=""),
    ylim=c(0, 100), xlab="Month", ylab="Percent Soft",
    panel = function(x, y, ...) {
      panel.xyplot(x, y, type="p",  pch=20,
                   col="red", ...)
      panel.abline(h=20, col="red", lty=1, lwd=1, ...) }
  )
  
  print(plot_obj)
  dev.off()
  
  #save as pdf
  filename=paste("soft_crab_by_month.pdf", sep="")
  pdf(file=file.path(wd, filename))
  
  plot_obj <- lattice::xyplot(
    percsoft~month|year+cfa, data=w,  main=paste("Percent Soft Shell by Month", sep="")
    ,ylim=c(0, 100), xlab="Month", ylab="Percent Soft",
    panel = function(x, y, ...) {
      lattice::panel.xyplot(x, y, type="p",  pch=20,
                            col="red", ...)
      lattice::panel.abline(h=20, col="red", lty=1, lwd=1, ...) }
  )
  print(plot_obj)
  
  
  dev.off()
  print(paste(filename, " created", sep=""))
  options(default.options)
  
  #Use the following lines to determine total weights of discarded soft crab by area
  
  discard=as.data.frame(xtabs(discardkg~year+cfa, data=x))
  discard$Freq=discard$Freq/1000
  names(discard)=c("Year","Area","Obs.Soft.mt")
  discard$Area=as.character(discard$Area)
  discard$Area[which(discard$Area=="North")]="N-ENS"
  
  discard=discard[discard$Year %in% unique(out$Year),] #Trim old years
  discard=discard[discard$Area %in% unique(out$Area),] #Trim 4X
  
  
  #Bring in landings and % of observer coverage from summary above
  
  soft.sum=merge(softsum, discard, by=c("Year", "Area"))
  soft.sum=soft.sum[with(soft.sum, order(Area, as.numeric(Year))),]
  soft.sum=soft.sum[,c(-3, -4, -5)]
  soft.sum$tot.kg.soft=NA
  soft.sum$tot.kg.soft=soft.sum$Obs.Soft.mt/(soft.sum$Percent*.01)
  
  #Clean up names, rounding, etc for inclusion in presentation
  soft.sum=soft.sum[with(soft.sum, order(Area, as.numeric(Year))),]
  names(soft.sum)=c("Year", "Area", "Landings","% Observed", "Observed Soft", "Total Soft")
  soft.sum$`Observed Soft`=round(soft.sum$`Observed Soft`,2)
  soft.sum$`Total Soft`=round(soft.sum$`Total Soft`, 2)
  yers=c(iy, iy-1, iy-2)
  soft.sum=soft.sum[soft.sum$Year %in% yers,]
  
  write.table(soft.sum,file=file.path(wd, "softsummary.csv"), sep=",")
  
  #Save a pdf version of this table
  pdf(file = file.path(wd,  "softsummary.pdf"))
  grid.table(soft.sum, theme=ttheme_default(base_colour="black"), rows=NULL)
  dev.off()
  
  options(knitr.kable.NA = '-')
  #names(table1a) = c("Area", "Year", "Landings", "% Observed", "Observed Soft", "Total Soft")
  tx = soft.sum[,c("Area", "Year", "Landings", "% Observed", "Observed Soft", "Total Soft")]
  tx$Area = "         "
  tx %>%
    kbl(row.names = F, "latex", longtable = F, booktabs = T) %>%
    pack_rows(index = table(soft.sum$Area), indent = F) %>%
    kable_classic() %>%
    column_spec(1, bold = T, width = "5em") %>%
    row_spec(0,bold=TRUE) %>% 
    kable_styling(latex_options = c("striped", "scale_down")) %>%
    as_image(file = file.path(wd,  "softsummary2.pdf"))
  
  
  
  #-------------------------------------------------
  # Use below to save a text file of the locations of soft crab to plot in MapInfo
  
  #years=unique(x$year)
  #
  #for (y in years)    {
  #
  
  x=jj
  #iy= max(as.numeric(as.character(x$year)))
  iyear=which(x$year %in% iy)
  
  # Need to remove any extraeneous points
  xyear=x[iyear,]
  xyear=xyear[!is.na(xyear$cfa),]
  
  xyear$X=xyear$lon
  xyear$Y=(xyear$lat)
  xyear$EID =1:nrow(xyear)
  #set up for mapping
  xyear$color=NA
  xyear$color="green4"
  xyear$color[which(xyear$percentsoft>=10)]="yellow"
  xyear$color[which(xyear$percentsoft>=20)]="red"
  xyear$color=(as.factor(xyear$color))
  
  as.numeric(paste(1:nrow(xyear),  sep=""))
  xyear=as.EventData(xyear, projection="LL")
  
  
  #---------------------------------------------
  # NENS Soft Crab Map
  
  filename=paste(iy, "_nens_soft_crab_positions_",hl, ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  softbyarea(xyear, "nens")
  dev.off()
  
  filename=paste("nens_soft_crab_positions_",hl, ".pdf", sep="")
  pdf(file=file.path(wd, filename))
  softbyarea(xyear, "nens")
  dev.off()
  
  
  #----------------------------------------------------------
  # CFA 23 Soft Crab Map
  
  
  filename=paste(iy, "_cfa23_soft_crab_positions_",hl, ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  softbyarea(xyear, "cfa23zoom")
  dev.off()
  
  filename=paste("cfa23_soft_crab_positions_",hl, ".pdf", sep="")
  pdf(file=file.path(wd, filename), width = 7)
  softbyarea(xyear, "cfa23zoom")
  dev.off()
  
  #----------------------------------------------------------
  # CFA 24 Soft Crab Map
  
  
  filename=paste(iy, " CFA24 Soft Crab Positions(",hl, ").emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  softbyarea(xyear, "cfa24zoom")
  dev.off()
  
  filename=paste("cfa24_soft_crab_positions_",hl, ".pdf", sep="")
  pdf(file=file.path(wd, filename), width = 7)
  softbyarea(xyear, "cfa24zoom")
  dev.off()
  
  
  #######################
  #Following Script Looks at Crab Measured and Creates Stacked Barplots for Each Area
  #######################
  
  # Remove CW's outside norms and remove production (pre-sorted) samples
  # need to determine which dataset to use
  
  
  l=lfobsquery
  l=l[is.finite(l$FISH_LENGTH),]
  FilterCW=l[l$FISH_LENGTH>50 & l$FISH_LENGTH<170,]
  a=FilterCW
  
  
  # --------------------------------------
  # convert lat's and long's to recognizable format for recode.areas
  
  h=names(a)
  h[h=="LATITUDE"] = "lat"
  h[h=="LONGITUDE"] = "lon"
  names(a) = h
  a$lon=-a$lon
  a$year=NA
  
  
  
  #----------------------------------------
  # create columns for area and CFA
  
  x=a
  
  cfa=c("cfanorth", "cfa23", "cfa24", "cfa4x")
  x$cfa=NA
  for  (a in cfa){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$cfa[rowindex]=a
  }
  
  area=c("cfanorth", "cfasouth", "cfa4x")
  
  x$area=NA
  for (a in area){
    rowindex= filter.region.polygon(x,recode.areas(a))
    x$area[rowindex]=a
  }
  
  
  x$area=factor(x$area, levels=c("cfa4x","cfasouth","cfanorth"), labels=c("4X","South","North"))
  x$cfa=factor(x$cfa, levels=c("cfa4x","cfa24","cfa23","cfanorth"), labels=c("4X","CFA 24","CFA 23","North"))
  
  #4X fishing activity on 4X line doesn't get recoded properly by recode.areas
  #force thes into 4X
  
  x$month=lubridate::month(x$BOARD_DATE, label = T)
  x$long=NA
  x$long=-(as.numeric(x$lon))
  i4x=which(x$month %in% c("Nov", "Dec", "Jan", "Feb") & x$long>63.3)
  x$cfa[i4x]="4X"
  
  
  #Following lines force 4X into proper dataset by year
  # i.e. Winter 2011 gets grouped under 2010 "Season"
  i4x=which(x$cfa %in% "4X")
  
  x$BOARD_DATE[i4x] = x$BOARD_DATE[i4x] - lubridate::days(91) #subtracts 91 days from all dates ( 7776000seconds)
  x$year=as.character(lubridate::year(x$BOARD_DATE))        #determine year
  x$BOARD_DATE[i4x] = x$BOARD_DATE[i4x] + lubridate::days(91) #add the days back
  
  
  #  logs$year=as.character(lubridate::year(logs$date_landed))  
  
  
  yrs=as.character(sort(as.numeric(unique(x$year))))
  x$year=factor(x$year,levels=yrs, labels=yrs)
  x$season=NA
  x$season[x$month %in% c("Jan", "Feb", "Mar")]="Winter"
  x$season[x$month %in% c("Apr", "May", "Jun")]="Spring"
  x$season[x$month %in% c("Jul", "Aug", "Sep")]="Summer"
  x$season[x$month %in% c("Oct", "Nov", "Dec")]="Fall"
  
  #iyear=as.character(max(as.numeric(as.character(x$year))))
  iyear=iy
  iyear2=iy-1
  
  yrs=c(iyear2, iyear)
  
  #Produce lentgh / freqs for past two seasons for inclusion in documents
  
  for (yr in yrs) {
    # divide into north, south, cfa 23, cfa 24, and 4X as required
    i=which(x$year==yr)
    inorth= which(x$area=="North")
    isouth= which(x$area=="South")
    i23= which(x$cfa=="CFA 23")
    i24= which(x$cfa=="CFA 24")
    i4x= which(x$cfa=="4X")
    
    n=x[intersect(i,inorth) ,]
    s=x[intersect(i,isouth) ,]
    cfa4x=x[intersect(i,i4x) ,]
    cfa23=x[intersect(i,i23) ,]
    cfa24=x[intersect(i,i24) ,]
    
    # --------------------------------------
    # use the following script to count number of crab sampled
    crabobservedsouth=length(s$FISH_NO)
    crabobservednorth=length(n$FISH_NO)
    crabobserved4x=length(cfa4x$FISH_NO)
    crabobservedcfa23=length(cfa23$FISH_NO)
    crabobservedcfa24=length(cfa24$FISH_NO)
    
    print(paste("Crab Measured in S-ENS in ",yr,"="," ",crabobservedsouth))
    print(paste("Crab Measured in N-ENS in ",yr,"="," ",crabobservednorth))
    print(paste("Crab Measured in 4X in ",yr,"="," ",crabobserved4x))
    print(paste("Crab Measured in CFA 23 in ",yr,"="," ",crabobservedcfa23))
    print(paste("Crab Measured in CFA 24 in ",yr,"="," ",crabobservedcfa24))
    
    
    
    
    # --------------------------------------
    # divide into 5 CC's and create histograms of CW
    #Ensure that a folder has been created for current year in C:/Rsaves/fishery....
    
    areas=c("North", "South", "4X")
    
    for (a in areas) {
      
      #wd=paste("C:/Rsaves/fishery/",iyear,"/observer", sep="")
      #setwd(wd)
      
      iarea=which(x$area==a)
      n=x[intersect(i,iarea) ,]
      if(nrow(n)==0){
        warning(paste("No data for area ", a, " and year ", yr, "!!", sep = "," ))
        next()
      }
      nCC1=n[n$SHELLCOND_CD==1,]
      nCC2=n[n$SHELLCOND_CD==2,]
      nCC3=n[n$SHELLCOND_CD==3,]
      nCC4=n[n$SHELLCOND_CD==4,]
      nCC5=n[n$SHELLCOND_CD==5,]
      nhistCC1= hist(nCC1$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC2= hist(nCC2$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC3= hist(nCC3$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC4= hist(nCC4$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC5= hist(nCC5$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      plot= rbind(nhistCC1$counts, nhistCC2$counts, nhistCC3$counts, nhistCC4$counts, nhistCC5$counts)
      
      cc1perc= round((sum(nhistCC1$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc2perc= round((sum(nhistCC2$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc3perc= round((sum(nhistCC3$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc4perc= round((sum(nhistCC4$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc5perc= round((sum(nhistCC5$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc = c(cc5perc, cc4perc, cc3perc, cc2perc, cc1perc)
      total= (sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts))
      
      
      # --------------------------------------
      # create stacked barplots with legends
      
      if (n$area[1]=="South"){
        areaname="S-ENS"}
      if (n$area[1]=="North"){
        areaname="N-ENS"}
      
      if (n$area[1]=="4X"){
        areaname="4X"}
      
      
      
      filename=paste(yr," ", areaname, " Size Freq", ".emf", sep="")
      
      emf(file=file.path(wd, filename), bg="white")
      splot(plot, areaname, cc, total, yr)
      dev.off()
      print(paste(filename, " created", sep=""))
      
      filename=paste(yr,areaname, "size_freq.pdf", sep="_")
      pdf(file=file.path(wd, filename))
      splot(plot, areaname, cc, total, yr)
      dev.off()
      print(paste(filename, " created", sep=""))
      
    }
  }
  # --------------------------------------
  # to plot areas (N-ENS and S-ENS) by season
  
  areas=c("North", "South")
  
  for (a in areas) {
    iarea=which(x$area==a)
    n=x[intersect(i,iarea) ,]
    if(nrow(n)==0){
      warning(paste("No data for area ", a, " and year ", yr, "!!", sep = "," ))
      next()
    }
    
    if (n$area[1]=="South"){
      areaname="S-ENS"}
    if (n$area[1]=="North"){
      areaname="N-ENS"}
    
    seasons=c("Spring","Summer")
    
    for  (s in seasons) {
      
      z=n[n$season==s,]
      if(nrow(z)==0){
        warning(paste("No data for area ", areaname, " and season ",s, "!!", sep = "," ))
        next()
      }
      nCC1=z[z$SHELLCOND_CD==1,]
      nCC2=z[z$SHELLCOND_CD==2,]
      nCC3=z[z$SHELLCOND_CD==3,]
      nCC4=z[z$SHELLCOND_CD==4,]
      nCC5=z[z$SHELLCOND_CD==5,]
      nhistCC1= hist(nCC1$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC2= hist(nCC2$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC3= hist(nCC3$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC4= hist(nCC4$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      nhistCC5= hist(nCC5$FISH_LENGTH, breaks=seq(50, 170, by=3),  plot=FALSE)
      plot= rbind(nhistCC1$counts, nhistCC2$counts, nhistCC3$counts, nhistCC4$counts, nhistCC5$counts)
      
      cc1perc= round((sum(nhistCC1$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc2perc= round((sum(nhistCC2$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc3perc= round((sum(nhistCC3$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc4perc= round((sum(nhistCC4$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      cc5perc= round((sum(nhistCC5$counts)/(sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts)))*100, 1)
      total= (sum(nhistCC1$counts)+ sum(nhistCC2$counts)+ sum(nhistCC3$counts)+ sum(nhistCC4$counts)+ sum(nhistCC5$counts))
      
      
      # --------------------------------------
      # create stacked barplots with legends
      
      
      filename=paste(iyear," ", areaname," ", z$season[1]," Size Freq", ".emf", sep="")
      require(devEMF)
      emf(file=file.path(wd, filename), bg="white")
      
      SENS= barplot(plot [c(5:1),], space=0,names.arg=seq(50, 170, by=3)[-1],
                    main=paste(iyear,z$season[1],areaname,sep=" ") ,
                    legend.text=c(paste("CC5 (",cc5perc,"%)"),
                                  paste("CC4 (",cc4perc,"%)"),
                                  paste("CC3 (",cc3perc,"%)"),
                                  paste("CC2 (",cc2perc,"%)"),
                                  paste("CC1 (",cc1perc,"%)")),
                    xlab="Carapace Width in mm", ylab="Number of Crab")
      text(x=ncol(plot)*0.88, y=max(colSums(plot))*0.72, labels=paste("n=",total,"") )
      abline(v=14.5, lty=2)
      dev.off()
      print(paste(filename, " created", sep=""))
      
      filename=paste( areaname, z$season[1],"size_freq.pdf", sep="_")
      pdf(file=file.path(wd, filename))
      
      SENS= barplot(plot [c(5:1),], space=0,names.arg=seq(50, 170, by=3)[-1],
                    main=paste(iyear,z$season[1],areaname,sep=" ") ,
                    legend.text=c(paste("CC5 (",cc5perc,"%)"),
                                  paste("CC4 (",cc4perc,"%)"),
                                  paste("CC3 (",cc3perc,"%)"),
                                  paste("CC2 (",cc2perc,"%)"),
                                  paste("CC1 (",cc1perc,"%)")),
                    xlab="Carapace Width in mm", ylab="Number of Crab")
      text(x=ncol(plot)*0.88, y=max(colSums(plot))*0.72, labels=paste("n=",total,"") )
      abline(v=14.5, lty=2)
      dev.off()
      print(paste(filename, " created", sep=""))
    }
  }
  
  #--------------------------------
  # To Determine mean size of catch
  
  
  meansizes = compute.means( x=x, var=c("FISH_LENGTH"), index=c("year", "cfa")  )
  # Add a calculated mass in g for the mean size crab
  meansizes$mass=NA
  meansizes$mass=(1.543*10^-4)*((meansizes$FISH_LENGTH)^3.206)
  ## changed this to get 2020 data to add to the plot
  toplot=meansizes
  #toplot=meansizes[as.numeric(meansizes$year)<(max(as.numeric(meansizes$year))),]
  names(toplot)=c("year", "cfa", "cw", "mass")
  
  
  # Create a plot of mean cw by year for each area
  
  #filename=paste(iyear," ", "Mean CW Observed", ".emf", sep="")
  filename=paste(iyear," ", "Mean CW Observed", ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  cwplot(toplot)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  filename=paste("mean_cw_observed", ".pdf", sep="")
  pdf(file=file.path(wd, filename))
  cwplot(toplot)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  #Create a basic plot showing the relationship of cw to mass
  
  
  base=data.frame(matrix(NA, nrow=150, ncol=2))
  names(base)=c("cw", "mass")
  base$cw=c(1:150)
  base$mass=(1.543*10^-4)*((base$cw)^3.206)
  
  filename=paste(iyear," ", "CW vs Mass", ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  baseplot(x=base)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  filename=paste("cw_vs_mass", ".pdf", sep="")
  pdf(file=file.path(wd, filename))
  baseplot(x=base)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  
  #Create a plot of mean mass by year for each area
  
  filename=paste(iyear," ", "Mean Weight Observed", ".emf", sep="")
  win.metafile(file=file.path(wd, filename), width=10)
  massplot(x = toplot)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  filename=paste("mean_weight_observed.pdf", sep="")
  pdf(file=file.path(wd, filename))
  massplot(x = toplot)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  
  #--------------------------------------------
  #Create a map of survey stations for past year
  
  # Call variable required by PBS Mapping
  
  #All sets
  
  
  all$yr=NA
  all$yr=lubridate::year(all$BOARD_DATE)
  all$yr[which(lubridate::month(all$BOARD_DATE) < 4)] = all$yr[which(lubridate::month(all$BOARD_DATE) < 4)] - 1 
  names(all)=tolower(names(all))
  all$X=-all$start_long
  all$Y=all$start_lat
  all$EID=1:nrow(all)
  all=as.EventData(all, projection="LL")
  
  # Creates map of all survey stations for a given year
  a=all[all$yr==iy,]
  filename="All_survey_stations.pdf"
  
  #PDF
  pdf(file=file.path(wd, filename), width = 9)
  makemap(a, area="all", title=paste(iy, "Snow Crab Survey", sep=" "))
  addPoints(data=a, col="black", pch=20, cex=.6)
  dev.off()
  print(paste(filename, " created", sep=""))
  
  
  planned = read.csv(file.path("S:", "Survey", "Annual Files by Year", paste("ENS Snow Crab ",iy," Survey", sep = ""), paste(iy, "_Survey_Stations.csv", sep ="")))
  notcomplete = planned[which(!as.numeric(planned$Station) %in% as.numeric(a$station)),]
  notcomplete$X = notcomplete$Long.Start
  notcomplete$Y = notcomplete$Lat.Start
  notcomplete$EID=1:nrow(notcomplete)
  notcomplete=as.EventData(notcomplete, projection="LL")
  
  filename="All_survey_stations_plus_fail.pdf"
  
  pdf(file=file.path(wd, filename), width = 9)
  makemap(a, area="all", title=paste(iy, "Planned Snow Crab Survey", sep=" "))
  addPoints(data=a, col="black", pch=20, cex=.6)
  addPoints(data=notcomplete, col="red", pch=20, cex=.6)
  
  dev.off()
  print(paste(filename, " created", sep=""))
  
  
  
  #addition for catch rate table for presentation
  
  logs.fixed2 = cleanlogscr
  areas = unique(na.omit(logs.fixed$cfa0))
  output = NULL
  for (a in areas) {
    q = which (logs.fixed$cfa0 == a)
    catchrate = sum(logs.fixed$pro_rated_slip_wt_lbs[q], na.rm=T)/(sum(logs.fixed$num_of_traps[q], na.rm=T))
    output = rbind(output, data.frame(area=a, catchrate=catchrate))
  }
  logs.fixed$area=logs.fixed$cfa0
  logs.fixed$area[logs.fixed$cfa0=="cfa23"]="CFA 23"
  logs.fixed$area[logs.fixed$cfa0=="cfa24"]="CFA 24"
  
  cpue = compute.catchrate( x=logs.fixed, var="catchrate", index=c("year","area" ) )
  names(cpue)=c("year", "area", "cpuelbs")
  cpue$cpuekg=(cpue$cpuelbs/2.204626)
  cpue$area[cpue$area=="north"]="N-ENS"
  cpue$area[cpue$area=="cfa4X"]="CFA 4X"
  
  year.land$cfa[year.land$cfa=="cfa23"]="CFA 23"
  year.land$cfa[year.land$cfa=="cfa24"]="CFA 24"
  year.land$cfa[year.land$cfa=="cfa4x"]="CFA 4X"
  year.land$cfa[year.land$cfa=="north"]="N-ENS"
  
  test<- cpue %>%
    dplyr::left_join(year.land, by = c("year" = "year", "area" = "cfa"))
  
  test$area[test$area=="cfa4X"]="CFA 4X"
  
  table1<-test %>%
    dplyr::select(area, year, mt, cpuekg) %>%
    dplyr::group_by(area) %>%
    dplyr::filter(year > '2001')
  
  source(file.path("S:", "CA's", "CA_db", "CA_database_functions.R"))
  TAC = CA.getTable("TAC")
  TACy = TAC[which(TAC$yr > iy-3),]
  TACy$tac[which(TACy$area == "CFA 24")] = TACy$tac[which(TACy$area == "CFA 24")] + TACy$tac[which(TACy$area == "CFA 24 Millbrook")] 
  TACy = TACy[-which(TACy$area == "CFA 24 Millbrook"),] 
  TACy$area[which(TACy$area == "4X")] = "CFA 4X"
  names(TACy) = c("year", "area", "TAC")
  table1a = merge(table1, TACy, by = c("area", "year"))
  table1a$cpuepounds = table1a$cpuekg/.45359
  options(knitr.kable.NA = '-')
  
  names(table1a) = c("Area", "Year", "Landings", "Catch Rate (kg/trap)", "TAC", "Catch Rate (lbs/trap)")
  tx = table1a[,c("Area","Year", "TAC", "Landings", "Catch Rate (lbs/trap)")]
  tx$Area = "         "
  tx %>%
    kbl(digits = 0, "latex", longtable = T, booktabs = T) %>%
    pack_rows(index = table(table1a$Area), indent = F) %>%
    kable_classic() %>%
    column_spec(1, bold = T, width = "5em") %>%
    row_spec(0,bold=TRUE) %>% 
    kable_styling(latex_options = c("striped", "scale_down")) %>%
    as_image(file = file.path(wd,  "landing_data_Tac.pdf"))
  
  
  write.csv(table1, file = file.path(wd, "landing_data2.csv"), row.names = FALSE)
  write.csv(table1a, file = file.path(wd, "landing_data_Tac.csv"), row.names = FALSE)
  
  write.table(north,file = file.path(wd, "North monthly cpue.csv"), sep=",")
  write.table(c23,file = file.path(wd, "c23 monthly cpue.csv"), sep=",")
  write.table(c24,file = file.path(wd, "c24 monthly cpue.csv"), sep=",")
  write.csv(wk, file = file.path(wd, "wk.csv"), row.names = FALSE)
  write.csv(logs.fixed, file = file.path(wd, "logcheck.csv"), row.names = FALSE)
}
compute.catchrate = function (x, var, index) {
  for (i in index) x[,i] = as.factor(x[,i])
  res = as.data.frame.table( by( data=x, INDICES=x[,index],
                                 FUN=function(q) {
                                   sum( q$pro_rated_slip_wt_lbs, na.rm=T)/sum(q$num_of_traps, na.rm=T)
                                 } ) )
  names(res) = c(index, var)
  for (i in index) { res[,i] = as.character( res[,i] ) }
  return(res) }


compute.sums = function (x, var, index) {
  for (i in index) x[,i] = as.factor(x[,i])
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
                                     FUN=function(q) { sum(q, na.rm=T)}, simplify=T))
  names(res) = c(index, var)
  for (i in index) { res[,i] = as.character( res[,i] ) }
  return(res) }

aland=function(x){
  cf=unique(x$cfa)
  if(length(unique(x$cfa)) == 3){
    cols=c("blue","red", "green4")
    point=c(1,2,15)
    
  }
  else{
    cols=c("blue","red", "black", "green4")
    point=c(1,2,4,15)
    
  }
  
  plot(x$year, x$cpuekg, type="n", ylab="Catch Rate (kg/ trap)", xlab="Year", ylim=c(0,1.2*(max(x$cpuekg, na.rm = T))), lty=1, col="red", pch=19)
  
  for (y in 1:length(cf)) {
    c=x[x$cfa==cf[1],]
    c=x[x$cfa==cf[y],]
    lines(x=c$year, y=c$cpuekg, col=cols[y], lwd=2 )
    points(x=c$year, y=c$cpuekg, col=cols[y], pch=point[y])
    
  }
  legend("topleft",paste(cf), bty="n", col=cols, lwd=2, pch=point, lty=1)
  #legend("topleft",paste(cf), bty="n", col=cols, lty=1)
  
}
# calculate mean catch rates for the season by CFA & season (Q of year)
compute.catchrate.quarter = function (x, var, index) {
  for (i in index) x[,i] = as.factor(x[,i])
  res = as.data.frame.table( by( data=x, INDICES=x[,index],
                                 FUN=function(q) {
                                   sum( q$pro_rated_slip_wt_lbs, na.rm=T)/sum(q$num_of_traps, na.rm=T)
                                 } ) )
  names(res) = c(index, var)
  for (i in index) { res[,i] = as.character( res[,i] ) }
  return(res) }

smoothcatch=function(x){
  
  cols=c("black", "blue","red", "green4" )
  icon = c(4, 1, 2, 0)
  x$yeargroup = x$year
  x$yeargroup[which(x$area == "4X" & x$week < 6 )] = as.numeric(x$year[which(x$area == "4X" & x$week < 6 )])-1
  retx = ggplot(x, aes(time, cpuekg, colour=area, shape = area,
                       group=interaction(yeargroup, area))) + 
    geom_point(size = 1, stroke = .5) + geom_line(size = .5) + xlab("Week") + ylab("Catch Rate (kg/ trap)") + 
    scale_color_manual(values = cols) + scale_shape_manual(values=icon) + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = c(0.95, 0.9),legend.key = element_rect(colour = NA, fill = NA),)
  
  return(retx)
  
  #cf=unique(x$area)
  #cf = cf[order(cf)]
  #non4x=x[x$area %in% c("CFA 23", "CFA 24", "N-ENS"),]
  #  fc=unique(non4x$area)
  
  # cols=c("black", "blue","red", "green4" )
  #point=c(8, 16, 17, 15)
  
  #  plot(x$time,x$cpuekg , type="n", ylab="Catch Rate (kg/ trap)",
  #       xlab="Week", ylim=c(0,(max(x$cpuekg))), xaxp=c(min(as.numeric(pst)), max(as.numeric(pst)), 2))
  #  legend("topright",paste(cf), bty="n", col=cols, pch=point)
  
  ## for (y in 1:length(cf)) {
  ##  c=x[x$area==cf[y],]
  #  c=c[order(c$time),]
  #  c=c[c$run_lbs>quantile(c$run_lbs,.05),]
  #  points(x=c$time, y=c$cpuekg, col=cols[y], pch=point[y], cex=0.6)
  #  for (y in 1:length(cf) )
  #  {for (p in pst){
  #    c=x[x$area==cf[y],]
  #    c=c[order(c$time),]
  #    c=c[c$run_lbs>quantile(c$run_lbs,.05),]
  #    this=c[c$year==p,]
  #    if(x$area)
  #    lines(x=this$time, y=this$cpuekg, col=cols[y], cex=0.6)
  #  }}}
  
}


compute.means = function (x, var, index) {
  for (i in index) x[,i] = as.factor(x[,i])
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
                                     FUN=function(q) { mean(q, na.rm=T)}, simplify=T))
  names(res) = c(index, var)
  for (i in index) { res[,i] = as.character( res[,i] ) }
  return(res) }

change.resolution = function (x, res=10) {
  x = convert.degdec2degmin(x)
  x$lon = trunc(x$lon*(100/res)) / (100/res)
  x$lat = trunc(x$lat*(100/res)) / (100/res)
  x = convert.degmin2degdec(x)
  return(x)
}

convert.degmin2degdec = function (x) {
  x$lat = trunc(x$lat) + round((x$lat - trunc(x$lat)) /60 * 100, 6)
  x$lon = trunc(x$lon) + round((x$lon - trunc(x$lon)) /60 * 100, 6)
  return (x)
}

plotmonthlycpue=function(c23, c24, north, moncpue, year){
  # win.graph(width = 14, height = 8)
  toplot=c23 #change for each cfa to create CPUE by month
  
  
  plot(toplot$lbspertrap, main= paste(year,"Monthly","Catch Rate", sep=" "),
       cex.main=1.2, xlab= "Month", type="n",axes=F, ylab="Catch Rate (lbs/trap)",
       ylim=c(0,1.2*(max(moncpue$lbspertrap))), col="blue")
  axis(1, at = 1:length(toplot$lbspertrap), labels = toplot$month, cex.axis=0.8)
  axis(2)
  toplot$lbspertrap[toplot$lbspertrap=="0"]=NA
  points(toplot$lbspertrap, pch=19, col="blue")
  lines(toplot$lbspertrap, col="blue", lwd=2)
  text(5.3, 60, toplot$cfa, col="blue", cex=1.5, pos=4)
  
  
  #--- Following text adds another data series to the existing plot
  toplot=c24
  toplot=toplot[toplot$month %in% c("April","May","June","July","August","September"),]
  toplot$lbspertrap[toplot$lbspertrap=="0"]=NA
  points(toplot$lbspertrap, pch=19, col="red")
  lines(toplot$lbspertrap, col="red", lwd=2)
  text(5.3, 40, toplot$cfa, col="red", cex=1.5, pos=4)
  
  #--- Following text adds another dataseries to the existing plot
  toplot=north
  toplot=toplot[toplot$month %in% c("April","May","June","July","August","September"),]
  toplot$kg[toplot$kg=="0"]=NA
  points(toplot$kg, pch=19, col="green4")
  lines(toplot$kg, col="green4", lwd=2)
  text(5.3, 20, toplot$cfa, col="green4", cex=1.5, pos=4)
  
  
}

compute.vessels = function (x, var, index) {
  for (i in index) x[,i] = as.factor(x[,i])
  res = as.data.frame.table( by( data=x, INDICES=x[,index],
                                 FUN=function(q) {
                                   length(unique(q$vr_number))
                                 } ) )
  names(res) = c(index, var)
  for (i in index) { res[,i] = as.character( res[,i] ) }
  
  return(res)   }



fishmap=function(a, b, area, cy){
  makemap(x,area=area, title="Reported Fishing Positions", addlabels=F)
  #last= logs[iyear1,]
  last=as.EventData(b, projection= "LL")
  addPoints(data=last, col="yellow", pch=20, cex=0.8)
  #current= logs[iyear,]
  current=as.EventData(a, projection= "LL")
  addPoints(data=current, col="red", pch=20, cex=0.8)
  coverup(area=area)
  legend("bottomright", paste(c(cy-1, cy)), pch=20, col=c("yellow","red"), bty="o", bg="grey75", pt.cex=1.5)
  
}

seasonmap=function(a, b, area, cy, season){
  
  makemap(x,area=area, title=paste(season, " Fishing Positions", sep = ""), addlabels=F)
  
  last=as.EventData(b, projection= "LL")
  addPoints(data=last, col="yellow", pch=20, cex=0.8)
  current=as.EventData(a, projection= "LL")
  addPoints(data=current, col="red", pch=20, cex=0.8)
  
  coverup(area=area)
  legend("bottomright", paste(c(cy-1, cy)), pch=20, col=c("yellow","red"), bty="o", bg="grey75", pt.cex=1.5)
}
softbyarea=function(dta, areastr){
  #makemap(dta, area=areastr, addlabels=F,title=paste(dta$year[1]))
  makemap(dta, area=areastr, addlabels=F)
  if(nrow(dta[dta$color=="green4",])>0)addPoints(data=dta[dta$color=="green4",], col= "black", bg="green4", pch=21)
  if(nrow(dta[dta$color=="yellow",])>0)addPoints(data=dta[dta$color=="yellow",], col= "black", bg="yellow", pch=21)
  if(nrow(dta[dta$color=="red",])>0)addPoints(data=dta[dta$color=="red",], col= "black", bg="red", pch=21)
  
  points(x=-59.5, y=47.05,  col="black", bg="green4", pch=21, cex=1.1) # add legend like key
  text("0-10% Soft", x= -59.45, y=47.05, font=1, cex=.85, pos=4, col="white")
  points(x=-59.5, y=47,  col="black", bg="yellow", pch=21, cex=1.1) # add legend like key
  text("10-20% Soft", x= -59.45, y=47, font=1, cex=.85, pos=4, col="white")
  points(x=-59.5, y=46.95,  col="black", bg="red", pch=21, cex=1.1) # add legend like key
  text("20+ % Soft", x= -59.45, y=46.95, font=1, cex=.85, pos=4, col="white")
}


splot=function(dta, areaname, cc, tot, yr){
  SENS= barplot(dta [c(5:1),], space=0,names.arg=seq(50, 170, by=3)[-1],
                main=paste(yr,"Size Frequency Distribution in",areaname, sep=" ") ,
                legend.text=c(paste("CC5 (",cc[1],"%)"),
                              paste("CC4 (",cc[2],"%)"),
                              paste("CC3 (",cc[3],"%)"),
                              paste("CC2 (",cc[4],"%)"),
                              paste("CC1 (",cc[5],"%)")),
                xlab="Carapace Width in mm", ylab="Number of Crab")
  text(x=ncol(dta)*0.88, y=max(colSums(dta))*0.72, labels=paste("n=",tot,"") )
  abline(v=14.5, lty=2)
}
cwplot=function(tplt){
  cf=unique(tplt$cfa)
  cols=c("black", "red", "cornflowerblue","green4")
  point=c(1,2,4,15)
  
  
  plot(tplt$year, tplt$cw, type="n", main= "Mean CW Observed", cex.main=1.2,
       ylab="Mean CW(mm)", xlab="Year", ylim=c(0.8*(min(tplt$cw, na.rm=T)),1.2*(max(tplt$cw, na.rm=T))), lty=1, col="red", pch=19)
  
  for (y in 1:length(cf)) {
    c=tplt[tplt$cfa==cf[1],]
    c=tplt[tplt$cfa==cf[y],]
    lines(x=c$year, y=c$cw, col=cols[y], lwd=2 )
    points(x=c$year, y=c$cw, col=cols[y], pch=point[y])
  }
  
  legend("bottomright",paste(cf), bty="n", col=cols, pch=point)
  legend("bottomright",paste(cf), bty="n", col=cols, lty=1)
}
baseplot=function(x){
  plot(x$cw, x$mass, type="l", main= "Weight at Size", cex.main=1.2,
       ylab="Mass(g)", xlab="CW(mm)", ylim=c(min(x$mass, na.rm=T),1.2*(max(x$mass, na.rm=T))),
       lty=1, pch=19, col="black", lwd=2)
  abline(v=95, lty=2, col="red")
}
massplot=function(x){
  
  cf=unique(x$cfa)
  cols=c("black", "red", "cornflowerblue","green4")
  point=c(1,2,4,15)
  
  plot(x$year, x$mass, type="n", main= "Calculated Mean Weight", cex.main=1.2,
       ylab="Mass(g)", xlab="Year", ylim=c(0.8*(min(x$mass, na.rm=T)),1.2*(max(x$mass, na.rm=T))), lty=1, col="red", pch=19)
  
  for (y in 1:length(cf)) {
    c=x[x$cfa==cf[1],]
    c=x[x$cfa==cf[y],]
    lines(x=c$year, y=c$mass, col=cols[y], lwd=2 )
    points(x=c$year, y=c$mass, col=cols[y], pch=point[y])
  }
  
  legend("bottomright",paste(cf), bty="n", col=cols, pch=point)
  legend("bottomright",paste(cf), bty="n", col=cols, lty=1)
}
setup.lattice.options = function () {
  trellis.par.set(col.whitebg())
  s.bkg = trellis.par.get("strip.background")
  s.bkg$col="gray95"
  trellis.par.set("strip.background", s.bkg)
}
