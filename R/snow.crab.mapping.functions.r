
#----------------------------------------------------
# generate map using PBSmapping plotting functions
#----------------------------------------------------

makemap= function(x,area="ens", addlabels=T, title="") {
sc.shap.dir = file.path(bio.datadirectory, "bio.snowcrab", "maps", "shapefiles")
ped.shap.dir = file.path(bio.datadirectory, "Science", "PED", "OverlayBoxes")
mar.shap.dir = file.path(bio.datadirectory, "polygons", "Basemaps", "Marine", "Bathymetry")
man.shap.dir = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab")
  borders= read.csv(file=file.path(sc.shap.dir,"areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  basemap= importShapefile(file.path(ped.shap.dir,"map_base_region"))
  dm200= importShapefile(file.path(mar.shap.dir,"dm200_region"))
  dm100= importShapefile(file.path(mar.shap.dir,"dm100_region"))
  zones= importShapefile(file.path(man.shap.dir,"sczones2010_polyline"))
  land= importShapefile(file.path(bio.datadirectory,"polygons","Basemaps","Terrestrial","landmass_region"))
  coast=importShapefile(file.path(bio.datadirectory, "polygons", "Basemaps", "Marine", "Coastline","coastline_polyline"))
  axis=importShapefile(file.path(ped.shap.dir,"axis_polyline"))
    
# Provide projection information
#---------------------------------
  proj.abbr=attr(basemap, "projection") # abbreviated projection info
  proj.full=attr(basemap, "prj") # full projection info

  ylim=c(b$slat,b$nlat)
  xlim=c(-(b$wlon),-(b$elon))
 
  plotPolys(basemap, projection=proj.abbr, plt=c(.08,.99,.08,.99), col="royalblue2", border="black", font.lab=2,
  xlab="Longitude", ylab="Latitude", axes=T, tck=-.01, tckLab=TRUE, ylim=ylim, xlim=xlim)
  title(main=title, line=2)

  addPolys(dm200, col="steelblue2", border="steelblue2")

  addPolys(dm100, col="lightblue1", border="lightblue1")

  addLines(zones, col="darkgoldenrod1", lwd=2)
  
#Overlay land and coastline such that any bad data (on land) is hidden
 
  addPolys(land, col="khaki", border="khaki")

  addLines(coast, col="black")
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

#function to add area labels
#--------------------------------------------
  if (addlabels) {
    text("CFA 23", x=-58.05, y=44.55, font=2, cex=0.8)
    text("CFA 24", x=-60.9, y=43.75, font=2, cex=0.8)
    text("CFA 4X", x=-64.2, y=43.25, font=2, cex=0.8)
    text("N-ENS", x= -59.15, y=46.65, font=2, cex=0.8)
  }
}

#Allows replotting of land mass over points once plotted

coverup=function(x, area=area){
  sc.shap.dir = file.path(bio.datadirectory, "bio.snowcrab", "maps", "shapefiles")
  borders= read.csv(file=file.path(sc.shap.dir,"areaborders.csv"), head=T, sep=",")
  
  b=borders[which(borders$area==area),]

land= importShapefile(file.path(bio.datadirectory,"polygons","Basemaps","Terrestrial", "landmass_region"))
coast=importShapefile(file.path(bio.datadirectory, "polygons", "Basemaps", "Marine", "Coastline","coastline_polyline"))
addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

  }

#Determine which shrimp boxes are required based on timeframe

get.box.id = function( datetype=timeframe, dateitem=focus ) {

  boxes = NULL

if (datetype=="month") {
    if (dateitem %in% "April"){
      boxes=c("ceha", "cehb", "lh", "bn", "ch", "ceh")}    
    if (dateitem %in% "May"){
      boxes=c("ceha", "cehb", "lh", "bn","sb", "ch", "ceh")}
    if (dateitem %in% "June"){
      boxes=c("ceha", "cehb", "lh", "bn", "ch", "sb", "ceh", "ech", "eaeh") }
    if (dateitem %in% c("July","August")){
      boxes=c("sb") }
 #non-sense test line
    if (dateitem %in% "Test"){
      boxes=c("ch", "cehb", "eaeh")}
    if (dateitem %in% "Test2"){
      boxes=c("lh")}
  }

 
if (datetype=="week") {
  if (dateitem %in% c(14:19)){
    boxes=c("ceha", "cehb", "lh", "bn23", "ch", "ceh")}
  if (dateitem %in% c(20:22)){
    boxes=c("ceha", "cehb", "lh", "bn23", "bn24", "ch", "ceh", "sb")}
  if (dateitem %in% c(23:24)){
    boxes=c("ceha", "cehb", "lh", "bn23", "bn24", "ch", "sb", "ceh", "ech", "eaeh")}
  #if (dateitem %in% c(25:26)){
#  boxes=c("lh", "bn23", "bn24", "ch", "ech", "sb", "ceha", "cehb", "eaeh")}
  if (dateitem %in% c(25:26)){         #2013 add sliver to east of ceh for weeks 25 & 26
  boxes=c("lh", "bn23", "bn24", "ch", "ech", "sb", "cehb", "eaeh", "sliver")}
    #boxes=c("lh", "bn23", "bn24", "ch", "sb", "ceh", "ech")}  
  if (dateitem %in%  c(27:31)){
    boxes=c("sb")}
    } 

   return(boxes)


if (datetype=="all") {
    boxes=c("ceha", "cehb", "lh", "bn23", "bn24", "ch", "sb", "ceh", "ech", "eaeh")
     return(boxes)
}
}


#determine proper boxes to add

get.boxes2plot=function(datetype=timeframe, dateitem=focus ) {

        if (datetype=="month"){
        monthstoprocess=dateitem
        out = NULL
        for (mn in monthstoprocess) {
        m = get.box.id( "month", mn )
        out = unique( c( out, m ) )
        }
  }


        if (datetype=="week"){  
        weekstoprocess=dateitem
        out=NULL
        for (we in weekstoprocess) {
        w = get.box.id( "week", we )
        out = unique( c( out, w ) )
        }
  }

  if (datetype=="all"){  
        weekstoprocess=dateitem
        out=NULL
        for (we in weekstoprocess) {
        w = get.box.id( "all", we )
        out = unique( c( out, w ) )
        }
  }
return(out)
}

