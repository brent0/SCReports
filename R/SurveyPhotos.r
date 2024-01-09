### Source this script the call:
#   > archive.photos()
#   > reduce.quality()
#   >
#   > createSurveyPhotosKML(offline = T) # Or with offline = F for server upload
library("EBImage")
library("kmlbuilder")
library("RCurl")
library("aegis")
library("bio.snowcrab")

SC.photo.directory = file.path(data_root, "bio.snowcrab", "data", "photos")
SC.survey.rawdata = file.path(data_root, "bio.snowcrab", "data", "rawsurvey")
photolist = list.files(file.path(SC.photo.directory, "Allphotos"))

archive.photos = function(){
  print("Adding new photos to master directory:")
  dirlist = c(list.files(SC.survey.rawdata, pattern = NULL, all.files = TRUE, full.names = TRUE, recursive = TRUE))

  for (i in 1:length(dirlist)) {
    file.rename(dirlist[i], gsub(" ", "", dirlist[i]))
  }

  dirlist = grep(".JPG", dirlist, ignore.case = TRUE, value = T)

  i = which(grepl("AllPhotos", dirlist)) #Find indicies where Allphotos is in the filename
  if (length(i) != 0)
    dirlist = dirlist[-i] #remove files in AllPhotos directory

  i = which(grepl("APR", dirlist)) #Find indicies where Allphotos is in the filename
  if (length(i) != 0)
    dirlist = dirlist[-i] #remove files in AllPhotos directory


  nfiles = length(dirlist)

  if (nfiles > 0)
    for (f in 1:nfiles) {
      end = unlist(strsplit(dirlist[f], "/"))
      start = end[-length(end)]
      end = end[length(end)]
      yearappend = substr(unlist(strsplit(dirlist[f], "/"))[6], 1, 4)
      end = gsub("a", "", end)
      end = gsub("b", "", end)
      end = gsub("c", "", end)
      end = gsub("d", "", end)
      end = gsub("R", "", end)
      end = gsub("r", "", end)
      end = gsub(".JPG",
                 paste(yearappend, ".JPG", sep = ""),
                 end,
                 ignore.case = TRUE)
      end = gsub("EP", "ep", end)

      i = grepl("ep", end, ignore.case = TRUE)
      if (i == FALSE)
        next


      if (nchar(end) != 13)
        print(paste("unconventional name: ", end))
      start[length(start) + 1] = end
      start = paste(start, collapse = "/")

      file.copy(dirlist[f],start,overwrite = FALSE,recursive = FALSE,copy.mode = TRUE)
      file.copy(start,file.path(SC.photo.directory, "Allphotos"),overwrite = FALSE,recursive = FALSE,copy.mode = TRUE)
      file.remove(start)
    }
  print("Done archiving survey photos")
}

reduce.quality = function(){
  pho.from = file.path(SC.photo.directory, "AllPhotos" )
  pho.to = file.path(SC.photo.directory, "resized" )
  fdr = c(list.files(pho.from, pattern = NULL, all.files = FALSE, full.names = T))
  tdr = c(list.files(pho.to, pattern = NULL, all.files = FALSE))

  fdr = fdr[which(!basename(fdr) %in% tdr)]

  for(i in 1:length(fdr)){
    x <- readImage(fdr[i])
    y <- resize(x, w = 600, h = 400)
    writeImage(y, file.path(pho.to, basename(fdr[i])))
    print(paste(basename(fdr[i]), " resized", sep = ""))
  }

}

createSurveyPhotosKML =  function(offline = T){

  options(digits = 22)

  if(offline){
    outname = "SurveyPhotosOffline.kml"
    kmldat = paste(file.path(".", "AllPhotos") , .Platform$file.sep, sep = "")
    jp = ".jpg"
  }else{
    outname = "SurveyPhotos.kml"
    kmldat ="http://www.enssnowcrab.com/SCimages/SurveyPhotos/"
    kmldat_red ="http://www.enssnowcrab.com/SCimages/SurveyPhotosReduced/"
    jp = ".JPG"
  }

  year.assessment = 2021
  p = bio.snowcrab::load.environment( year.assessment=year.assessment )
  #set =  snowcrab.db( DS="set.clean", p=p )
  set = snowcrab.db(DS = "setInitial", p=p, include.bad = T)
  set = set[which(set$yr >= 2004),]
  set$chrono = format(set$timestamp, format="%Y-%b-%d %H:%M:%S")
  set$temp = round(set$t, 2)
  
  ind = which(nchar(set$station)<3)
  set$station[ind] = paste("0", set$station[ind], sep="")
  ind = which(nchar(set$station)<3)
  set$station[ind] = paste("0", set$station[ind], sep="")
  set$station = paste("ep", set$station, sep="")
  
  
  set = set[rev(order(set$yr)),]

  dfd = split(set, set$station)
  outframe = NULL
  outframe = cbind(names(dfd), NA, NA, NA, NA)
  outframe = data.frame(outframe)
  names(outframe) = c("name", "description", "lat", "lon", "icon_transparency")

for(i in 1:length(dfd)){
    st = dfd[[i]]
    yearsince = abs(st$yr[1] - year.assessment)
    if(yearsince < 8) yearsince = 1 - (yearsince/10)
    else yearsince = .15
    outframe$icon_transparency[i] = yearsince
    outframe$lat[i] = abs(st$lat[1])
    outframe$lon[i] = abs(st$lon[1])
    des = ""
    
    for(j in 1:nrow(st)){

      photoloc = paste(kmldat, st$station[j] ,st$yr[j], sep = "")
      if(offline) photoloc_reduced = photoloc
      else photoloc_reduced = paste(kmldat_red, st$station[j], st$yr[j], sep = "")

     
        
        if(paste(st$station[j] ,st$yr[j], jp, sep = "") %in% photolist){
          des = paste(des, "</br><b>", st$chrono[j], "&nbsp;&nbsp;&nbsp;(Bottom Temperature:", st$temp[j], "&deg;C)</b></br></br><a href= ",photoloc , jp, " target='_blank'><img border='0' src= ", photoloc_reduced, jp, " width='400' height='300'></a></br></br>" ,sep = "")
        }
        else{
          des = paste(des, "<b>(", st$year[j], "&nbsp;&nbsp;&nbsp;(Bottom Temperature:", st$temp[j], "&deg;C)</b></br> No Photo taken, perhaps a human or camera error. </br></br>", sep = "")
        }

    }

    outframe$name[i] = st$station[1] 
    outframe$description[i] = paste("<![CDATA[<table width='400'><TR><TD>" ,des, "</table> ]]>", sep = "")

     
  }
  outframe$lon = as.numeric(outframe$lon)*-1
  outframe$icon_color = "green"
  outframe$icon_href = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
  df = as.data.frame(outframe)
  mykml = RKmlObject()
  mykml$addPoint(df)
  mykml$writekml(path = file.path(data_root , "bio.snowcrab", "output", "kml" , outname))

}

