# This file lists available reports to generate, this is a new project and
# more reports will become available as I move code here.

if(0){ # Do not want to execute report creation on package loading, run
       # individual lines as reports are needed

  if(!exists("current_year"))  current_year = year(now()) -1

  rmd.dir = file.path(find.package()[grep("SCReports", find.package())], "inst", "markdown")
  setwd(file.path(code_root, "SCReports", "inst", "markdown"))
  
#  GENERATE Gully MPA data that is to be entered into the Gully MPA_Post-Activity # 
#  and Incident Report.                                                           #
#                                                                                 #
#        THIS NEEDS TO BE SUBMITTED 90 DAYS AFTER OPERATIONS WITHING THE AREA     #
#        NOV/DEC                                                                  #
  suppressWarnings(MPA.gully.data(current_year))

  suppressWarnings(MPA.SAB.data(current_year))
#  GENERATE January Industry Meeting presentations. Observer and logbook          #
#  databases need to be final from the previous season. e Gully MPA_Post-Activity # 
#  and Incident Report.                                                           #
#                                                                                 #
 
  
  
#  !!!!!!! ADD TEMPERATURE PLOTS FOR TRAP LOGGERS  !!!!!!!!!  #
  
  January.industry.meeting.data(current_year)
  V2LSCF.SCtemp.data(current_year)
  V2LSCF.summary.data(current_year)
  Preliminary.survey.data(current_year)
  #Accoustic.data(current_year)
    
  ##Need to add one that creates one presentation for all years as this is what we do starting 2021.
  
  rmarkdown::render("NENSPreRap.Rmd", output_file = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "NENSPreRap.pdf"))
  
  #rmarkdown::render("CFA23PreRap.Rmd", 
  #                  output_file = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "CFA23PreRap.pdf"))
  
  #rmarkdown::render("CFA24PreRap.Rmd", 
  #                  output_file = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "CFA24PreRap.pdf"))

  rmarkdown::render("AllAreasPreRap.Rmd", output_file = file.path(data_root, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "PreRap.pdf"))
  
}
