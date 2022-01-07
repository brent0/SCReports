# This file lists available reports to generate, this is a new project and
# more reports will become available as I move code here.

if(0){ # Do not want to execute report creation on package loading, run
       # individual lines as reports are needed

  if(!exists("current_year"))  current_year = year(now())
  
  
#  GENERATE Gully MPA data that is to be entered into the Gully MPA_Post-Activity # 
#  and Incident Report.                                                           #
#                                                                                 #
#        THIS NEEDS TO BE SUBMITTED 90 DAYS AFTER OPERATIONS WITHING THE AREA     #
#        NOV/DEC                                                                  #
  suppressWarnings(MPA.gully.data(current_year))

  
#  GENERATE January Industry Meeting presentations. Observer and logbook          #
#  databases need to be final from the previous season. e Gully MPA_Post-Activity # 
#  and Incident Report.                                                           #
#                                                                                 #
#        THIS NEEDS TO BE SUBMITTED 90 DAYS AFTER OPERATIONS WITHING THE AREA     #
#        NOV/DEC                                                                  #
  
  January.industry.meeting.data(current_year)
  Accoustic.data(current_year)
    
  ##Need to add one that creates one presentation for all years as this is what we do starting 2021.
  
  rmarkdown::render(file.path(getwd(), "inst", "markdown", "NENSPreRap.Rmd"), 
                    output_file = file.path(bio.datadirectory, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "NENSPreRap.pdf"))
  
  rmarkdown::render(file.path(getwd(), "inst", "markdown", "CFA23PreRap.Rmd"), 
                    output_file = file.path(bio.datadirectory, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "CFA23PreRap.pdf"))
  
  rmarkdown::render(file.path(getwd(), "inst", "markdown", "CFA24PreRap.Rmd"), 
                    output_file = file.path(bio.datadirectory, "bio.snowcrab", "reports", current_year, "JanuaryMeetings", "CFA24PreRap.pdf"))
  
}
