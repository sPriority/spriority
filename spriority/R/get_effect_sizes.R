library(tidyverse)
library(metafor)

options(dplyr.summarise.inform = FALSE)

data <- read.csv("~/Postdoc/sPRIORITY/Meta-analysis/sPriority_database_230411_BD.csv")[1:855,]

#Get number of species present for each effect size
#Calculate effect size using both control scenarios if available
#CONSIDER TIME LAG

#Paper --> Environment --> Sequence --> Time point --> Species

#####################################
#Create function to get effect sizes
#####################################

#Start simple by only considering scenarios with only two species interacting (A then B, B then A, simultaneous AB)

data<-data%>%
  filter(Number_of_species_in_sequence==2)

#Code function to get effect sizes

get_effect_sizes<-function(data,
                           time_unit=NULL,
                           scenario=2,
                           measure="SMD",
                           report=TRUE){

  #data is a dataframe with the raw data from the sPriority database

  #If not NULL, time_unit can be either "Hours", "Days", "Weeks", "Months" or "Years"
  #Set time_unit=NULL if time units do not have to be standardised across studies

  #There are two possible scenarios to calculate an effect size
  # Scenario 1: keep the time lag between the arrival of the target species or species group and the measurement constant
  # Scenario 2: keep the time lag between the start of the experiment and the measurement (time_after_start) constant

  #measure is a character string to specify which effect size should be calculated (see metafor::escalc).
  #measure can be:
  #"MD": raw mean difference
  #"SMD" (default), standardised mean difference (Hedges' g)
  #"SMDH": standardised mean difference with heteroscedastic population variances in the two groups
  #"SMD1": standardised mean difference where the mean difference is divided by the standard deviation of the second group
  #"SMD1H": same as SMD1, but with heteroscedastic population variances
  #"ROM": log transformed ratio of means

  #If report is TRUE, a detailed report is exported as a text file.

  #Error interceptions

  if (is.null(time_unit)==FALSE) {

    if (time_unit=="Hours"|time_unit=="Days"|time_unit=="Weeks"|time_unit=="Months"|time_unit=="Years") {}
    else {stop("time_unit must be Hours, Days, Weeks, Months, or Years")}

  }

  if (scenario==1|scenario==2) {} else {stop("scenario must be 1 or 2")}

  if (measure=="MD"|measure=="SMD"|measure=="SMDH"|measure=="SMD1"|measure=="SMD1H"|measure=="ROM") {}
  else {stop("measure must be MD, SMD, SMDH, SMD1, SMD1H, ROM. See metafor::escalc for more information.")}

  if (is.logical(report)==FALSE){stop("report must be TRUE or FALSE")}

  #Standardise time units if time_unit is not NULL

  if (is.null(time_unit)==FALSE) {data<-standardise_time_units(data, time=time_unit)}

  #Create list to store effect sizes

  results<-vector("list", nrow(data)) #Pre-allocate an empty list to store results

  #Create text file

  if (report==TRUE){

    file_name<-paste("report", format(Sys.time(),'_%Y%m%d_%H%M%S'), ".txt", sep="")

    write.table(paste("Analysis report - ", Sys.time(), sep=""),
                file=file_name,
                row.names = FALSE,
                col.names = FALSE,
                quote=FALSE)

    write("-------------------------------------",
          file=file_name,
          append=TRUE)

    write(" ",
          file=file_name,
          append=TRUE)

    write(paste("Scenario ", scenario, "\n ", sep=""),
          file=file_name,
          append=TRUE)
    }

  #Loop over data

  for (i in 1:nrow(data)){ #For each observation in data

    if (data$Number_of_introduction_events[i] == 1 | data$Position_in_sequence[i] == 1){

      if (report==TRUE){

        if (data$Number_of_introduction_events[i] == 1) {

          write(paste("Row ", i, ": this is a synchronous scenario. No effect size calculated.", sep=""),
                file=file_name,
                append=TRUE)}

        if (data$Position_in_sequence[i] == 1) {

          write(paste("Row ", i, ": the species or species group arrives first. No effect size calculated.", sep=""),
                file=file_name,
                append=TRUE)}}}

    else {

      #If this is not a synchronous arrival sequence OR the species is not among the first to arrive,
      #then do the following:

      #Create list to store results

      es_list<-list(row_target=i,
                    row_control_reverse=NULL,
                    row_control_sync=NULL,
                    es_reverse=data[i,],
                    es_sync=data[i,])

      #Get infos about observation

      paper<-data$Paper_ID[i] #Get paper ID
      envir<-data$Environment_ID[i] #Get environment ID
      seq<-data$Sequence_ID[i] #Get sequence ID
      time<-data$Time_after_start[i] #Get time point
      sp<-data$Species_name[i] #Get species name

      #Get species list for inoculation sequence

      sp_in_sequence<-sort(unique(data$Species_name[data$Paper_ID==paper &
                                                      data$Sequence_ID==seq &
                                                      data$Environment_ID==envir]))

      #Find the right reference situation

      if (scenario==1){

        #Scenario 1: keep the time lag between the arrival of the target species or species group
        #and the measurement constant

        #######################
        #Find reverse scenario
        #######################

        index_reverse<-which(data$Paper_ID==paper &
                               data$Environment_ID==envir &
                               data$Time_after_start==time-data$Time_since_first_intro[i] &
                               data$Species_name==sp &
                               data$Number_of_introduction_events != 1 & #Multiple introduction events
                               data$Position_in_sequence == 1) #Species must arrive in position 1

        if (length(index_reverse)>0) {

          #Check species composition (2 arrival scenarios must involve the same species)

          for (j in index_reverse){

            sp_in_reverse_sequence<-sort(unique(data$Species_name[data$Paper_ID==paper &
                                                                    data$Sequence_ID==data$Sequence_ID[j] &
                                                                    data$Environment_ID==envir])) #Get species list for reverse sequence

            if (identical(sp_in_sequence, sp_in_reverse_sequence)) {

              es_list$row_control_reverse<-j #Store row index in list if there is a match
              break #Exit loop

            }}}

        if (is.null(es_list$row_control_reverse)) {

          index_reverse<-NULL
          if (report==TRUE) {write(paste("Row ", i, ": no reverse scenario found", sep=""),
                                   file=file_name,
                                   append=TRUE)}}

          else {

            index_reverse<-es_list$row_control_reverse
            if (report==TRUE) {write(paste("Row ", i, ": reverse scenario found on line ", index_reverse, sep=""),
                                     file=file_name,
                                     append=TRUE)}
            }

        ###########################
        #Find synchronous scenario
        ###########################

        index_sync<-which(data$Paper_ID==paper &
                            data$Environment_ID==envir &
                            data$Time_after_start==time-data$Time_since_first_intro[i] &
                            data$Species_name==sp &
                            data$Number_of_introduction_events == 1 & #Only one introduction event
                            data$Position_in_sequence == 1) #Species must arrive in position 1

        if (length(index_sync)>0) {

          #Check species composition (2 arrival scenarios must involve the same species)

          for (j in index_sync){

            sp_in_sync_sequence<-sort(unique(data$Species_name[data$Paper_ID==paper &
                                                                 data$Sequence_ID==data$Sequence_ID[j] &
                                                                 data$Environment_ID==envir])) #Get species list for synchronous sequence

            if (identical(sp_in_sequence, sp_in_sync_sequence)) {

              es_list$row_control_sync<-j #Store row index in list if there is a match
              break #Exit loop

            }}}

        if (is.null(es_list$row_control_sync)) {

          index_sync<-NULL
          if (report==TRUE){write(paste("Row ", i, ": no synchronous scenario found", sep=""),
                                  file=file_name,
                                  append=TRUE)}}

          else {

            index_sync<-es_list$row_control_sync
            if (report==TRUE){write(paste("Row ", i, ": synchronous scenario found on line ", index_sync, sep=""),
                                    file=file_name,
                                    append=TRUE)}}

        ##############################################
        #Calculate effect sizes using metafor package
        ##############################################

        if (is.null(index_reverse)==FALSE){

          #Calculate effect size using the reverse scenario

          es_list$es_reverse<-cbind(es_list$es_reverse,
                                    escalc(measure=measure,
                                           m1i=data$Avg_value_original[i],
                                           sd1i=data$SD_value[i],
                                           n1i=data$n_value[i],
                                           m2i=data$Avg_value_original[index_reverse],
                                           sd2i=data$SD_value[index_reverse],
                                           n2i=data$n_value[index_reverse]))
        }

        else {es_list$es_reverse<-NULL}

        if (is.null(index_sync)==FALSE) {

          #Calculate effect size using the synchronous scenario

          es_list$es_sync<-cbind(es_list$es_sync,
                                 escalc(measure=measure,
                                        m1i=data$Avg_value_original[i],
                                        sd1i=data$SD_value[i],
                                        n1i=data$n_value[i],
                                        m2i=data$Avg_value_original[index_sync],
                                        sd2i=data$SD_value[index_sync],
                                        n2i=data$n_value[index_sync]))
        }

        else {es_list$es_sync<-NULL}

      }

      if (scenario==2){

        #Scenario 2: keep the time lag between the start of the experiment
        #and the measurement (time_since_start) constant

        #######################
        #Find reverse scenario
        #######################

        index_reverse<-which(data$Paper_ID==paper &
                               data$Environment_ID==envir &
                               data$Time_after_start==time & #Constant in scenario 2
                               data$Species_name==sp &
                               data$Number_of_introduction_events != 1 & #Multiple introduction events
                               data$Position_in_sequence == 1) #Species must arrive in position 1

        if (length(index_reverse)>0) {

          #Check species composition (2 arrival scenarios must involve the same species)

          for (j in index_reverse){

            sp_in_reverse_sequence<-sort(unique(data$Species_name[data$Paper_ID==paper &
                                                                    data$Sequence_ID==data$Sequence_ID[j] &
                                                                    data$Environment_ID==envir])) #Get species list for reverse sequence

            if (identical(sp_in_sequence, sp_in_reverse_sequence)) {

              es_list$row_control_reverse<-j #Store row index in list if there is a match
              break #Exit loop

            }}}

        if (is.null(es_list$row_control_reverse)) {

          index_reverse<-NULL
          if (report==TRUE) {write(paste("Row ", i, ": no reverse scenario found", sep=""),
                                   file=file_name,
                                   append=TRUE)}}

          else {

            index_reverse<-es_list$row_control_reverse
            if (report==TRUE) {write(paste("Row ", i, ": reverse scenario found on line ", index_reverse, sep=""),
                                     file=file_name,
                                     append=TRUE)}}

        ###########################
        #Find synchronous scenario
        ###########################

        index_sync<-which(data$Paper_ID==paper &
                               data$Environment_ID==envir &
                               data$Time_after_start==time & #Constant in scenario 2
                               data$Species_name==sp &
                               data$Number_of_introduction_events == 1 & #Only one introduction event
                               data$Position_in_sequence == 1) #Species must arrive in position 1

        if (length(index_sync)>0) {

          #Check species composition (2 arrival scenarios must involve the same species)

          for (j in index_sync){

            sp_in_sync_sequence<-sort(unique(data$Species_name[data$Paper_ID==paper &
                                                                    data$Sequence_ID==data$Sequence_ID[j] &
                                                                    data$Environment_ID==envir])) #Get species list for synchronous sequence

            if (identical(sp_in_sequence, sp_in_sync_sequence)) {

              es_list$row_control_sync<-j #Store row index in list if there is a match
              break #Exit loop

            }}}

        if (is.null(es_list$row_control_sync)) {

          index_sync<-NULL
          if (report==TRUE) {write(paste("Row ", i, ": no synchronous scenario found", sep=""),
                                           file=file_name,
                                           append=TRUE)}}

        else {

          index_sync<-es_list$row_control_sync
          if (report==TRUE) {write(paste("Row ", i, ": synchronous scenario found on line ", index_sync, sep=""),
                                   file=file_name,
                                   append=TRUE)}}

        ##############################################
        #Calculate effect sizes using metafor package
        ##############################################

        if (is.null(index_reverse)==FALSE){

          #Calculate effect size using the reverse scenario

          es_list$es_reverse<-cbind(es_list$es_reverse,
                                    escalc(measure=measure,
                                           m1i=data$Avg_value_original[i],
                                           sd1i=data$SD_value[i],
                                           n1i=data$n_value[i],
                                           m2i=data$Avg_value_original[index_reverse],
                                           sd2i=data$SD_value[index_reverse],
                                           n2i=data$n_value[index_reverse]))
        }

        else {es_list$es_reverse<-NULL}

        if (is.null(index_sync)==FALSE) {

          #Calculate effect size using the synchronous scenario

          es_list$es_sync<-cbind(es_list$es_sync,
                                  escalc(measure=measure,
                                         m1i=data$Avg_value_original[i],
                                         sd1i=data$SD_value[i],
                                         n1i=data$n_value[i],
                                         m2i=data$Avg_value_original[index_sync],
                                         sd2i=data$SD_value[index_sync],
                                         n2i=data$n_value[index_sync]))
        }

        else {es_list$es_sync<-NULL}

      }

      results[[i]]<-es_list

    }

  }

return(results)

}

#Test function
test<-get_effect_sizes(data=data,
                       time_unit="Days",
                       scenario=2,
                       measure="SMD",
                       report=TRUE)

table<-es_table(test)
