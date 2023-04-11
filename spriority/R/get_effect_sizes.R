library(tidyverse)

options(dplyr.summarise.inform = FALSE)

data <- read.csv("~/Postdoc/sPRIORITY/Meta-analysis/sPriority_database_230411_BD.csv")[1:855,]

#Get number of species present for each effect size
#Calculate effect size using both control scenarios if available
#CONSIDER TIME LAG

#Paper --> Environment --> Sequence --> Time point --> Species

#########################################
#Create function for back-transformation
#########################################

back_transform<-function(x){}

#####################################
#Create function to get effect sizes
#####################################

#Start simple by only considering scenarios with only two species interacting (A then B, B then A, simultaneous AB)

data<-data%>%
  filter(Number_of_species_in_sequence==2)

#Standardise time units

data1<-standardise_time_units(data, time="Days")

#Code function to get effect sizes

get_effect_sizes<-function(data, time_unit=NULL){

  #data is the data sheet with raw data from sPriority database
  #If not NULL, time_unit can be either "Hours", "Days", "Weeks", "Months" or "Years"

  #Standardise time units

  if (is.null(time_unit)==FALSE) {data<-standardise_time_units(data, time=time_unit)}

  #Create list to store effect sizes

  results<-list()

  data$Hedge_d_reverse<-NA
  data$Hedge_d_sync<-NA

  for (i in 1:nrow(data)){ #For each observation in data, calculate an effect size

    paper<-data$Paper_ID[i] #Get paper ID
    envir<-data$Environment_ID[i] #Get environment ID
    seq<-data$Sequence_ID[i] #Get sequence ID
    time<-data$Time_after_start[i] #Get time point
    sp<-data$Species_name[i] #Get species name

    #Reconstruct all arrival sequences for the paper

    arrival_seq<-data %>%
      filter(Paper_ID==paper & Environment_ID==envir & Time_after_start==time)%>%
      group_by(Sequence_ID, Position_in_sequence, Time_since_first_intro)%>%
      summarise(Species_name=Species_name)

    #Is there any synchronous reference scenario in this study?

    sequence_ID_ref<-unique(data$Sequence_ID[which(data$Paper_ID==paper &
                                              data$Environment_ID==envir &
                                              data$Number_of_introduction_events==1)])

    if (seq %in% sequence_ID_ref){
      #If response variable measured in a synchronous scenario, keep NA value for effect size
    }

    else {

      if (length(sequence_ID_ref)==0){
      #If there is no synchronous scenario
      #Calculate effect size only based on reverse scenario
        }

      if (length(sequence_ID_ref)==1){ #There is one synchronous sequence of arrival

        #Calculate effect size based on synchronous sequence of arrival (only one synchronous sequence available)

        avgT<-data$Avg_value_original[i] #Average value of treatment group
        sdT<-data$SD_value[i] #Standard deviation of treatment group
        nT<-data$n_value[i] #Sample size of treatment group

        index<-which(data$Paper_ID==paper &
                       data$Environment_ID==envir &
                       data$Sequence_ID==sequence_ID_ref &
                       data$Time_after_start==time &
                       data$Species_name==sp) #Find line number for reference scenario

        avgC<-data$Avg_value_original[index] #Average value of control group
        sdC<-data$SD_value[index] #Standard deviation of control group
        nC<-data$n_value[index] #Sample size of control group

        s<-sqrt(((nT-1)*sdT^2+(nC-1)*sdC^2)/(nT+nC-2))

        J<-1-(3/(4*(nT+nC)-9))

        data$Hedge_d_sync[i]<-J*(avgT-avgC)/s

      }

      if (length(sequence_ID_ref)>1){ #If there is more than one synchronous scenario, choose right one for comparison

        #Number of arrival events before the arrival of the target species

        n_prec<-data$Position_in_sequence[i]-1

        #Find Sequence_ID for the right synchronous scenario

      }

    }






  }

return(data)

}

data1<-get_effect_sizes(data=data)
