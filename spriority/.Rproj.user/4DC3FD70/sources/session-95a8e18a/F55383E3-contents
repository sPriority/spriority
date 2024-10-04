reconstruct_sequence <- function(i, data){

  #Get infos about observation

  paper<-data$Paper_ID[i] #Get paper ID
  envir<-data$Environment_ID[i] #Get environment ID
  seq<-data$Sequence_ID[i] #Get sequence ID
  time<-data$Time_after_start[i] #Get time point
  sp<-data$Species_name[i] #Get species name
  pos<-data$Position_in_sequence[i] #Get position in sequence
  duration<-data$Experiment_duration[i] #Time interval between first intro event and end of the experiment
  response <- data$Variable_name[i] #Variable name

  #Get species list for inoculation sequence in environment

  sp_list<-data %>%
    filter(Paper_ID==paper & Sequence_ID==seq & Environment_ID==envir) %>%
    select(Position_in_sequence, Species_name, Time_since_first_intro) %>%
    arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
    distinct(Position_in_sequence, Species_name, Time_since_first_intro)

  return(sp_list)

}
