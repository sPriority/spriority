find_reverse_scenario<-function(data,
                                sp_list,
                                pos_target,
                                pos_rev,
                                paper,
                                envir){

  #data is the sPriority database
  #sp_list is a dataframe with three columns (Position_in_sequence, Species_name, Time_since_first_intro). This is the target arrival sequence.
  #pos_target is the position of the target species in the target arrival sequence
  #pos_rev is the position of the target species in the reverse arrival sequence
  #paper is the paper ID
  #envir is the environment ID

  #Find the sequence ID of the right reverse scenario
  #sp_list_rev is the reverse arrival scenario to look for

  sp_list_rev<-sp_list[,1:2]
  index_pos<-which(sp_list$Position_in_sequence==pos_target)
  index_k<-which(sp_list$Position_in_sequence==pos_rev)
  sp_list_rev$Position_in_sequence[index_pos]<-pos_rev
  sp_list_rev$Position_in_sequence[index_k]<-pos_target
  sp_list_rev <- sp_list_rev %>%
    arrange(Position_in_sequence, Species_name)

  #Get Sequence_ID values in environment

  seq_id<-unique(data$Sequence_ID[data$Paper_ID==paper &
                                    data$Environment_ID==envir])

  for (id in seq_id){ #Loop over all sequence IDs to find the one identical to the target reference scenario

    sp_list_id<-data %>%
      filter(Paper_ID==paper & Sequence_ID==id & Environment_ID==envir & Number_of_introduction_events>1) %>%
      select(Position_in_sequence, Species_name, Time_since_first_intro) %>%
      arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
      distinct(Position_in_sequence, Species_name, Time_since_first_intro)

    if (identical(as.numeric(sp_list_id[,1]), as.numeric(sp_list_rev[,1]))==TRUE &
        identical(sp_list_id[,2], sp_list_rev[,2])==TRUE) {

      seq_rev<-id
      break}

    else {

      sp_list_id<-NULL
      seq_rev<-NULL}}

  #Now check that the target and reverse sequences use the same time intervals

  if (is.null(sp_list_id)==FALSE){

      if (nrow(sp_list_id)>1){

          time_intervals_target <- (sp_list %>%
                                    arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
                                    distinct(Position_in_sequence, Time_since_first_intro))$Time_since_first_intro

          time_intervals_reverse <- (sp_list_id %>%
                                      arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
                                      distinct(Position_in_sequence, Time_since_first_intro))$Time_since_first_intro

          if (identical(time_intervals_target, time_intervals_reverse)==FALSE){

            sp_list_id<-NULL
            seq_rev<-NULL}}}

  return(list(seq_rev=seq_rev, sp_list_rev=sp_list_id))

}
