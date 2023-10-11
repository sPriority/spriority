find_sync_scenario<-function(data,
                              sp_list,
                              paper,
                              envir){

  #data is the sPriority database
  #sp_list is a dataframe with three columns (Position_in_sequence, Species_name, Time_since_first_intro). This is the target arrival sequence.
  #paper is the paper ID
  #envir is the environment ID

  #Find the sequence ID of the right synchronous scenario
  #sp_list_sync is the synchronous arrival scenario to look for

  sp_list_sync<-sp_list[,1:2]
  sp_list_sync$Position_in_sequence<-1 #All species must arrive first, at the same time
  sp_list_sync <- sp_list_sync %>%
    arrange(Position_in_sequence, Species_name)

  #Get Sequence_ID values in environment

  seq_id<-unique(data$Sequence_ID[data$Paper_ID==paper &
                                    data$Environment_ID==envir])

  for (id in seq_id){ #Loop over all sequence IDs to find the one identical to the target reference scenario

    sp_list_id<-data %>%
      filter(Paper_ID==paper & Sequence_ID==id & Environment_ID==envir) %>%
      select(Position_in_sequence, Species_name, Time_since_first_intro) %>%
      arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
      distinct(Position_in_sequence, Species_name, Time_since_first_intro)

    if (identical(as.numeric(sp_list_id[,1]), as.numeric(sp_list_sync[,1]))==TRUE &
        identical(sp_list_id[,2], sp_list_sync[,2])==TRUE) {

      seq_sync<-id
      break}

    else {

      sp_list_id<-NULL
      seq_sync<-NULL}}

  #Now check that all species have time_since_first_intro equals to zero

  time_intervals_sync <- (sp_list_id %>%
                              arrange(Position_in_sequence, Species_name, Time_since_first_intro) %>%
                              distinct(Position_in_sequence, Time_since_first_intro))$Time_since_first_intro

  if (length(time_intervals_sync)>1 | time_intervals_sync != 0){

    sp_list_id<-NULL
    seq_rev<-NULL}

  return(list(seq_sync=seq_rev, sp_list_sync=sp_list_id))

}
