es_table<-function(x){

  #x is a list created by get_effect_sizes

  k_reverse<-0
  k_sync<-0

  for (i in 1:length(x)){
    
    if (is.null(x[[i]]$Reverse$row_control_reverse)==FALSE){

      k_reverse<-k_reverse+1

      n<-nrow(x[[i]]$Reverse$es_reverse)

      if (length(x[[i]]$Reverse$sp_prior)==0){x[[i]]$Reverse$sp_prior <- NA}

      if (k_reverse==1) {data_reverse<-data.frame(x[[i]]$Reverse$es_reverse,
                                                  Sp_prior=x[[i]]$Reverse$sp_prior,
                                                  Sp_target=x[[i]]$Reverse$sp_target,
                                                  row_ref=x[[i]]$Reverse$row_control_reverse,
                                                  row_target=x[[i]]$Reverse$row_target)}

      else {data_reverse<-rbind(data_reverse, data.frame(x[[i]]$Reverse$es_reverse,
                                                         Sp_prior=x[[i]]$Reverse$sp_prior,
                                                         Sp_target=x[[i]]$Reverse$sp_target,
                                                         row_ref=x[[i]]$Reverse$row_control_reverse,
                                                         row_target=x[[i]]$Reverse$row_target))}}

    if (is.null(x[[i]]$Sync$row_control_sync)==FALSE){

      k_sync<-k_sync+1

      if (length(x[[i]]$Sync$sp_prior)==0){x[[i]]$Sync$sp_prior <- NA}

      if (k_sync==1) {data_sync<-data.frame(x[[i]]$Sync$es_sync,
                                            Sp_prior=paste(x[[i]]$Sync$sp_prior, collapse="_"),
                                            Sp_target=paste(x[[i]]$Sync$sp_target, collapse="_"),
                                            row_ref=x[[i]]$Sync$row_control_sync,
                                            row_target=x[[i]]$Sync$row_target)}

      else {data_sync<-rbind(data_sync, data.frame(x[[i]]$Sync$es_sync,
                                                   Sp_prior=paste(x[[i]]$Sync$sp_prior, collapse="_"),
                                                   Sp_target=paste(x[[i]]$Sync$sp_target, collapse="_"),
                                                   row_ref=x[[i]]$Sync$row_control_sync,
                                                   row_target=x[[i]]$Sync$row_target))}}

  }

  if (k_reverse > 0) {data_reverse$reference_scenario<-"Reverse"}
  if (k_sync > 0) {data_sync$reference_scenario<-"Synchronous"}

  if (k_reverse > 0 & k_sync > 0) {data<-rbind(data_reverse, data_sync)}
  if (k_reverse > 0 & k_sync == 0) {data<-data_reverse}
  if (k_reverse == 0 & k_sync > 0) {data<-data_sync}

  data<-data[order(data$Paper_ID, data$Observation_ID),]

  return(data)
}
