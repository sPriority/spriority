es_table<-function(x){

  #x is a list created by get_effect_sizes

  k_reverse<-0
  k_sync<-0

  for (i in 1:length(x)){

    if (is.null(x[[i]]$row_control_reverse)==FALSE){

      k_reverse<-k_reverse+1

      if (k_reverse==1) {data_reverse<-x[[i]]$es_reverse}
      else {data_reverse<-rbind(data_reverse, x[[i]]$es_reverse)}}

    if (is.null(x[[i]]$row_control_sync)==FALSE){

      k_sync<-k_sync+1

      if (k_sync==1) {data_sync<-x[[i]]$es_sync}
      else {data_sync<-rbind(data_sync, x[[i]]$es_sync)}}

  }

  data_reverse$reference_scenario<-"Reverse"
  data_sync$reference_scenario<-"Synchronous"

  data<-rbind(data_reverse, data_sync)

  data<-data[order(data$Paper_ID, data$Observation_ID),]

  return(data)
}
