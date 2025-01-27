standardise_time_units<-function(data, time="Days"){

  #data is the data sheet with raw data from sPriority database
  #time can be either "Hours", "Days", "Weeks", "Months" or "Years"

  for (i in 1:nrow(data)){

    if (time=="Hours"){

      #Time_intro

      if (data$Time_intro_units[i]=="Hour(s)"){}
      if (data$Time_intro_units[i]=="Day(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*24
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*24
        data$Time_intro_units[i]<-"Hour(s)"
      }

      if (data$Time_intro_units[i]=="Week(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*7*24
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*7*24
        data$Time_intro_units[i]<-"Hour(s)"
      }

      if (data$Time_intro_units[i]=="Month(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*30.5*24
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*30.5*24
        data$Time_intro_units[i]<-"Hour(s)"
      }

      if (data$Time_intro_units[i]=="Year(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*365*24
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*365*24
        data$Time_intro_units[i]<-"Hour(s)"
      }

      #Time_after_start

      if (data$Time_after_start_units[i]=="Hour(s)"){}
      if (data$Time_after_start_units[i]=="Day(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*24
        data$Time_after_start_units[i]<-"Hour(s)"
      }

      if (data$Time_after_start_units[i]=="Week(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*7*24
        data$Time_after_start_units[i]<-"Hour(s)"
      }

      if (data$Time_after_start_units[i]=="Month(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*30.5*24
        data$Time_after_start_units[i]<-"Hour(s)"
      }

      if (data$Time_after_start_units[i]=="Year(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*365*24
        data$Time_after_start_units[i]<-"Hour(s)"
      }

      #Experiment_duration

      if (data$Experiment_duration_units[i]=="Hour(s)"){}
      if (data$Experiment_duration_units[i]=="Day(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*24
        data$Experiment_duration_units[i]<-"Hour(s)"
      }

      if (data$Experiment_duration_units[i]=="Week(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*7*24
        data$Experiment_duration_units[i]<-"Hour(s)"
      }

      if (data$Experiment_duration_units[i]=="Month(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*30.5*24
        data$Experiment_duration_units[i]<-"Hour(s)"
      }

      if (data$Experiment_duration_units[i]=="Year(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*365*24
        data$Experiment_duration_units[i]<-"Hour(s)"
      }

    }

    if (time=="Days"){

      #Time_intro

      if (data$Time_intro_units[i]=="Hour(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/24
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/24
        data$Time_intro_units[i]<-"Day(s)"
      }

      if (data$Time_intro_units[i]=="Day(s)"){}
      if (data$Time_intro_units[i]=="Week(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*7
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*7
        data$Time_intro_units[i]<-"Day(s)"

      }

      if (data$Time_intro_units[i]=="Month(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*30.5
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*30.5
        data$Time_intro_units[i]<-"Day(s)"
      }

      if (data$Time_intro_units[i]=="Year(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*365
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*365
        data$Time_intro_units[i]<-"Day(s)"
      }

      #Time_after_start

      if (data$Time_after_start_units[i]=="Hour(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/24
        data$Time_after_start_units[i]<-"Day(s)"
      }

      if (data$Time_after_start_units[i]=="Day(s)"){}
      if (data$Time_after_start_units[i]=="Week(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*7
        data$Time_after_start_units[i]<-"Day(s)"

      }

      if (data$Time_after_start_units[i]=="Month(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*30.5
        data$Time_after_start_units[i]<-"Day(s)"
      }

      if (data$Time_after_start_units[i]=="Year(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*365
        data$Time_after_start_units[i]<-"Day(s)"
      }

      #Experiment_duration

      if (data$Experiment_duration_units[i]=="Hour(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/24
        data$Experiment_duration_units[i]<-"Day(s)"
      }

      if (data$Experiment_duration_units[i]=="Day(s)"){}
      if (data$Experiment_duration_units[i]=="Week(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*7
        data$Experiment_duration_units[i]<-"Day(s)"

      }

      if (data$Experiment_duration_units[i]=="Month(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*30.5
        data$Experiment_duration_units[i]<-"Day(s)"
      }

      if (data$Experiment_duration_units[i]=="Year(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*365
        data$Experiment_duration_units[i]<-"Day(s)"
      }

    }

    if (time=="Weeks"){

      #Time_intro

      if (data$Time_intro_units[i]=="Hour(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/24/7
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/24/7
        data$Time_intro_units[i]<-"Week(s)"
      }

      if (data$Time_intro_units[i]=="Day(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/7
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/7
        data$Time_intro_units[i]<-"Week(s)"
      }

      if (data$Time_intro_units[i]=="Week(s)"){}
      if (data$Time_intro_units[i]=="Month(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*30.5/7
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*30.5/7
        data$Time_intro_units[i]<-"Week(s)"
      }

      if (data$Time_intro_units[i]=="Year(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*365/7
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*365/7
        data$Time_intro_units[i]<-"Week(s)"
      }

      #Time_after_start

      if (data$Time_after_start_units[i]=="Hour(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/24/7
        data$Time_after_start_units[i]<-"Week(s)"
      }

      if (data$Time_after_start_units[i]=="Day(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/7
        data$Time_after_start_units[i]<-"Week(s)"
      }

      if (data$Time_after_start_units[i]=="Week(s)"){}
      if (data$Time_after_start_units[i]=="Month(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*30.5/7
        data$Time_after_start_units[i]<-"Week(s)"
      }

      if (data$Time_after_start_units[i]=="Year(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*365/7
        data$Time_after_start_units[i]<-"Week(s)"
      }

      #Experiment_duration

      if (data$Experiment_duration_units[i]=="Hour(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/24/7
        data$Experiment_duration_units[i]<-"Week(s)"
      }

      if (data$Experiment_duration_units[i]=="Day(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/7
        data$Experiment_duration_units[i]<-"Week(s)"
      }

      if (data$Experiment_duration_units[i]=="Week(s)"){}
      if (data$Experiment_duration_units[i]=="Month(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*30.5/7
        data$Experiment_duration_units[i]<-"Week(s)"
      }

      if (data$Experiment_duration_units[i]=="Year(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*365/7
        data$Experiment_duration_units[i]<-"Week(s)"
      }

    }

    if (time=="Months"){

      #Time_intro

      if (data$Time_intro_units[i]=="Hour(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/24/30.5
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/24/30.5
        data$Time_intro_units[i]<-"Month(s)"
      }

      if (data$Time_intro_units[i]=="Day(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/30.5
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/30.5
        data$Time_intro_units[i]<-"Month(s)"
      }

      if (data$Time_intro_units[i]=="Week(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*7/30.5
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*7/30.5
        data$Time_intro_units[i]<-"Month(s)"
      }

      if (data$Time_intro_units[i]=="Month(s)"){}

      if (data$Time_intro_units[i]=="Year(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*12
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*12
        data$Time_intro_units[i]<-"Month(s)"
      }

      #Time_after_start

      if (data$Time_after_start_units[i]=="Hour(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/24/30.5
        data$Time_after_start_units[i]<-"Month(s)"
      }

      if (data$Time_after_start_units[i]=="Day(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/30.5
        data$Time_after_start_units[i]<-"Month(s)"
      }

      if (data$Time_after_start_units[i]=="Week(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*7/30.5
        data$Time_after_start_units[i]<-"Month(s)"
      }

      if (data$Time_after_start_units[i]=="Month(s)"){}

      if (data$Time_after_start_units[i]=="Year(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*12
        data$Time_after_start_units[i]<-"Month(s)"
      }

      #Experiment_duration

      if (data$Experiment_duration_units[i]=="Hour(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/24/30.5
        data$Experiment_duration_units[i]<-"Month(s)"
      }

      if (data$Experiment_duration_units[i]=="Day(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/30.5
        data$Experiment_duration_units[i]<-"Month(s)"
      }

      if (data$Experiment_duration_units[i]=="Week(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*7/30.5
        data$Experiment_duration_units[i]<-"Month(s)"
      }

      if (data$Experiment_duration_units[i]=="Month(s)"){}

      if (data$Experiment_duration_units[i]=="Year(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*12
        data$Experiment_duration_units[i]<-"Month(s)"
      }

    }

    if (time=="Years"){

      #Time_intro

      if (data$Time_intro_units[i]=="Hour(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/24/365
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/24/365
        data$Time_intro_units[i]<-"Year(s)"
      }

      if (data$Time_intro_units[i]=="Day(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/365
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/365
        data$Time_intro_units[i]<-"Year(s)"
      }

      if (data$Time_intro_units[i]=="Week(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]*7/365
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]*7/365
        data$Time_intro_units[i]<-"Year(s)"
      }

      if (data$Time_intro_units[i]=="Month(s)"){

        data$Time_since_first_intro[i]<-data$Time_since_first_intro[i]/12
        data$Time_since_last_intro[i]<-data$Time_since_last_intro[i]/12
        data$Time_intro_units[i]<-"Year(s)"
      }

      if (data$Time_intro_units[i]=="Year(s)"){}

      #Time_after_start

      if (data$Time_after_start_units[i]=="Hour(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/24/365
        data$Time_after_start_units[i]<-"Year(s)"
      }

      if (data$Time_after_start_units[i]=="Day(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/365
        data$Time_after_start_units[i]<-"Year(s)"
      }

      if (data$Time_after_start_units[i]=="Week(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]*7/365
        data$Time_after_start_units[i]<-"Year(s)"
      }

      if (data$Time_after_start_units[i]=="Month(s)"){

        data$Time_after_start[i]<-data$Time_after_start[i]/12
        data$Time_after_start_units[i]<-"Year(s)"
      }

      if (data$Time_after_start_units[i]=="Year(s)"){}

      #Experiment_duration

      if (data$Experiment_duration_units[i]=="Hour(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/24/365
        data$Experiment_duration_units[i]<-"Year(s)"
      }

      if (data$Experiment_duration_units[i]=="Day(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/365
        data$Experiment_duration_units[i]<-"Year(s)"
      }

      if (data$Experiment_duration_units[i]=="Week(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]*7/365
        data$Experiment_duration_units[i]<-"Year(s)"
      }

      if (data$Experiment_duration_units[i]=="Month(s)"){

        data$Experiment_duration[i]<-data$Experiment_duration[i]/12
        data$Experiment_duration_units[i]<-"Year(s)"
      }

      if (data$Experiment_duration_units[i]=="Year(s)"){}

    }

  }

return(data)}
