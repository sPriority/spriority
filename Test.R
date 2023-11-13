#Load R package
library(spriority)

#Get spriority data
data <- read.csv("PATH_TO_SPRORITY_CSV")

#Get effect sizes
es<-get_effect_sizes(data=data,
                     time_unit="Days", #Standardize time units across dataset
                     scenario=2, #Scenario to calculate effect sizes (see help page for more infos)
                     measure="SMD", #standardized mean difference (see help page for more metrics)
                     report=TRUE) #Export a detailed report

#Create effect size table
table<-es_table(es)