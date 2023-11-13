library(tidyverse)
library(metafor)

options(dplyr.summarise.inform = FALSE)

data <- read.csv("~/Postdoc/sPRIORITY/Meta-analysis/sPriority_database_230411_BD.csv")[1:855,]

test<-get_effect_sizes(data=data,
                       time_unit="Days",
                       scenario=1,
                       measure="RII1",
                       report=TRUE)

###############################

test2.smd<-get_effect_sizes(data=data,
                        time_unit="Days",
                        scenario=2,
                        measure="SMD",
                        report=TRUE)

test2.rii1<-get_effect_sizes(data=data,
                            time_unit="Days",
                            scenario=2,
                            measure="RII1",
                            report=TRUE)

table2.smd<-es_table(test2.smd)
table2.rii1<-es_table(test2.rii1)
