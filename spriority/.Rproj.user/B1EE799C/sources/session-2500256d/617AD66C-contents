\name{get_effect_sizes}
\alias{get_effect_sizes}
\title{Calculate effect sizes using sPriority data}
\description{
This function calculates effect sizes using the sPriority database of experiments that manipulated the order of arrival of species or groups of species.
}
\usage{
get_effect_sizes(data,
                 time_unit=NULL,
                 scenario=2,
                 measure="SMD",
                 report=TRUE)
}
\arguments{
\item{data}{The sPriority database as a data table.}

\item{time_unit}{A character string. Choose time units to standardise studies in the sPriority database. Can be either "Hours", "Days", "Weeks", "Months" or "Years". If NULL (default), time units will not be standardised.}

\item{scenario}{A numeric value to choose the scenario that should be used to calculate effect sizes. Can be either 1 or 2. Default to 2.

\bold{Scenario 1}: the time lag between the arrival of the target species or species group and the measurement will be kept constant.

\bold{Scenario 2}: the time lag between the start of the experiment and the measurement (time_after_start) will be kept constant.}

\item{measure}{A character string to specify which effect size should be calculated (see metafor::escalc). Default to "SMD". The following options are available:

"MD": raw mean difference

"SMD"" (default): standardised mean difference (Hedges' g)

"SMDH": standardised mean difference with heteroscedastic population variances in the two groups

"SMD1": standardised mean difference where the mean difference is divided by the standard deviation of the second group

"SMD1H": same as SMD1, but with heteroscedastic population variances

"ROM": log transformed ratio of means}

\item{report}{Boolean. If TRUE (default), a detailed report will be exported as a text file.}
}
