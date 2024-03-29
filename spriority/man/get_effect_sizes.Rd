\name{get_effect_sizes}
\alias{get_effect_sizes}
\title{Calculate effect sizes using sPriority data}
\description{
This function calculates effect sizes using the sPriority database of experiments that manipulated the order of arrival of species or groups of species. A progress bar is shown by default.
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

\item{measure}{A character string to specify which effect size should be calculated (see \code{metafor::escalc}). Default to "SMD". The following options are available:

"MD": raw mean difference

"SMD"" (default): standardised mean difference (Hedges' g)

"SMDH": standardised mean difference with heteroscedastic population variances in the two groups

"SMD1": standardised mean difference where the mean difference is divided by the standard deviation of the second group

"SMD1H": same as SMD1, but with heteroscedastic population variances

"ROM": log transformed ratio of means

"RII1": priority effect index used in Delory et al 2019. When history matters: The overlooked role of priority effects in grassland overyielding. Functional ecology 33: 2369–2380.

"RII2": priority effect index used in Delory et al 2019. The exotic species Senecio inaequidens pays the price for arriving late in temperate European grassland communities. Oecologia 191: 657–671.}

\item{report}{Boolean. If TRUE (default), a detailed report will be exported as a text file.}
}

\value{

Returns a list with as many elements as rows in the sPriority data table. Each element of the main list consists of a list of two elements named "Reverse" and "Sync".


The Reverse list contains the following elements:

\bold{row_target}: the row number of the target species or species group under consideration. The target species is the species for which we want to calculate an effect size.

\bold{row_control_reverse}: the row number of the corresponding reverse scenario

\bold{sp_target}: the name of the target species or species group

\bold{sp_prior}: the name(s) of the species or species groups introduced before the target species

\bold{es_reverse}: the row of the target species with one (\bold{yi}) or two (\bold{yi} and vi) extra columns. The column "yi" contains the calculated effect size (reverse reference scenario). The column "vi" contains the sampling variance (only calculated with \code{metafor::escalc}).


The Sync list contains the following elements:

\bold{row_target}: the row number of the target species or species group under consideration. The target species is the species for which we want to calculate an effect size.

\bold{row_control_sync}: the row number of the corresponding synchronous scenario

\bold{sp_target}: the name of the target species or species group

\bold{sp_prior}: the name(s) of the species or species groups introduced before the target species

\bold{es_reverse}: the row of the target species with one (\bold{yi}) or two (\bold{yi} and vi) extra columns. The column "yi" contains the calculated effect size (synchronous reference scenario). The column "vi" contains the sampling variance (only calculated with \code{metafor::escalc}).

NULL means that the function did not find the required information to calculate an effect size.
}
