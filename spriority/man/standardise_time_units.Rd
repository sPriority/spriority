\name{standardise_time_units}
\alias{standardise_time_units}
\title{Standardise time units in sPriority data}
\description{
This function homogenises time units used in sPriority data.
}
\usage{
standardise_time_units(data,
                       time_unit="Days")
}
\arguments{
\item{data}{The sPriority database as a data table.}

\item{time_unit}{A character string. Choose time units to standardise studies in the sPriority database. Can be either "Hours", "Days", "Weeks", "Months" or "Years".}
}

\value{

A new data table with standardised time units across experiments.
}
