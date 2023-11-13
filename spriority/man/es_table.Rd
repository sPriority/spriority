\name{es_table}
\alias{es_table}
\title{Create an effect size table}
\description{
This function creates an effect size table more suitable for data analysis.
}
\usage{
es_table(x)
}
\arguments{
\item{x}{A list created by \code{\link{get_effect_sizes}}}
}

\value{

A data table with the following extra columns:

\bold{yi}: the calculated effect size

\bold{vi}: the sampling variance (only calculated with \code{metafor:escalc})

\bold{Sp_prior}: the name(s) of the species or species groups introduced before the target species

\bold{Sp_target}: the name of the target species or species group

\bold{row_ref}: the row number of the reference scenario in the sPriority database

\bold{row_target}: the row number of the target species under consideration in the sPriority database

\bold{reference_scenario}: Either "Synchronous" or "Reverse"
}
