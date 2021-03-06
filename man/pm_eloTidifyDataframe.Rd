% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eloFunctions.R
\name{pm_eloTidifyDataframe}
\alias{pm_eloTidifyDataframe}
\title{pm_eloTidifyDataframe}
\usage{
pm_eloTidifyDataframe(eloDataframe)
}
\arguments{
\item{eloDataframe}{A dataframe resulting from a call to pm_eloRunTimeSlice or pm_eloRunMultipleTimeSclies}
}
\value{
A dataframe in 'tidy' format (player_name, elo_ variables, match_date, actualResult)
}
\description{
Convert an ELO dataframe to 'tidy' format (i.e. flatten player and opponent)
}
\examples{

}
