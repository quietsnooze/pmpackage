% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eloFunctions.R
\name{pm_eloExtractDatabaseFromDataframe}
\alias{pm_eloExtractDatabaseFromDataframe}
\title{pm_eloExtractDatabaseFromDataframe}
\usage{
pm_eloExtractDatabaseFromDataframe(eloDataframe)
}
\arguments{
\item{eloDataframe}{A dataframe resulting from a call to pm_eloRunTimeSlice or pm_eloRunMultipleTimeSclies}
}
\value{
a list with two items: a dictionary for ELO scores, a dictionary for number of matches played
}
\description{
Prepare dictionarys for playerScore and playerMatches for seen data (i.e. from a dataframe that has already had pm_eloRunTimeSlice on it).
}
\examples{


}
