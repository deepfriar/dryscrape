% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_get.R
\name{ds.get_schedule}
\alias{ds.get_schedule}
\title{Get Schedule
\code{get_schedule()} imports the schedule page corresponding to a given date rangema}
\usage{
ds.get_schedule(start = "2018-10-01", end = "2019-06-30",
  try_tolerance = 3, agents = ds.user_agents[[1]])
}
\arguments{
\item{start}{character or date object. Default \code{"2018-10-01"}.}

\item{end}{character or date object. Default \code{"2019-06-30"}.}

\item{try_tolerance}{numeric. Number of times to bang head against wall. Default \code{3}.}

\item{agents}{character. Agent string. Default should work fine.}
}
\value{
a JSON list object
}
\description{
Get Schedule
\code{get_schedule()} imports the schedule page corresponding to a given date rangema
}
