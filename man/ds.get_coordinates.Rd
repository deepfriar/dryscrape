% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_get.R
\name{ds.get_coordinates}
\alias{ds.get_coordinates}
\title{Get Coordinates}
\usage{
ds.get_coordinates(season = "20182019", game_id = "20001",
  source = "nhl", date, away_team, try_tolerance = 3,
  agents = ds.user_agents[[1]])
}
\arguments{
\item{season}{character. The season.  Default \code{"20182019"}.}

\item{game_id}{character. The game id. Default \code{"20001"}.}

\item{source}{character. \code{"nhl"} or \code{"espn"}. Default \code{"nhl"}.}

\item{date}{character or date object. Naturally, this must be the date of game \code{game_id}.}

\item{away_team}{character.}

\item{try_tolerance}{numeric. Number of times to bang head against wall. Default \code{3}.}

\item{agents}{character. Agent string. Default should work fine.}
}
\value{
a JSON list object
}
\description{
\code{get_coordinates()} imports the event coordinates corresponding to a given year and game ID
}
