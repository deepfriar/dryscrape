### DRYSCRAPE ###
#     Last edit: friar (2019-03-03)
# Previous edit: Manny (2017-07-02)


## Description
# Dryscrape contains all functions and tools related to scraping data for Corsica
# Dependencies: Rcurl, rjson, dplyr, lubridate, doMC, user_functions, rvest

## Objects

#' All valid game codes
#' @format character vector
ds.all_games <- as.character(c(20001:21230,
                               30111:30117,
                               30121:30127,
                               30131:30137,
                               30141:30147,
                               30151:30157,
                               30161:30167,
                               30171:30177,
                               30181:30187,
                               30211:30217,
                               30221:30227,
                               30231:30237,
                               30241:30247,
                               30311:30317,
                               30321:30327,
                               30411:30417)) # %>% as.character()

#' A set of sensible HTTP user agent strings
#' @format character vector
ds.user_agents <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
                    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
                    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
                    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36")

#' Play-by-play column names
#' @format character vector
ds.pbp_colnames <- c("season", "game_id", "game_date", "session",
                     "event_index", "game_period", "game_seconds",
                     "event_type", "event_description", "event_detail",
                     "event_team", "event_player_1", "event_player_2", "event_player_3",
                     "event_length", "coords_x", "coords_y", "players_substituted",
                     "home_on_1", "home_on_2", "home_on_3", "home_on_4", "home_on_5", "home_on_6",
                     "away_on_1", "away_on_2", "away_on_3", "away_on_4", "away_on_5", "away_on_6",
                     "home_goalie", "away_goalie", "home_team", "away_team",
                     "home_skaters", "away_skaters", "home_score", "away_score",
                     "game_score_state", "game_strength_state", "highlight_code")

#' Standard team abbrevations
#' @format character vector
ds.team_list <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                  "STL", "T.B", "TOR", "VAN", "WPG", "WSH", "PHX", "ATL", "VGK", "L.V")

#' ESPN event codes
#' @format data.frame
ds.espn_codes <- data.frame(event = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                                      "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                                      "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing"),
                            code  = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                                                   516, 517, 518, 519, 520, 521, 522, 0,
                                                   9999, 1401, 1402, -2147483648, 1, 5)))

## Meta Functions

#' Seconds from MS
#' \code{seconds_from_ms()} returns a numeric vector of representation in seconds of a given vector in M:S format
#' @param ms string. Minutes and seconds.
#' @return a number of seconds.
#' @export
ds.seconds_from_ms <- function(ms) {
  strsplit(as.character(ms), ":") %>%
    unlist() %>%
    corsicaUtils::nabs() %>%
    matrix(ncol = 2,
           byrow = TRUE
           ) ->
    time_mat

  seconds <- 60*time_mat[, 1] + time_mat[, 2]

  return(seconds)

}

#' Clean Nums
#' \code{clean_nums()} returns a list of player number identifiers for a given event description
#' @param x event descriptions
#' @return a list of player number IDs
#' @export
ds.clean.nums <- function(x) {
  t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
  t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
  return(t2)

}

#' Is On
#' \code{is_on()} returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the row index in the given PBP pbject
#' @param player the player.
#' @param pbp data.frame. The play-by-play.
#' @param venue character. \code{"home"} oe \code{"away"}.
#' @return a numeric vector.
#' @export
is_on <- function(player, pbp, venue) {
  regex <- paste(player, ",|", player, "$", sep = "")

  the_team <- pbp[[if(tolower(venue)=="home") {"home_team"} else {"away_team"}]]

  am_on <- data.frame(cumsum(1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON"  & pbp$event_team == the_team) -
                             1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == the_team)))

  colnames(am_on) <- player

  am_on
}

#' Find Goalie
#' \code{find_goalie()} returns a vector containing all goaltenders in a given player vector
#' @param players vector of players.
#' @param roster data.frame. The roster
#' @return a vector of goalies.
#' @export
ds.find_goalie <- function(players, roster) {
  index <- which(players %in% roster$team_num[which(roster$player_position == "G")])

  corsicaUtils::na_if_null(players[index])
}

#' Fix Names
#' \code{fix_names()} returns a vector of player names corrected for multiple spelling variants
#' Some more of this work is handled in the \code{salo} package. Eventual unification is a desideratum.
#' @param name_vect character vector. Player names.
#' @return a vector of player names, standardized.
#' @export
ds.fix_names <- function(name_vect) {
  name_vect[which(name_vect == "PK.SUBBAN" | name_vect == "P.K.SUBBAN")] <- "P.K..SUBBAN"
  name_vect[which(name_vect == "TJ.OSHIE" | name_vect == "T.J.OSHIE")] <- "T.J..OSHIE"
  name_vect[which(name_vect == "BJ.CROMBEEN" | name_vect == "B.J.CROMBEEN" | name_vect == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
  name_vect[which(name_vect == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
  name_vect[which(name_vect == "CAMERON.BARKER")] <- "CAM.BARKER"
  name_vect[which(name_vect == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
  name_vect[which(name_vect == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
  name_vect[which(name_vect == "DANIEL.CLEARY")] <- "DAN.CLEARY"
  name_vect[which(name_vect == "DANIEL.GIRARDI")] <- "DAN.GIRARDI"
  name_vect[which(name_vect == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
  name_vect[which(name_vect == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
  name_vect[which(name_vect == "DWAYNE.KING")] <- "DJ.KING"
  name_vect[which(name_vect == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
  name_vect[which(name_vect == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
  name_vect[which(name_vect == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
  name_vect[which(name_vect == "J P.DUMONT" | name_vect == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
  name_vect[which(name_vect == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
  name_vect[which(name_vect == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
  name_vect[which(name_vect == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
  name_vect[which(name_vect == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
  name_vect[which(name_vect == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
  name_vect[which(name_vect == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
  name_vect[which(name_vect == "MATTHEW.CARLE")] <- "MATT.CARLE"
  name_vect[which(name_vect == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
  name_vect[which(name_vect == "JOSEPH.CORVO")] <- "JOE.CORVO"
  name_vect[which(name_vect == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
  name_vect[which(name_vect == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
  name_vect[which(name_vect == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
  name_vect[which(name_vect == "MICHAEL.FERLAND")] <- "MICHEAL.FERLAND"
  name_vect[which(name_vect == "PIERRE.PARENTEAU" | name_vect == "PIERRE-ALEX.PARENTEAU" | name_vect == "PA.PARENTEAU" | name_vect == "P.A.PARENTEAU" | name_vect == "P-A.PARENTEAU")] <- "P.A..PARENTEAU"
  name_vect <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", name_vect) # but don't confuse Alexandre and Alexandre R. Picard
  name_vect <- gsub("CHRISTOPHER.", "CHRIS.", name_vect)
  name_vect[which(name_vect == "NICOLAS.PETAN")] <- "NIC.PETAN"
  name_vect[which(name_vect == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
  name_vect[which(name_vect == "MATTHEW.BENNING")] <- "MATT.BENNING"
  name_vect[which(name_vect == "JAMES.HOWARD")] <- "JIMMY.HOWARD"
  name_vect[which(name_vect == "EMMANUEL.FERNANDEZ")] <- "MANNY.FERNANDEZ"
  name_vect[which(name_vect == "EMMANUEL.LEGACE")] <- "MANNY.LEGACE"
  name_vect[which(name_vect == "SIMEON.VARLAMOV")] <- "SEMYON.VARLAMOV"
  name_vect[which(name_vect == "MAXIME.TALBOT")] <- "MAX.TALBOT"
  name_vect[which(name_vect == "MITCHELL.MARNER")] <- "MITCH.MARNER"
  name_vect[which(name_vect == "ANDREW.MILLER")] <- "DREW.MILLER" # sometimes the wrong thing; corrected for elsewhere
  name_vect[which(name_vect == "EDWARD.PURCELL")] <- "TEDDY.PURCELL"
  name_vect[which(name_vect == "NICKLAS.GROSSMAN")] <- "NICKLAS.GROSSMANN"

  name_vect
}

## General Functions
#' Who
#' \code{who()} searches a given player ID and returns the player's full name
#' @param player_id numeric. Player ID.
#' @return full name.
#' @export
ds.who <- function(player_id) {
  player <- ds.get_player_profile(player_id)

  player$people[[1]]$fullName
}

