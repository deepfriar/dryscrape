# lower-level functions for obtaining Web data
# these are mostly wrapped by scrape functions and themselves mostly wrap pull functions

#' Get PBP
#'
#' \code{get_pbp()} imports the PBP page corresponding to a given year and game ID
#'
#' @param season        character. The season.  Default \code{"20182019"}.
#' @param game_id       character. The game id. Default \code{"20001"}.
#' @param try_tolerance   numeric. Number of times to bang head against wall. Default \code{3}.
#' @param agents        character. Agent string. Default should work fine.
#' @return a list of two character vectors
#' @export
ds.get_pbp <- function(season        = "20182019",
                       game_id       = "20001",
                       try_tolerance = 3,
                       agents        = ds.user_agents[[1]]) {
  url <- paste("http://www.nhl.com/scores/htmlreports/",
               as.character(season),
               "/PL0",
               as.character(game_id),
               ".HTM",
               sep = ""
  )

  raw_text  <- ds.pull_text(url, agents, try_tolerance)

  html      <- xml2::read_html(raw_text)

  all       <- rvest::html_nodes(html, "td")
  body      <- rvest::html_nodes(html, ".bborder")
  full_text <- rvest::html_text(all)
  body_text <- rvest::html_text(body)

  list(full_text, body_text)
}

#' Get Shifts
#'
#' \code{get_shifts()} imports the shift report page corresponding to a given year, game ID and venue
#' @inheritParams ds.get_pbp
#' @param venue character. \code{"home"} or \code{"away"}. Default \code{"away"}.
#' @param source character. \code{"htm"} or \code{"json"}. Default \code{"htm"}.
#' @return a list of two character vectors
#' @export
ds.get_shifts <- function(season        = "20182019",
                          game_id       = "20001",
                          venue         = "home",
                          source        = "htm",
                          try_tolerance = 3,
                          agents        = ds.user_agents[[1]]) {
  if(tolower(source) == "htm") {
    url <- paste("http://www.nhl.com/scores/htmlreports/",
                 season,
                 if(tolower(venue)=="home") {"/TH0"} else {"/TV0"},
                 game_id,
                 ".HTM",
                 sep = ""
    )

    raw_text  <- ds.pull_text(url, agents, try_tolerance)

    html <- xml2::read_html(raw_text)

    outer_text <- rvest::html_text(rvest::html_nodes(html, ".border"))
    inner_text <- rvest::html_text(rvest::html_nodes(html, ".bborder"))

    list(outer=outer_text, inner=inner_text)
  } else if(tolower(source) == "json") {
    year <- substr(season, 0, 4)

    url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
                 as.character(year),
                 "0",
                 as.character(game_id),
                 sep = ""
    )

    ds.pull_JSON(url, agents, try_tolerance)
  }
}


#' Get Roster
#'
#' \code{get_roster()} imports the Roster page corresponding to a given year and game ID
#' @inheritParams ds.get_pbp
#' @return a character vector
#' @export
ds.get_roster <- function(season        = "20182019",
                          game_id       = "20001",
                          try_tolerance = 3,
                          agents        = ds.user_agents[[1]]) {
  url <- paste("http://www.nhl.com/scores/htmlreports/",
               as.character(season),
               "/RO0",
               as.character(game_id),
               ".HTM",
               sep = ""
  )

  raw_text  <- ds.pull_text(url, agents, try_tolerance)
  html      <- xml2::read_html(raw_text)
  all       <- rvest::html_nodes(html, "tr")

  rvest::html_text(all)
}

#' Get Highlights
#'
#' \code{get_highlights()} imports the highlights page corresponding to a given year and game ID
#' @inheritParams ds.get_pbp
#' @return a JSON list object
#' @export
ds.get_highlights <- function(season        = "20182019",
                              game_id       = "20001",
                              try_tolerance = 3,
                              agents        = ds.user_agents[[1]]) {
  url <- paste("http://live.nhle.com/GameData/",
               as.character(season),
               "/",
               substr(as.character(season), 0, 4),
               "0",
               as.character(game_id),
               "/gc/gcgm.jsonp",
               sep = ""
  )

  ds.pull_JSON(url, agents, try_tolerance, clean=TRUE)
}

#' Get Coordinates
#'
#' \code{get_coordinates()} imports the event coordinates corresponding to a given year and game ID
#' @inheritParams ds.get_pbp
#' @param source character. \code{"nhl"} or \code{"espn"}. Default \code{"nhl"}.
#' @param date character or date object. Naturally, this must be the date of game \code{game_id}.
#' @param away_team character.
#' @return a JSON list object
#' @export
ds.get_coordinates <- function(season        = "20182019",
                               game_id       = "20001",
                               source        = "nhl",
                               date,
                               away_team,
                               try_tolerance = 3,
                               agents        = ds.user_agents[[1]]) {

  if(tolower(source) == "espn") {

    day <- gsub("-", "", as.character(date))

    url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=",
                 day,
                 sep = ""
    )

    raw_text <- ds.pull_text(url, agents, try_tolerance)

    game_ids <- unique(unlist(regmatches(raw_text, gregexpr("gameId=[0-9]+", raw_text))))
    teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(raw_text, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", raw_text))))))

    teams[which(teams == "PHX")]       <- "ARI"
    teams[which(teams == "TB")]        <- "T.B"
    teams[which(teams == "NJ")]        <- "N.J"
    teams[which(teams == "SJ")]        <- "S.J"
    teams[which(teams == "LA")]        <- "L.A"
    teams[which(teams == "COYOTES")]   <- "ARI"
    teams[which(teams == "THRASHERS")] <- "ATL"

    if (as.numeric(season) < 20110000) {teams[which(teams == "WPG")] <- "ATL"}

    team_mat <- matrix(unique(teams), byrow = TRUE, ncol = 2) %>% data.frame()

    url_match <- cbind(game_ids, team_mat) %>%
      data.frame() %>%
      dplyr::rename(awayteam = .data$X1, hometeam = .data$X2)

    game_url <- dplyr::first(as.character(url_match$game_ids[which(as.character(url_match$awayteam) == as.character(away_team) |
                                                                     as.character(url_match$hometeam) == as.character(away_team))]))

    url <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", game_url, sep = "")

    raw_text  <- ds.pull_text(url, agents, try_tolerance)

    events <- unlist(regmatches(raw_text, gregexpr("<Play.*?/Play>", raw_text)))

    if(length(events) > 0) {
      event_mat <- do.call(cbind, strsplit(events, "[\\[~]")) %>%
        t() %>%
        data.frame() %>%
        dplyr::select(5, 3, 4, 6, 7, 11)

      colnames(event_mat)  <- c("event_code", "xcoord", "ycoord", "time", "period", "description")

      event_mat$event_type <- ds.espn_codes$event[match(event_mat$event_code, ds.espn_codes$code)]
      event_mat$seconds    <- 1200 * (corsicaUtils::nabs(event_mat$period) - 1) + ds.seconds_from_ms(event_mat$time)

      event_mat
    } else {NULL}
  } else if(tolower(source) == "nhl") {
    year <- substr(season, 0, 4)

    url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                 as.character(year),
                 "0",
                 as.character(game_id),
                 "/feed/live?site=en_nhl",
                 sep = ""
    )

    raw_json <- ds.pull_JSON(url, agents, try_tolerance)

    if(is.null(raw_json)) {NULL} else {
      event_mat <- Reduce(rbind, lapply(raw_json$liveData$plays$allPlays, ds.parse_event))

      event_mat$game_id <- corsicaUtils::na_if_null(corsicaUtils::nabs(raw_json$gameData$game$pk))

      event_mat
    }
  }
}

#' Get Team Profile
#' \code{get_team_profile()} imports the team profile page corresponding to a given team ID
#' @param team_id numeric. Default \code{1} (NJD).
#' @inheritParams ds.get_pbp
#' @return a JSON list object
#' @export
ds.get_team_profile <- function(team_id       = 1,
                                try_tolerance = 3,
                                agents        = ds.user_agents[[1]]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/teams/", as.character(team_id), sep = "")

  ds.pull_JSON(url, agents, try_tolerance)
}


#' Get player Profile
#' \code{get_team_profile()} imports the team profile page corresponding to a given team ID
#' @param player_id numeric. Default \code{8477474} (Madison Bowey).
#' @inheritParams ds.get_pbp
#' @return a JSON list object
#' @export
ds.get_player_profile <- function(player_id     = 8477474,
                                  try_tolerance = 3,
                                  agents        = ds.user_agents[[1]]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/people/", as.character(player_id), sep = "")

  ds.pull_JSON(url, agents, try_tolerance)
}

#' Get Schedule
#' \code{get_schedule()} imports the schedule page corresponding to a given date rangema
#' @param start character or date object. Default \code{"2018-10-01"}.
#' @param   end character or date object. Default \code{"2019-06-30"}.
#' @inheritParams ds.get_pbp
#' @return a JSON list object
#' @export
ds.get_schedule <- function(start         = "2018-10-01",
                            end           = "2019-06-30",
                            try_tolerance = 3,
                            agents        = ds.user_agents[[1]]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
               as.character(start),
               "&endDate=",
               as.character(end),
               sep = ""
  )

  ds.pull_JSON(url, agents, try_tolerance)
}

#' Get all players
#' @param season character. Default \code{20182019}.
#' @inheritParams ds.get_pbp
#' @return a JSON list object
#' @export
ds.get_all_players <- function(season = "20182019",
                               try_tolerance = 3,
                               agents = ds.user_agents[[1]]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/teams?expand=team.roster&season=", as.character(season))

  ds.pull_JSON(url, agents, try_tolerance)

}
