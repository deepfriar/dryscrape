# functions that parse data into usable formats once it's been scraped

#' Parse PBP Event
#'
#' \code{parse_event()} parses a single event from the PBP JSON object
#'
#' @param x a single entry.
#' @return  a data.frame
#' @export
ds.parse_event <- function(x) {
  player_ids <- x$players %>%
    sapply(function(p) as.character(p$player$id)) %>%
    unlist() %>%
    c(rep(NA,
          times = (4 - length(x$players))
    ))

  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
             event_id = corsicaUtils::na_if_null(corsicaUtils::nabs(x$about$eventIdx)),
             event_code = corsicaUtils::na_if_null(as.character(x$result$eventCode)),
             event_type = corsicaUtils::na_if_null(as.character(x$result$eventTypeId)),
             event_description = corsicaUtils::na_if_null(as.character(x$result$description)),
             event_detail = corsicaUtils::na_if_null(as.character(x$result$secondaryType)),
             datetime = corsicaUtils::na_if_null(as.character(lubridate::parse_date_time(x$about$dateTime, "y-m-d.H:M:S."))),
             game_period = corsicaUtils::na_if_null(corsicaUtils::nabs(x$about$period)),
             period_time_elapsed = corsicaUtils::na_if_null(as.character(x$about$periodTime)),
             period_time_remaining = corsicaUtils::na_if_null(as.character(x$about$periodTimeRemaining)),
             event_team = corsicaUtils::na_if_null(as.character(x$team$id)),
             event_player_1 = corsicaUtils::na_if_null(player_ids[1]),
             event_player_2 = corsicaUtils::na_if_null(player_ids[2]),
             event_player_3 = corsicaUtils::na_if_null(player_ids[3]),
             event_player_4 = corsicaUtils::na_if_null(player_ids[4]),
             coords_x = corsicaUtils::na_if_null(x$coordinates$x),
             coords_y = corsicaUtils::na_if_null(x$coordinates$y),
             highlight_id = corsicaUtils::na_if_null(corsicaUtils::nabs(x$about$eventId))
  )
}

#' Parse Highlight
#' \code{parse_highlight()} parses a single highlight from the Highlights JSON object
#' @inheritParams ds.parse_event
#' @return a data.frame
#' @export
ds.parse_highlight <- function(x) {
  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
             event_id = corsicaUtils::na_if_null(x$id),
             highlight_id = corsicaUtils::na_if_null(x$feeds[[1]]$neulionId),
             event_team_1 = corsicaUtils::na_if_null(x$t1),
             event_team_2 = corsicaUtils::na_if_null(x$t2),
             event_period = corsicaUtils::na_if_null(x$p),
             event_seconds = corsicaUtils::na_if_null(x$sip),
             event_type = corsicaUtils::na_if_null(x$type)
  )

}

#' Parse Game
#'
#' \code{parse_game()} parses a single game from the Schedule >> Date JSON object
#'
#' \code{parse_game()} is an inner function for \code{parse_date()}
#'
#' @inheritParams ds.parse_event
#' @return a data.frame
#' @export
ds.parse_game <- function(x) {

  data.frame(game_id = corsicaUtils::na_if_null(corsicaUtils::nabs(x$gamePk)),
             game_date = corsicaUtils::na_if_null(as.character(as.Date(x$gameDate))),
             season = corsicaUtils::na_if_null(as.character(x$season)),
             session = corsicaUtils::na_if_null(as.character(x$gameType)),
             game_status = corsicaUtils::na_if_null(as.character(x$status$detailedState)),
             away_team_id = corsicaUtils::na_if_null(corsicaUtils::nabs(x$teams$away$team$id)),
             home_team_id = corsicaUtils::na_if_null(corsicaUtils::nabs(x$teams$home$team$id)),
             game_venue = corsicaUtils::na_if_null(as.character(x$venue$name)),
             game_datetime = corsicaUtils::na_if_null(as.character(lubridate::parse_date_time(x$gameDate, "y-m-d.H:M:S.")))
  )

}

#' Parse Date
#'
#' \code{parse_date()} parses a single date from the Schedule JSON object
#'
#' \code{parse_date()} uses an inner function \code{parse_game()}
#'
#' @inheritParams ds.parse_event
#' @return a data.frame
#' @export
ds.parse_date <- function(x) {Reduce(rbind, lapply(x$games, ds.parse_game))}

#' Parse Player
#' \code{parse_player()} parses a single player from the PBP JSON object and returns a data frame
#'
#' @inheritParams ds.parse_event
#' @return a data.frame
#' @export
ds.parse_player <- function(x) {
  data.frame(player_id = x$person$id,
             player_name = x$person$fullName,
             player_number = x$jerseyNumber,
             position = x$position$code
  )
}

#' Parse Shifts
#' \code{parse_shifts()} returns a matrix containing shift information for a single player
#' @param player the player
#' @param venue character. \code{"Home"} or \code{"Away"}.
#' @param inner inner (second) element of the output of \code{ds.get_shifts}.
#' @param outer outer  (first) element of the output of \code{ds.get_shifts}.
#' @return a matrix containing shift info
#' @export
ds.parse_shifts <- function(player, venue, inner, outer) {
  index <- which(outer[-1] == player)

  # previously this was doubled into home and away cases, which did not differ at all
  # finding and removing a bug friar introduced here was impeded by the use of the pipe
  foo <- inner[which(inner == "Shift #" | inner == "Pr\u00e9sence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index]-3)] # %>%
  foo <- matrix(foo, ncol = 6, byrow = TRUE) # %>%
  foo <- data.frame(foo) # %>%
  foo <- dplyr::mutate(foo, num_first_last = player, venue = venue) # %>%
  foo <- dplyr::filter(foo, .data$X2 != "Per") # %>%
  
  data.frame(foo)

}

#' Parse Shift
#' \code{parse_shift()} parses a single shift from the Shifts JSON object
#' @param x A single entry
#' @return a data.frame
#' @export
ds.parse_shift <- function(x) {
  data.frame(game_date        = NA,
             game_id          = corsicaUtils::na_if_null(corsicaUtils::nabs(x$gameId)),
             season           = NA,
             session          = NA,
             shift_number     = corsicaUtils::na_if_null(corsicaUtils::nabs(x$eventNumber)),
             shift_period     = corsicaUtils::na_if_null(corsicaUtils::nabs(x$period)),
             shift_start      = corsicaUtils::na_if_null(as.character(x$startTime)),
             shift_end        = corsicaUtils::na_if_null(as.character(x$endTime)),
             shift_duration   = corsicaUtils::na_if_null(as.character(x$duration)),
             team             = corsicaUtils::na_if_null(as.character(x$teamAbbrev)),
             player_id        = corsicaUtils::na_if_null(as.character(x$playerId)),
             player_name_fist = corsicaUtils::na_if_null(as.character(x$firstName)),
             player_name_last = corsicaUtils::na_if_null(as.character(x$lastName))
  )
}

ds.parse_all_players <- function(x) {
  x <- x$teams
  x <- purrr::map(x, function(i) {lapply(c(abbreviation="abbreviation", roster="roster"), getElement, object=i)})
  x <- purrr::transpose(x)
  x <- dplyr::as_tibble(x)
  x <- tidyr::unnest(x, .data$abbreviation)
  x <- dplyr::group_by(x, .data$abbreviation)
  x <- dplyr::mutate(x, roster = list(dplyr::first(.data$roster)$roster))
  x <- dplyr::mutate(x, roster = list(purrr::transpose(dplyr::first(.data$roster))))
  x <- dplyr::mutate(x, roster = list(dplyr::as_tibble(dplyr::first(.data$roster))))
  x <- tidyr::unnest(x, .data$roster)
  x <- dplyr::group_by(x)
  x <- dplyr::mutate(x, num = 1:dplyr::n())
  x <- dplyr::group_by(x, .data$abbreviation, .data$num)
  x <- dplyr::mutate(x, 
                     person   = list(dplyr::as_tibble(dplyr::first(.data$person))),
                     position = list(dplyr::as_tibble(dplyr::first(.data$position))))
  x <- tidyr::unnest(x, .data$person)
  x <- tidyr::unnest(x, .data$position)
  x <- dplyr::group_by(x)
  x <- dplyr::select(x, 
                     team     = .data$abbreviation,
                     fullName = .data$fullName,
                     pos      = .data$code,
                     id       = .data$id)
  
  x
}

ds.parse_profile <- function(x) {
  x <- dplyr::as_tibble(purrr::transpose(x$people))
  x <- dplyr::select(x, -.data$primaryPosition)
  x <- dplyr::select(x, .data$id, .data$fullName, .data$firstName, .data$lastName, .data$birthDate)
  x <- tidyr::unnest(x)
  
  x
}
