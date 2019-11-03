# very top-level functions, plus helpers used only by other functions in this file

#' Compile Games
#'
#' \code{compile_games()} collects, parses and compiles all game data corresponding to a given vector of game IDs and season
#'
#' This is the principal function you should find yourself calling.
#'
#' @param games           numeric vector. The game IDs. Defaults to all regular-season and playoff game IDs.
#' @param season               character. The season. Default \code{"20182019"}.
#' @param pause                  numeric. Seconds to pause between games. Default \code{1}.
#' @param try_tolerance          numeric. Number of times to bang head against wall. Default \code{3}.
#' @param agents        character vector. HTTP user agent strings. Defaults to a single sensible string.
#' @param presave                logical. Save scraped games for later retrieval? Default \code{TRUE}.
#' @param use_presaved           logical. Reload any presaved games instead of downloading? Default \code{TRUE}.
#' @param data_dir             character. Path to saved games. Default "nhlgamedata".
#' @return A list object containing c([[1]] = PBP, [[2]] = Roster, [[3]] = Shifts)
#' @export
ds.compile_games <- function(games         = ds.all_games,
                             season        = "20182019",
                             pause         = 1,
                             try_tolerance = 3,
                             agents        = ds.user_agents[[1]],
                             presave       = TRUE,
                             use_presaved  = TRUE,
                             data_dir      = "nhlgamedata") {

  # First collect the raw data
  nested_games <- ds.scrape_nested_games(games, season, pause, try_tolerance, agents, presave, use_presaved, data_dir)

  # Then parse and compile it
  ds.compile_nested_games(nested_games)
}

#' Compile Nested Games
#' \code{compile_nested_games()} parses and compiles all game data from a list of scraped game results
#' @param nested_games list. The output of \code{\link{ds.scrape_nested_games}}.
#' @return A list object containing c([[1]] = PBP, [[2]] = Roster, [[3]] = Shifts)
#' @export
ds.compile_nested_games <- function(nested_games) {
  # now rbind each element together across games
  # actually this bit is clever. three cheers for Manny
  unpacked <- do.call(base::Map, c(rbind, nested_games)) # #based

  # isolate the separate bound components of the scraped games
  pbp        <- unpacked[[1]]
  roster     <- unpacked[[2]]
  shifts     <- unpacked[[3]]
  highlights <- unpacked[[4]]
  coords     <- unpacked[[5]]

  # replace variant names with standard ones
  roster$player_name <- ds.fix_names(roster$player_name)
  shifts$player_name <- ds.fix_names(shifts$player_name)

  # disambiguate SEBASTIAN.AHO and DREW.MILLER etc.
  roster <- ds.5ebastian(roster)
  shifts <- ds.5ebastian(shifts)

  # summarize the shifts
  shift_summary <- dplyr::bind_rows(ds.shift_sum(shifts, "ON"), ds.shift_sum(shifts, "OFF"))

  # Join highlight information, if any, to pxp
  if(!is.null(highlights)) {

    highlights$event_match <- ifelse(highlights$event_type == 505, "GOAL", "SHOT")

    new_pbp <- dplyr::left_join(pbp,
                                highlights %>%
                                  dplyr::mutate(game_seconds = 1200*(corsicaUtils::nabs(.data$event_period) - 1) + corsicaUtils::nabs(.data$event_seconds)) %>%
                                  dplyr::rename(highlight_code = .data$highlight_id) %>%
                                  dplyr::select(.data$game_id, .data$game_seconds, .data$event_match, .data$highlight_code) %>% # what is better about this than fucking [, ]?????
                                  data.frame(),
                                by = c("game_id" = "game_id", "game_seconds" = "game_seconds", "event_type" = "event_match")) %>%
      data.frame()

  } else {new_pbp <- pbp %>% dplyr::mutate(highlight_code = NA) %>% data.frame()}

  # Merge coords, if any, into pxp
  new_pbp <- if(!is.null(coords)) {
    dplyr::left_join(new_pbp,
                     coords %>%
                       dplyr::rename(coords_x = .data$xcoord, coords_y = .data$ycoord) %>%
                       dplyr::select(.data$game_id, .data$seconds, .data$event_type, .data$coords_x, .data$coords_y) %>%
                       data.frame(),
                     by = c("game_id" = "game_id", "game_seconds" = "seconds", "event_type" = "event_type"))

  } else {new_pbp %>% dplyr::mutate(coords_x = NA, coords_y = NA)} %>%
    data.frame()

  # take only the first row for each event
  new_pbp <- new_pbp %>%
    dplyr::group_by(.data$game_id, .data$game_seconds, .data$event_description) %>%
    dplyr::slice(1) %>%
    data.frame()

  # these call a helper function from the helper functions file
  new_pbp$game_period       <- corsicaUtils::nabs(new_pbp$game_period)
  shift_summary$game_period <- corsicaUtils::nabs(shift_summary$game_period)

  # incorporate the shift summary and the pxp into one data.frame
  new_pbp <- dplyr::bind_rows(new_pbp, shift_summary) %>%
    dplyr::mutate(priority = 1 * (.data$event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK")) +
                             2 * (.data$event_type == "GOAL") +
                             3 * (.data$event_type == "STOP") +
                             4 * (.data$event_type == "PENL") +
                             5 * (.data$event_type == "OFF") +
                             6 * (.data$event_type == "ON") +
                             7 * (.data$event_type == "FAC")) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$game_period, .data$game_seconds, .data$priority) %>%
    dplyr::mutate(event_index = cumsum(!is.na(.data$game_id))) %>%
    data.frame()

  # figure out who's on ice for each event
  home_on_mat <- ds.are_on(shifts, new_pbp, "Home")
  away_on_mat <- ds.are_on(shifts, new_pbp, "Away")

  # turn those into nice data.frames
  home_on_df <- which(home_on_mat == 1, arr.ind = TRUE) %>%
    data.frame() %>%
    dplyr::group_by(.data$row) %>%
    dplyr::summarise(home_on_1 = colnames(home_on_mat)[unique(col)[1]],
                     home_on_2 = colnames(home_on_mat)[unique(col)[2]],
                     home_on_3 = colnames(home_on_mat)[unique(col)[3]],
                     home_on_4 = colnames(home_on_mat)[unique(col)[4]],
                     home_on_5 = colnames(home_on_mat)[unique(col)[5]],
                     home_on_6 = colnames(home_on_mat)[unique(col)[6]]) %>%
    data.frame()

  away_on_df <- which(away_on_mat == 1, arr.ind = TRUE) %>%
    data.frame() %>%
    dplyr::group_by(.data$row) %>%
    dplyr::summarise(away_on_1 = colnames(away_on_mat)[unique(col)[1]],
                     away_on_2 = colnames(away_on_mat)[unique(col)[2]],
                     away_on_3 = colnames(away_on_mat)[unique(col)[3]],
                     away_on_4 = colnames(away_on_mat)[unique(col)[4]],
                     away_on_5 = colnames(away_on_mat)[unique(col)[5]],
                     away_on_6 = colnames(away_on_mat)[unique(col)[6]]) %>%
    data.frame()

  # pick out goalies
  home_goalie <- ds.are_goalie(home_on_df, roster)
  away_goalie <- ds.are_goalie(away_on_df, roster)

  # tack on-ice player IDs onto pxp... right?
  full_pbp <- new_pbp %>%
    dplyr::arrange(.data$game_id, .data$event_index) %>%
    dplyr::mutate(home_goalie = NA, away_goalie = NA)

  full_pbp$row <- 1:nrow(full_pbp)

  full_pbp <- dplyr::left_join(full_pbp, home_on_df, "row")
  full_pbp <- dplyr::left_join(full_pbp, away_on_df, "row")

  full_pbp$home_goalie[home_on_df$row] <- home_goalie
  full_pbp$away_goalie[away_on_df$row] <- away_goalie

  # choose columns in which to replace codes with names
  player_cols <- c(paste0("event_player_", 1:3),
                   paste0(c("home", "away"), "_goalie"),
                   base::setdiff(Reduce(base::union, lapply(list(home_on_df, away_on_df), colnames)), "row"))

  # Replace player codes with player names
  full_pbp[, player_cols] <- lapply(player_cols, ds.name_player, full_pbp=full_pbp, roster=roster)

  # Assemble the whole pxp frame
  full_pbp <- full_pbp %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$event_index) %>%
    dplyr::mutate(home_skaters = 6 - 1 * (is.na(.data$home_on_1) == TRUE) -
                                     1 * (is.na(.data$home_on_2) == TRUE) -
                                     1 * (is.na(.data$home_on_3) == TRUE) -
                                     1 * (is.na(.data$home_on_4) == TRUE) -
                                     1 * (is.na(.data$home_on_5) == TRUE) -
                                     1 * (is.na(.data$home_on_6) == TRUE) -
                                     1 * (!is.na(.data$home_goalie)),
                  away_skaters = 6 - 1 * (is.na(.data$away_on_1) == TRUE) -
                                     1 * (is.na(.data$away_on_2) == TRUE) -
                                     1 * (is.na(.data$away_on_3) == TRUE) -
                                     1 * (is.na(.data$away_on_4) == TRUE) -
                                     1 * (is.na(.data$away_on_5) == TRUE) -
                                     1 * (is.na(.data$away_on_6) == TRUE) -
                                     1 * (!is.na(.data$away_goalie)),
                  home_score   = cumsum(.data$event_type == "GOAL" & .data$event_team == .data$home_team) - 1 * (.data$event_type == "GOAL" & .data$event_team == .data$home_team),
                  away_score   = cumsum(.data$event_type == "GOAL" & .data$event_team == .data$away_team) - 1 * (.data$event_type == "GOAL" & .data$event_team == .data$away_team),
                  event_length = corsicaUtils::nabs(dplyr::lead(.data$game_seconds, 1) - .data$game_seconds)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(game_strength_state = paste(ifelse(is.na(.data$home_goalie) == TRUE, "E", .data$home_skaters),
                                              ifelse(is.na(.data$away_goalie) == TRUE, "E", .data$away_skaters), sep = "v"),
                  game_score_state    = paste(.data$home_score, .data$away_score, sep = "v")) %>%
    dplyr::select(tidyselect::one_of(ds.pbp_colnames)) %>%
    dplyr::arrange(.data$game_id, .data$event_index) %>%
    data.frame()

  # Patch a single team abbreviation
  full_pbp$event_team[which(full_pbp$event_team == "PHX")] <- "ARI"

  list(PBP = full_pbp, Roster = roster, Shifts = shifts)
}

#' Fix problem of multiple players named "Sebastian Aho" etc.
#' imho just including their real-life middle initials is a better solution than "5ebastian"
#' This function to be updated over time as they move on from their rookie teams
#' @param df a data.frame
#' @return the data.frame but with Sebastians Aho etc. disambiguated
#' @export
ds.5ebastian <- function(df) {
  # there don't appear to be any games played by anyone named Mikko Lehtonen within the usable time period

  df$player_name[df$player_name=="SEBASTIAN.AHO"   & df$team == "CAR"] <- "SEBASTIAN.A..AHO" # these should surely be by position also
  df$player_name[df$player_name=="SEBASTIAN.AHO"   & df$team == "NYI"] <- "SEBASTIAN.J..AHO" # ditto
  df$player_name[df$player_name=="DREW.MILLER"     & df$team == "EDM"] <- "ANDREW.MILLER"
  df$player_name[df$player_name=="ALEX.PICARD"     & df$team == "CBJ"] <- "ALEXANDRE.PICARD"
  df$player_name[df$player_name=="COLIN.WHITE"     & df$team == "OTT"] <- "COLIN.A..WHITE"
  df$player_name[df$player_name=="ERIK.GUSTAFSSON" & df$team == "PHI"] <- "ERIK.GUSTAFSSON.OF.PHI"

  df$player_name[df$player_name=="SEAN.COLLINS"    & df$pos  == "D"]   <- "SEAN.P..COLLINS"

  df
}

# New one-liners used only within this file

#' Are Goalie
#' \code{are_goalie()} wraps \code{\link{ds.find_goalie}()} to identify goalies
#' @param on_df  data.frame of who is on ice for the team
#' @param roster data.frame recording the team's roster
#' @return vector of goalies
#' @export
ds.are_goalie <- function(on_df, roster) {
  do.call(c, on_df[, -1] %>% split(1:nrow(on_df)) %>% lapply(ds.find_goalie, roster)) %>% as.character()
}

#' Are On
#' \code{are_on()} wraps \code{\link{is_on}()} to identify who all is on ice for each event
#' @param shifts data.frame of shifts
#' @param new_pbp data.frame of play-by-play events
#' @param venue character. \code{"home"} or \code{"away"}.
#' @return a data.frame of player on-ice statuses by event
#' @export
ds.are_on <- function(shifts, new_pbp, venue) {
  data.frame(lapply(unique(shifts$team_num), is_on, pbp = dplyr::arrange(new_pbp, .data$game_id, .data$event_index), venue = venue))
}

#' Name Player
#' \code{name_player()} looks up names for coded players in a play-by-play object under construction
#' @param player    character. which player.
#' @param full_pbp data.frame. The play-by-play.
#' @param roster   data.frame. The team's roster.
#' @return a character vector.
#' @export
ds.name_player <- function(player, full_pbp, roster) {roster$player_name[match(ds.who_is(full_pbp, player), ds.who_is(roster, "team_num"))]}

#' Who Is
#' \code{who_is()} is a helper function used by \code{\link{ds.name_player}()}.
#' @param x data.frame.
#' @param i column name.
#' @return \code{x[[i]]} mangled with \code{x$game_id}.
#' @export
ds.who_is <- function(x, i) {paste(x$game_id, x[[i]], sep=".")}

#' summary of shifts
#' This is just a helper function used to reduce repetition within a function above.
#' @param shifts data.frame. Shift endpoint info.
#' @param type    character. \code{"ON"} or \code{"OFF"}. Default \code{"ON"}.
ds.shift_sum <- function(shifts, type="ON") {
  the_seconds <- if(type=="ON") {"start_seconds"} else if(type=="OFF") {"end_seconds"} else {stop("type must be 'ON' or 'OFF'")}

  shifts %>%
    dplyr::filter(!is.na(.data$shift_duration)) %>%
    dplyr::group_by(.data$game_id,
                    .data$game_date,
                    .data$season,
                    .data$session,
                    .data$home_team,
                    .data$away_team,
                    .data$team,
                    .data$game_period,
                    .data[[the_seconds]]) %>%
    dplyr::rename(game_seconds = .data[[the_seconds]], event_team = .data$team) %>%
    dplyr::summarise(event_type = type, players_substituted = paste(unique(.data$team_num), collapse = ", ")) %>%
    data.frame()
}
