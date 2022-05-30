# high-level functions in the scraping process
# lower-level functions are in the get and pull files

#' Scrape Multiple Games
#' \code{scrape_nested_games()} collects all game data corresponding to a given vector of game IDs and season
#' @inheritParams ds.compile_games
#' @return A list of several five-element lists, one for each game
#' @export
ds.scrape_nested_games <- function(games         = ds.all_games,
                                   season        = "20182019",
                                   pause         = 1,
                                   try_tolerance = 3,
                                   agents        = ds.user_agents[[1]],
                                   presave       = TRUE,
                                   use_presaved  = TRUE,
                                   data_dir      = "nhlgamedata") {
  foreach::foreach(g = as.character(games)) %do% {

    message(get("g"),
        "...",
        # "\n",
        # sep = "",
        appendLF = TRUE
    )

    inner <- ds.scrape_game(season, get("g"), try_tolerance, agents, presave, use_presaved, data_dir)
    Sys.sleep(pause)

    return(inner)

  }
}

#' Scrape Game
#'
#' \code{scrape_game()} collects the data for a game corresponding to a given season and game ID
#' @inheritParams ds.compile_games
#' @param game_id character. Game ID. Default \code{20001}.
#' @return A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights)
#' @export
ds.scrape_game <- function(season        = "20182019",
                           game_id       = "20001",
                           try_tolerance = 3,
                           agents        = ds.user_agents[[1]],
                           presave       = TRUE,
                           use_presaved  = TRUE,
                           data_dir      = "nhlgamedata") {

  season_  <- as.character(season)
  game_id_ <- as.character(game_id)

  # Shortcut to reload presaved games
  if(use_presaved) {
    fnord <- paste0(data_dir, "/", season_, "/", game_id_, ".rds")

    if(file.exists(fnord)) {return(readRDS(fnord))}
  }

  pbp         <- ds.get_pbp(       season_, game_id_,                                 try_tolerance, agents)
  home_shifts <- ds.get_shifts(    season_, game_id_, venue = "home", source = "htm", try_tolerance, agents)
  away_shifts <- ds.get_shifts(    season_, game_id_, venue = "away", source = "htm", try_tolerance, agents)
  roster      <- ds.get_roster(    season,  game_id_,                                 try_tolerance, agents)
  highlights  <- ds.get_highlights(season_, game_id_,                                 try_tolerance, agents)

  pbp_full <- pbp[[1]]
  pbp_body <- pbp[[2]]

  home_shifts_outer <- home_shifts[[1]]
  home_shifts_inner <- home_shifts[[2]]
  away_shifts_outer <- away_shifts[[1]]
  away_shifts_inner <- away_shifts[[2]]

  highlight_df <- Reduce(rbind, lapply(highlights$video$events, ds.parse_highlight))

  pbp_raw <- matrix(pbp_body, byrow = TRUE, ncol = 8) %>%
    data.frame() %>%
    dplyr::filter(.data$X2 != "Per")

  if(is.null(pbp_raw))  {return(list(NULL, NULL, NULL, NULL, NULL))}
  if(nrow(pbp_raw) < 1) {return(list(NULL, NULL, NULL, NULL, NULL))}

  game_date_ <- gsub("^[a-zA-Z]*, ", "", pbp_full[grep("^[a-zA-Z]*, ", pbp_full)]) %>%
    as.Date(format = "%B %d, %Y") %>%
    dplyr::first() %>%
    as.character()

  game_id_unique <- paste(substr(season_, 0, 4), "0", as.character(game_id_), sep = "")

  session_ <- ifelse(corsicaUtils::nabs(game_id_) > 30000, "P", "R")

  home_team_ <- gsub(" On Ice", "", pbp_body[8])
  away_team_ <- gsub(" On Ice", "", pbp_body[7])

  home_team_[which(home_team_ == "PHX")] <- "ARI"
  away_team_[which(away_team_ == "PHX")] <- "ARI"

  # pull coordinates from espn
  coordinates_df <- tryCatch({
    ds.get_coordinates(season_, game_id_, source = "espn", date = game_date_, away_team = away_team_, try_tolerance, agents)
  }, error=function(e) {})

  # check for problems
  dupe_check <- if(is.null(coordinates_df)) {data.frame()} else {
    coordinates_df %>%
      dplyr::filter(corsicaUtils::nabs(.data$period) < 5, .data$event_type == "GOAL") %>%
      dplyr::group_by(.data$seconds) %>%
      dplyr::summarise(dupes = dplyr::n()) %>%
      dplyr::filter(.data$dupes > 1) %>%
      data.frame()
  }

  # replace with coordinates from the nhl api if there is a problem with the espn ones
  coordinates_df <- if(nrow(dupe_check) == 0 & !is.null(coordinates_df)) {coordinates_df} else {

    nhl_coord_df <- ds.get_coordinates(season_, game_id_, source = "nhl", date = game_date_, away_team = away_team_, try_tolerance, agents)

    # if we get something evil instead of a data.frame, the next bit crashes
    # 20172018 game 394
    # so let's try giving back NULL if we get something evil

    if(is.data.frame(nhl_coord_df)) {
      nhl_coord_df <- nhl_coord_df %>%
        dplyr::rename(time        = .data$period_time_elapsed,
                      xcoord      = .data$coords_x,
                      ycoord      = .data$coords_y,
                      period      = .data$game_period,
                      description = .data$event_description) %>%
        dplyr::mutate(event_code = NA,
                      seconds    = 1200 * (corsicaUtils::nabs(.data$period) - 1) + ds.seconds_from_ms(.data$time),
                      event_type = as.character(.data$event_type)) %>%
        dplyr::select(.data$event_code,
                      .data$xcoord,
                      .data$ycoord,
                      .data$time,
                      .data$period,
                      .data$description,
                      .data$event_type,
                      .data$seconds) %>%
        data.frame()

      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "MISSED_SHOT")]  <- "MISS"
      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "BLOCKED_SHOT")] <- "BLOCK"
      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "FACEOFF")]      <- "FAC"
      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "GIVEAWAY")]     <- "GIVE"
      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "TAKEAWAY")]     <- "TAKE"
      nhl_coord_df$event_type[which(nhl_coord_df$event_type == "PENALTY")]      <- "PENL"

      dupe_check <- nhl_coord_df %>%
        dplyr::filter(corsicaUtils::nabs(.data$period) < 5, .data$event_type == "GOAL") %>%
        dplyr::group_by(.data$seconds) %>%
        dplyr::summarise(dupes = dplyr::n()) %>%
        dplyr::filter(.data$dupes > 1) %>%
        data.frame()

      if(nrow(dupe_check) > 0) {NULL} else {nhl_coord_df}
    } else {NULL}

  }

  pbp_df <- pbp_raw %>% # OH LAWD HE COMIN
    dplyr::filter(.data$X4 != "", .data$X2 != "") %>%
    dplyr::mutate(game_date    = game_date_,
                  game_id      = game_id_unique,
                  season       = as.character(season_),
                  session      = session_,
                  home_team    = home_team_,
                  away_team    = away_team_,
                  time_elapsed = regmatches(.data$X4, regexpr("[0-9]+:[0-9]{2}", .data$X4)),
                  game_seconds = 1200*(corsicaUtils::nabs(.data$X2) - 1) + ds.seconds_from_ms(.data$time_elapsed),
                  event_team   = unlist(lapply(regmatches(as.character(.data$X6),
                                                          gregexpr(paste("(^",
                                                                         paste(ds.team_list, collapse = "|^"),
                                                                         ")", sep = ""),
                                                                   as.character(.data$X6))),
                                               corsicaUtils::na_if_null)),
                  event_player_1 = unlist(lapply(regmatches(as.character(.data$X6),
                                                            gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(.data$X6))),
                                                 ds.clean.nums))[seq(1, 3*length(.data$X6), 3)],
                  event_player_2 = unlist(lapply(regmatches(as.character(.data$X6),
                                                            gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(.data$X6))),
                                                 ds.clean.nums))[seq(2, 3*length(.data$X6), 3)],
                  event_player_3 = unlist(lapply(regmatches(as.character(.data$X6),
                                                            gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(.data$X6))),
                                                 ds.clean.nums))[seq(3, 3*length(.data$X6), 3)],
                  event_zone = gsub(". [zZ]one",
                                    "",
                                    unlist(lapply(regmatches(as.character(.data$X6),
                                                             gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(.data$X6))),
                                                  corsicaUtils::na_if_null))),
                  event_detail = gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.",
                                      "",
                                      unlist(lapply(regmatches(as.character(.data$X6),
                                                               gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],",
                                                                        as.character(.data$X6))),
                                                    corsicaUtils::na_if_null)))) %>%
    dplyr::rename(game_period       = .data$X2,
                  event_type        = .data$X5,
                  event_description = .data$X6) %>%
    dplyr::select(.data$game_period,
                  .data$event_type,
                  .data$event_description,
                  .data$game_date:.data$event_detail) %>%
    data.frame()

  pbp_df <- dplyr::bind_rows(
    pbp_df %>%
      dplyr::filter(.data$event_type == "FAC") %>%
      dplyr::mutate(event_player_1 = paste(.data$away_team, .data$event_player_1, sep = ""),
                    event_player_2 = paste(.data$home_team, .data$event_player_2, sep = ""),
                    event_player_3 = NA),

    pbp_df %>%
      dplyr::filter(.data$event_type %in% c("HIT", "BLOCK", "PENL")) %>%
      dplyr::group_by(.data$event_team) %>%
      dplyr::mutate(event_player_1 = paste(dplyr::first(.data$event_team), .data$event_player_1, sep = ""),
                    event_player_2 = paste(unique(c(.data$home_team, .data$away_team))[which(unique(c(.data$home_team, .data$away_team)) != dplyr::first(.data$event_team))], .data$event_player_2, sep = ""),
                    event_player_3 = NA),

    pbp_df %>%
      dplyr::filter(.data$event_type %in% c("SHOT", "MISS", "GIVE", "TAKE")) %>%
      dplyr::group_by(.data$event_team) %>%
      dplyr::mutate(event_player_1 = paste(dplyr::first(.data$event_team), .data$event_player_1, sep = ""),
                    event_player_2 = NA,
                    event_player_3 = NA),

    pbp_df %>%
      dplyr::filter(.data$event_type %in% c("GOAL")) %>%
      dplyr::group_by(.data$event_team) %>%
      dplyr::mutate(event_player_1 = paste(dplyr::first(.data$event_team), .data$event_player_1, sep = ""),
                    event_player_2 = paste(dplyr::first(.data$event_team), .data$event_player_2, sep = ""),
                    event_player_3 = paste(dplyr::first(.data$event_team), .data$event_player_3, sep = "")),

    pbp_df %>%
      dplyr::filter(.data$event_type %in% c("FAC", "HIT", "BLOCK", "PENL", "SHOT", "MISS", "GIVE", "TAKE", "GOAL") == FALSE) %>%
      data.frame()
    ) %>%
    dplyr::mutate(event_player_1 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, .data$event_player_1),
                  event_player_2 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, .data$event_player_2),
                  event_player_3 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, .data$event_player_3)) %>%
    data.frame()

  if(!is.null(roster)) {

    # 2019-03-30: have unfucked this regex, which used to drop Oshie and others with . or ' in their name
    pos_match <- regmatches(as.character(roster[1]), gregexpr("[0-9]+(\\\r\\\n|\\\n)[A-Z]+(\\\r\\\n|\\\n)[A-Z )(-.']+(\\\r\\\n|\\\n)", as.character(roster[1]))) %>%
      unlist() %>%
      strsplit("(\\\r\\\n|\\\n)") %>%
      unlist() %>%
      matrix(ncol = 3, byrow = TRUE) %>%
      data.frame() %>%
      dplyr::rename(player_number   = .data$X1,
                    player_position = .data$X2,
                    player_name     = .data$X3)

    pos_match$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", pos_match$player_name)

  }

  if(!is.null(home_shifts) & !is.null(away_shifts) & length(home_shifts_outer[-1]) > 0 & length(away_shifts_outer[-1]) > 0) {

    roster_df <- dplyr::bind_rows(
      data.frame(team_name      = home_shifts_outer[1],
                 team           = home_team_,
                 venue          = "Home",
                 num_first_last = home_shifts_outer[-1]
      ),

      data.frame(team_name      = away_shifts_outer[1],
                 team           = away_team_,
                 venue          = "Away",
                 num_first_last = away_shifts_outer[-1]
      )
    ) %>%
      data.frame() %>%
      dplyr::filter(grepl("[A-Z0-9]", .data$num_first_last) == TRUE) %>%
      dplyr::mutate(game_date = game_date_,
                    game_id = game_id_unique,
                    season = as.character(season_),
                    session = session_,
                    player_number = unlist(regmatches(as.character(.data$num_first_last), gregexpr("^[0-9]+", as.character(.data$num_first_last)))),
                    team_num = paste(.data$team, .data$player_number, sep = "")) %>%
      data.frame()

    name_mat <- strsplit(gsub("^[0-9]+ ", "", roster_df$num_first_last), ", ") %>%
      unlist() %>%
      as.character() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      data.frame()

    roster_df$first_name      <- name_mat[, 2]
    roster_df$last_name       <- name_mat[, 1]
    roster_df$player_name     <- paste(roster_df$first_name, roster_df$last_name, sep = ".")
    roster_df$name_match      <- gsub("[^A-Z]|\\([A-Z]+\\)", "", roster_df$player_name)
    roster_df$player_position <- pos_match$player_position[match(roster_df$name_match, pos_match$name_match)]

    # here friar has decomposed a pipeline to help find a bug he introduced into it earlier
    shifts_df <- dplyr::bind_rows( # HERE
      do.call(rbind,
              lapply(as.list(home_shifts_outer[-1]), ds.parse_shifts, venue = "Home", outer = home_shifts_outer, inner = home_shifts_inner)) %>%
        data.frame() %>%
        dplyr::mutate(team = home_team_),

      do.call(rbind,
              lapply(as.list(away_shifts_outer[-1]), ds.parse_shifts, venue = "Away", outer = away_shifts_outer, inner = away_shifts_inner)) %>%
        data.frame() %>%
        dplyr::mutate(team = away_team_)
    )

    shifts_df <- shifts_df %>%
      data.frame()

    shifts_df <- shifts_df %>%
      dplyr::rename(shift_number   = .data$X1,
                    game_period    = .data$X2,
                    shift_start    = .data$X3,
                    shift_end      = .data$X4,
                    shift_duration = .data$X5)

    shifts_df <- shifts_df %>%
      dplyr::select(.data$shift_number:.data$shift_duration, .data$num_first_last, .data$team, .data$venue)

    shifts_df <- shifts_df %>%
      dplyr::mutate(game_date = game_date_,
                    game_id   = game_id_unique,
                    season    = as.character(season_),
                    session   = session_,
                    home_team = home_team_,
                    away_team = away_team_) %>%
      data.frame()

    shifts_df$player_name <- roster_df$player_name[match(shifts_df$num_first_last, roster_df$num_first_last)]
    shifts_df$game_period <- as.character(shifts_df$game_period)
    shifts_df$game_period[which(shifts_df$game_period == "OT")] <- "4"
    shifts_df$team_num <- paste(shifts_df$team, gsub("[^0-9]", "", shifts_df$num_first_last), sep = "")

    start_mat <- do.call(rbind, strsplit(as.character(shifts_df$shift_start), " / ")) %>% data.frame()
    end_mat   <- do.call(rbind, strsplit(as.character(shifts_df$shift_end),   " / ")) %>% data.frame()

    shifts_df$start_seconds <- 1200 * (corsicaUtils::nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(start_mat[, 1])
    shifts_df$end_seconds   <- 1200 * (corsicaUtils::nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(end_mat[, 1])

    shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])

    shifts_df <- shifts_df %>%
      dplyr::filter(ds.seconds_from_ms(.data$shift_duration) + (.data$start_seconds - 1200 * (corsicaUtils::nabs(.data$game_period) - 1)) <= 1200) %>%
      data.frame()

  } else {

    shifts <- ds.get_shifts(season_, game_id_, venue = NULL, source = "json", try_tolerance, agents)

    shifts_df <- Reduce(rbind, lapply(shifts$data, ds.parse_shift))

    if(is.null(shifts_df)) {return(list(NULL, NULL, NULL, NULL, NULL))}

    shifts_df <- shifts_df %>%
      dplyr::mutate(game_date = game_date_,
                    season = as.character(season_),
                    session = session_,
                    home_team = home_team_,
                    away_team = away_team_) %>%
      data.frame()

    year <- substr(season, 0, 4)

    url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                 as.character(year),
                 "0",
                 as.character(game_id),
                 "/feed/live?site=en_nhl",
                 sep = ""
    )

    raw_json <- ds.pull_JSON(url, agents, try_tolerance)

    home_roster <- raw_json$liveData$boxscore$teams$home
    away_roster <- raw_json$liveData$boxscore$teams$away

    home_player_data <- Reduce(rbind, lapply(home_roster$players, ds.parse_player))
    away_player_data <- Reduce(rbind, lapply(away_roster$players, ds.parse_player))

    player_data <- dplyr::bind_rows(
      home_player_data %>%
        dplyr::mutate(team      = home_roster$team$abbreviation,
                      team_name = toupper(home_roster$team$name),
                      venue     = "Home"),

      away_player_data %>%
        dplyr::mutate(team      = away_roster$team$abbreviation,
                      team_name = toupper(away_roster$team$name),
                      venue     = "Away")
    ) %>%
      data.frame()

    player_data$team_num <- paste(player_data$team, player_data$player_number, sep = "")

    name_match <- Reduce(rbind, lapply(player_data$player_id, ds.scrape_player_profile))

    roster_df <- player_data %>%
      dplyr::mutate(first_name      = toupper(name_match$player_name_first[match(.data$player_id, name_match$player_id)]),
                    last_name       = toupper(name_match$player_name_last[match(.data$player_id, name_match$player_id)]),
                    num_first_last  = NA,
                    game_date       = game_date_,
                    game_id         = game_id_unique,
                    season          = as.character(season_),
                    session         = session_,
                    home_team       = home_team_,
                    away_team       = away_team_,
                    player_name     = paste(.data$first_name, .data$last_name, sep = "."),
                    name_match      = gsub("[^A-Z]|\\([A-Z]+\\)", "", .data$player_name),
                    player_position = substr(.data$position, 0, 1)) %>%
      dplyr::select(.data$team_name,
                    .data$team,
                    .data$venue,
                    .data$num_first_last,
                    .data$game_date,
                    .data$game_id,
                    .data$season,
                    .data$session,
                    .data$player_number,
                    .data$team_num,
                    .data$first_name,
                    .data$last_name,
                    .data$player_name,
                    .data$name_match,
                    .data$player_position) %>%
      data.frame()

    shifts_df <- shifts_df %>%
      dplyr::rename(game_period = .data$shift_period) %>%
      dplyr::mutate(num_first_last = NA,
                    venue          = ifelse(.data$team == home_team_, "Home","Away"),
                    game_date      = game_date_,
                    game_id        = game_id_unique,
                    season         = as.character(season_),
                    session        = session_,
                    home_team      = home_team_,
                    away_team      = away_team_,
                    player_name    = player_data$player_name[match(.data$player_id, player_data$player_id)],
                    team_num       = player_data$team_num[match(.data$player_id, player_data$player_id)],
                    start_seconds  = 1200 * (corsicaUtils::nabs(.data$game_period) - 1) + ds.seconds_from_ms(.data$shift_start),
                    end_seconds    = 1200 * (corsicaUtils::nabs(.data$game_period) - 1) + ds.seconds_from_ms(.data$shift_end)) %>%
      dplyr::select(.data$shift_number,
                    .data$game_period,
                    .data$shift_start,
                    .data$shift_end,
                    .data$shift_duration,
                    .data$num_first_last,
                    .data$team,
                    .data$venue,
                    .data$game_date,
                    .data$game_id,
                    .data$season,
                    .data$session,
                    .data$home_team,
                    .data$away_team,
                    .data$player_name,
                    .data$team_num,
                    .data$start_seconds,
                    .data$end_seconds) %>%
      data.frame()

    shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])

    shifts_df <- shifts_df %>%
      dplyr::filter(ds.seconds_from_ms(.data$shift_duration) + (.data$start_seconds - 1200 * (corsicaUtils::nabs(.data$game_period) - 1)) <= 1200) %>% data.frame()
  } # phew

  if(!is.null(highlight_df)) {

    dupe_check <- highlight_df %>%
      dplyr::filter(corsicaUtils::nabs(.data$event_period) < 5, .data$event_type == 505) %>%
      dplyr::group_by(.data$event_id) %>%
      dplyr::summarise(dupes = dplyr::n()) %>%
      dplyr::filter(.data$dupes > 1) %>%
      data.frame()

    highlight_df <- if(nrow(dupe_check) > 0) {NULL} else {

      highlight_df %>%
        dplyr::mutate(game_date = game_date_,
                      game_id   = game_id_unique,
                      season    = as.character(season_),
                      session   = session_,
                      home_team = home_team_,
                      away_team = away_team_) %>%
        data.frame()

    }

  }

  coordinates_df <- if(is.null(coordinates_df)) {coordinates_df} else {
    coordinates_df %>%
      dplyr::mutate(game_date = game_date_,
                    game_id   = game_id_unique,
                    season    = as.character(season_),
                    session   = session_,
                    home_team = home_team_,
                    away_team = away_team_) %>%
      data.frame()

  }

  it <- list(PBP = pbp_df, Roster = roster_df, Shifts = shifts_df, Highlights = highlight_df, Coords = coordinates_df)

  if(presave) {
    if(!dir.exists(paste0(data_dir, "/", season_))) {dir.create(paste0(data_dir, "/", season_), recursive=TRUE)}

    saveRDS(it, paste0(data_dir, "/", season_, "/", game_id_, ".rds"))
  }

  it
}

#' Scrape Team Profile
#' \code{scrape_team_profile()} collects and parses the data for a team corresponding to a given team ID
#' @inheritParams ds.get_team_profile
#' @return a data.frame
#' @export
ds.scrape_team_profile <- function(team_id,
                                   try_tolerance = 3,
                                   agents = ds.user_agents[[1]]) {

  team_id_ <- corsicaUtils::nabs(team_id)

  team <- ds.get_team_profile(team_id_, try_tolerance, agents)

  data.frame(team_id              = corsicaUtils::na_if_null(corsicaUtils::nabs(team$teams[[1]]$id)),
             team_name            = corsicaUtils::na_if_null(team$teams[[1]]$name),
             team_alias           = corsicaUtils::na_if_null(team$teams[[1]]$abbreviation),
             team_venue           = corsicaUtils::na_if_null(team$teams[[1]]$venue$name),
             team_location        = corsicaUtils::na_if_null(team$teams[[1]]$locationName),
             team_city            = corsicaUtils::na_if_null(team$teams[[1]]$venue$city),
             team_division_id     = corsicaUtils::na_if_null(corsicaUtils::nabs(team$teams[[1]]$division$id)),
             team_division_name   = corsicaUtils::na_if_null(team$teams[[1]]$division$name),
             team_conference_id   = corsicaUtils::na_if_null(corsicaUtils::nabs(team$teams[[1]]$conference$id)),
             team_conference_name = corsicaUtils::na_if_null(team$teams[[1]]$conference$name),
             franchise_id         = corsicaUtils::na_if_null(corsicaUtils::nabs(team$teams[[1]]$franchiseId)),
             is_active            = corsicaUtils::na_if_null(as.logical(team$teams[[1]]$active)))

}

#' Scrape Player Profile
#' \code{scrape_player_profile()} collects and parses the data for a player corresponsing to a given player ID
#' @inheritParams ds.get_player_profile
#' @return a data.frame
#' @export
ds.scrape_player_profile <- function(player_id     = 8477474,
                                     try_tolerance = 3,
                                     agents        = ds.user_agents[[1]]) {

  player_id_ <- corsicaUtils::nabs(player_id)

  player     <- ds.get_player_profile(player_id_, try_tolerance, agents)

  data.frame(player_id            = corsicaUtils::na_if_null(corsicaUtils::nabs(player$people[[1]]$id)),
             player_name_first    = corsicaUtils::na_if_null(as.character(player$people[[1]]$firstName)),
             player_name_last     = corsicaUtils::na_if_null(as.character(player$people[[1]]$lastName)),
             player_name_full     = corsicaUtils::na_if_null(as.character(player$people[[1]]$fullName)),
             player_jerseynum     = corsicaUtils::na_if_null(corsicaUtils::nabs(player$people[[1]]$primaryNumber)),
             player_position      = corsicaUtils::na_if_null(as.character(player$people[[1]]$primaryPosition$code)),
             player_birth_date    = corsicaUtils::na_if_null(as.character(as.Date(player$people[[1]]$birthDate))),
             player_birth_city    = corsicaUtils::na_if_null(as.character(player$people[[1]]$birthCity)),
             player_birth_country = corsicaUtils::na_if_null(as.character(player$people[[1]]$birthCountry)),
             player_nationality   = corsicaUtils::na_if_null(as.character(player$people[[1]]$nationality)),
             player_height        = corsicaUtils::na_if_null(as.character(player$people[[1]]$height)),
             player_weight        = corsicaUtils::na_if_null(corsicaUtils::nabs(player$people[[1]]$weight)),
             player_handedness    = corsicaUtils::na_if_null(as.character(player$people[[1]]$shootsCatches)),
             is_active            = corsicaUtils::na_if_null(as.logical(player$people[[1]]$active)),
             is_rookie            = corsicaUtils::na_if_null(as.logical(player$people[[1]]$rookie)))

}

#' Scrape Schedule
#' \code{scrape_schedule()} collects and parses the schedule data for a range corresponding to a given start and end date
#' @inheritParams ds.get_schedule
#' @return a data.frame
#' @export
ds.scrape_schedule <- function(start         = "2018-10-01",
                               end           = "2019-06-30",
                               try_tolerance = 3,
                               agents        = ds.user_agents[[1]]) {
  start_ <- as.character(start); end_ <- as.character(end)

  sched <- ds.get_schedule(start_, end_, try_tolerance, agents)

  Reduce(rbind, lapply(sched$dates, ds.parse_date))
}

