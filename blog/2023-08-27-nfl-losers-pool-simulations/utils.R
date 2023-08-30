# Losers Pool Functions 

#' download fivethirtyeight projections from end of 2022 season, update team labels
.collect_fivethirtyeight_elo = function() {
  
  if(any(grepl("538_elo.csv", list.files("blog/2023-08-27-nlf-losers-pool-simulations")))) {
    elo_raw = read.csv("blog/2023-08-27-nlf-losers-pool-simulations/538_elo.csv")
  } else {
    elo_raw = data.table::fread("https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv")
  }
  
  rbind(elo_raw |> dplyr::select(date, team = team1, elo = qbelo1_post),
        elo_raw |> dplyr::select(date, team = team2, elo = qbelo2_post)) |> 
    dplyr::group_by(team) |> 
    dplyr::arrange(date) |> 
    dplyr::slice(1L) |> 
    dplyr::select(-date) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(team = dplyr::case_when(team == "OAK" ~ "LV", 
                                team == "LAR" ~ "LA", 
                                team == "WSH" ~ "WAS", 
                                TRUE ~ team))
  
}

#' Run simulations based on ELO
#' Update team ELO based on game outcome
.elo_model <- function(teams, games, week_num, ...) {
  
  # round out (away from zero)
  # this way the simulator never simulates a tie
  # the simulator will still allow ties to be simulated if you want
  # ... but not on playoff games
  round_out <- function(x) {
    x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
    x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
    return(x)
  }
  
  # we're going to store elo as a new columns in the teams data
  # it won't start off there of course, so we need to determine it
  # from our arguments
  if (!("elo" %in% colnames(teams))) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull the elo info from custom arguments
      teams <- teams %>%
        dplyr::inner_join(args$elo %>% dplyr::select(team, elo), by = c("team" = "team"))
    } else {
      # error with a friendly error message if no elo data is passed in
      stop("Pass in a tibble `elo` as an argument to `simulate_nfl()`")
    }
  }
  
  # isolate the ratings data by sim and by team only
  # we will want to join to the games data later and don't want excess columns
  ratings <- teams %>% dplyr::select(sim, team, elo)
  
  # simulate game outcomes
  games <- games %>%
    # add in the away team's elo to the game data
    # note we join on both `sim` and the team
    # always join on `sim` to make sure each sim cares about only its data
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename(away_elo = elo) %>%
    # repeat for the home team as well
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
    dplyr::rename(home_elo = elo) %>%
    dplyr::mutate(
      # calculate the elo difference
      elo_diff = home_elo - away_elo,
      # add in a small HFA amount if played at home
      elo_diff = elo_diff + ifelse(location == "Home", 20, 0),
      # make an adjustment for rest
      elo_diff = elo_diff + (home_rest - away_rest) / 7 * 25,
      # playoff games swing elo more
      elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2),
      # from elo, we calculate the home team's win percentage
      wp = 1 / (10^(-elo_diff / 400) + 1),
      # we also can calculate the estimate (mean points home team wins by)
      estimate = elo_diff / 25,
      result = dplyr::case_when(
        # !!! ALWAYS DO THIS NEXT LINE IN YOUR `result` CHANGES !!!
        # you have to make sure you're only changing unfinished games in current week
        # if you don't do this, it will usually error out on a friendly error message
        is.na(result) & week == week_num ~ 
          as.integer(round_out(rnorm(n(), estimate, 13))),
        # if not this week or known result, leave as-is
        TRUE ~ as.integer(result)
      ),
      # simplify to 1 = win, 0 = loss, 0.5 = tie to help calculate elo shift
      outcome = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ 1,
        result < 0 ~ 0,
        TRUE ~ 0.5
      ),
      # calculate the amount to adjust home team's elo by
      elo_input = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ elo_diff * 0.001 + 2.2,
        result < 0 ~ -elo_diff * 0.001 + 2.2,
        TRUE ~ 1.0,
      ),
      elo_mult = log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input,
      elo_shift = 20 * elo_mult * (outcome - wp)
    ) %>%
    # we don't want these columns in `games` any more
    # remove any columns you don't need when you're done
    # otherwise the next week they'll get joined as `col.x` and `col.y`
    # which will almost certainly break your script
    dplyr::select(
      -away_elo, -home_elo, -elo_diff, -wp, -estimate,
      -outcome, -elo_input, -elo_mult
    )
  
  # apply elo shifts
  teams <- teams %>%
    # join games results from this week to away teams (within same sim!)
    # note this is a LEFT join, we don't want to remove any teams rows
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, away_team, elo_shift),
                     by = c("sim" = "sim", "team" = "away_team")
    ) %>%
    # away team's elo gets subtracted by elo amount
    # if the team wasn't an away team, do nothing
    dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    # we don't want to keep `elo_shift` in `teams` either, remove it
    dplyr::select(-elo_shift) %>%
    # repeat the above except now do it for the home team
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, home_team, elo_shift),
                     by = c("sim" = "sim", "team" = "home_team")
    ) %>%
    # note that a team on a bye will have `elo_shift` as NA for both joins
    # this means it won't change, which is what we want
    dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    dplyr::select(-elo_shift)
  
  # we need to keep `elo_shift` out of `games` too and we're done with it
  games <- games %>%
    dplyr::select(-elo_shift)
  
  # return the updated teams and games information
  # note that `teams` will now have an updated `elo` column which will
  # be used for the next week's games
  # note that starting `elo` values are the same per-team... 
  # ... but after that will differ per sim depending on that sim's results
  return(list(teams = teams, games = games))
}

.run_sims = function(num_sims, initial_elo) {
  future::plan("multisession")
  out = nflseedR::simulate_nfl(
    nfl_season = 2023,
    process_games = .elo_model,
    elo = initial_elo,
    sim_include = "REG",
    simulations = num_sims,
    sims_per_round = 8)
  future::plan("sequential")
  
  out
}




#' Stich `nflreadr::load_schedules()` data with game predictions with game 
#' predictions made by `nflseedR::simulate_nfl` starting teams with their
#' ELO scores taken from end of previous season by fivethirtyeight
#' 
#' @param SIMULATIONS if NULL, read in past compiled data, otherwise resimualte the season
get_projections = function(SIMULATIONS=NULL) {
  
  if(is.null(SIMULATIONS)) {
    return(read.csv("sim-data.csv"))
  } else {
    elo = .collect_fivethirtyeight_elo()
    
    initial_elo <- tibble::tibble(
      team = elo$team,
      elo = elo$elo
    )
    
    data = nflseedR::load_sharpe_games() |>
      dplyr::left_join(elo |> dplyr::rename(elo_home = elo), by = c("home_team" = "team")) |>
      dplyr::left_join(elo |> dplyr::rename(elo_away = elo), by = c("away_team" = "team"))
    
    sim = .run_sims(SIMULATIONS, initial_elo)
    write.csv(sim$game_summary, file = "sim-data.csv", row.names = FALSE)
    sim$game_summary
    
  }
  
}


#' Greedy Algorithm
#' Pick the lowest win probability starting with the first week, 
#' while not picking the same team twice
pick_greedy = function(data) {
  
  results = data.frame()    # To store the selected rows
  selected_teams = c()        # To keep track of selected IDs
  selected_weeks = c()      # To keep track of selected weeks
  
  unique_weeks = sort(unique(data$week))
  
  for (week in unique_weeks) {
    
    week_data = data[data$week == week, ]
    valid_rows = week_data[!(week_data$team %in% selected_teams) & !(week_data$week %in% selected_weeks), ]
    
    if (nrow(valid_rows) > 0) {
      min_value_row = valid_rows[which.min(valid_rows$pct), ]
      results = rbind(results, min_value_row)
      selected_teams = c(selected_teams, min_value_row$team)
      selected_weeks = c(selected_weeks, min_value_row$week)
    }
  }
  
  results |> 
    arrange(week) |> 
    mutate(prob = cumprod(1 - pct))
}


#' Optimal Algorithm
#' Gets the global minimum based on traveling salesmen problem
#' As seen here
pick_optimal = function(data) {
  require(clue)
  
  # format to matrix
  dat = data |> 
    select(team, pct, week) |> 
    # mutate(pct = 1-pct) |> 
    tidyr::pivot_wider(names_from = week, values_from = pct, values_fill = 1) 
  
  dat_m = dat |> 
    select(-team) |> 
    as.matrix()
  
  rownames(dat_m) = dat$team
  colnames(dat_m)
  
  assignment <- as.vector(clue::solve_LSAP(t(dat_m), maximum = FALSE))
  assignment.prob <- diag(dat_m[assignment, ])
  survival.prob <- prod(assignment.prob)
  exp.survival.time <- sum(cumprod(assignment.prob))
  survivor.picks <- row.names(dat_m)[assignment]
  
  out = data.frame(week = as.numeric(colnames(dat_m)), 
                   team = survivor.picks, 
                   pct = assignment.prob) |> 
    arrange(week) |> 
    mutate(prob = cumprod(1 - pct))
  
}
