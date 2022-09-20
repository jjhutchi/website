# Simulations of the season of outcomes, 
# conditional on the past picks of losers pool participants, 
# evaluating the performance of different models. 

# assume some pick distribution of the pick preferences in the model, 
# simulate the remainder of the pool conditional on their beliefs, and compare 
# the performance of my pick schedules

# setback - we should be re-running the OC model at each week within the simulation
# but this is computationally costly, as we would have to run a simulation within each week of each simulation

#' uniform prior across teams with probabilities below some threshold. 
#' @param avail_picks the set of teams possible to pick from
#' @param week
#' 
unif_pick = function() {
  
}

pacman::p_load(data.table, dplyr)

# parameters
START_WEEK = 3 # current week picking from 
END_WEEK = 10  # last week optimizing picks until

#TODO: change from offline to read in real time google sheet data
own = read.csv("ownership.csv")
df = read.csv("nfl-proj.csv") |> 
  filter(between(week, START_WEEK, END_WEEK))

setDT(df)
setDT(own)

# quickly get the set of picks for each player
# assume they pick a team with a win rate below some threshold across weeks with no foresight

weeks = START_WEEK:END_WEEK
player_list = unique(own$Player)

# add game outcomes - win game if rv below pwin
df[, outcome := runif(1) < pwin, .(team, week)]

for(player in player_list) {
  player = "Roc1"
  past_picks = own[Player == player, ]$pick
  
  for(w in weeks) {
    
    thresh = 0.35
    pick_available = FALSE
    
    # check that there is a pick available, o/w increase threshold probability
    while(!pick_available) {
      pick_options = df[!team %in% past_picks & week == w & pwin <= thresh, ]
      pick_available = nrow(pick_options) > 0
      thresh = thresh + 0.2
    }
    
    pick = sample_n(pick_options, 1)
    
    # picked a losing team
    if(pick$outcome) {
      # lost, remove team
    } else {
      # continue, add pick to past picks
    }
    past_picks = c(past_picks, pick$team)
    # add pick to player's list 
    print(paste(w, pick$team))
    
  }
}