# animate both optimization algorithms
# 
# source this script to generate the data from the blog post. 

#TODO: add delta label

# dependencies
pacman::p_load(data.table, ggplot2, gganimate, dplyr)

# ggplot theme 
theme_set(
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), 
      # axis.ticks.y = element_blank(),
      plot.title=element_text(size = 16, face="bold"),
      plot.title.position = "plot",
      plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
      plot.caption=element_text(size=8, margin=margin(t=8), color="#7a7d7e"), 
      legend.position = "bottom"
    )
)

#' Read in FiveThirtyEight weekly projections
read_data = function(path, init_week = "2022-09-08") {
  cols = c("date", "season", "team1", "team2", "qbelo_prob1", "qbelo_prob2")
  dt = fread(path, select = cols)
  dt |> 
    mutate(pwin = pmin(qbelo_prob1, qbelo_prob2), 
           team = case_when(
             qbelo_prob1 < qbelo_prob2 ~ team1, 
             qbelo_prob1 >= qbelo_prob2 ~ team2,
             TRUE ~ "ERROR"), 
           week = floor(as.numeric(difftime(date + 1, init_week, units="days")) / 7) + 1) |> 
    select(-c(team1, team2, qbelo_prob1, qbelo_prob2))}


#TODO: read in the already picked teams in step 1 to show
#TODO: add picked team labels 
#TODO: better formatting
#TODO: the weird point at 1
#TODO fix indexing with i - not hitting them all

#' Calculate the two lowest teams per week
#' Return a dataframe of the two lowest remaining picks per week + already selected picks
#' Plot parameters: 
#' - distance_bar: show all
#' - pick: show all potential picks
frame_two_lowest_picks = function() {
  out = df |>
    filter(!team %in% picks$team,
           !week %in% picks$week) |>
    arrange(week, pwin) |>
    group_by(week) |>
    slice(1:2) |>
    ungroup() |>
    mutate(label = case_when(row_number() == 1 ~ "Keep the two lowest win probabilities per week",
                             TRUE ~ ""),
           distance_bar = "show",
           pick = "show", 
           delta = NA)
  
  prev_picks = picks |> 
    mutate(label = "", 
           distance_bar = "highlight", 
           pick = "highlight", 
           delta = 0)
  
  rbind(out, prev_picks)
  
}

#' Highlight the largest distance between picks
#' Return a dataframe of the two lowest remaing picks per week, with parameter to highlight the largest difference
#' Plot parameters: 
#' - distance_bar: highlight max, show rest
#' - pick: show all 
frame_largest_distance = function() {
  out = df |>
    filter(!team %in% picks$team,
           !week %in% picks$week) |>
    arrange(week, pwin) |>
    group_by(week) |>
    slice(1:2) |>
    mutate(delta = lead(pwin) - pwin,
           delta = ifelse(is.na(delta), lag(delta), delta)) |>
    ungroup() |>
    mutate(label = case_when(row_number() == 1 ~ "Identify largest distance",
                             TRUE ~ ""),
           distance_bar = ifelse(delta == max(delta), "highlight", "show"),
           pick = ifelse(delta == max(delta) & lag(distance_bar) != "highlight", "highlight", "show")) # kinda jank, but want only the first point to be highlighted
  
  prev_picks = picks |> 
    mutate(label = "", 
           distance_bar = "highlight", 
           pick = "highlight", 
           delta = 0)
  
  rbind(out, prev_picks)
}

#' Highlight the selected team
#' Return a dataframe with the distance bars, removing the largest bar, highlighting the pick
#' Plot parameters: 
#' - distance_bar: show all except week of the pick
#' - pick: highlight selection, show rest
frame_make_pick = function() {
  out = df |> 
    filter(!team %in% picks$team,
           !week %in% picks$week) |>
    arrange(week, pwin) |>
    group_by(week) |>
    slice(1:2) |>
    mutate(delta = lead(pwin) - pwin) |>
    ungroup() |>
    mutate(label = case_when(row_number() == 1 ~ "Make pick",
                             TRUE ~ ""),
           distance_bar = "show",
           pick = case_when(delta == max(delta, na.rm = TRUE) ~ "highlight",
                            TRUE ~ "show")) |>
    filter(!lag(pick) == "highlight" | row_number() == 1) # remove second best pick #TODO: lose first row
  
  prev_picks = picks |> 
    mutate(label = "", 
           distance_bar = "hightlight", 
           pick = "highlight", 
           delta = 0)
  
  rbind(out, prev_picks)
}

#' Get the optimal pick per week
oc_pick = function() {
  df |> 
    filter(!team %in% picks$team,
           !week %in% picks$week) |>
    arrange(week, pwin) |>
    group_by(week) |>
    slice(1:2) |>
    mutate(delta = lead(pwin) - pwin) |> 
    ungroup() |> 
    arrange(desc(delta)) |> 
    slice(1)
}


# run plot data generation ---- 
path = "https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv"
start_week = 3
end_week = 14

df = read_data(path)
df = select(df, week, team, pwin) |> 
  filter(week %in% start_week:end_week)

# store picked teams and weeks here
picks = data.frame()
plot_data = data.frame()
counter = 0

for(week in unique(df$week)) {
  # print(week)
  
  counter = counter + 1
  frame1 = frame_two_lowest_picks()
  frame1 = frame1 |> mutate(frame = counter)
  
  counter = counter + 1
  frame2 = frame_largest_distance()
  frame2 = frame2 |> mutate(frame = counter)
  
  counter = counter + 1
  frame3 = frame_make_pick()
  frame3 = frame3 |> mutate(frame = counter)
  
  pick = oc_pick()
  picks = rbind(picks, pick)
  
  plot_data = rbind(plot_data, 
                    frame1, 
                    frame2, 
                    frame3)
}

# removing weird data point 
# plot_data = plot_data |> filter(pwin < 1)

p = ggplot(plot_data, aes(x = week, y = pwin, color = distance_bar, label = team)) + 
  geom_line(aes(group = week), size = 2, alpha = 0.7) + 
  geom_point(aes(color = pick), size = 3) + 
  scale_color_manual(values = c("white", "grey50", "#481567FF"), limits = c("hide", "show", "highlight")) + 
  geom_text(aes(x = 3, y = 0.4, label = label), hjust = 0, color = "black", size = 6) + 
  geom_text(aes(label = team, x = week, y = pwin, color = pick), nudge_x = 0.4) + 
  scale_x_continuous(breaks = seq(start_week, end_week, 1)) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(legend.position = "none") +
  labs(title = "Opportunity Cost Model Algorithm - Animation", 
       y = "Win Probability",
       x = "Week Number")
  # labs(title = 'Title : {closest_state}') # for frame debugging
  

anim = p + transition_states(frame, 
                             transition_length = 1, 
                             state_length = 10)

# anim # runs faster

# generate gif and save 
oc_anim = animate(anim, fps = 2, height = 450, width = 800)
anim_save("content/blog/2022-09-07-nfl-losers-pool-2022/oc-anim.gif", oc_anim)
