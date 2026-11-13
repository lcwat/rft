# initial exploration of the reversal learning and inhibition task data

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(slider)
source('util/rft_theme.r')

# load data ---------------------------------------------------------------

# grab data from rl folder
files <- list.files('data/rl', full.names = T)

# separate all pokes from hits and glue them together
for (file in files) {
  
  i = which(file == files)
  
  # get file type
  type = str_extract(file, '[a-z]+_[a-z]+(?=.txt)')
  
  # get chamber no.
  chamber = str_extract(file, 'PLAKKE\\d')
  
  # determine which to stick together
  if (type == 'all_pokes') {
    if (i == 1) {
      poke_data = read_csv(file, show_col_types = F)
      poke_data = poke_data |> 
        add_column(chamber = chamber)
    }
    else {
      rat_poke = read_csv(file, show_col_types = F)
      rat_poke = rat_poke |> 
        add_column(chamber = chamber)
      poke_data = poke_data |> bind_rows(rat_poke)
    }
  }
  else if (type == 'hit_data') {
    if (i == 2) {
      hit_data = read_csv(file, show_col_types = F)
      hit_data = hit_data |> 
        add_column(chamber = chamber) |> 
        mutate(rat_ID = as.character(rat_ID))
    }
    else {
      rat_hit = read_csv(file, show_col_types = F)
      rat_hit = rat_hit |> 
        mutate(rat_ID = as.character(rat_ID)) |> 
        add_column(chamber = chamber)
      hit_data = hit_data |> bind_rows(rat_hit)
    }
  }
  
  print(i)
}

# only have poke data before 10/28/26 update for rl!

# clean -------------------------------------------------------------------

# parse rat_ids, dates
poke_data_clean <- poke_data |> 
  mutate(
    rat_ID = factor(str_extract(rat_ID, '[A-Z]\\d+(?=_)')), 
    date = mdy_hms(date), 
    day = day(date), 
    month = month(date)
  ) |> 
  group_by(rat_ID) |> 
  mutate(
    session = consecutive_id(month, day) # assign unique ids for sessions
  ) |> 
  ungroup() |> 
  mutate(
    rat_ID = if_else(is.na(rat_ID), 'test', rat_ID)
  ) |> 
  filter(rat_ID != 'test')

# clean up hit data
hit_data_clean <- hit_data |> 
  mutate(
    rat_ID = factor(str_extract(rat_ID, '[A-Z]\\d+(?=_)')), 
    date = mdy_hms(date), 
    day = day(date), 
    month = month(date)
  ) |> 
  group_by(rat_ID) |> 
  mutate(
    session = consecutive_id(month, day) # assign unique ids for sessions
  ) |> 
  ungroup() |> 
  group_by(rat_ID, session) |> 
  mutate(
    trial = consecutive_id(time_in_level) # assign unique ids for trials
  ) |> 
  ungroup() |> 
  mutate(
    rat_ID = if_else(is.na(rat_ID), 'test', rat_ID)
  ) |> 
  filter(rat_ID != 'test')


# pokes only --------------------------------------------------------------

# get raw counts of pokes 
poke_data_clean |> 
  count(rat_ID, session) |> 
  ggplot(aes(x = rat_ID, y = n, fill = factor(session))) +
  geom_col(position = 'dodge') +
  labs(y = 'count', x = 'rat') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d('Session', option = 'magma') + 
  rft_theme()

ggsave('fig_output/exploration/rl/count_pokes_by_session.png', device = 'png', units = 'in', width = 6, height = 7)

# rate of pokes within trials
poke_data_clean |> 
  group_by(rat_ID, session) |> 
  arrange(time_in_level) |> 
  mutate(
    roll_count = slide_index_dbl(
      .x = time_in_level, .i = time_in_level, 
      .f = ~length(.x), .before = 60, .complete = F
    )
  ) |> 
  ungroup() |> 
  ggplot(aes(x = time_in_level, y = roll_count, color = factor(session))) + 
  geom_path() + 
  labs(y = 'Hits / min', x = 'Time in level (s)') +
  scale_color_viridis_d(guide = 'none', option = 'magma', end = .8)+
  facet_grid(rows = vars(rat_ID), cols = vars(session), scales = 'free_y') +
  rft_theme()

ggsave(
  'fig_output/exploration/rl/rl_rolling_hits.png', device = 'png', 
  width = 10, height = 8, units = 'in'
)

# get locations of pokes
poke_data_clean |> 
  ggplot(aes(x = poke_x, y = poke_y, color = time_in_level)) + 
  geom_point(alpha = .3) + 
  scale_x_continuous('x', breaks = seq(-8, 8)) + 
  scale_y_continuous('', breaks = NULL) +
  coord_fixed(xlim = c(-8, 8), ylim = c(-5, 5)) + 
  scale_color_viridis_c('Time in game (s)', option = 'magma') + 
  facet_wrap(~chamber)

ggsave(
  'fig_output/exploration/inhib_hit_data.png', device = 'png', 
  width = 8, height = 6, units = 'in'
)

# it does appear that rats in chambers 7 and 8 have more freedom to poke at shapes
# that aren't behind the cage bars, should show this to Bhavana

# hits --------------------------------------------------------------------

# see how many rewards were realized
hit_data_clean |> 
  filter(!is.na(reward)) |> 
  count(rat_ID, session, object_id)

# visualize rolling distractor and reward hits for all rats

# has to be both object and hit to count
hit_data_clean |> 
  group_by(rat_ID, session, reward) |>
  filter(hit == 'hit') |> 
  arrange(time_in_level) |> 
  mutate(
    roll_count = slide_index_dbl(
      .x = time_in_level, .i = time_in_level, 
      .f = ~length(.x), .before = 60, .complete = F
    ), 
    session_labelled = glue::glue('session {session}')
  ) |> 
  ggplot(aes(x = time_in_level, y = roll_count, color = reward)) + 
  geom_path() + 
  labs(
    x = 'Time (s)', 
    y = 'Rolling count / min'
  ) +
  scale_color_manual(values = c('orange2', 'blue')) + 
  facet_grid(rows = vars(rat_ID), cols = vars(session_labelled)) + 
  rft_theme()

# save
ggsave('fig_output/exploration/rl/rl_rolling_reward_distractor_hits.png', device = 'png', width = 8, height = 8, units = 'in')

# rolling proportion of pokes that are successful hits of reward
poke_data_roll <- poke_data_clean |> 
  group_by(rat_ID, session) |> 
  arrange(time_in_level) |> 
  mutate(
    poke_roll = slide_index_dbl(
      .x = time_in_level, .i = time_in_level, 
      .f = ~length(.x), .before = 60, .complete = F
    )
  ) |> 
  ungroup()

hit_data_roll <- hit_data_clean |> 
  group_by(rat_ID, session, reward) |> 
  filter(hit == 'hit') |> 
  arrange(time_in_level) |> 
  mutate(
    hit_roll = slide_index_dbl(
      .x = time_in_level, .i = time_in_level, 
      .f = ~length(.x), .before = 60, .complete = F
    )
  ) |> 
  ungroup()

# combine
poke_hit_roll <- poke_data_roll |> 
  left_join(hit_data_roll, join_by(rat_ID, session, time_in_level, poke_x, poke_y, level))

# clean up
poke_hit_roll <- poke_hit_roll |> 
  mutate(
    hit_roll = if_else(is.na(hit_roll), 0, hit_roll)
  ) |> 
  group_by(rat_ID, session) |> 
  mutate(
    hit_prop = hit_roll / poke_roll
  ) |> 
  ungroup()

# plot
poke_hit_roll |> 
  filter(!is.na(reward)) |> 
  mutate(
    session_label = str_c('session ', session)
  ) |> 
  ggplot(aes(x = time_in_level, y = hit_prop, color = reward)) + 
  geom_path() + 
  scale_color_manual(values = c('orange2', 'blue')) +
  labs(
    x = 'Time (s)', 
    y = 'Rolling P(Hit | Poke)'
  ) + 
  facet_grid(rows = vars(rat_ID), cols = vars(session_label))

ggsave('fig_output/exploration/inhib_rolling_hit_prob.png', device = 'png', width = 6, height = 8, units = 'in')

# visualize hits distance relative to distractor

# need trials with hit rewards 
hit_dists <- hit_data_clean |> 
  group_by(rat_ID, session, trial) |> 
  mutate(
    reward_hit = if_else(any(hit == 'hit' & reward == 'rewarded'), T, F),
    distractor_hit = if_else(any(hit == 'hit' & reward == 'punished'), T, F),
    hit_dist_from_distractor = sqrt((poke_x - object_x)^2 + (poke_y - object_y)^2)
  ) |> 
  ungroup()

hit_dists |> 
  filter(reward_hit & hit == 'not_hit' & reward == 'punished') |> 
  mutate(
    session_label = str_c('session ', session)
  ) |> 
  ggplot(aes(x = hit_dist_from_distractor)) + 
  geom_density(fill = 'blue', color = NA) + 
  labs(
    x = 'Reward hit distance from distractor'
  ) + 
  facet_grid(rows = vars(rat_ID), cols = vars(session_label))

ggsave('fig_output/exploration/inhib_reward_dist_distributions.png', device = 'png', width = 6, height = 8)  
  
# hit plot
hit_dists |> 
  mutate(
    session_label = str_c('session ', session)
  ) |> 
  filter(reward_hit & hit == 'hit') |> 
  ggplot() + 
  geom_segment(
    aes(x = poke_x, y = poke_y, xend = object_x, yend = object_y), 
    alpha = .3,
    color = 'blue'
  ) + 
  geom_point(
    aes(x = object_x, y = object_y), 
    alpha = .3, 
    color = 'blue', 
    fill = 'blue',
    shape = 23
  ) +
  coord_fixed() + 
  facet_grid(rows = vars(rat_ID), cols = vars(session_label))

ggsave('fig_output/exploration/inhib_reward_hit_lines.png', device = 'png', width = 6, height = 8)  

hit_dists |> 
  mutate(
    session_label = str_c('session ', session)
  ) |> 
  filter(distractor_hit & hit == 'hit') |> 
  ggplot() + 
  geom_segment(
    aes(x = poke_x, y = poke_y, xend = object_x, yend = object_y), 
    alpha = .5,
    color = 'orange2'
  ) + 
  geom_point(
    aes(x = object_x, y = object_y), 
    alpha = .5, 
    color = 'orange2',
    shape = 10
  ) +
  coord_fixed() + 
  facet_grid(rows = vars(rat_ID), cols = vars(session_label))

ggsave('fig_output/exploration/inhib_distractor_hit_lines.png', device = 'png', width = 6, height = 8)

# rl specific vis ---------------------------------------------------------

# since this is trial based, makes more sense to track cumulative reward and 
# punishers

# assign ids to hits within sequences
hit_seqs <- hit_data_clean |> 
  group_by(rat_ID, session, trial) |>
  mutate(
    reward_hit = if_else(
      hit == 'hit' & reward == 'rewarded', 
      1, 
      if_else(hit == 'hit' & reward == 'punished', -1, 0)
    ),
    reward_obj = if_else(reward_hit == 1, object_id, NA)
  ) |> 
  ungroup() |> 
  group_by(rat_ID, session) |> 
  mutate(
    reward_id = cumsum(reward_hit)
  ) |> 
  ungroup() |> 
  group_by(rat_ID, session, trial) |> 
  fill(reward_obj, .direction = 'downup') |> 
  mutate(
    reward_id = max(reward_id, na.rm = T)
  ) |> 
  ungroup() |> 
  group_by(rat_ID, session) |> 
  fill(reward_obj, .direction = 'downup') |> 
  mutate(
    switch = if_else(
      reward_obj != lag(reward_obj, default = '') & reward_id != lag(reward_id, default = 0) & trial > 1,
      1, 
      0
    )
  ) |> 
  ungroup()

# most rats take longer to reach the reward switch contingency, means that 
# they should know which is the reward for a long time but still making mistakes, 
# and the game strictly requires 6 consecutive reward trials to switch reward
hit_seqs |> 
  group_by(rat_ID, session) |> 
  mutate(switch_no = cumsum(switch)) |> 
  filter(switch == 1 & switch_no == 1) |> 
  ggplot(aes(x = trial)) + 
  geom_bar()

hit_seqs |> 
  ggplot(aes(x = trial, y = reward_id)) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = 'Trial', y = 'Cumulative Reward') +
  rft_theme() +
  facet_grid(rows = vars(rat_ID), cols = vars(session))

hit_seqs |> 
  group_by(rat_ID, session) |> 
  mutate(
    switch_num = cumsum(switch)
  ) |> 
  
  

# could also look at trial based reward hits starting when they find the correct 
# reward, then track their distractor hits, aggregate across sessions and rats
# to see if they are getting better at exploiting reward

# perseverative errors, specifically, after switch of rule and finding new reward, 
# do they return to the previous rewarded stimulus? 
