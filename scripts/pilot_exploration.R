# initial pilot data exploration 
# August 24, 2026
# Luke Watson
# connect lab

# load libraries ----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

# get files 
files <- list.files('data', full.names = T)

# load in and concat files
for (file in files) {
  i = which(files == file)
  
  if (i == 1) {
    pd = read_csv(file, show_col_types = F)
  }
  else {
    pda = read_csv(file, show_col_types = F)
    
    pd <- pd |> bind_rows(pda)
  }
}

# trials defined by rat and date, parse apart by rat, then date to assign day/trial no. 
pd <- pd |> 
  mutate(
    rat_ID = str_extract(rat_ID, '[A-Z]\\d+(?=_)'), 
    date = mdy_hms(date), 
    day = day(date), 
    month = month(date)
  ) |> 
  mutate(
    rat_ID = if_else(is.na(rat_ID), 'test', rat_ID)
  )

opd <- pd |> 
  arrange(rat_ID)

trial = numeric()

for (i in 1:nrow(opd)) {
  if (i == 1) {
    counter = 1
  }
  else if (opd$rat_ID[i] != opd$rat_ID[i-1]) {
    counter = 1
  }
  else if(opd$day[i] != opd$day[i-1]) {
    counter = counter + 1
  }
  else if(opd$day[i] == opd$day[i-1] & opd$month[i] != opd$month[i-1]) {
    counter = counter + 1
  }
  
  trial = c(trial, counter)
}

# add trial id
opd <- opd |> 
  add_column(trial_ID = trial)

# arrange by time in level
opd <- opd |> 
  arrange(rat_ID, trial_ID, time_in_level)

# plots -------------------------------------------------------------------

# distribution of pokes by trial for each rat
opd |> 
  count(rat_ID, trial_ID) |> 
  ggplot(aes(x = trial_ID, y = n)) + 
  geom_col(width = .9) + 
  facet_wrap(~rat_ID)

# test rats aren't really doing much and didn't really get tested

# plot screen pokes within trials to measure 'engagement'
opd |> 
  ggplot(aes(x = time_in_level)) + 
  geom_histogram() + 
  theme_bw() + 
  facet_wrap(~trial_ID)

opd |> 
  group_by(rat_ID, trial_ID) |> 
  summarize(
    sd = sd(time_in_level), 
    n = n()
  ) |> 
  view()

# definitely biased toward start of level

# plot pokes by shaping level for each rat
opd |> 
  count(rat_ID, level) |> 
  ggplot(aes(x = level, y = n)) + 
  geom_col(width = .9) + 
  facet_wrap(~rat_ID) + 
  theme_bw()

# some rats haven't really progressed, others seem to make it through each level 
# fine 

# are they improving over time? do the hit distances converge over time? 
opd <- opd |> 
  mutate(
    hit_distance = sqrt((reward_x - poke_x)^2 + (reward_y - poke_y)^2)
  )
  
opd |> 
  ggplot(aes(x = hit_distance, fill = level)) + 
  geom_density(color = NA) + 
  theme_bw() + 
  facet_wrap(~rat_ID)

# kind of remarkably similar hit distance distributions for rats on each level! 
# shaping 1 averages around 4, shaping 2 around 2, and shaping 4/testing around .75

# heatmap plot
opd |> 
  ggplot(aes(color = level)) + 
  geom_point(aes(x = poke_x, y = poke_y)) + 
  geom_point(aes(x = reward_x, y = reward_y), alpha = .3, shape = 17) + 
  geom_segment(
    aes(x = poke_x, xend = reward_x, y = poke_y, yend = reward_y), 
    alpha = .4
  ) + 
  scale_color_viridis_d(option = 'rocket', direction = -1, begin = .2) + 
  theme_void() + 
  coord_fixed() + 
  facet_wrap(~rat_ID) + 
  theme(
    strip.background = element_rect(fill = 'grey'), 
    strip.text = element_text(face = 'bold')
  )

# matches the above histograms, rats appear to hone in on shapes with more experience 
# as evidenced by tighter poke distributions and lower hit distances on later shaping levels
