# initial exploration of the reversal learning and inhibition task data

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(slider)

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

# only have poke data before 10/28/26 update! have to find a way to see if this
# information can be useful at all

# perhaps measuring engagement? 

# clean -------------------------------------------------------------------

# parse rat_ids, dates
poke_data_clean <- poke_data |> 
  mutate(
    rat_ID = factor(str_extract(rat_ID, '[A-Z]\\d+(?=_)')), 
    date = mdy_hms(date), 
    day = day(date), 
    month = month(date)
  ) |> 
  mutate(
    rat_ID = if_else(is.na(rat_ID), 'test', rat_ID)
  ) |> 
  filter(rat_ID != 'test')

# get raw counts of pokes 
poke_data_clean |> 
  count(rat_ID, month, day) |> 
  ggplot(aes(x = rat_ID, y = n, fill = factor(day))) +
  geom_col(position = 'dodge') +
  scale_fill_viridis_d(option = 'magma')

# rate of pokes within trials
poke_data_clean |> 
  group_by(rat_ID, month, day) |> 
  arrange(time_in_level) |> 
  mutate(
    roll_count = slide_index_dbl(
      .x = time_in_level, .i = time_in_level, 
      .f = ~length(.x), .before = 60, .complete = F
    )
  ) |> 
  ungroup() |> 
  ggplot(aes(x = time_in_level, y = roll_count, color = factor(day))) + 
  geom_path() + 
  labs(y = 'Hits / min', x = 'Time in level (s)') +
  scale_color_viridis_d('Day', option = 'magma')+
  facet_grid(rows = vars(rat_ID), cols = vars(day))

ggsave(
  'fig_output/exploration/rl_rolling_hits_min.png', device = 'png', 
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
  'fig_output/exploration/rl_hit_data.png', device = 'png', 
  width = 8, height = 6, units = 'in'
)

# it does appear that rats in chambers 7 and 8 have more freedom to poke at shapes
# that aren't behind the cage bars, should show this to Bhavana
