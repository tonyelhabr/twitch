
# https://github.com/isabellabenabaye/ggplot2-reference/blob/master/ggplot2-theme-elements-reference.R
library(tidyverse)
dir_proj <- '05-worville_transfers'
df <- file.path(dir_proj, 'guardian_transfer_data.csv') %>% read_csv()
df
# df %>% count(flow_id)
# df %>% arrange(flow_id) %>% relocate(flow_id)
# df %>% group_by(previous_league) %>% summarize(across(c(flow_id, transfer_id), max)) %>% arrange(desc(flow_id))
# df %>% skimr::skim()
# df %>% count(transfer_type)
# df %>% count(window_open, transfer_year)

big5_v <- c('1. Bundesliga', 'La Liga', 'Ligue 1', 'Premier League', 'Serie A')
big5 <- 
  tibble(
    league = big5_v,
    league_lab = big5_v %>% str_remove('1[.] ')
  )
big5

red_wv <- rgb(242, 74, 74, maxColorValue = 255)
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

n_player <-
  df %>% 
  inner_join(
    big5 %>% rename_all(~sprintf('new_%s', .x))
  ) %>% 
  filter(transfer_type == 'fee') %>% 
  # filter(price_in_pounds >= 0) %>% 
  count(transfer_year, league = new_league_lab) %>% 
  arrange(league, transfer_year)
n_player

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

theme_set(theme_minimal())
update_geom_defaults(
  'text', 
  list(family = 'Roboto Slab', color = 'white', size = pts(12))
)

update_geom_defaults(
  'point',
  list(
    size = pts(28),
    color = gray_wv,
    fill = red_wv,
    shape = 21
  )
)

p_n_player <-
  n_player %>% 
  ggplot() +
  aes(x = transfer_year, y = n) +
  geom_line(
    size = 0.5,
    color = 'white'
  ) +
  geom_point() +
  geom_text(
    aes(label = n),
    fontface = 'bold',
    size = pts(12)
  ) +
  scale_x_continuous(expand = c(0.075, 0.075)) +
  scale_y_continuous(
    breaks = c(0, 50, 100, 150),
    limits = c(0, 160)
  ) +
  facet_wrap(~league, scales = 'fixed', nrow = 1) +
  theme(
    plot.margin = margin(20, 50, 60, 50),
    
    text = element_text(family = 'Roboto Slab', color = 'white'),
    
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 12, face = 'bold', hjust = 0.5),
    
    axis.title = element_text(family = 'IBM Plex Sans', face = 'bold', size = 12),
    plot.tag = element_text(face = 'bold', size = 34, hjust = 0),
    plot.tag.position = c(-0.04, -0.08),
    
    panel.grid.minor = element_blank(),
    panel.spacing = margin(20, 20, 20, 20),
    
    strip.text = element_text(color = 'white', face = 'bold', size = 12, family = 'IBM Plex Sans'),
    axis.text = element_text(color = 'white', size = 10, family = 'IBM Plex Sans'),
    
    panel.grid.major = element_line(linetype = '44', size = 0.4, color = gray_grid_wv),
    plot.background = element_rect(fill = gray_wv, color = gray_wv),
    panel.background = element_rect(fill = gray_wv, color = gray_wv)
  ) +
  labs(
    title = 'Fee-costing moves are mostly down, but rebounded in Germany',
    subtitle = 'All moves in the summer transfer window with a fee in the top five European leagues from 2017 to 2021',
    tag = '(not) The Athletic',
    y = 'Total players bought for a fee',
    x = NULL
  )
p_n_player

ggsave(
  plot = p_n_player,
  filename = file.path(dir_proj, 'viz-n_player.png'),
  width = 12,
  height = 12/1.5
)


epl <-
  df %>% 
  filter(new_league == 'Premier League') %>% 
  # filter(price_in_pounds > 0)
  filter(transfer_type == 'fee')
epl
epl %>% arrange(desc(price_in_pounds)) %>% select(player_name, transfer_year, price_in_pounds)

p_epl <-
  epl %>% 
  select(transfer_year, price_in_pounds) %>% 
  drop_na(price_in_pounds) %>% 
  mutate(
    across(price_in_pounds, ~round(.x / 1000000, 0))
  ) %>% 
  ggplot() +
  aes(x = price_in_pounds) +
  geom_dotplot(
    binwidth = 1,
    color = 'white',
    fill = red_wv
  ) +
  scale_x_continuous(
    breaks = c(0, 30, 60, 90),
    limit = c(0, 100)
  ) +
  facet_wrap(~transfer_year, scales = 'fixed', ncol = 1) +
  theme(
    plot.margin = margin(20, 50, 60, 50),
    
    axis.text.y = element_blank(),
    
    text = element_text(family = 'Roboto Slab', color = 'white'),
    
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 12, face = 'bold', hjust = 0.5),
    
    axis.title = element_text(family = 'IBM Plex Sans', face = 'bold', size = 12),
    
    plot.tag = element_text(face = 'bold', size = 34, hjust = 0),
    plot.tag.position = c(-0.06, -0.08),

    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    
    strip.text = element_text(color = 'white', face = 'bold', size = 10, family = 'IBM Plex Sans'),
    axis.text = element_text(color = 'white', size = 10, family = 'IBM Plex Sans'),

    plot.background = element_rect(fill = gray_wv, color = gray_wv),
    panel.background = element_rect(fill = gray_wv, color = gray_wv)
  ) +
  labs(
    title = 'The outlook of Premier League spending changed this season',
    subtitle = 'All moves in the summer transfer window with a fee in the Premier League from 2017 to 2021',
    tag = '(not) The Athletic',
    y = NULL,
    x = 'Transfer fee (£millions)'
  )
p_epl
ggsave(
  plot = p_epl,
  filename = file.path(dir_proj, 'p-epl.png'),
  width = 10,
  height = 10/1.5
)



# weird ----
blue_wv <- rgb(0, 133, 202, maxColorValue = 255)
yellow_wv <- rgb(255, 183, 0, maxColorValue = 255)

inner_join(
  big5 %>% rename_all(~sprintf('new_%s', .x))
)

tibble(
  lab = c('Bought', 'Loaned in', 'Loaned in', 'Promoted from academy', 'Free transfer'),
  transfer_type = c('fee', 'loan', 'loan_extended', 'undisclosed', 'free')
)
df %>% count(previous_club == new_club)
df %>% filter(player_name %>% str_detect('Chal'))
df %>% count(transfer_id, sort = TRUE)
# df %>% filter(player_name == 'Trevoh Chalobah') %>% glimpse()
df %>% filter(player_name == 'Harvey Elliott') %>% glimpse()
df %>% filter(new_club == 'Chelsea') %>% filter(transfer_year == 2020) # %>% glimpse()

  select(where(is.character)) %>% 
  count(big_deal)

fracs <-
  df %>% 
  inner_join(
    big5 %>% rename_all(~sprintf('new_%s', .x))
  ) %>% 
  count(transfer_year, league = new_league_lab, transfer_type) %>% 
  group_by(transfer_year, league) %>% 
  mutate(frac = n / sum(n)) %>% 
  ungroup()
fracs

fracs %>% 
  ggplot() +
  aes(y = frac, x = transfer_year, fill = transfer_type) +
  geom_col(position = 'stack') +
  facet_wrap(~league, scales = 'fixed', nrow = 1)

