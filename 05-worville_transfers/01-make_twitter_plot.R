
# https://github.com/isabellabenabaye/ggplot2-reference/blob/master/ggplot2-theme-elements-reference.R
library(tidyverse)
dir_proj <- '05-worville_transfers'
df <- file.path(dir_proj, 'guardian_transfer_data.csv') %>% read_csv()
df
df %>% count(flow_id)
df %>% arrange(flow_id) %>% relocate(flow_id)
df %>% group_by(previous_league) %>% summarize(across(c(flow_id, transfer_id), max)) %>% arrange(desc(flow_id))
df %>% skimr::skim()
df %>% count(transfer_type)
df %>% count(window_open, transfer_year)

big5_v <- c('1. Bundesliga', 'La Liga', 'Ligue 1', 'Premier League', 'Serie A')
big5 <- 
  tibble(
    league = big5_v,
    league_lab = big5_v %>% str_remove('1[.] ')
  )
big5

red_wv <- rgb(231, 80, 73, maxColorValue = 255)
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
update_geom_defaults('text', list(family = 'Roboto Slab', color = 'white', size = pts(12)))
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
    # labels = c(0, 50, 100, 150),
    breaks = c(0, 50, 100, 150),
    limits = c(0, 160)
    # expand = c(0.1, 0.1)
  ) +
  facet_wrap(~league, scales = 'fixed', nrow = 1) +
  theme(
    plot.margin = margin(20, 50, 60, 50),
    # text = element_text(color = 'white'),
    text = element_text(family = 'Roboto Slab', color = 'white'),
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 12, face = 'bold', hjust = 0.5),
    axis.title = element_text(family = 'IBM Plex Sans', face = 'bold', size = 12),
    plot.tag = ggplot2::element_text(face = 'bold', size = 34, hjust = 0),
    # plot.tag.position = 
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
  filename = file.path(dir_proj, 'p-n_player.png'),
  width = 12,
  height = 12/1.5
)
